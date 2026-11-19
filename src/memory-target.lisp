(in-package #:revision-protocol)

;;; In-memory target over sexp states (R0b): the reference REVISION-TARGET for
;;; the law suite. It holds one sexp state and a stamp (list of revision ids);
;;; it applies plan steps with DIFF-PROTOCOL:APPLY-DIFF using its ENGINE.
;;;
;;; Methods below are written against the public protocol where they can;
;;; the slots hold the state, the stamp and the test hooks.

(defclass in-memory-target (revision-target)
  ((state :initarg :state :initform nil :accessor %state)
   (revisions :initarg :revisions :initform nil :accessor %revisions-stamp)
   (engine :initarg :engine :reader %target-engine)
   (transactional :initarg :transactional :initform t :reader %transactional)
   (fidelities :initarg :fidelities :initform '(:exact :shadow :model)
               :reader %fidelities)
   (fail-step :initarg :fail-step :initform nil :reader %fail-step)
   (stamp-hook :initarg :stamp-hook :initform nil :reader %stamp-hook))
  (:documentation "Reference REVISION-TARGET holding a sexp state in memory."))

(defun in-memory-target-p (x)
  "T when X is an IN-MEMORY-TARGET."
  (typep x 'in-memory-target))

(defun make-in-memory-target (&key state revisions
                                (engine (make-instance 'diff:sexp-diff-engine))
                                (transactional t)
                                (fidelities '(:exact :shadow :model))
                                fail-step stamp-hook)
  "A fresh IN-MEMORY-TARGET.
STATE: initial sexp state (copied). REVISIONS: initial stamp (ids), not
checked against any store. ENGINE: DIFF-PROTOCOL engine (default sexp).
TRANSACTIONAL: TARGET-TRANSACTIONAL-P. FIDELITIES: TARGET-FIDELITIES, a subset
of (:EXACT :SHADOW :MODEL) kept in that order.
In-memory fidelities: :EXACT and :SHADOW apply the plan step diffs to a copy of
the target state; :MODEL is the store model of the goal (REVISION-STATE of
PLAN-GOAL), not simulated on the target. FAIL-STEP: NIL or a function
(STEP ATTEMPT) called before each attempt to apply a step; an error it signals
is a step failure (STEP-FAILED with that CAUSE) — fault injection for tests.
STAMP-HOOK: NIL or a function (TARGET IDS) called after every stamp
EXECUTE-PLAN makes."
  (check-type revisions list)
  (check-type engine diff:diff-engine)
  (check-type fidelities list)
  (assert (subsetp fidelities '(:exact :shadow :model)) (fidelities)
          'type-error :datum fidelities
                      :expected-type '(member :exact :shadow :model))
  (check-type fail-step (or null function))
  (check-type stamp-hook (or null function))
  (make-instance 'in-memory-target
                 :state (copy-tree state)
                 :revisions (copy-list revisions)
                 :engine engine
                 :transactional transactional
                 :fidelities (remove-if-not (lambda (f) (member f fidelities))
                                            '(:exact :shadow :model))
                 :fail-step fail-step
                 :stamp-hook stamp-hook))

;;; ------------------------------------------------------------------
;;; State, stamp, capabilities
;;; ------------------------------------------------------------------

(defmethod target-revisions ((target in-memory-target))
  (copy-list (%revisions-stamp target)))

(defun %stamp-id (x)
  "Revision id of X, a REVISION or an id string."
  (etypecase x
    (revision (revision-id x))
    (string (copy-seq x))))

(defmethod target-stamp ((target in-memory-target) revisions)
  (check-type revisions list)
  (setf (%revisions-stamp target)
        (remove-duplicates (mapcar #'%stamp-id revisions)
                           :test #'equal :from-end t))
  (target-revisions target))

(defmethod target-state ((target in-memory-target))
  (copy-tree (%state target)))

(defmethod (setf target-state) (state (target in-memory-target))
  (setf (%state target) (copy-tree state))
  state)

(defmethod target-fidelities ((target in-memory-target))
  (copy-list (%fidelities target)))

(defmethod target-transactional-p ((target in-memory-target))
  (and (%transactional target) t))

;;; ------------------------------------------------------------------
;;; Drift, capture, transactions
;;; ------------------------------------------------------------------

(defun %stamped-state (target store)
  "State of TARGET's stamped revision in STORE; NIL when unstamped.
REVISION-ERROR when TARGET is stamped with several revisions."
  (let ((ids (%revisions-stamp target)))
    (when (rest ids)
      (error 'revision-error
             :message (format nil "target is stamped with several revisions ~s"
                              ids)))
    (and ids (revision-state store (first ids)))))

(defmethod target-drift ((target in-memory-target) store)
  (let ((engine (content-engine store))
        (stamped (%stamped-state target store))
        (actual (%state target)))
    (unless (diff:state-equal engine stamped actual)
      (diff:compute-diff engine stamped actual :level :changes))))

(defmethod capture-revision ((target in-memory-target) store
                             &key (parents nil parents-p) message provenance
                               valid-at id)
  (let ((revision (commit-revision store (copy-tree (%state target))
                                   :parents (if parents-p
                                                parents
                                                (target-revisions target))
                                   :message message
                                   :provenance provenance
                                   :valid-at valid-at
                                   :id id)))
    (setf (%revisions-stamp target) (list (revision-id revision)))
    revision))

(defmethod call-with-target-transaction ((target in-memory-target) thunk)
  (unless (%transactional target)
    (error 'revision-error :message "the target is not transactional"))
  (let ((state (copy-tree (%state target)))
        (stamp (copy-list (%revisions-stamp target)))
        (done nil))
    (unwind-protect
         (multiple-value-prog1 (funcall thunk)
           (setf done t))
      (unless done
        (setf (%state target) state
              (%revisions-stamp target) stamp)))))

;;; ------------------------------------------------------------------
;;; Plan execution
;;; ------------------------------------------------------------------

(defun %remaining-steps (plan stamp)
  "Steps of PLAN still to run on a target stamped STAMP: all of them at
PLAN-START, the steps after the one whose TO is STAMP otherwise.
REVISION-ERROR for a stamp that is neither."
  (let ((steps (plan-steps plan)))
    (if (equal stamp (plan-start plan))
        steps
        (loop for tail on steps
              when (equal stamp (let ((to (step-to (first tail))))
                                  (and to (list to))))
                return (rest tail)
              finally (error 'revision-error
                             :message (format nil "target stamp ~s is neither the plan's start nor any step's TO"
                                              stamp))))))

(defun %stamp-step (target step)
  "Stamp TARGET with STEP's TO (NIL: the empty state); run the stamp hook."
  (let ((to (step-to step)))
    (setf (%revisions-stamp target) (and to (list to)))
    (let ((hook (%stamp-hook target)))
      (when hook
        (funcall hook target (target-revisions target))))))

(defun %try-step (target step attempt)
  "Apply STEP's diff to TARGET → NIL, or the error that prevented it (the
target state is then unchanged). The FAIL-STEP hook runs first."
  (handler-case
      (let ((hook (%fail-step target)))
        (when hook
          (funcall hook step attempt))
        (setf (%state target)
              (values (diff:apply-diff (%state target) (step-diff step))))
        nil)
    (error (cause) cause)))

(defmethod execute-plan ((target in-memory-target) plan)
  (let* ((store (plan-store plan))
         (start-state (copy-tree (%state target)))
         (start-stamp (copy-list (%revisions-stamp target)))
         (steps (%remaining-steps plan start-stamp))
         (skipped '()))
    (let ((drift (target-drift target store)))
      (when drift
        (restart-case (error 'target-drift-detected :target target :drift drift)
          (capture-and-continue (&key id (message "captured drift"))
            :report "Capture the drifted state as a revision, re-plan from it and run that."
            (capture-revision target store :id id :message message)
            (return-from execute-plan
              (execute-plan target (plan-transition store target
                                                    (first (plan-goal plan))))))
          (discard-drift ()
            :report "Reset the target to its stamped revision's state, then run."
            (setf (%state target) (%stamped-state target store)))
          (abort ()
            :report "Run nothing; return NIL."
            (return-from execute-plan nil)))))
    (dolist (step steps (values (target-revisions target) (nreverse skipped)))
      (block one-step
        (unless (step-reversible-p step)
          (restart-case (error 'irreversible-step :step step)
            (skip-step ()
              :report "Leave the state and the stamp; continue with the next step."
              (push step skipped)
              (return-from one-step))
            (stamp-only ()
              :report "Stamp the step's TO without changing the state; continue."
              (%stamp-step target step)
              (return-from one-step))
            (abort-transition ()
              :report "Stop here; return NIL."
              (return-from execute-plan nil))))
        (loop for attempt from 1
              for cause = (%try-step target step attempt)
              while cause
              do (restart-case (error 'step-failed :step step :cause cause
                                                   :attempt attempt)
                   (retry-step ()
                     :report "Apply the step again."
                     nil)
                   (skip-step ()
                     :report "Leave the state and the stamp; continue with the next step."
                     (push step skipped)
                     (return-from one-step))
                   (rollback-to-start ()
                     :report "Restore the state and stamp from before this run; return NIL."
                     (setf (%state target) start-state
                           (%revisions-stamp target) start-stamp)
                     (return-from execute-plan nil))))
        (%stamp-step target step)))))

;;; ------------------------------------------------------------------
;;; Plan projections
;;; ------------------------------------------------------------------

(defun %plan-projection (target plan fidelity)
  "MEMORY-PROJECTION of TARGET after PLAN at FIDELITY, stale-checked against
TARGET's stamp and state now."
  (let* ((stamp (copy-list (%revisions-stamp target)))
         (seen (copy-tree (%state target)))
         (state (ecase fidelity
                  ((:exact :shadow)
                   ;; The steps EXECUTE-PLAN would run, on a throwaway copy.
                   (let ((s (copy-tree seen)))
                     (dolist (step (%remaining-steps plan stamp) s)
                       (setf s (values (diff:apply-diff s (step-diff step)))))))
                  (:model
                   (revision-state (plan-store plan) (first (plan-goal plan)))))))
    (make-instance 'memory-projection
                   :state state
                   :fidelity fidelity
                   :parents (plan-goal plan)
                   :engine (%target-engine target)
                   :target target
                   :seen-stamp stamp
                   :seen-state seen)))

(defmethod project-plan ((target in-memory-target) plan &key fidelity)
  (check-type fidelity (member nil :exact :shadow :model))
  (let* ((supported (target-fidelities target))
         (wanted (or fidelity (first supported))))
    (if (member wanted supported)
        (%plan-projection target plan wanted)
        (restart-case (error 'projection-unsupported :fidelity wanted
                                                     :supported supported)
          (use-shadow ()
            :test (lambda (c) (declare (ignore c)) (member :shadow supported))
            :report "Project at :SHADOW fidelity instead."
            (%plan-projection target plan :shadow))
          (accept-model-fidelity ()
            :test (lambda (c) (declare (ignore c)) (member :model supported))
            :report "Project at :MODEL fidelity instead."
            (%plan-projection target plan :model))
          (apply-for-real ()
            :report "Execute the plan on the target, then project its state exactly."
            (when (execute-plan target plan)
              (make-instance 'memory-projection
                             :state (copy-tree (%state target))
                             :fidelity :exact
                             :parents (plan-goal plan)
                             :engine (%target-engine target))))))))
