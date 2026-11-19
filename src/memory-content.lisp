(in-package #:revision-protocol)

;;; R0b methods of IN-MEMORY-STORE and of diff values: content, state-diff,
;;; plans, projections, composite revisions.
;;;
;;; Content is classified when it is read, not at commit: a revision keeps
;;; CONTENT exactly as given (REVISION-CONTENT is EQ to it), and the missing
;;; form is derived on demand. Derivation is a pure function of an immutable
;;; first-parent chain and the store's engine, so nothing is written back.
;;;
;;; Locking: %WITH-STORE is used only to snapshot revisions with %-helpers
;;; (%RESOLVE, %FIRST-PARENT). Every DIFF-PROTOCOL call and every public GF
;;; runs after the lock is released.

;;; ------------------------------------------------------------------
;;; Capability queries
;;; ------------------------------------------------------------------

(defmethod content-primary ((store in-memory-store))
  (%primary store))

(defmethod content-engine ((store in-memory-store))
  (%engine store))

(defmethod permit-force-p ((store in-memory-store) name)
  (let ((gate (%permit-force store)))
    (if (functionp gate)
        (and (funcall gate name) t)
        gate)))

;;; ------------------------------------------------------------------
;;; Content
;;; ------------------------------------------------------------------

(defun %content-parts (content)
  "Classify CONTENT → (values state state-p delta). Pure."
  (typecase content
    (diff:diff (values nil nil content))
    (revision-payload (values (payload-state content)
                              (payload-state-p content)
                              (payload-delta content)))
    ;; Any other value, a REVISION-MANIFEST included, is a state.
    (t (values content t nil))))

(defun %state-chain-locked (store designator)
  "Lock held. → (values base-state chain). CHAIN lists the delta-only
revisions from the nearest first-parent ancestor-or-self that has a state,
oldest first, ending at DESIGNATOR's revision; BASE-STATE is that ancestor's
state (NIL, the empty state, when the chain reaches past the root)."
  (let ((chain '()))
    (loop for r = (%resolve store designator) then (%first-parent store r)
          while r
          do (multiple-value-bind (state state-p) (%content-parts (revision-content r))
               (when state-p
                 (return-from %state-chain-locked (values state chain)))
               (push r chain)))
    (values nil chain)))

(defmethod revision-state ((store in-memory-store) designator)
  (%check-designator designator)
  (multiple-value-bind (state chain)
      (%with-store (store) (%state-chain-locked store designator))
    ;; Outside the lock: fold the deltas, oldest first.
    (dolist (r chain)
      (setf state (values (diff:apply-diff
                           state (nth-value 2 (%content-parts (revision-content r)))))))
    ;; Callers never alias stored structure (a manifest is not a cons: EQ).
    (copy-tree state)))

(defmethod revision-delta ((store in-memory-store) designator)
  (%check-designator designator)
  (multiple-value-bind (revision parent)
      (%with-store (store)
        (let ((r (%resolve store designator)))
          (values r (%first-parent store r))))
    (or (nth-value 2 (%content-parts (revision-content revision))) ; as stored: EQ
        (diff:compute-diff (%engine store)
                           (and parent (revision-state store parent))
                           (revision-state store revision)
                           :level :changes))))

(defmethod delta-inverse ((delta diff:diff))
  ;; HANDLER-CASE unwinds before INVERT-DIFF's SKIP-CHANGE / USE-VALUE could be
  ;; chosen by an outer handler: an opaque change has no inverse, period.
  (handler-case (diff:invert-diff delta)
    (diff:irreversible-change () nil)))

(defmethod delta-footprint ((delta diff:diff))
  (diff:diff-footprint delta))

(defmethod delta-footprint ((delta null))
  nil)

(defun %operand-state (store x)
  "The state STATE-DIFF compares for operand X."
  (typecase x
    (revision-target (target-state x))
    (transition-plan (revision-state (plan-store x) (first (plan-goal x))))
    (projection (projection-state x))
    (t (revision-state store x))))

(defun %filter-diff (diff footprint)
  "DIFF keeping only the top-level changes whose footprint overlaps FOOTPRINT."
  (diff:make-diff (remove-if-not (lambda (c)
                                   (%footprints-overlap-p (diff:change-footprint c)
                                                          footprint))
                                 (diff:diff-changes diff))
                  :engine (diff:diff-engine diff)
                  :level (diff:diff-level diff)))

(defmethod state-diff ((store in-memory-store) a b &key footprint (level :changes))
  (check-type footprint list)
  (let* ((sa (%operand-state store a))
         (sb (%operand-state store b))
         (d (diff:compute-diff (%engine store) sa sb :level level)))
    (if footprint
        (%filter-diff d footprint)
        d)))

;;; ------------------------------------------------------------------
;;; Plans (store side)
;;; ------------------------------------------------------------------

(defun %toward (store id base)
  "Next id from ID toward BASE along one parent edge: the first parent that is
BASE or has BASE as an ancestor; with BASE NIL, the first parent (NIL past the
root)."
  (let ((parents (revision-parent-ids (resolve-revision store id))))
    (if base
        (find-if (lambda (p) (ancestor-p store base p)) parents)
        (first parents))))

(defun %path-to-base (store id base)
  "(ID … BASE), one parent edge per element. BASE NIL: ends with NIL."
  (loop for x = id then (%toward store x base)
        collect x
        until (or (null x) (equal x base))))

(defun %plan-steps-along (store direction ids states)
  "PLAN-STEPs between consecutive elements of IDS. STATES: id → state cache."
  (flet ((state-of (id)
           (and id
                (multiple-value-bind (s found) (gethash id states)
                  (if found
                      s
                      (setf (gethash id states) (revision-state store id)))))))
    (loop for tail on ids
          for from = (first tail)
          for to = (second tail)
          while (rest tail)
          collect (make-instance 'plan-step
                                 :direction direction
                                 :from from
                                 :to to
                                 :diff (diff:compute-diff (%engine store)
                                                          (state-of from)
                                                          (state-of to)
                                                          :level :changes)
                                 :reversible-p
                                 (or (eq direction :up)
                                     (and (delta-inverse (revision-delta store from))
                                          t))))))

(defmethod plan-transition ((store in-memory-store) target to)
  (let ((start (target-revisions target)))
    (when (rest start)
      (error 'revision-error
             :message (format nil "target is stamped with several revisions ~s; multi-head plans are not supported"
                              start)))
    (let* ((goal (revision-id (resolve-revision store to)))
           (from (and start (revision-id (resolve-revision store (first start)))))
           (merge (and from (merge-base store from goal)))
           (base (and merge (revision-id merge)))
           (down (if from (%path-to-base store from base) (list nil)))
           (up (reverse (%path-to-base store goal base)))
           (states (make-hash-table :test 'equal)))
      (make-instance 'transition-plan
                     :store store
                     :start (copy-list start)
                     :goal (list goal)
                     :base base
                     :steps (append (%plan-steps-along store :down down states)
                                    (%plan-steps-along store :up up states))))))

(defmethod step-footprint ((step plan-step))
  (delta-footprint (step-diff step)))

(defun %write-plan (plan stream)
  (dolist (step (plan-steps plan))
    (format stream "~(~a~) ~a -> ~a~{ ~a~}~:[ (irreversible)~;~]~%"
            (step-direction step)
            (or (step-from step) "-")
            (or (step-to step) "-")
            (step-footprint step)
            (step-reversible-p step))))

(defmethod describe-plan ((plan transition-plan) stream)
  (cond ((null stream)
         (with-output-to-string (out) (%write-plan plan out)))
        ((eq stream t)
         (%write-plan plan *standard-output*)
         nil)
        (t
         (%write-plan plan stream)
         nil)))

;;; ------------------------------------------------------------------
;;; Projections
;;; ------------------------------------------------------------------

(defclass memory-projection (projection)
  ((state :initarg :state :initform nil :accessor %projected-state)
   (fidelity :initarg :fidelity :reader projection-fidelity)
   (parents :initarg :parents :initform nil :reader %projected-parents)
   (conflicts :initarg :conflicts :initform nil :reader %projected-conflicts)
   (writable :initarg :writable :initform nil :reader projection-writable-p)
   (discarded :initform nil :accessor %discarded)
   (engine :initarg :engine :reader %projection-engine)
   ;; Staleness, for target projections only (NIL target: never stale).
   (target :initarg :target :initform nil :reader %projected-target)
   (seen-stamp :initarg :seen-stamp :initform nil :reader %seen-stamp)
   (seen-state :initarg :seen-state :initform nil :reader %seen-state))
  (:documentation "In-memory PROJECTION: a sexp state with its parents,
conflicts (merge projections) and, for a target projection, the target's stamp
and state when it was made."))

(defun %live-projection (p)
  "P, after checking it is neither discarded nor stale."
  (when (%discarded p)
    (error 'revision-error :message "the projection was discarded"))
  (let ((target (%projected-target p)))
    (when (and target
               (not (and (equal (%seen-stamp p) (target-revisions target))
                         (diff:state-equal (%projection-engine p)
                                           (%seen-state p)
                                           (target-state target)))))
      (error 'projection-stale :projection p)))
  p)

(defmethod projection-state ((p memory-projection))
  (copy-tree (%projected-state (%live-projection p))))

(defmethod projection-view ((p memory-projection))
  (projection-state p))

(defmethod (setf projection-state) (state (p memory-projection))
  (unless (projection-writable-p p)
    (error 'revision-error :message "the projection is not writable"))
  (%live-projection p)
  (setf (%projected-state p) (copy-tree state))
  state)

(defmethod projection-parents ((p memory-projection))
  (copy-list (%projected-parents p)))

(defmethod projection-conflicts ((p memory-projection))
  (copy-list (%projected-conflicts p)))

(defmethod projection-discarded-p ((p memory-projection))
  (%discarded p))

(defmethod discard-projection ((p memory-projection))
  (setf (%discarded p) t))

(defmethod project-state ((store in-memory-store) designator
                          &key fidelity writable)
  (declare (ignore fidelity))           ; in memory: always :EXACT
  (let ((id (revision-id (resolve-revision store designator))))
    (make-instance 'memory-projection
                   :state (revision-state store id)
                   :fidelity :exact
                   :parents (list id)
                   :writable (and writable t)
                   :engine (%engine store))))

(defmethod project-merge ((store in-memory-store) a b)
  (let* ((ida (revision-id (resolve-revision store a)))
         (idb (revision-id (resolve-revision store b)))
         (base (merge-base store ida idb))
         (engine (%engine store))
         (base-state (and base (revision-state store base)))
         (ours (revision-state store ida))
         (theirs (revision-state store idb)))
    (multiple-value-bind (merged conflicts)
        (diff:three-way-diff engine base-state ours theirs)
      (let ((p (make-instance 'memory-projection
                              :state (values (diff:apply-diff base-state merged))
                              :fidelity :exact
                              :parents (list ida idb)
                              :conflicts (copy-list conflicts)
                              :engine engine)))
        (values p (copy-list conflicts))))))

(defun %resolve-conflicts (state conflicts side engine)
  "STATE with the SIDE (CONFLICT-OURS / CONFLICT-THEIRS) change of each conflict
applied. One change may sit in several conflicts: it is applied once."
  (values (diff:apply-diff state
                           (diff:make-diff (remove-duplicates (mapcar side conflicts)
                                                              :test #'eq :from-end t)
                                           :engine engine))))

(defmethod commit-projection ((store in-memory-store) (projection memory-projection)
                              &key (parents nil parents-p) message ref provenance
                                valid-at id)
  (%live-projection projection)
  (let ((state (copy-tree (%projected-state projection)))
        (conflicts (%projected-conflicts projection))
        (engine (%projection-engine projection)))
    (when conflicts
      (restart-case (error 'merge-conflict :conflicts (copy-list conflicts)
                                           :projection projection)
        (take-ours ()
          :report "Resolve every conflict to our side."
          (setf state (%resolve-conflicts state conflicts #'diff:conflict-ours engine)))
        (take-theirs ()
          :report "Resolve every conflict to their side."
          (setf state (%resolve-conflicts state conflicts #'diff:conflict-theirs engine)))
        (use-merged ()
          :report "Commit as projected (base values at the conflicts)."
          nil)
        (abort ()
          :report "Commit nothing; return NIL."
          (return-from commit-projection nil))))
    (commit-revision store state
                     :parents (if parents-p parents (projection-parents projection))
                     :message message
                     :ref ref
                     :provenance provenance
                     :valid-at valid-at
                     :id id)))

;;; ------------------------------------------------------------------
;;; Composite revisions
;;; ------------------------------------------------------------------

(defmethod commit-composite ((store in-memory-store) parts
                             &key (parents nil parents-p) ref
                               (expected nil expected-p) message provenance
                               valid-at id)
  ;; Absent :PARENTS / :EXPECTED keep their COMMIT-REVISION meaning.
  (let ((manifest (make-manifest parts)))
    (apply #'commit-revision store manifest
           :ref ref :message message
           :provenance provenance :valid-at valid-at :id id
           (append (and parents-p (list :parents parents))
                   (and expected-p (list :expected expected))))))

(defmethod composite-manifest ((store in-memory-store) designator)
  (let ((content (revision-content (resolve-revision store designator))))
    (and (typep content 'revision-manifest) content)))

(defun %composite-parts (store designator)
  "MANIFEST-PARTs of the composite at DESIGNATOR in dependency order."
  (let ((manifest (composite-manifest store designator)))
    (unless manifest
      (error 'revision-error
             :message (format nil "~s is not a composite revision" designator)))
    (%dependency-order (manifest-parts manifest))))

(defun %part-plan (part targets)
  "→ (values plan reason) for PART: a TRANSITION-PLAN, or NIL and :NO-STORE,
:NO-TARGET or :NOT-FOUND."
  (let ((store (part-store part))
        (target (getf targets (part-role part))))
    (cond ((null store) (values nil :no-store))
          ((null target) (values nil :no-target))
          ((null (handler-case (resolve-revision store (part-revision-id part))
                   (revision-not-found () nil)))
           (values nil :not-found))
          (t (values (plan-transition store target (part-revision-id part)) nil)))))

(defmethod plan-composite ((store in-memory-store) designator targets)
  (mapcar (lambda (part) (cons (part-role part) (values (%part-plan part targets))))
          (%composite-parts store designator)))

(defmethod restore-composite ((store in-memory-store) designator targets)
  (let ((out '()))
    (dolist (part (%composite-parts store designator) (nreverse out))
      (let ((target (getf targets (part-role part))))
        (multiple-value-bind (plan reason) (%part-plan part targets)
          (push (part-role part) out)
          (push (if plan
                    (progn (execute-plan target plan)
                           (target-revisions target))
                    (restart-case (error 'composite-part-unavailable
                                         :part part :reason reason)
                      (skip-part ()
                        :report "Leave this part unrestored."
                        :skipped)
                      (rebuild-from-recipe ()
                        :test (lambda (c)
                                (declare (ignore c))
                                (and (part-recipe part) target))
                        :report "Rebuild the part on its target with its recipe."
                        (funcall (part-recipe part) part target)
                        (target-stamp target (list (part-revision-id part)))
                        (target-revisions target))))
                out))))))
