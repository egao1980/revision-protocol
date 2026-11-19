(in-package #:revision-protocol)

;;; In-memory target over sexp states (R0b): the reference REVISION-TARGET for
;;; the law suite. It holds one sexp state and a stamp (list of revision ids);
;;; it applies plan steps with DIFF-PROTOCOL:APPLY-DIFF using its ENGINE.
;;;
;;; Contract stubs: every method signals a plain SIMPLE-ERROR through
;;; %R0B-STUB so the R0b suite loads and fails. The implementation replaces
;;; the bodies and may add slots.

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

(defmethod target-revisions ((target in-memory-target))
  (%r0b-stub 'target-revisions))

(defmethod target-stamp ((target in-memory-target) revisions)
  (declare (ignore revisions))
  (%r0b-stub 'target-stamp))

(defmethod target-state ((target in-memory-target))
  (%r0b-stub 'target-state))

(defmethod (setf target-state) (state (target in-memory-target))
  (declare (ignore state))
  (%r0b-stub '(setf target-state)))

(defmethod target-drift ((target in-memory-target) store)
  (declare (ignore store))
  (%r0b-stub 'target-drift))

(defmethod capture-revision ((target in-memory-target) store
                             &key parents message)
  (declare (ignore store parents message))
  (%r0b-stub 'capture-revision))

(defmethod target-fidelities ((target in-memory-target))
  (%r0b-stub 'target-fidelities))

(defmethod target-transactional-p ((target in-memory-target))
  (%r0b-stub 'target-transactional-p))

(defmethod call-with-target-transaction ((target in-memory-target) thunk)
  (declare (ignore thunk))
  (%r0b-stub 'call-with-target-transaction))

(defmethod execute-plan ((target in-memory-target) plan)
  (declare (ignore plan))
  (%r0b-stub 'execute-plan))

(defmethod project-plan ((target in-memory-target) plan &key fidelity)
  (declare (ignore plan fidelity))
  (%r0b-stub 'project-plan))
