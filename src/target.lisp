(in-package #:revision-protocol)

;;; ------------------------------------------------------------------
;;; Target layer (R0b) — where a revision is realized: a working copy, a DB
;;; connection, a live run, an image. A target is stamped with the ids of the
;;; revisions applied to it (TARGET-REVISIONS) and has a current state
;;; (TARGET-STATE) in the store's content form. Targets know no store; the
;;; store is passed (or carried by the plan).
;;; ------------------------------------------------------------------

(defclass revision-target () ()
  (:documentation "Base class of revision targets."))

(defun revision-target-p (x)
  "T when X is a REVISION-TARGET."
  (typep x 'revision-target))

(defgeneric target-revisions (target)
  (:documentation "→ fresh list of the revision ids applied to TARGET now (NIL
for a fresh target; several heads possible after TARGET-STAMP)."))

(defgeneric target-stamp (target revisions)
  (:documentation "Record REVISIONS (a list of revisions or id strings) as
applied to TARGET without applying anything → fresh list of ids, in the given
order, duplicates removed. NIL clears the stamp."))

(defgeneric target-state (target)
  (:documentation "→ TARGET's current state, in the content form of the store
its revisions come from. In-memory target: a fresh copy of its sexp state."))

(defgeneric (setf target-state) (state target)
  (:documentation "Overwrite TARGET's state outside revision control (what an
editor or a manual DDL does). Leaves the stamp alone, so the target drifts."))

(defgeneric target-drift (target store)
  (:documentation "→ NIL when TARGET-STATE equals (CONTENT-ENGINE's
STATE-EQUAL) the state of the stamped revision (NIL state when unstamped);
otherwise the DIFF from that state to TARGET-STATE. More than one stamped
revision → REVISION-ERROR (R0b)."))

(defgeneric capture-revision (target store &key parents message provenance
                                                valid-at id)
  (:documentation "Commit TARGET-STATE to STORE as a new revision → REVISION.
PARENTS (designators) defaults to TARGET-REVISIONS; MESSAGE, PROVENANCE, VALID-AT
and ID as in COMMIT-REVISION (a backend that cannot honor a caller ID refuses it
with REVISION-ERROR). Then stamps TARGET with the new revision, so TARGET-DRIFT is
NIL afterwards. On any condition nothing is committed and the stamp is kept."))

(defgeneric target-fidelities (target)
  (:documentation "→ the projection fidelities TARGET supports, a subset of
(:EXACT :SHADOW :MODEL) in that order, best first (capability :projection)."))

(defgeneric target-transactional-p (target)
  (:documentation "True when TARGET can apply under WITH-TARGET-TRANSACTION
(capability :transactional-apply)."))

(defgeneric call-with-target-transaction (target thunk)
  (:documentation "Call THUNK with no arguments → its values. A non-local exit
(error, THROW, restart) restores TARGET's state and stamp from before the call.
A target that is not TARGET-TRANSACTIONAL-P signals REVISION-ERROR without
calling THUNK."))

(defmacro with-target-transaction ((target) &body body)
  "Run BODY atomically against TARGET: a non-local exit undoes BODY's changes to
TARGET. See CALL-WITH-TARGET-TRANSACTION."
  `(call-with-target-transaction ,target (lambda () ,@body)))

;;; ------------------------------------------------------------------
;;; Plans — values. PLAN-TRANSITION computes one; DESCRIBE-PLAN shows it;
;;; EXECUTE-PLAN runs it; PROJECT-PLAN predicts it.
;;;
;;; Path: BASE = MERGE-BASE of the target's revision and TO (NIL when the
;;; target is fresh or the two are unrelated: NIL stands for the empty state
;;; before any root). :DOWN steps go from the current revision to BASE, :UP
;;; steps from BASE to TO. Each step crosses one parent edge; at a merge the
;;; path takes the first parent that is BASE or has BASE as an ancestor (TO's
;;; side likewise, reversed). Every step's DIFF is STATE-DIFF of its FROM and
;;; TO states (NIL = empty state), level :CHANGES.
;;; ------------------------------------------------------------------

(defclass plan-step ()
  ((direction :initarg :direction :reader step-direction
              :type (member :up :down))
   (from :initarg :from :initform nil :reader step-from)
   (to :initarg :to :initform nil :reader step-to)
   (diff :initarg :diff :initform nil :reader step-diff)
   (reversible-p :initarg :reversible-p :initform t :reader step-reversible-p))
  (:documentation "One plan step. FROM and TO are revision ids, or NIL for the
empty state before any root. DIFF takes FROM's state to TO's state.
REVERSIBLE-P is NIL for a :DOWN step that undoes a revision whose
REVISION-DELTA has no DELTA-INVERSE (EXECUTE-PLAN signals IRREVERSIBLE-STEP)."))

(defgeneric step-footprint (step)
  (:documentation "→ DELTA-FOOTPRINT of STEP-DIFF."))

(defmethod print-object ((s plan-step) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~(~a~) ~a -> ~a"
            (if (slot-boundp s 'direction) (step-direction s) "?")
            (or (step-from s) "-") (or (step-to s) "-"))))

(defclass transition-plan ()
  ((store :initarg :store :reader plan-store)
   (start :initarg :start :initform nil :reader %plan-start)
   (goal :initarg :goal :initform nil :reader %plan-goal)
   (base :initarg :base :initform nil :reader plan-base)
   (steps :initarg :steps :initform nil :reader %plan-steps))
  (:documentation "Value returned by PLAN-TRANSITION. START: the target's
revision ids when planned. GOAL: the ids the target has after the plan (R0b:
one id). BASE: the merge-base id or NIL. STEPS: :DOWN steps then :UP steps."))

(defun transition-plan-p (x)
  "T when X is a TRANSITION-PLAN."
  (typep x 'transition-plan))

(defun plan-start (plan)
  "Revision ids of the target when PLAN was computed. A fresh list."
  (copy-list (%plan-start plan)))

(defun plan-goal (plan)
  "Revision ids the target is stamped with after PLAN. A fresh list."
  (copy-list (%plan-goal plan)))

(defun plan-steps (plan)
  "PLAN-STEPs in execution order. A fresh list."
  (copy-list (%plan-steps plan)))

(defgeneric plan-transition (store target to)
  (:documentation "→ TRANSITION-PLAN taking TARGET from its current revision to
the revision TO designates: down to the merge-base, up to TO. Pure: touches
neither STORE nor TARGET. TARGET at TO already → a plan with no steps. TARGET
stamped with more than one revision → REVISION-ERROR (multi-head plans are a
later milestone). Signals REVISION-NOT-FOUND for TO."))

(defgeneric describe-plan (plan stream)
  (:documentation "Write one line per step to STREAM (NIL → return a string,
T → *STANDARD-OUTPUT*; otherwise return NIL):
  <up|down> <from> -> <to>[ <footprint string>]…[ (irreversible)]
FROM / TO print as the id, or \"-\" for NIL; footprint strings in
STEP-FOOTPRINT order, each preceded by one space; \" (irreversible)\" when
STEP-REVERSIBLE-P is NIL. A plan with no steps writes nothing (\"\")."))

(defgeneric execute-plan (target plan)
  (:documentation "Run PLAN's steps against TARGET → (values target-revisions
skipped-steps). After each applied step TARGET is stamped with (STEP-TO) — or
NIL for the empty state — before the next step starts.
Before the first step, TARGET-DRIFT (against PLAN-STORE) non-NIL signals
TARGET-DRIFT-DETECTED with restarts CAPTURE-AND-CONTINUE (&key ID MESSAGE:
CAPTURE-REVISION of the drifted state as a child of the stamp, with ID and
MESSAGE, default \"captured drift\"; re-plan from it to the goal, run that),
DISCARD-DRIFT (reset the state to the stamped revision's, then run) and ABORT
(return NIL, nothing changed).
Resumable: when TARGET is stamped with the TO of some step (an earlier run
stopped there), execution continues after that step. A stamp that is neither
PLAN-START nor any step's TO → REVISION-ERROR.
A :DOWN step that is not STEP-REVERSIBLE-P signals IRREVERSIBLE-STEP with
restarts SKIP-STEP (no change, no stamp; collected into the second value;
continue), STAMP-ONLY (stamp STEP-TO without changing the state; continue) and
ABORT-TRANSITION (stop; return NIL; the target keeps its last stamp).
An error while applying a step signals STEP-FAILED (CAUSE = that error) with
restarts RETRY-STEP (apply the step again, ATTEMPT + 1), SKIP-STEP (as above)
and ROLLBACK-TO-START (restore the state and stamp from before this call;
return NIL). Unhandled, STEP-FAILED propagates and TARGET keeps the stamp of
the last completed step."))
