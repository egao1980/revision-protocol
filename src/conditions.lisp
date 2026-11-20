(in-package #:revision-protocol)

;;; Condition tree (R0a):
;;;
;;;   revision-error
;;;     revision-not-found
;;;     ambiguous-revision
;;;     ref-conflict      restarts: use-current, retry
;;;     tag-immutable     restarts: create-new-tag (new-name),
;;;                                 replace-annotation (0.4.0; TAG-REVISION
;;;                                 only, when TAG-REWRITE-P)
;;;     cycle-refused
;;;     vc-tool-version-error (0.3.0; signalled by the git / jj backends)
;;;
;;; Restarts are established by the store operation that can recover
;;; (MOVE-REF, COMMIT-REVISION, DELETE-REF, TAG-REVISION), not by the
;;; condition. The names below are exported so handlers can
;;; (invoke-restart 'stack-rev:use-current) etc.

(define-condition revision-error (error)
  ((message :initarg :message :reader revision-error-message :initform nil))
  (:report (lambda (c s)
             (format s "revision error~@[: ~a~]" (revision-error-message c))))
  (:documentation "Root of every condition the revision protocol signals."))

(define-condition revision-not-found (revision-error)
  ((designator :initarg :designator :reader revision-not-found-designator
               :initform nil))
  (:report (lambda (c s)
             (format s "revision not found: ~s~@[: ~a~]"
                     (revision-not-found-designator c)
                     (revision-error-message c))))
  (:documentation "DESIGNATOR names no revision (or ref) in the store."))

(define-condition ambiguous-revision (revision-error)
  ((designator :initarg :designator :reader ambiguous-revision-designator
               :initform nil)
   (candidates :initarg :candidates :reader ambiguous-revision-candidates
               :initform nil))
  (:report (lambda (c s)
             (format s "ambiguous revision ~s: candidates ~s~@[: ~a~]"
                     (ambiguous-revision-designator c)
                     (ambiguous-revision-candidates c)
                     (revision-error-message c))))
  (:documentation "An id prefix matches more than one revision.
CANDIDATES is the list of matching full ids."))

(define-condition ref-conflict (revision-error)
  ((name :initarg :name :reader ref-conflict-name :initform nil)
   (expected :initarg :expected :reader ref-conflict-expected :initform nil)
   (actual :initarg :actual :reader ref-conflict-actual :initform nil))
  (:report (lambda (c s)
             (format s "ref ~s moved: expected ~s, found ~s~@[: ~a~]"
                     (ref-conflict-name c)
                     (ref-conflict-expected c)
                     (ref-conflict-actual c)
                     (revision-error-message c))))
  (:documentation "Compare-and-swap on ref NAME failed. EXPECTED and ACTUAL
are revision ids, or NIL for an absent ref. Restarts: USE-CURRENT, RETRY."))

(define-condition tag-immutable (revision-error)
  ((name :initarg :name :reader tag-immutable-name :initform nil)
   (target :initarg :target :reader tag-immutable-target :initform nil))
  (:report (lambda (c s)
             (format s "tag ~s is immutable (points at ~s)~@[: ~a~]"
                     (tag-immutable-name c)
                     (tag-immutable-target c)
                     (revision-error-message c))))
  (:documentation "An operation would move, retarget or delete tag NAME.
TARGET is the id the tag points at. Restarts: CREATE-NEW-TAG (where offered);
REPLACE-ANNOTATION from TAG-REVISION when only the annotation differs and the
store answers TAG-REWRITE-P true."))

(define-condition cycle-refused (revision-error)
  ((id :initarg :id :reader cycle-refused-id :initform nil)
   (parents :initarg :parents :reader cycle-refused-parents :initform nil))
  (:report (lambda (c s)
             (format s "revision ~s with parents ~s would create a cycle~@[: ~a~]"
                     (cycle-refused-id c)
                     (cycle-refused-parents c)
                     (revision-error-message c))))
  (:documentation "A commit would make the graph cyclic (caller-supplied
ID that is an ancestor-or-self of one of PARENTS)."))

(define-condition vc-tool-version-error (revision-error)
  ((tool :initarg :tool :reader vc-tool-version-error-tool :initform nil)
   (found :initarg :found :reader vc-tool-version-error-found :initform nil)
   (required :initarg :required :reader vc-tool-version-error-required
             :initform nil))
  (:report (lambda (c s)
             (format s "~a ~a is too old or missing: found ~a, need ~a or later"
                     (or (vc-tool-version-error-tool c) "tool")
                     (if (vc-tool-version-error-found c) "version" "program")
                     (or (vc-tool-version-error-found c) "nothing")
                     (vc-tool-version-error-required c))))
  (:documentation "The version-control program TOOL (a string: \"git\", \"jj\")
that a backend store drives is older than REQUIRED (a version string), or cannot be
run at all. FOUND is the version string the program reported, or NIL when it could
not be started. Signalled by a backend's store constructor (MAKE-GIT-STORE,
MAKE-JJ-STORE) before any other command of that program runs; the backends
re-export this symbol."))

;;; ------------------------------------------------------------------
;;; Condition tree (R0b) — all under REVISION-ERROR:
;;;
;;;   merge-conflict              restarts: take-ours, take-theirs, use-merged, abort
;;;   irreversible-step           restarts: skip-step, stamp-only, abort-transition
;;;   target-drift-detected       restarts: capture-and-continue, discard-drift, abort
;;;   step-failed                 restarts: retry-step, skip-step, rollback-to-start
;;;   projection-unsupported      restarts: use-shadow, accept-model-fidelity,
;;;                                         apply-for-real (only those applicable)
;;;   projection-stale
;;;   composite-part-unavailable  restarts: skip-part, rebuild-from-recipe
;;;
;;; plus restart FORCE on REF-CONFLICT, offered only when PERMIT-FORCE-P.
;;; ABORT is CL:ABORT. As in R0a, the operation that can recover establishes
;;; the restarts (COMMIT-PROJECTION, EXECUTE-PLAN, PROJECT-PLAN,
;;; RESTORE-COMPOSITE, MOVE-REF / COMMIT-REVISION / DELETE-REF).
;;; ------------------------------------------------------------------

(define-condition merge-conflict (revision-error)
  ((conflicts :initarg :conflicts :reader merge-conflict-conflicts :initform nil)
   (projection :initarg :projection :reader merge-conflict-projection
               :initform nil))
  (:report (lambda (c s)
             (format s "merge has ~d conflict~:p~@[: ~a~]"
                     (length (merge-conflict-conflicts c))
                     (revision-error-message c))))
  (:documentation "COMMIT-PROJECTION of a merge projection that still has
conflicts. CONFLICTS is the list of DIFF-PROTOCOL:DIFF-CONFLICT values,
PROJECTION the projection. Restarts: TAKE-OURS, TAKE-THEIRS, USE-MERGED, ABORT."))

(define-condition irreversible-step (revision-error)
  ((step :initarg :step :reader irreversible-step-step :initform nil))
  (:report (lambda (c s)
             (format s "plan step ~s cannot be undone~@[: ~a~]"
                     (irreversible-step-step c)
                     (revision-error-message c))))
  (:documentation "EXECUTE-PLAN reached a :DOWN step whose revision delta has no
inverse. STEP is the PLAN-STEP. Restarts: SKIP-STEP, STAMP-ONLY,
ABORT-TRANSITION."))

(define-condition target-drift-detected (revision-error)
  ((target :initarg :target :reader target-drift-detected-target :initform nil)
   (drift :initarg :drift :reader target-drift-detected-drift :initform nil))
  (:report (lambda (c s)
             (format s "target ~s drifted from its stamped revision~@[: ~a~]"
                     (target-drift-detected-target c)
                     (revision-error-message c))))
  (:documentation "EXECUTE-PLAN found the target's state differs from the state
of its stamped revision. DRIFT is the diff (stamped state → actual state).
Restarts: CAPTURE-AND-CONTINUE, DISCARD-DRIFT, ABORT."))

(define-condition step-failed (revision-error)
  ((step :initarg :step :reader step-failed-step :initform nil)
   (cause :initarg :cause :reader step-failed-cause :initform nil)
   (attempt :initarg :attempt :reader step-failed-attempt :initform 1))
  (:report (lambda (c s)
             (format s "plan step ~s failed (attempt ~d)~@[: ~a~]"
                     (step-failed-step c)
                     (step-failed-attempt c)
                     (or (step-failed-cause c) (revision-error-message c)))))
  (:documentation "Applying a plan step signalled an error. CAUSE is that
condition, ATTEMPT counts from 1. Restarts: RETRY-STEP, SKIP-STEP,
ROLLBACK-TO-START."))

(define-condition projection-unsupported (revision-error)
  ((fidelity :initarg :fidelity :reader projection-unsupported-fidelity
             :initform nil)
   (supported :initarg :supported :reader projection-unsupported-supported
              :initform nil))
  (:report (lambda (c s)
             (format s "projection fidelity ~s unsupported (supported: ~s)~@[: ~a~]"
                     (projection-unsupported-fidelity c)
                     (projection-unsupported-supported c)
                     (revision-error-message c))))
  (:documentation "The requested projection FIDELITY is not among SUPPORTED.
Restarts (each only when applicable): USE-SHADOW, ACCEPT-MODEL-FIDELITY,
APPLY-FOR-REAL."))

(define-condition projection-stale (revision-error)
  ((projection :initarg :projection :reader projection-stale-projection
               :initform nil))
  (:report (lambda (c s)
             (format s "projection ~s is stale: its target changed~@[: ~a~]"
                     (projection-stale-projection c)
                     (revision-error-message c))))
  (:documentation "A projection of a target was read or committed after the
target's revisions or state changed."))

(define-condition composite-part-unavailable (revision-error)
  ((part :initarg :part :reader composite-part-unavailable-part :initform nil)
   (reason :initarg :reason :reader composite-part-unavailable-reason
           :initform nil))
  (:report (lambda (c s)
             (format s "composite part ~s cannot be restored (~s)~@[: ~a~]"
                     (composite-part-unavailable-part c)
                     (composite-part-unavailable-reason c)
                     (revision-error-message c))))
  (:documentation "RESTORE-COMPOSITE cannot bring PART back. REASON is
:NO-STORE (the part's store is not available), :NO-TARGET (no target given for
its role) or :NOT-FOUND (its store no longer has the revision). Restarts:
SKIP-PART, REBUILD-FROM-RECIPE (only when the part has a recipe)."))
