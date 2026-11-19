(in-package #:revision-protocol)

;;; ------------------------------------------------------------------
;;; Content layer (R0b).
;;;
;;; A revision carries a state, a delta, or both. COMMIT-REVISION's CONTENT
;;; argument is classified by the store:
;;;   - a DIFF-PROTOCOL:DIFF        → a delta against the first parent's state
;;;                                   (a root: against NIL, the empty state);
;;;   - a REVISION-PAYLOAD          → state and/or delta given explicitly
;;;                                   (e.g. an opaque migration step whose
;;;                                   resulting state the model knows);
;;;   - anything else               → a state.
;;; REVISION-CONTENT still returns CONTENT exactly as given (R0a).
;;; A store declares which form it keeps (CONTENT-PRIMARY) and derives the
;;; other: REVISION-STATE folds deltas from the root; REVISION-DELTA diffs the
;;; first parent's state and the revision's state.
;;; ------------------------------------------------------------------

(defun %r0b-stub (operation &optional detail)
  "Contract stub: signal a plain SIMPLE-ERROR (never a REVISION-ERROR), so the
R0b suite loads and fails without false passes. Removed by the implementation."
  (error "revision-protocol: ~s~@[ ~s~] is not implemented (R0b stub)"
         operation detail))

(defclass revision-payload ()
  ((state :initarg :state :initform nil :reader payload-state)
   (state-p :initarg :state-p :initform nil :reader payload-state-p)
   (delta :initarg :delta :initform nil :reader payload-delta))
  (:documentation "Explicit revision content: a STATE (when PAYLOAD-STATE-P),
a DELTA (a DIFF-PROTOCOL:DIFF against the first parent, or NIL), or both."))

(defun revision-payload-p (x)
  "T when X is a REVISION-PAYLOAD."
  (typep x 'revision-payload))

(defun make-payload (&key (state nil state-p) delta)
  "Content carrying STATE (when supplied — NIL is a valid state) and/or DELTA
(a DIFF-PROTOCOL:DIFF against the first parent's state). At least one of them
must be given."
  (check-type delta (or null diff:diff))
  (unless (or state-p delta)
    (error 'revision-error :message "MAKE-PAYLOAD needs :STATE or :DELTA"))
  (make-instance 'revision-payload :state state :state-p state-p :delta delta))

(defmethod print-object ((p revision-payload) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~:[~;state~]~:[~; delta~]"
            (payload-state-p p) (payload-delta p))))

;;; ------------------------------------------------------------------
;;; Capability queries. R0b does not depend on capability-protocol (it lives
;;; in blackboard-protocol); each capability the plan names is a query GF
;;; here, and a capability-protocol adapter maps them later.
;;; ------------------------------------------------------------------

(defgeneric content-primary (store)
  (:documentation "→ :STATE or :DELTA, the form STORE keeps for each revision
(capability :primary). The other form is derived."))

(defgeneric content-engine (store)
  (:documentation "→ the DIFF-PROTOCOL engine STORE uses to diff and apply its
states. In-memory store: a SEXP-DIFF-ENGINE unless :ENGINE was given."))

(defgeneric permit-force-p (store name)
  (:documentation "True when the FORCE restart may be offered on a REF-CONFLICT
for ref NAME (the permission gate). Default method: NIL.
Where offered (MOVE-REF, COMMIT-REVISION, DELETE-REF, next to USE-CURRENT and
RETRY), FORCE redoes the operation with EXPECTED omitted (unconditional) and
returns what the operation returns. Not offered, (FIND-RESTART (QUOTE FORCE)) is NIL."))

(defmethod permit-force-p ((store revision-store) name)
  (declare (ignore name))
  nil)

;;; ------------------------------------------------------------------
;;; Content protocol
;;; ------------------------------------------------------------------

(defgeneric revision-state (store designator)
  (:documentation "→ the state at DESIGNATOR. A :DELTA store folds the deltas
along first parents from the root, starting from NIL. A payload with a state
returns that state. Signals REVISION-NOT-FOUND, AMBIGUOUS-REVISION; content the
engine cannot apply signals the engine's DIFF-PROTOCOL condition."))

(defgeneric revision-delta (store designator)
  (:documentation "→ DIFF-PROTOCOL:DIFF from the first parent's state (NIL for a
root) to DESIGNATOR's state. A merge's delta is against its first parent. A
stored delta (or a payload delta) is returned as stored; otherwise it is
COMPUTE-DIFF at level :CHANGES."))

(defgeneric delta-inverse (delta)
  (:documentation "→ the inverse DIFF of DELTA, or NIL when it has none (it
contains an OPAQUE-CHANGE or another irreversible change). Never signals
IRREVERSIBLE-CHANGE."))

(defgeneric delta-footprint (delta)
  (:documentation "→ the resource strings DELTA touches: the same strings as
DIFF-PROTOCOL:DIFF-FOOTPRINT (deduped, sorted STRING<). NIL for NIL."))

(defgeneric state-diff (store a b &key footprint level)
  (:documentation "→ DIFF-PROTOCOL:DIFF from the state of A to the state of B.
A and B are each a revision designator, a REVISION-TARGET (its TARGET-STATE),
a TRANSITION-PLAN (the state of its goal revision) or a PROJECTION (its
PROJECTION-STATE). LEVEL (default :CHANGES) is passed to COMPUTE-DIFF.
FOOTPRINT, a list of resource strings, keeps only the top-level changes whose
footprint overlaps one of them (DIFF-PROTOCOL:FOOTPRINT-OVERLAP-P); NIL keeps
all."))
