(in-package #:revision-protocol)

;;; ------------------------------------------------------------------
;;; Projections (R0b) — the state at a revision, after a plan, or after a
;;; merge, without touching the target or committing anything.
;;;
;;; Fidelity, best first:
;;;   :EXACT   the state execution would produce (in-memory: always);
;;;   :SHADOW  produced by really applying on a throwaway copy of the target;
;;;   :MODEL   computed from a model; equal to execution only on the modelled
;;;            footprint (in-memory target: the goal revision.s stored state).
;;; ------------------------------------------------------------------

(defclass projection () ()
  (:documentation "Base class of projections. Backends subclass it."))

(defun projection-p (x)
  "T when X is a PROJECTION."
  (typep x 'projection))

(defgeneric project-state (store designator &key fidelity writable)
  (:documentation "→ PROJECTION of the state at DESIGNATOR. FIDELITY NIL means
the best the store offers; a backend may return a better fidelity than asked
(the in-memory store always returns :EXACT). WRITABLE T allows
(SETF PROJECTION-STATE); PROJECTION-PARENTS is (id of DESIGNATOR)."))

(defgeneric project-plan (target plan &key fidelity)
  (:documentation "→ PROJECTION of TARGET's state after PLAN, without changing
TARGET. FIDELITY NIL → the first of TARGET-FIDELITIES. A FIDELITY not in
TARGET-FIDELITIES signals PROJECTION-UNSUPPORTED with these restarts, each
established only when applicable:
  USE-SHADOW             :SHADOW is supported → a :SHADOW projection;
  ACCEPT-MODEL-FIDELITY  :MODEL is supported → a :MODEL projection;
  APPLY-FOR-REAL         always → EXECUTE-PLAN on TARGET, then an :EXACT
                         projection of the result (no longer stale-checked).
PROJECTION-PARENTS is PLAN-GOAL. Reading or committing the projection after
TARGET's stamp or state changed signals PROJECTION-STALE."))

(defgeneric project-merge (store a b)
  (:documentation "→ (values projection conflicts): DIFF-PROTOCOL:THREE-WAY-DIFF
of the MERGE-BASE state (NIL when unrelated), A's state (ours) and B's state
(theirs), applied to the base state. CONFLICTS (DIFF-CONFLICT values, also
PROJECTION-CONFLICTS) keep the base value. Nothing is committed.
PROJECTION-PARENTS is (id-of-A id-of-B); fidelity :EXACT."))

(defgeneric projection-fidelity (projection)
  (:documentation "→ :EXACT, :MODEL or :SHADOW."))

(defgeneric projection-view (projection)
  (:documentation "→ the domain's normal read API over the projected state: a
cl-stack-pathlib filesystem, a sql-orm schema, a read-only connection, a REPL
connection. In-memory (sexp): a fresh copy of the state. Signals
PROJECTION-STALE (target projections) and REVISION-ERROR once discarded."))

(defgeneric projection-state (projection)
  (:documentation "→ the projected state in the store's content form (what
STATE-DIFF compares). Same signals as PROJECTION-VIEW."))

(defgeneric (setf projection-state) (state projection)
  (:documentation "Replace a writable projection's state (edits before
COMMIT-PROJECTION). Not PROJECTION-WRITABLE-P → REVISION-ERROR."))

(defgeneric projection-parents (projection)
  (:documentation "→ fresh list of the revision ids COMMIT-PROJECTION uses as
parents by default."))

(defgeneric projection-conflicts (projection)
  (:documentation "→ unresolved DIFF-CONFLICTs (merge projections), else NIL."))

(defgeneric projection-writable-p (projection)
  (:documentation "True when (SETF PROJECTION-STATE) is allowed."))

(defgeneric projection-discarded-p (projection)
  (:documentation "True after DISCARD-PROJECTION."))

(defgeneric commit-projection (store projection &key parents message ref
                                                provenance valid-at id)
  (:documentation "Commit PROJECTION-STATE as a new revision → REVISION.
PARENTS defaults to PROJECTION-PARENTS; MESSAGE, REF, PROVENANCE, VALID-AT and ID
as in COMMIT-REVISION (a backend that cannot honor a caller ID refuses it with
REVISION-ERROR).
A projection with conflicts signals MERGE-CONFLICT with restarts TAKE-OURS
(each conflict resolved to ours), TAKE-THEIRS, USE-MERGED (commit as projected,
base values at the conflicts) and ABORT (return NIL, commit nothing).
Signals PROJECTION-STALE, and REVISION-ERROR for a discarded projection."))

(defgeneric discard-projection (projection)
  (:documentation "Release PROJECTION → T. Idempotent. Afterwards
PROJECTION-VIEW, PROJECTION-STATE and COMMIT-PROJECTION signal REVISION-ERROR."))
