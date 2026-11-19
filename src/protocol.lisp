(in-package #:revision-protocol)

;;; ------------------------------------------------------------------
;;; Revisions — immutable values. No writers are defined or exported.
;;; ------------------------------------------------------------------

(defconstant +minimum-id-prefix+ 4
  "Shortest string that RESOLVE-REVISION matches as an id prefix.
A shorter string resolves only as an exact id.")

(defclass revision ()
  ((id :initarg :id :reader revision-id :type string)
   (parent-ids :initarg :parent-ids :initform '() :reader %revision-parent-ids)
   (recorded-at :initarg :recorded-at :reader revision-recorded-at)
   (valid-at :initarg :valid-at :initform nil :reader revision-valid-at)
   (provenance :initarg :provenance :initform '() :reader %revision-provenance)
   (message :initarg :message :initform nil :reader revision-message)
   (content :initarg :content :initform nil :reader revision-content))
  (:documentation "One node of the revision DAG. Immutable: ID is chosen by
the store (or supplied by the caller where the backend allows it), PARENT-IDS
is ordered (first parent first; two or more = merge), RECORDED-AT is the
store's clock at commit (a DATETIME-PROTOCOL instant), VALID-AT is an optional
caller-supplied instant, PROVENANCE a plist, CONTENT an opaque handle (its
meaning is milestone R0b)."))

(defun revision-p (x)
  "T when X is a REVISION."
  (typep x 'revision))

(defun revision-parent-ids (revision)
  "Parent ids of REVISION, first parent first. A fresh list."
  (copy-list (%revision-parent-ids revision)))

(defun revision-provenance (revision)
  "Provenance plist of REVISION. A fresh list."
  (copy-list (%revision-provenance revision)))

(defun revision-time (revision axis)
  "REVISION's time on AXIS. :RECORDED → RECORDED-AT. :VALID → VALID-AT,
or RECORDED-AT when the revision has no valid time."
  (ecase axis
    (:recorded (revision-recorded-at revision))
    (:valid (or (revision-valid-at revision)
                (revision-recorded-at revision)))))

(defmethod print-object ((r revision) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~a" (if (slot-boundp r 'id) (revision-id r) "?"))))

;;; ------------------------------------------------------------------
;;; Refs — snapshots of a named pointer. Movable refs (:movable) or
;;; immutable tags (:tag). One namespace for both kinds.
;;; ------------------------------------------------------------------

(defclass revision-ref ()
  ((name :initarg :name :reader ref-name :type string)
   (kind :initarg :kind :reader ref-kind :type (member :movable :tag))
   (target :initarg :target :reader ref-target :type string)
   (annotation :initarg :annotation :initform nil :reader ref-annotation))
  (:documentation "Value returned by MOVE-REF, TAG-REVISION, FIND-REF and
LIST-REFS. TARGET is a revision id. Later ref updates do not change it."))

(defun revision-ref-p (x)
  "T when X is a REVISION-REF."
  (typep x 'revision-ref))

(defmethod print-object ((r revision-ref) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~a ~s → ~a"
            (if (slot-boundp r 'kind) (ref-kind r) "?")
            (if (slot-boundp r 'name) (ref-name r) "?")
            (if (slot-boundp r 'target) (ref-target r) "?"))))

;;; ------------------------------------------------------------------
;;; Designators — values naming a revision. RESOLVE-REVISION turns one
;;; into a REVISION. Also accepted wherever a designator is: a string
;;; (an id or an id prefix of at least +MINIMUM-ID-PREFIX+ chars; never a
;;; ref name) and a REVISION object.
;;; ------------------------------------------------------------------

(defclass revision-designator () ()
  (:documentation "Base class of designator values."))

(defun revision-designator-p (x)
  "T for anything RESOLVE-REVISION accepts: a designator value, a string id
(or prefix), or a revision."
  (typep x '(or revision-designator string revision)))

(deftype designator ()
  '(or revision-designator string revision))

(defclass ref-designator (revision-designator)
  ((name :initarg :name :reader designator-name :type string))
  (:documentation "The revision a ref of any kind (movable or tag) points at."))

(defclass tag-designator (revision-designator)
  ((name :initarg :name :reader designator-name :type string))
  (:documentation "The revision a tag points at. A movable ref of that name
does not match."))

(defclass as-of-designator (revision-designator)
  ((time :initarg :time :reader designator-time)
   (on :initarg :on :reader designator-on)
   (axis :initarg :axis :reader designator-axis :type (member :recorded :valid)))
  (:documentation "Walk first parents from ON (newest first) and take the
first revision whose REVISION-TIME on AXIS is <= TIME."))

(defclass parent-designator (revision-designator)
  ((base :initarg :base :reader designator-base)
   (n :initarg :n :reader designator-n :type (integer 1)))
  (:documentation "The Nth parent (1-based, git ^N) of BASE."))

(defclass ancestor-designator (revision-designator)
  ((base :initarg :base :reader designator-base)
   (n :initarg :n :reader designator-n :type (integer 0)))
  (:documentation "The Nth first-parent ancestor (git ~N) of BASE; N = 0 is
BASE itself."))

(defmethod print-object ((d ref-designator) stream)
  (print-unreadable-object (d stream :type t)
    (format stream "~s" (designator-name d))))

(defmethod print-object ((d tag-designator) stream)
  (print-unreadable-object (d stream :type t)
    (format stream "~s" (designator-name d))))

(defun %check-designator (x)
  (unless (typep x 'designator)
    (error 'type-error :datum x :expected-type 'designator))
  x)

(defun rev-ref (name)
  "Designator for the revision ref NAME (movable or tag) points at."
  (check-type name string)
  (make-instance 'ref-designator :name name))

(defun rev-tag (name)
  "Designator for the revision tag NAME points at."
  (check-type name string)
  (make-instance 'tag-designator :name name))

(defun rev-as-of (time &key on (axis :recorded))
  "Designator: the newest revision on the first-parent line of ON whose time on
AXIS (:RECORDED or :VALID) is <= TIME. TIME is a DATETIME-PROTOCOL instant.
ON is a designator; a string ON is a ref name (coerced with REV-REF)."
  (check-type time dt:instant)
  (check-type axis (member :recorded :valid))
  (when (null on)
    (error 'type-error :datum on :expected-type 'designator))
  (make-instance 'as-of-designator
                 :time time
                 :on (if (stringp on) (rev-ref on) (%check-designator on))
                 :axis axis))

(defun rev-parent (designator &optional (n 1))
  "Designator for the Nth parent (1-based) of DESIGNATOR."
  (%check-designator designator)
  (check-type n (integer 1))
  (make-instance 'parent-designator :base designator :n n))

(defun rev-ancestor (designator n)
  "Designator for the Nth first-parent ancestor of DESIGNATOR (0 = itself)."
  (%check-designator designator)
  (check-type n (integer 0))
  (make-instance 'ancestor-designator :base designator :n n))

;;; ------------------------------------------------------------------
;;; Store protocol
;;; ------------------------------------------------------------------

(defclass revision-store () ()
  (:documentation "Base class of revision stores (in-memory, git, jj, SQL
migration directories, agent sessions …)."))

(defun revision-store-p (x)
  "T when X is a REVISION-STORE."
  (typep x 'revision-store))

(defgeneric resolve-revision (store designator)
  (:documentation "→ REVISION named by DESIGNATOR.
String: exact id first, then a unique id prefix of length >= +MINIMUM-ID-PREFIX+.
Signals REVISION-NOT-FOUND or AMBIGUOUS-REVISION (with the candidate ids)."))

(defgeneric commit-revision (store content &key parents ref expected message
                                                provenance valid-at id)
  (:documentation "Record a new immutable revision → REVISION.
PARENTS: list of designators, first parent first. When omitted and REF names an
existing ref, the parent is REF's current target; otherwise a root.
REF: name of a movable ref moved to the new revision (created if absent).
EXPECTED: CAS value for REF (an id/designator, or NIL = ref must be absent);
when omitted it is the target REF had when the commit started.
VALID-AT: optional instant. PROVENANCE: plist. ID: caller-chosen id (a string).
A backend that chooses ids itself (git, jj) refuses a non-NIL ID with
REVISION-ERROR before anything is written; the in-memory store accepts it.
Atomic: on any condition nothing is committed and no ref moves.
Signals REVISION-NOT-FOUND (a parent), REF-CONFLICT (restarts USE-CURRENT →
returns REF's current target revision, nothing committed; RETRY → commit again
with EXPECTED = current, and defaulted PARENTS re-read), TAG-IMMUTABLE (REF is a
tag), CYCLE-REFUSED (ID is an ancestor-or-self of a parent), REVISION-ERROR
(ID exists already)."))

(defgeneric move-ref (store name designator &key expected)
  (:documentation "Point movable ref NAME at DESIGNATOR → REVISION-REF.
Creates NAME when absent. EXPECTED omitted: unconditional. EXPECTED NIL: NAME must
be absent. Otherwise NAME must currently point at EXPECTED (designator).
Signals REF-CONFLICT (restarts USE-CURRENT → current ref, unchanged, or NIL when
absent; RETRY → move with EXPECTED = current) and TAG-IMMUTABLE when NAME is a tag
(restart CREATE-NEW-TAG new-name → tag new-name at DESIGNATOR, return that tag)."))

(defgeneric tag-revision (store name designator &key annotation)
  (:documentation "Create immutable tag NAME at DESIGNATOR → REVISION-REF.
Same target and annotation as an existing tag: returns it (idempotent).
Existing tag otherwise: TAG-IMMUTABLE (restart CREATE-NEW-TAG new-name).
Existing movable ref NAME: REF-CONFLICT (expected NIL, actual its target)."))

(defgeneric delete-ref (store name &key expected)
  (:documentation "Delete movable ref NAME → T. EXPECTED as in MOVE-REF.
Missing NAME: REVISION-NOT-FOUND. A tag: TAG-IMMUTABLE. Stale EXPECTED:
REF-CONFLICT (USE-CURRENT → NIL, ref kept; RETRY → delete)."))

(defgeneric find-ref (store name)
  (:documentation "→ REVISION-REF for NAME, or NIL."))

(defgeneric list-refs (store &key kind prefix)
  (:documentation "→ REVISION-REFs sorted by name (STRING<). KIND :MOVABLE or
:TAG filters; NIL is both. PREFIX keeps names starting with it."))

(defgeneric revision-parents (store designator)
  (:documentation "→ parent REVISIONs of DESIGNATOR, first parent first."))

(defgeneric revision-children (store designator)
  (:documentation "→ REVISIONs that have DESIGNATOR as a parent (any order)."))

(defgeneric walk-revisions (store from &key order until reverse footprint)
  (:documentation "→ list of REVISIONs reachable from FROM (a designator or a
list of them), excluding UNTIL (designator or list) and its ancestors.
Without REVERSE every order lists parents before children:
  :TOPO          every revision after all of its parents in the result;
  :TIME          ascending RECORDED-AT, ties and clock skew resolved so that
                 parents still come first;
  :FIRST-PARENT  only the first-parent chain of FROM, root first.
REVERSE T returns the same list reversed (children first, as git log).
FOOTPRINT (R0b), a list of resource strings: keep only revisions whose
REVISION-DELTA footprint overlaps one of them (DIFF-PROTOCOL:FOOTPRINT-OVERLAP-P);
the walk order of the kept revisions is unchanged. NIL keeps all.
Each revision appears once."))

(defgeneric merge-base (store a b &key all)
  (:documentation "Best common ancestor(s) of A and B: common ancestors none of
whose descendants is also common. ALL NIL → one REVISION (deterministic) or NIL
when unrelated. ALL T → list of every best common ancestor."))

(defgeneric ancestor-p (store a b)
  (:documentation "T when A is B or an ancestor of B."))

(defgeneric revision-range (store include exclude)
  (:documentation "→ REVISIONs reachable from INCLUDE and not from EXCLUDE
(each a designator, a list of them, or NIL), in :TOPO order (git A ^B)."))

(defgeneric heads (store)
  (:documentation "→ every REVISION without children (any order)."))

(defgeneric ref-listener (store)
  (:documentation "Function called after each ref change with one event list
(:REF-MOVED name old-id new-id): OLD-ID NIL on creation (including tags),
NEW-ID NIL on deletion. Not called for failed or abandoned updates. NIL = none."))

(defgeneric (setf ref-listener) (function store)
  (:documentation "Install FUNCTION (or NIL) as STORE's ref listener."))
