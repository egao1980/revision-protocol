(in-package #:revision-protocol)

;;; ------------------------------------------------------------------
;;; Composite revisions (R0b). A composite revision's content is a
;;; REVISION-MANIFEST: one part per role (:conversation, :files, :db, :image,
;;; any keyword), each naming a revision in another store. Restoring plans one
;;; transition per part, dependencies first; there is no atomicity across
;;; stores (parts restored before a failure stay restored).
;;; ------------------------------------------------------------------

(defclass manifest-part ()
  ((role :initarg :role :reader part-role :type keyword)
   (store :initarg :store :initform nil :reader part-store)
   (revision-id :initarg :revision-id :reader part-revision-id :type string)
   (depends-on :initarg :depends-on :initform nil :reader %part-depends-on)
   (recipe :initarg :recipe :initform nil :reader part-recipe))
  (:documentation "One part of a manifest. STORE is the REVISION-STORE holding
REVISION-ID, or NIL when that store is not available in this image (an image
snapshot that was not kept, a remote store). DEPENDS-ON lists the roles that
must be restored first. RECIPE is NIL or a function (PART TARGET) that rebuilds
the part on TARGET when its revision cannot be restored."))

(defun part-depends-on (part)
  "Roles PART depends on. A fresh list."
  (copy-list (%part-depends-on part)))

(defmethod print-object ((p manifest-part) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~s ~a"
            (if (slot-boundp p 'role) (part-role p) "?")
            (if (slot-boundp p 'revision-id) (part-revision-id p) "?"))))

(defun make-manifest-part (role store designator &key depends-on recipe)
  "→ MANIFEST-PART for ROLE (a keyword). With a STORE,
DESIGNATOR is resolved now and the part keeps the revision id (signals
REVISION-NOT-FOUND). With STORE NIL, DESIGNATOR must be an id string, kept as
is. DEPENDS-ON: list of role keywords. RECIPE: NIL or a function (PART TARGET).
TYPE-ERROR for a non-keyword ROLE, a non-string DESIGNATOR without a store, or a
non-function RECIPE."
  (declare (ignore role store designator depends-on recipe))
  (%r0b-stub 'make-manifest-part))

(defclass revision-manifest ()
  ((parts :initarg :parts :initform nil :reader %manifest-parts))
  (:documentation "Content of a composite revision: MANIFEST-PARTs, one per
role, in the order given."))

(defun manifest-parts (manifest)
  "MANIFEST's parts in the order given. A fresh list."
  (copy-list (%manifest-parts manifest)))

(defun make-manifest (parts)
  "→ REVISION-MANIFEST of PARTS (MANIFEST-PARTs). REVISION-ERROR
when two parts share a role, a DEPENDS-ON names a role not in PARTS, or the
dependencies form a cycle."
  (declare (ignore parts))
  (%r0b-stub 'make-manifest))

(defgeneric commit-composite (store parts &key parents ref expected message)
  (:documentation "Commit a composite revision whose content is
(MAKE-MANIFEST PARTS) → REVISION. PARENTS, REF, EXPECTED, MESSAGE as in
COMMIT-REVISION. A composite revision has no state to diff: REVISION-STATE
returns the manifest."))

(defgeneric composite-manifest (store designator)
  (:documentation "→ the REVISION-MANIFEST of a composite revision, or NIL for a
revision whose content is not a manifest."))

(defgeneric plan-composite (store designator targets)
  (:documentation "→ list of (ROLE . PLAN) for the composite revision at
DESIGNATOR, dependencies before dependents, otherwise in manifest order. TARGETS
is a plist role → REVISION-TARGET. PLAN is the PLAN-TRANSITION of the part's
store and target to the part's revision, or NIL when the part has no store, no
target, or its store has no such revision. Not a composite → REVISION-ERROR."))

(defgeneric restore-composite (store designator targets)
  (:documentation "Execute PLAN-COMPOSITE's plans in order → plist role →
result, in that order: the part's TARGET-REVISIONS after EXECUTE-PLAN, or
:SKIPPED. A part without a plan signals COMPOSITE-PART-UNAVAILABLE (REASON
:NO-STORE, :NO-TARGET or :NOT-FOUND) with restarts SKIP-PART (result :SKIPPED)
and, when the part has a recipe and a target, REBUILD-FROM-RECIPE (call the
recipe with the part and its target, stamp the target with the part's revision
id; result = TARGET-REVISIONS). Conditions from EXECUTE-PLAN pass through
with their own restarts."))
