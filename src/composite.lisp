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
  (check-type role keyword)
  (check-type store (or null revision-store))
  (check-type depends-on list)
  (dolist (d depends-on)
    (check-type d keyword))
  (check-type recipe (or null function))
  (let ((id (if store
                (revision-id (resolve-revision store designator))
                (progn (check-type designator string)
                       (copy-seq designator)))))
    (make-instance 'manifest-part
                   :role role :store store :revision-id id
                   :depends-on (copy-list depends-on) :recipe recipe)))

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
  (check-type parts list)
  (dolist (p parts)
    (check-type p manifest-part))
  (let ((roles (mapcar #'part-role parts)))
    (loop for (role . more) on roles
          when (member role more)
            do (error 'revision-error
                      :message (format nil "two manifest parts have role ~s" role)))
    (dolist (p parts)
      (dolist (d (%part-depends-on p))
        (unless (member d roles)
          (error 'revision-error
                 :message (format nil "part ~s depends on ~s, which is not in the manifest"
                                  (part-role p) d))))))
  ;; Signals REVISION-ERROR on a dependency cycle.
  (%dependency-order parts)
  (make-instance 'revision-manifest :parts (copy-list parts)))

(defun %dependency-order (parts)
  "PARTS with every part after the parts it depends on; otherwise in the given
order (each round emits the first ready part). REVISION-ERROR on a cycle.
Assumes roles are unique and every dependency names a part in PARTS."
  (let ((pending (copy-list parts))
        (done '())
        (out '()))
    (loop while pending
          do (let ((ready (find-if (lambda (p)
                                     (every (lambda (d) (member d done))
                                            (%part-depends-on p)))
                                   pending)))
               (unless ready
                 (error 'revision-error
                        :message (format nil "manifest dependencies form a cycle among ~s"
                                         (mapcar #'part-role pending))))
               (push (part-role ready) done)
               (push ready out)
               (setf pending (remove ready pending :test #'eq))))
    (nreverse out)))

(defgeneric commit-composite (store parts &key parents ref expected message
                                              provenance valid-at id)
  (:documentation "Commit a composite revision whose content is
(MAKE-MANIFEST PARTS) → REVISION. PARENTS, REF, EXPECTED, MESSAGE, PROVENANCE,
VALID-AT and ID as in COMMIT-REVISION (same defaults, conditions and restarts; a
backend that cannot honor a caller ID refuses it with REVISION-ERROR). A composite revision has no state to diff: REVISION-STATE
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
