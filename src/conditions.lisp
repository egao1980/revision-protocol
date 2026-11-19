(in-package #:revision-protocol)

;;; Condition tree (R0a):
;;;
;;;   revision-error
;;;     revision-not-found
;;;     ambiguous-revision
;;;     ref-conflict      restarts: use-current, retry
;;;     tag-immutable     restart:  create-new-tag (new-name)
;;;     cycle-refused
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
TARGET is the id the tag points at. Restart: CREATE-NEW-TAG (where offered)."))

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
