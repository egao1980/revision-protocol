(in-package #:revision-protocol)

;;; In-memory reference store.
;;;
;;; Contract (see the brief, docs/capabilities/revision.md in cl-stack):
;;;  - default ids: 40 lowercase hex characters, unique within the store;
;;;    COMMIT-REVISION :ID overrides (any non-empty string);
;;;  - RECORDED-AT is (DATETIME-PROTOCOL:NOW) at commit, so tests pin it with
;;;    DATETIME-PROTOCOL:WITH-FIXED-CLOCK;
;;;  - safe for concurrent use from several threads (one store lock via
;;;    bordeaux-threads bt2); ref CAS is linearizable;
;;;  - the listener runs after the update, outside the lock.
;;;
;;; Milestone R0a ships this class with STUB methods only. Every method below
;;; signals a plain SIMPLE-ERROR (never a REVISION-ERROR) so the test suite
;;; loads and fails without false passes. Implementers add slots as needed.

(defclass in-memory-store (revision-store)
  ((listener :initform nil))
  (:documentation "Reference REVISION-STORE kept in Lisp memory."))

(defun in-memory-store-p (x)
  "T when X is a IN-MEMORY-STORE."
  (typep x 'in-memory-store))

(defun make-in-memory-store ()
  "A fresh, empty IN-MEMORY-STORE."
  (make-instance 'in-memory-store))

(defun %not-implemented (operation)
  (error "revision-protocol: ~s is not implemented for IN-MEMORY-STORE (R0a stub)"
         operation))

(defmethod resolve-revision ((store in-memory-store) designator)
  (declare (ignore designator))
  (%not-implemented 'resolve-revision))

(defmethod commit-revision ((store in-memory-store) content
                            &key parents ref expected message provenance
                              valid-at id)
  (declare (ignore content parents ref expected message provenance valid-at id))
  (%not-implemented 'commit-revision))

(defmethod move-ref ((store in-memory-store) name designator &key expected)
  (declare (ignore name designator expected))
  (%not-implemented 'move-ref))

(defmethod tag-revision ((store in-memory-store) name designator &key annotation)
  (declare (ignore name designator annotation))
  (%not-implemented 'tag-revision))

(defmethod delete-ref ((store in-memory-store) name &key expected)
  (declare (ignore name expected))
  (%not-implemented 'delete-ref))

(defmethod find-ref ((store in-memory-store) name)
  (declare (ignore name))
  (%not-implemented 'find-ref))

(defmethod list-refs ((store in-memory-store) &key kind prefix)
  (declare (ignore kind prefix))
  (%not-implemented 'list-refs))

(defmethod revision-parents ((store in-memory-store) designator)
  (declare (ignore designator))
  (%not-implemented 'revision-parents))

(defmethod revision-children ((store in-memory-store) designator)
  (declare (ignore designator))
  (%not-implemented 'revision-children))

(defmethod walk-revisions ((store in-memory-store) from &key order until reverse)
  (declare (ignore from order until reverse))
  (%not-implemented 'walk-revisions))

(defmethod merge-base ((store in-memory-store) a b &key all)
  (declare (ignore a b all))
  (%not-implemented 'merge-base))

(defmethod ancestor-p ((store in-memory-store) a b)
  (declare (ignore a b))
  (%not-implemented 'ancestor-p))

(defmethod revision-range ((store in-memory-store) include exclude)
  (declare (ignore include exclude))
  (%not-implemented 'revision-range))

(defmethod heads ((store in-memory-store))
  (%not-implemented 'heads))

(defmethod ref-listener ((store in-memory-store))
  (%not-implemented 'ref-listener))

(defmethod (setf ref-listener) (function (store in-memory-store))
  (declare (ignore function))
  (%not-implemented '(setf ref-listener)))
