(in-package #:revision-protocol/tests)

;;; 0.4.0 (#2): COMMIT-REVISION :EXPECTED without :REF signals; TAG-REVISION
;;; keeps tags immutable but offers REPLACE-ANNOTATION where TAG-REWRITE-P.

(deftest expected-without-ref-signals
  (let* ((s (%linear-store))
         (n (length (rev:walk-revisions s (rev:heads s)))))
    (ok (signals (%commit s :expected "cccc0003") 'rev:revision-error) "an id")
    (ok (signals (%commit s :expected nil) 'rev:revision-error) "NIL counts as given")
    (ok (signals (%commit s :id "dddd0004" :parents '("cccc0003") :expected "cccc0003")
                 'rev:revision-error)
        "with explicit parents")
    (ok (not (typep (%capture (rev:revision-error) (%commit s :expected nil))
                    'rev:ref-conflict))
        "not a ref conflict")
    (ok (= n (length (rev:walk-revisions s (rev:heads s)))) "nothing committed")
    (ok (equal "cccc0003" (%id s (rev:rev-ref "main"))))
    (testing "commit-composite passes it through"
      (ok (signals (rev:commit-composite s (list (rev:make-manifest-part :x s "aaaa0001"))
                                         :expected "cccc0003")
                   'rev:revision-error)))
    (testing "with :ref it still is a CAS value"
      (ok (equal "dddd0005"
                 (rev:revision-id (%commit s :id "dddd0005" :ref "main"
                                             :expected "cccc0003")))))))

(defun %replacing (c)
  (declare (ignore c))
  (invoke-restart 'rev:replace-annotation))

(deftest tag-annotation-stays-immutable
  (let ((s (%linear-store))
        (events '()))
    (rev:tag-revision s "v1" "aaaa0001" :annotation "first")
    (setf (rev:ref-listener s) (lambda (e) (push e events)))
    (let ((c (%capture (rev:tag-immutable)
               (rev:tag-revision s "v1" "aaaa0001" :annotation "second"))))
      (ok (typep c 'rev:tag-immutable) "a different annotation signals")
      (ok (equal "aaaa0001" (rev:tag-immutable-target c))))
    (ok (equal "first" (rev:ref-annotation (rev:find-ref s "v1"))) "unchanged")
    (testing "same annotation is idempotent"
      (ok (equal "first" (rev:ref-annotation
                          (rev:tag-revision s "v1" "aaaa0001" :annotation "first")))))
    (testing "REPLACE-ANNOTATION rewrites the annotation in place"
      (ok (eq t (rev:tag-rewrite-p s)) "in-memory store rewrites tags")
      (let ((r (handler-bind ((rev:tag-immutable #'%replacing))
                 (rev:tag-revision s "v1" (rev:rev-tag "v1") :annotation "second"))))
        (ok (equal "second" (rev:ref-annotation r)))
        (ok (eq :tag (rev:ref-kind r)))
        (ok (equal "aaaa0001" (rev:ref-target r)))
        (ok (equal "second" (rev:ref-annotation (rev:find-ref s "v1"))))))
    (testing "to NIL"
      (handler-bind ((rev:tag-immutable #'%replacing))
        (rev:tag-revision s "v1" "aaaa0001"))
      (ok (null (rev:ref-annotation (rev:find-ref s "v1")))))
    (ok (null events) "an annotation rewrite is not a ref move")))

(deftest replace-annotation-offered-only-when-applicable
  (let ((s (%linear-store)))
    (rev:tag-revision s "v1" "aaaa0001" :annotation "first")
    (flet ((offered-p (thunk)
             (handler-case
                 (handler-bind ((rev:tag-immutable
                                  (lambda (c)
                                    (return-from offered-p
                                      (and (find-restart 'rev:replace-annotation c) t)))))
                   (funcall thunk)
                   :no-signal)
               (error () :other-error))))
      (ok (eq t (offered-p (lambda () (rev:tag-revision s "v1" "aaaa0001" :annotation "x"))))
          "same target, other annotation")
      (ok (eq nil (offered-p (lambda () (rev:tag-revision s "v1" "bbbb0002" :annotation "first"))))
          "another target: tags never move")
      (ok (eq nil (offered-p (lambda () (rev:move-ref s "v1" "aaaa0001"))))
          "not from move-ref")
      (let ((g (make-instance '%no-rewrite-store)))
        (rev:tag-revision g "v1" "aaaa0001" :annotation "first")
        (ok (eq nil (rev:tag-rewrite-p g)))
        (ok (eq nil (offered-p (lambda () (rev:tag-revision g "v1" "aaaa0001" :annotation "x"))))
            "a store without TAG-REWRITE-P")))))

(defclass %no-rewrite-store (rev:in-memory-store) ()
  (:default-initargs :engine (make-instance 'diff-protocol:sexp-diff-engine))
  (:documentation "An in-memory store that answers TAG-REWRITE-P NIL."))

(defmethod rev:tag-rewrite-p ((s %no-rewrite-store))
  nil)

(defmethod initialize-instance :after ((s %no-rewrite-store) &key)
  (%commit s :id "aaaa0001"))

(deftest tag-rewrite-p-default
  (ok (eq nil (rev:tag-rewrite-p (make-instance '%bare-store))) "default: NIL"))
