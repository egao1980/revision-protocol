(in-package #:revision-protocol/tests)

;;; Designator values and their algebra (rev-parent, rev-ancestor).

(deftest designators-are-values
  (let ((d (rev:rev-ref "main")))
    (ok (rev:revision-designator-p d))
    (ok (typep d 'rev:ref-designator))
    (ok (equal "main" (rev:designator-name d))))
  (ok (typep (rev:rev-tag "v1") 'rev:tag-designator))
  (ok (rev:revision-designator-p "3f9a"))
  (let ((d (rev:rev-as-of (%t 10) :on "main" :axis :valid)))
    (ok (typep d 'rev:as-of-designator))
    (ok (eq :valid (rev:designator-axis d)))
    (ok (typep (rev:designator-on d) 'rev:ref-designator) "string :on is a ref name")
    (ok (dt:= (%t 10) (rev:designator-time d))))
  (let ((d (rev:rev-ancestor (rev:rev-ref "main") 3)))
    (ok (typep d 'rev:ancestor-designator))
    (ok (= 3 (rev:designator-n d)))
    (ok (typep (rev:designator-base d) 'rev:ref-designator)))
  (ok (= 1 (rev:designator-n (rev:rev-parent "abcd")))))

(deftest designator-constructors-check-types
  (ok (signals (rev:rev-ref :main) 'type-error))
  (ok (signals (rev:rev-parent "abcd" 0) 'type-error))
  (ok (signals (rev:rev-ancestor "abcd" -1) 'type-error))
  (ok (signals (rev:rev-as-of 10 :on "main") 'type-error))
  (ok (signals (rev:rev-as-of (%t 10) :on "main" :axis :wall) 'type-error))
  (ok (signals (rev:rev-as-of (%t 10)) 'type-error))
  (ok (signals (rev:rev-parent 42) 'type-error)))

(deftest parent-n-of-a-merge
  (let ((s (%merge-store)))
    (ok (equal "bbbb0003" (%id s (rev:rev-parent (rev:rev-ref "main")))))
    (ok (equal "bbbb0003" (%id s (rev:rev-parent (rev:rev-ref "main") 1))))
    (ok (equal "ffff0002" (%id s (rev:rev-parent (rev:rev-ref "main") 2))))
    (ok (signals (rev:resolve-revision s (rev:rev-parent (rev:rev-ref "main") 3))
                 'rev:revision-not-found))
    (ok (signals (rev:resolve-revision s (rev:rev-parent "aaaa0001"))
                 'rev:revision-not-found)
        "a root has no parent")))

(deftest ancestor-n-of-a-ref
  (let ((s (%merge-store)))
    (ok (equal "eeee0004" (%id s (rev:rev-ancestor (rev:rev-ref "main") 0))))
    (ok (equal "bbbb0003" (%id s (rev:rev-ancestor (rev:rev-ref "main") 1))))
    (ok (equal "aaaa0001" (%id s (rev:rev-ancestor (rev:rev-ref "main") 2))))
    (ok (signals (rev:resolve-revision s (rev:rev-ancestor (rev:rev-ref "main") 3))
                 'rev:revision-not-found))))

(deftest designators-compose
  (let ((s (%merge-store)))
    (testing "second parent of the tip, then its parent"
      (ok (equal "aaaa0001"
                 (%id s (rev:rev-parent (rev:rev-parent (rev:rev-ref "main") 2))))))
    (testing "ancestor of a parent"
      (ok (equal "aaaa0001"
                 (%id s (rev:rev-ancestor (rev:rev-parent (rev:rev-ref "main") 1) 1)))))
    (testing "ancestor of an id prefix"
      (ok (equal "aaaa0001" (%id s (rev:rev-ancestor "bbbb" 1)))))
    (testing "ancestor of an as-of"
      (ok (equal "aaaa0001"
                 (%id s (rev:rev-ancestor (rev:rev-as-of (%t 250) :on "main") 1)))))
    (testing "as-of on a parent"
      (ok (equal "aaaa0001"
                 (%id s (rev:rev-as-of (%t 199)
                                       :on (rev:rev-parent (rev:rev-ref "main")))))))))
