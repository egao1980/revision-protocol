(in-package #:revision-protocol/tests)

;;; walk-revisions, merge-base, ancestor-p, revision-range.

(deftest walk-topo-parents-before-children
  (let* ((s (%criss-cross-store))
         (walk (rev:walk-revisions s (list "dddd0004" "eeee0005") :order :topo)))
    (ok (%set= '("aaaa0001" "bbbb0002" "cccc0003" "dddd0004" "eeee0005")
               (%ids walk)))
    (ok (= 5 (length walk)) "each revision once")
    (ok (%parents-first-p s walk))
    (ok (equal "aaaa0001" (rev:revision-id (first walk))))))

(deftest walk-default-order-is-topo
  (let ((s (%merge-store)))
    (ok (%parents-first-p s (rev:walk-revisions s "eeee0004")))
    (ok (= 4 (length (rev:walk-revisions s "eeee0004"))))))

(deftest walk-topo-reverse-children-first
  (let* ((s (%merge-store))
         (walk (rev:walk-revisions s (rev:rev-ref "main") :order :topo :reverse t)))
    (ok (equal "eeee0004" (rev:revision-id (first walk))))
    (ok (equal "aaaa0001" (rev:revision-id (car (last walk)))))
    (ok (%parents-first-p s (reverse walk)))))

(deftest walk-first-parent
  (let ((s (%merge-store)))
    (ok (equal '("aaaa0001" "bbbb0003" "eeee0004")
               (%ids (rev:walk-revisions s (rev:rev-ref "main")
                                         :order :first-parent))))
    (ok (equal '("eeee0004" "bbbb0003" "aaaa0001")
               (%ids (rev:walk-revisions s (rev:rev-ref "main")
                                         :order :first-parent :reverse t))))))

(deftest walk-time-ascending
  (let ((s (%merge-store)))
    (ok (equal '("aaaa0001" "ffff0002" "bbbb0003" "eeee0004")
               (%ids (rev:walk-revisions s "eeee0004" :order :time))))))

(deftest walk-time-respects-parents-under-clock-skew
  (let ((s (%store)))
    (%commit s :id "aaaa0001" :at 500)
    ;; child recorded before its parent (skewed clock)
    (%commit s :id "bbbb0002" :parents '("aaaa0001") :at 100)
    (%commit s :id "cccc0003" :parents '("aaaa0001") :at 300)
    (let ((walk (rev:walk-revisions s (list "bbbb0002" "cccc0003") :order :time)))
      (ok (equal "aaaa0001" (rev:revision-id (first walk))))
      (ok (%parents-first-p s walk)))))

(deftest walk-until-excludes-ancestors
  (let ((s (%merge-store)))
    (ok (%set= '("eeee0004" "ffff0002")
               (%ids (rev:walk-revisions s "eeee0004" :until "bbbb0003"))))
    (ok (equal '("eeee0004")
               (%ids (rev:walk-revisions s "eeee0004" :order :first-parent
                                                      :until "bbbb0003"))))))

(deftest walk-missing-start
  (let ((s (%merge-store)))
    (ok (signals (rev:walk-revisions s "ffff9999") 'rev:revision-not-found))))

(deftest merge-base-simple-fork
  (let ((s (%store)))
    (%commit s :id "aaaa0001" :at 100)
    (%commit s :id "bbbb0002" :parents '("aaaa0001") :at 200)
    (%commit s :id "cccc0003" :parents '("bbbb0002") :at 300)
    (%commit s :id "dddd0004" :parents '("bbbb0002") :at 310)
    (%commit s :id "eeee0005" :parents '("dddd0004") :at 320)
    (ok (equal "bbbb0002" (rev:revision-id (rev:merge-base s "cccc0003" "eeee0005"))))
    (ok (equal '("bbbb0002") (%ids (rev:merge-base s "cccc0003" "eeee0005" :all t))))))

(deftest merge-base-of-ancestor-is-the-ancestor
  (let ((s (%linear-store)))
    (ok (equal "aaaa0001" (rev:revision-id (rev:merge-base s "aaaa0001" "cccc0003"))))
    (ok (equal "cccc0003" (rev:revision-id (rev:merge-base s "cccc0003" "cccc0003"))))))

(deftest merge-base-criss-cross
  (let ((s (%criss-cross-store)))
    (let ((all (%ids (rev:merge-base s "dddd0004" "eeee0005" :all t))))
      (ok (%set= '("bbbb0002" "cccc0003") all)))
    (let ((one (rev:merge-base s "dddd0004" "eeee0005")))
      (ok (member (rev:revision-id one) '("bbbb0002" "cccc0003") :test #'equal))
      (testing "deterministic"
        (ok (equal (rev:revision-id one)
                   (rev:revision-id (rev:merge-base s "dddd0004" "eeee0005"))))))))

(deftest merge-base-unrelated-is-nil
  (let ((s (%store)))
    (%commit s :id "aaaa0001")
    (%commit s :id "bbbb0002")
    (ok (null (rev:merge-base s "aaaa0001" "bbbb0002")))
    (ok (null (rev:merge-base s "aaaa0001" "bbbb0002" :all t)))))

(deftest ancestor-p
  (let ((s (%merge-store)))
    (ok (rev:ancestor-p s "aaaa0001" "eeee0004"))
    (ok (rev:ancestor-p s "ffff0002" (rev:rev-ref "main")) "through the second parent")
    (ok (rev:ancestor-p s "bbbb0003" "bbbb0003") "reflexive")
    (ng (rev:ancestor-p s "eeee0004" "aaaa0001"))
    (ng (rev:ancestor-p s "bbbb0003" "ffff0002"))))

(deftest revision-range-include-exclude
  (let ((s (%merge-store)))
    (testing "main ^feature"
      (ok (%set= '("bbbb0003" "eeee0004")
                 (%ids (rev:revision-range s (rev:rev-ref "main") (rev:rev-ref "feature"))))))
    (testing "feature ^main is empty"
      (ok (null (rev:revision-range s (rev:rev-ref "feature") (rev:rev-ref "main")))))
    (testing "no exclude = all ancestors, topo order"
      (let ((all (rev:revision-range s "eeee0004" nil)))
        (ok (= 4 (length all)))
        (ok (%parents-first-p s all))))
    (testing "lists of designators"
      (ok (%set= '("bbbb0003" "ffff0002")
                 (%ids (rev:revision-range s (list "bbbb0003" "ffff0002")
                                           (list "aaaa0001"))))))))
