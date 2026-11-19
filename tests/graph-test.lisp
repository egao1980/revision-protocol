(in-package #:revision-protocol/tests)

;;; Commit, resolve, parents, children.

(deftest in-memory-store-is-a-revision-store
  (let ((s (%store)))
    (ok (rev:revision-store-p s))
    (ok (rev:in-memory-store-p s))))

(deftest commit-root-and-resolve-by-id
  (let* ((s (%store))
         (r (%commit s :at 1700000000 :content '(:file "x")
                       :message "first" :provenance '(:author "ada")))
         (id (rev:revision-id r)))
    (ok (rev:revision-p r))
    (ok (stringp id))
    (ok (null (rev:revision-parent-ids r)))
    (ok (equal '(:file "x") (rev:revision-content r)))
    (ok (equal "first" (rev:revision-message r)))
    (ok (equal '(:author "ada") (rev:revision-provenance r)))
    (ok (dt:= (%t 1700000000) (rev:revision-recorded-at r)))
    (ok (null (rev:revision-valid-at r)))
    (let ((again (rev:resolve-revision s id)))
      (ok (equal id (rev:revision-id again)))
      (ok (equal "first" (rev:revision-message again))))))

(deftest default-ids-are-40-hex-and-unique
  (let* ((s (%store))
         (ids (loop repeat 50 collect (rev:revision-id (%commit s)))))
    (ok (every (lambda (id)
                 (and (= 40 (length id))
                      (every (lambda (ch) (find ch "0123456789abcdef")) id)))
               ids))
    (ok (= 50 (length (remove-duplicates ids :test #'equal))))))

(deftest caller-supplied-id
  (let ((s (%store)))
    (ok (equal "a1" (rev:revision-id (%commit s :id "a1"))))
    (ok (equal "a1" (%id s "a1")))))

(deftest valid-at-is-kept
  (let* ((s (%store))
         (r (%commit s :at 500 :valid-at (%t 42))))
    (ok (dt:= (%t 42) (rev:revision-valid-at r)))
    (ok (dt:= (%t 42) (rev:revision-time r :valid)))
    (ok (dt:= (%t 500) (rev:revision-time r :recorded)))))

(deftest resolve-by-ref-and-tag
  (let ((s (%linear-store)))
    (ok (equal "cccc0003" (%id s (rev:rev-ref "main"))))
    (rev:tag-revision s "v1" "bbbb0002")
    (ok (equal "bbbb0002" (%id s (rev:rev-tag "v1"))))
    (testing "rev-ref matches tags too"
      (ok (equal "bbbb0002" (%id s (rev:rev-ref "v1")))))
    (testing "rev-tag does not match a movable ref"
      (ok (signals (rev:resolve-revision s (rev:rev-tag "main"))
                   'rev:revision-not-found)))
    (testing "a bare string is an id, never a ref name"
      (ok (signals (rev:resolve-revision s "main")
                   'rev:revision-not-found)))))

(deftest resolve-revision-object
  (let* ((s (%linear-store))
         (b (rev:resolve-revision s "bbbb0002")))
    (ok (equal "bbbb0002" (%id s b)))))

(deftest commit-on-ref-defaults-parent-to-tip
  (let ((s (%linear-store)))
    (ok (equal '("aaaa0001") (rev:revision-parent-ids
                              (rev:resolve-revision s "bbbb0002"))))
    (ok (equal '("bbbb0002") (rev:revision-parent-ids
                              (rev:resolve-revision s "cccc0003"))))
    (ok (null (rev:revision-parent-ids (rev:resolve-revision s "aaaa0001"))))))

(deftest explicit-nil-parents-is-a-root
  (let ((s (%linear-store)))
    (let ((r (%commit s :id "dddd0004" :parents '() :ref "main")))
      (ok (null (rev:revision-parent-ids r)))
      (ok (equal "dddd0004" (%id s (rev:rev-ref "main")))))))

(deftest merge-parents-keep-order
  (let* ((s (%merge-store))
         (m (rev:resolve-revision s "eeee0004")))
    (ok (equal '("bbbb0003" "ffff0002") (rev:revision-parent-ids m)))
    (ok (equal '("bbbb0003" "ffff0002")
               (%ids (rev:revision-parents s "eeee0004"))))
    (ok (null (rev:revision-parents s "aaaa0001")))))

(deftest parents-accept-designators
  (let* ((s (%merge-store))
         (r (%commit s :id "9999aaaa"
                       :parents (list (rev:rev-ref "main") (rev:rev-ref "feature")))))
    (ok (equal '("eeee0004" "ffff0002") (rev:revision-parent-ids r)))))

(deftest children
  (let ((s (%merge-store)))
    (ok (%set= '("bbbb0003" "ffff0002") (%ids (rev:revision-children s "aaaa0001"))))
    (ok (equal '("eeee0004") (%ids (rev:revision-children s "ffff0002"))))
    (ok (null (rev:revision-children s "eeee0004")))))

(deftest missing-parent-commits-nothing
  (let ((s (%linear-store)))
    (ok (signals (%commit s :id "dddd0004" :parents '("ffff9999"))
                 'rev:revision-not-found))
    (ok (signals (rev:resolve-revision s "dddd0004") 'rev:revision-not-found))
    (ok (equal '("cccc0003") (%ids (rev:heads s))))))

(deftest parents-and-children-of-missing-revision
  (let ((s (%linear-store)))
    (ok (signals (rev:revision-parents s "ffff9999") 'rev:revision-not-found))
    (ok (signals (rev:revision-children s "ffff9999") 'rev:revision-not-found))))

(deftest not-found-carries-the-designator
  (let* ((s (%linear-store))
         (d (rev:rev-ref "nope"))
         (c (%capture (rev:revision-not-found) (rev:resolve-revision s d))))
    (ok (typep c 'rev:revision-not-found))
    (ok (eq d (rev:revision-not-found-designator c)))))

(deftest short-id-prefixes
  (let ((s (%store)))
    (%commit s :id "abcd0001")
    (%commit s :id "abcd0002")
    (%commit s :id "abce0003")
    (%commit s :id "ab")
    (testing "exact id"
      (ok (equal "abcd0001" (%id s "abcd0001"))))
    (testing "unique prefix"
      (ok (equal "abce0003" (%id s "abce"))))
    (testing "ambiguous prefix lists the candidates"
      (let ((c (%capture (rev:ambiguous-revision) (rev:resolve-revision s "abcd"))))
        (ok (typep c 'rev:ambiguous-revision))
        (ok (equal "abcd" (rev:ambiguous-revision-designator c)))
        (ok (%set= '("abcd0001" "abcd0002") (rev:ambiguous-revision-candidates c)))))
    (testing "exact id wins over prefix matches"
      (ok (equal "ab" (%id s "ab"))))
    (testing "prefix shorter than +minimum-id-prefix+ does not match"
      (ok (signals (rev:resolve-revision s "abc") 'rev:revision-not-found)))
    (testing "no match"
      (ok (signals (rev:resolve-revision s "ffff") 'rev:revision-not-found)))))

(deftest heads
  (let ((s (%store)))
    (ok (null (rev:heads s)))
    (%commit s :id "aaaa0001")
    (%commit s :id "bbbb0002" :parents '("aaaa0001"))
    (%commit s :id "cccc0003" :parents '("aaaa0001"))
    (ok (%set= '("bbbb0002" "cccc0003") (%ids (rev:heads s))))
    (%commit s :id "dddd0004" :parents '("bbbb0002" "cccc0003"))
    (ok (equal '("dddd0004") (%ids (rev:heads s))))))
