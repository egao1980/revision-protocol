(in-package #:revision-protocol/tests)

;;; Refs, tags, CAS, restarts, listener.

(deftest move-ref-creates-and-moves
  (let ((s (%linear-store)))
    (let ((r (rev:move-ref s "topic" "aaaa0001")))
      (ok (rev:revision-ref-p r))
      (ok (equal "topic" (rev:ref-name r)))
      (ok (eq :movable (rev:ref-kind r)))
      (ok (equal "aaaa0001" (rev:ref-target r))))
    (rev:move-ref s "topic" (rev:rev-ref "main"))
    (ok (equal "cccc0003" (%id s (rev:rev-ref "topic"))))
    (testing "move-ref to a missing revision"
      (ok (signals (rev:move-ref s "topic" "ffff9999") 'rev:revision-not-found))
      (ok (equal "cccc0003" (%id s (rev:rev-ref "topic")))))))

(deftest ref-values-are-snapshots
  (let* ((s (%linear-store))
         (before (rev:find-ref s "main")))
    (rev:move-ref s "main" "aaaa0001")
    (ok (equal "cccc0003" (rev:ref-target before)))
    (ok (equal "aaaa0001" (rev:ref-target (rev:find-ref s "main"))))))

(deftest find-ref-and-list-refs
  (let ((s (%linear-store)))
    (rev:move-ref s "feature/x" "aaaa0001")
    (rev:move-ref s "feature/y" "bbbb0002")
    (rev:tag-revision s "v1" "bbbb0002" :annotation "first release")
    (ok (null (rev:find-ref s "nope")))
    (let ((tag (rev:find-ref s "v1")))
      (ok (eq :tag (rev:ref-kind tag)))
      (ok (equal "first release" (rev:ref-annotation tag))))
    (ok (equal '("feature/x" "feature/y" "main" "v1")
               (mapcar #'rev:ref-name (rev:list-refs s))))
    (ok (equal '("feature/x" "feature/y" "main")
               (mapcar #'rev:ref-name (rev:list-refs s :kind :movable))))
    (ok (equal '("v1") (mapcar #'rev:ref-name (rev:list-refs s :kind :tag))))
    (ok (equal '("feature/x" "feature/y")
               (mapcar #'rev:ref-name (rev:list-refs s :prefix "feature/"))))))

(deftest move-ref-cas
  (let ((s (%linear-store)))
    (testing "matching expected moves"
      (rev:move-ref s "main" "bbbb0002" :expected "cccc0003")
      (ok (equal "bbbb0002" (%id s (rev:rev-ref "main")))))
    (testing "expected nil creates only when absent"
      (rev:move-ref s "fresh" "aaaa0001" :expected nil)
      (ok (equal "aaaa0001" (%id s (rev:rev-ref "fresh"))))
      (ok (signals (rev:move-ref s "fresh" "bbbb0002" :expected nil) 'rev:ref-conflict)))
    (testing "stale expected signals ref-conflict with the slots filled"
      (let ((c (%capture (rev:ref-conflict)
                 (rev:move-ref s "main" "aaaa0001" :expected "cccc0003"))))
        (ok (typep c 'rev:ref-conflict))
        (ok (equal "main" (rev:ref-conflict-name c)))
        (ok (equal "cccc0003" (rev:ref-conflict-expected c)))
        (ok (equal "bbbb0002" (rev:ref-conflict-actual c))))
      (ok (equal "bbbb0002" (%id s (rev:rev-ref "main"))) "ref unchanged"))))

(deftest ref-conflict-use-current-restart
  (let ((s (%linear-store)))
    (rev:move-ref s "main" "bbbb0002")
    (let ((r (handler-bind ((rev:ref-conflict
                              (lambda (c)
                                (declare (ignore c))
                                (invoke-restart 'rev:use-current))))
               (rev:move-ref s "main" "aaaa0001" :expected "cccc0003"))))
      (ok (rev:revision-ref-p r))
      (ok (equal "bbbb0002" (rev:ref-target r)))
      (ok (equal "bbbb0002" (%id s (rev:rev-ref "main")))))))

(deftest ref-conflict-retry-restart
  (let ((s (%linear-store))
        (signalled 0))
    (rev:move-ref s "main" "bbbb0002")
    (let ((r (handler-bind ((rev:ref-conflict
                              (lambda (c)
                                (declare (ignore c))
                                (incf signalled)
                                (invoke-restart 'rev:retry))))
               (rev:move-ref s "main" "aaaa0001" :expected "cccc0003"))))
      (ok (= 1 signalled))
      (ok (equal "aaaa0001" (rev:ref-target r)))
      (ok (equal "aaaa0001" (%id s (rev:rev-ref "main")))))))

(deftest commit-revision-cas
  (let ((s (%linear-store)))
    (testing "stale expected: nothing committed"
      (ok (signals (%commit s :id "dddd0004" :ref "main" :expected "bbbb0002")
                   'rev:ref-conflict))
      (ok (signals (rev:resolve-revision s "dddd0004") 'rev:revision-not-found))
      (ok (equal "cccc0003" (%id s (rev:rev-ref "main")))))
    (testing "use-current returns the current tip"
      (let ((r (handler-bind ((rev:ref-conflict
                                (lambda (c)
                                  (declare (ignore c))
                                  (invoke-restart 'rev:use-current))))
                 (%commit s :id "dddd0004" :ref "main" :expected "bbbb0002"))))
        (ok (equal "cccc0003" (rev:revision-id r)))
        (ok (signals (rev:resolve-revision s "dddd0004") 'rev:revision-not-found))))
    (testing "retry commits on the current tip"
      (let ((r (handler-bind ((rev:ref-conflict
                                (lambda (c)
                                  (declare (ignore c))
                                  (invoke-restart 'rev:retry))))
                 (%commit s :id "dddd0004" :ref "main" :expected "bbbb0002"))))
        (ok (equal "dddd0004" (rev:revision-id r)))
        (ok (equal '("cccc0003") (rev:revision-parent-ids r)))
        (ok (equal "dddd0004" (%id s (rev:rev-ref "main"))))))))

(deftest delete-ref
  (let ((s (%linear-store)))
    (rev:move-ref s "topic" "aaaa0001")
    (ok (eq t (rev:delete-ref s "topic")))
    (ok (null (rev:find-ref s "topic")))
    (ok (signals (rev:resolve-revision s (rev:rev-ref "topic")) 'rev:revision-not-found))
    (ok (signals (rev:delete-ref s "topic") 'rev:revision-not-found))
    (testing "revisions survive ref deletion"
      (ok (equal "aaaa0001" (%id s "aaaa0001"))))
    (testing "stale expected"
      (ok (signals (rev:delete-ref s "main" :expected "aaaa0001") 'rev:ref-conflict))
      (ok (rev:find-ref s "main"))
      (ok (null (handler-bind ((rev:ref-conflict
                                 (lambda (c)
                                   (declare (ignore c))
                                   (invoke-restart 'rev:use-current))))
                  (rev:delete-ref s "main" :expected "aaaa0001"))))
      (ok (rev:find-ref s "main")))))

(deftest tag-revision
  (let ((s (%linear-store)))
    (let ((tag (rev:tag-revision s "v1" "aaaa0001")))
      (ok (eq :tag (rev:ref-kind tag)))
      (ok (equal "aaaa0001" (rev:ref-target tag))))
    (testing "same target is idempotent"
      (ok (equal "aaaa0001" (rev:ref-target (rev:tag-revision s "v1" "aaaa0001")))))
    (testing "tag over a movable ref name"
      (ok (signals (rev:tag-revision s "main" "aaaa0001") 'rev:ref-conflict)))))

(deftest tags-never-move
  (let ((s (%linear-store)))
    (rev:tag-revision s "v1" "aaaa0001")
    (testing "move-ref on a tag"
      (let ((c (%capture (rev:tag-immutable) (rev:move-ref s "v1" "bbbb0002"))))
        (ok (typep c 'rev:tag-immutable))
        (ok (equal "v1" (rev:tag-immutable-name c)))
        (ok (equal "aaaa0001" (rev:tag-immutable-target c)))))
    (testing "re-tag to another revision"
      (ok (signals (rev:tag-revision s "v1" "bbbb0002") 'rev:tag-immutable)))
    (testing "commit onto a tag"
      (ok (signals (%commit s :ref "v1") 'rev:tag-immutable)))
    (testing "delete a tag"
      (ok (signals (rev:delete-ref s "v1") 'rev:tag-immutable)))
    (ok (equal "aaaa0001" (%id s (rev:rev-tag "v1"))))))

(deftest create-new-tag-restart
  (let ((s (%linear-store)))
    (rev:tag-revision s "v1" "aaaa0001")
    (flet ((new-tag (name)
             (lambda (c)
               (declare (ignore c))
               (invoke-restart 'rev:create-new-tag name))))
      (testing "from tag-revision"
        (let ((r (handler-bind ((rev:tag-immutable (new-tag "v1.1")))
                   (rev:tag-revision s "v1" "bbbb0002"))))
          (ok (equal "v1.1" (rev:ref-name r)))
          (ok (eq :tag (rev:ref-kind r)))
          (ok (equal "bbbb0002" (%id s (rev:rev-tag "v1.1"))))))
      (testing "from move-ref"
        (let ((r (handler-bind ((rev:tag-immutable (new-tag "v2")))
                   (rev:move-ref s "v1" "cccc0003"))))
          (ok (equal "v2" (rev:ref-name r)))
          (ok (equal "cccc0003" (%id s (rev:rev-tag "v2"))))))
      (ok (equal "aaaa0001" (%id s (rev:rev-tag "v1"))) "the old tag did not move"))))

(deftest cycle-refused
  (let ((s (%linear-store)))
    (let ((c (%capture (rev:cycle-refused)
               (%commit s :id "aaaa0001" :parents '("cccc0003")))))
      (ok (typep c 'rev:cycle-refused))
      (ok (equal "aaaa0001" (rev:cycle-refused-id c))))
    (ok (null (rev:revision-parent-ids (rev:resolve-revision s "aaaa0001"))))
    (ok (signals (%commit s :id "cccc0003" :parents '("cccc0003")) 'rev:cycle-refused)
        "self-parent")))

(deftest duplicate-id-refused
  (let ((s (%linear-store)))
    (ok (signals (%commit s :id "bbbb0002" :parents '() :message "other")
                 'rev:revision-error))
    (ok (equal "b" (rev:revision-message (rev:resolve-revision s "bbbb0002"))))))

(deftest ref-listener-events
  (let ((s (%store))
        (events '()))
    (ok (null (rev:ref-listener s)))
    (setf (rev:ref-listener s) (lambda (e) (push e events)))
    (ok (functionp (rev:ref-listener s)))
    (%commit s :id "aaaa0001" :ref "main")
    (%commit s :id "bbbb0002" :ref "main")
    (rev:move-ref s "main" "aaaa0001" :expected "bbbb0002")
    (rev:tag-revision s "v1" "bbbb0002")
    (rev:move-ref s "topic" "bbbb0002")
    (rev:delete-ref s "topic")
    (testing "failed and abandoned updates are silent"
      (ignore-errors (rev:move-ref s "main" "bbbb0002" :expected "bbbb0002"))
      (handler-bind ((rev:ref-conflict
                       (lambda (c) (declare (ignore c)) (invoke-restart 'rev:use-current))))
        (rev:move-ref s "main" "bbbb0002" :expected "bbbb0002"))
      (ignore-errors (rev:move-ref s "v1" "aaaa0001")))
    (ok (equal '((:ref-moved "main" nil "aaaa0001")
                 (:ref-moved "main" "aaaa0001" "bbbb0002")
                 (:ref-moved "main" "bbbb0002" "aaaa0001")
                 (:ref-moved "v1" nil "bbbb0002")
                 (:ref-moved "topic" nil "bbbb0002")
                 (:ref-moved "topic" "bbbb0002" nil))
               (reverse events)))
    (setf (rev:ref-listener s) nil)
    (rev:move-ref s "main" "bbbb0002")
    (ok (= 6 (length events)) "NIL removes the listener")))
