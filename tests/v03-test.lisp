(in-package #:revision-protocol/tests)

;;; 0.3.0: caller ids / valid time / provenance on every committing operation
;;; (#6), VC-TOOL-VERSION-ERROR (#4), the :REF-CAS and :CONFLICTED-REVISIONS
;;; capability queries (#5).

(defun %fixed-commit (fn &key (at 5000))
  (dt:with-fixed-clock ((%t at))
    (funcall fn)))

(defun %check-keys (store r &key id valid-at provenance)
  "R has ID, VALID-AT and PROVENANCE and STORE resolves ID to it."
  (ok (equal id (rev:revision-id r)) "caller id")
  (ok (eq r (rev:resolve-revision store id)) "resolves")
  (ok (eql (dt:instant-seconds valid-at)
           (let ((v (rev:revision-valid-at r))) (and v (dt:instant-seconds v))))
      "valid-at")
  (ok (equal provenance (rev:revision-provenance r)) "provenance"))

;;; #6 commit-composite

(deftest commit-composite-caller-keys
  (multiple-value-bind (files db cs) (%part-stores)
    (declare (ignore files))
    (let* ((parts (list (rev:make-manifest-part :db db "d1000000")))
           (r (rev:commit-composite cs parts :id "ckpt-1" :valid-at (%t 42)
                                             :provenance '(:agent "harness")
                                             :ref "main" :message "checkpoint")))
      (%check-keys cs r :id "ckpt-1" :valid-at (%t 42) :provenance '(:agent "harness"))
      (ok (equal "ckpt-1" (%id cs (rev:rev-ref "main"))) "ref moved")
      (ok (rev:composite-manifest cs "ckpt-1") "still a composite")
      (testing "the same rules as commit-revision"
        (ok (signals (rev:commit-composite cs parts :id "ckpt-1" :parents '()) 'rev:revision-error)
            "an existing id")
        (ok (typep (%capture (rev:cycle-refused)
                     (rev:commit-composite cs parts :id "ckpt-1" :parents '("ckpt-1")))
                   'rev:cycle-refused)
            "a cycle")
        (ok (signals (rev:commit-composite cs parts :id 7) 'type-error))
        (ok (signals (rev:commit-composite cs parts :valid-at 7) 'type-error)))
      (testing "omitted keys keep their defaults"
        (let ((r2 (%fixed-commit (lambda () (rev:commit-composite cs parts :ref "main")))))
          (ok (= 40 (length (rev:revision-id r2))) "store-chosen id")
          (ok (null (rev:revision-valid-at r2)))
          (ok (null (rev:revision-provenance r2)))
          (ok (equal '("ckpt-1") (rev:revision-parent-ids r2)) "parent = the ref's tip"))))))

;;; #6 commit-projection

(deftest commit-projection-caller-keys
  (let* ((s (%fixture-store))
         (p (rev:project-state s "c2000000" :writable t)))
    (setf (rev:projection-state p) (list :edited t))
    (let ((r (rev:commit-projection s p :id "proj-1" :valid-at (%t 7)
                                        :provenance '(:by "test") :ref "edits")))
      (%check-keys s r :id "proj-1" :valid-at (%t 7) :provenance '(:by "test"))
      (ok (equal '("c2000000") (rev:revision-parent-ids r)) "parents default to the projection's")
      (ok (equal "proj-1" (%id s (rev:rev-ref "edits"))))))
  (testing "a merge commit through a conflict restart keeps the keys"
    (let* ((s (%merge-fixture))
           (p (rev:project-merge s "ours0000" "cnfl0000"))
           (r (handler-bind ((rev:merge-conflict
                               (lambda (c) (declare (ignore c)) (invoke-restart 'rev:take-ours))))
                (rev:commit-projection s p :id "merge-1" :valid-at (%t 9) :provenance '(:x 1)))))
      (%check-keys s r :id "merge-1" :valid-at (%t 9) :provenance '(:x 1))
      (ok (= 2 (length (rev:revision-parent-ids r))) "a merge")))
  (testing "an existing id commits nothing"
    (let* ((s (%fixture-store))
           (p (rev:project-state s "c2000000" :writable t))
           (n (length (rev:walk-revisions s (rev:heads s)))))
      (ok (signals (rev:commit-projection s p :id "c1000000") 'rev:revision-error))
      (ok (= n (length (rev:walk-revisions s (rev:heads s))))))))

;;; #6 capture-revision and execute-plan's capture-and-continue

(deftest capture-revision-caller-keys
  (let* ((s (%fixture-store))
         (tg (%target-at s "c1000000")))
    (setf (rev:target-state tg) (list :captured 1))
    (let ((r (rev:capture-revision tg s :id "cap-1" :valid-at (%t 11)
                                        :provenance '(:drift t) :message "wip")))
      (%check-keys s r :id "cap-1" :valid-at (%t 11) :provenance '(:drift t))
      (ok (equal "wip" (rev:revision-message r)))
      (ok (equal '("c1000000") (rev:revision-parent-ids r)))
      (ok (equal '("cap-1") (rev:target-revisions tg)) "the target is stamped with the caller id")
      (ok (null (rev:target-drift tg s))))
    (testing "a refused id leaves the stamp"
      (setf (rev:target-state tg) (list :again 1))
      (ok (signals (rev:capture-revision tg s :id "c0000000") 'rev:revision-error))
      (ok (equal '("cap-1") (rev:target-revisions tg))))))

(deftest capture-and-continue-message-and-id
  (testing "default message"
    (let ((s (%fixture-store)))
      (multiple-value-bind (tg p) (%drifted s)
        (let ((before (%ids (rev:revision-children s "c1000000"))))
          (handler-bind ((rev:target-drift-detected
                           (lambda (c) (declare (ignore c))
                             (invoke-restart 'rev:capture-and-continue))))
            (rev:execute-plan tg p))
          (let ((new (set-difference (%ids (rev:revision-children s "c1000000"))
                                     before :test #'equal)))
            (ok (= 1 (length new)))
            (ok (equal "captured drift"
                       (and new (rev:revision-message (rev:resolve-revision s (first new)))))))))))
  (testing ":id and :message as restart arguments"
    (let ((s (%fixture-store)))
      (multiple-value-bind (tg p) (%drifted s)
        (ok (equal '("c3000000")
                   (handler-bind ((rev:target-drift-detected
                                    (lambda (c) (declare (ignore c))
                                      (invoke-restart 'rev:capture-and-continue
                                                      :id "drift-1" :message "agent edits"))))
                     (rev:execute-plan tg p))))
        (let ((r (rev:resolve-revision s "drift-1")))
          (ok (equal "agent edits" (rev:revision-message r)))
          (ok (equal '("c1000000") (rev:revision-parent-ids r)))
          (ok (%st= '(:junk 1) (rev:revision-state s r))))))))

;;; #4 vc-tool-version-error

(deftest vc-tool-version-error-condition
  (let ((c (make-condition 'rev:vc-tool-version-error :tool "git" :found "2.42.1" :required "2.43.0")))
    (ok (typep c 'rev:revision-error))
    (ok (equal "git" (rev:vc-tool-version-error-tool c)))
    (ok (equal "2.42.1" (rev:vc-tool-version-error-found c)))
    (ok (equal "2.43.0" (rev:vc-tool-version-error-required c)))
    (ok (equal "git version is too old or missing: found 2.42.1, need 2.43.0 or later"
               (princ-to-string c))))
  (let ((c (make-condition 'rev:vc-tool-version-error :tool "jj" :required "0.47.1")))
    (ok (null (rev:vc-tool-version-error-found c)) "FOUND NIL: the program cannot be started")
    (ok (equal "jj program is too old or missing: found nothing, need 0.47.1 or later"
               (princ-to-string c))))
  (ok (signals (error 'rev:vc-tool-version-error :tool "git" :required "2.43.0")
               'rev:revision-error)))

;;; #5 capability queries

(defclass %bare-store (rev:revision-store) ()
  (:documentation "A store with no methods of its own: the defaults."))

(deftest capability-queries
  (let ((s (%store)))
    (ok (eq t (rev:ref-cas-p s)) "in-memory refs compare-and-swap")
    (ok (eq nil (rev:conflicted-revisions-p s)) "in-memory revisions are never conflicted"))
  (let ((b (make-instance '%bare-store)))
    (ok (eq t (rev:ref-cas-p b)) "default: as the in-memory store")
    (ok (eq nil (rev:conflicted-revisions-p b)))))
