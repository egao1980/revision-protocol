(in-package #:revision-protocol/tests)

;;; Law 1: the graph is acyclic (see cycle-refused in ref-test); revisions
;;; never change; tags never move (tags-never-move); ref CAS is linearizable.

(defun %snapshot (r)
  (list (rev:revision-id r)
        (rev:revision-parent-ids r)
        (rev:revision-message r)
        (rev:revision-content r)
        (rev:revision-provenance r)
        (rev:revision-recorded-at r)
        (rev:revision-valid-at r)))

(defun %same-snapshot-p (a b)
  (destructuring-bind (ia pa ma ca pra ra va) a
    (destructuring-bind (ib pb mb cb prb rb vb) b
      (and (equal ia ib) (equal pa pb) (equal ma mb) (equal ca cb)
           (equal pra prb) (dt:= ra rb)
           (if va (and vb (dt:= va vb)) (null vb))))))

(deftest law-1-revisions-never-change
  (let* ((s (%merge-store))
         (before (%snapshot (rev:resolve-revision s "bbbb0003"))))
    (%commit s :id "1111aaaa" :parents '("bbbb0003") :ref "other" :at 900)
    (rev:move-ref s "main" "aaaa0001")
    (rev:tag-revision s "keep" "bbbb0003")
    (rev:delete-ref s "other")
    (ok (%same-snapshot-p before (%snapshot (rev:resolve-revision s "bbbb0003"))))))

(deftest law-1-returned-lists-are-not-shared
  (let* ((s (%store))
         (r (%commit s :id "aaaa0001" :provenance (list :author "ada")))
         (m (%commit s :id "bbbb0002" :parents '("aaaa0001"))))
    (declare (ignore r))
    (let ((ps (rev:revision-parent-ids m))
          (pv (rev:revision-provenance (rev:resolve-revision s "aaaa0001"))))
      (setf (first ps) "zzzz9999"
            (second pv) "mallory"))
    (ok (equal '("aaaa0001") (rev:revision-parent-ids (rev:resolve-revision s "bbbb0002"))))
    (ok (equal '(:author "ada")
               (rev:revision-provenance (rev:resolve-revision s "aaaa0001"))))))

(deftest law-1-no-revision-writers
  (dolist (reader '(rev:revision-id rev:revision-parent-ids rev:revision-recorded-at
                    rev:revision-valid-at rev:revision-provenance rev:revision-message
                    rev:revision-content))
    (ng (fboundp (list 'setf reader)) (format nil "no (setf ~(~a~))" reader))))

(deftest-parametrize law-1-ref-cas-linearizable
    ((threads per-thread)
     :ids ("2x50" "8x25")
     (2 50)
     (8 25))
  ;; THREADS workers each commit PER-THREAD revisions onto main with the
  ;; default parent (the tip), retrying on REF-CONFLICT. Linearizable CAS ⇒ one
  ;; line of exactly THREADS × PER-THREAD + 1 revisions and a single head.
  (let* ((s (%store))
         (failures '())
         (lock (bt2:make-lock :name "failures")))
    (%commit s :ref "main")
    (let ((workers
            (loop for i below threads
                  collect (bt2:make-thread
                           (lambda ()
                             (handler-case
                                 (handler-bind ((rev:ref-conflict
                                                  (lambda (c)
                                                    (declare (ignore c))
                                                    (invoke-restart 'rev:retry))))
                                   (dotimes (k per-thread)
                                     (rev:commit-revision s (list :n k) :ref "main")))
                               (error (e)
                                 (bt2:with-lock-held (lock)
                                   (push (princ-to-string e) failures)))))
                           :name (format nil "committer ~d" i)))))
      (mapc #'bt2:join-thread workers))
    (ok (null failures) (format nil "worker errors: ~{~a~^; ~}" (last failures 1)))
    (when (null failures)
      (ok (= (1+ (* threads per-thread))
             (length (rev:walk-revisions s (rev:rev-ref "main") :order :first-parent))))
      (ok (= 1 (length (rev:heads s)))))))
