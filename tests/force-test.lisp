(in-package #:revision-protocol/tests)

;;; R0b: the permission-gated FORCE restart on REF-CONFLICT.

(defun %force-store (&optional (permit t))
  "Linear store (main at c) whose FORCE gate is PERMIT."
  (let ((s (rev:make-in-memory-store :permit-force permit)))
    (%commit s :id "aaaa0001" :ref "main" :at 100)
    (%commit s :id "bbbb0002" :ref "main" :at 200)
    (%commit s :id "cccc0003" :ref "main" :at 300)
    s))

(defun %force-offered-p (thunk)
  "Call THUNK, which must signal REF-CONFLICT; T when FORCE was offered.
Leaves through USE-CURRENT."
  (let ((offered :not-signalled))
    (handler-bind ((rev:ref-conflict
                     (lambda (c)
                       (setf offered (and (find-restart 'rev:force c) t))
                       (invoke-restart 'rev:use-current))))
      (funcall thunk))
    offered))

(defmacro %forcing (&body body)
  `(handler-bind ((rev:ref-conflict
                    (lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'rev:force))))
     ,@body))

(deftest permit-force-gate
  (ok (null (rev:permit-force-p (rev:make-in-memory-store) "main")))
  (ok (rev:permit-force-p (rev:make-in-memory-store :permit-force t) "main"))
  (let ((s (rev:make-in-memory-store
            :permit-force (lambda (name) (string= name "scratch")))))
    (ok (rev:permit-force-p s "scratch"))
    (ok (null (rev:permit-force-p s "main")))))

(deftest force-is-offered-only-when-permitted
  (ok (null (%force-offered-p
             (let ((s (%force-store nil)))
               (lambda () (rev:move-ref s "main" "aaaa0001" :expected "bbbb0002"))))))
  (ok (eq t (%force-offered-p
             (let ((s (%force-store t)))
               (lambda () (rev:move-ref s "main" "aaaa0001" :expected "bbbb0002"))))))
  (let ((s (%force-store (lambda (name) (string= name "scratch")))))
    (rev:move-ref s "scratch" "cccc0003")
    (ok (null (%force-offered-p
               (lambda () (rev:move-ref s "main" "aaaa0001" :expected "bbbb0002")))))
    (ok (eq t (%force-offered-p
               (lambda () (rev:move-ref s "scratch" "aaaa0001" :expected "bbbb0002")))))))

(deftest force-move-ref
  (let ((s (%force-store)))
    (let ((r (%forcing (rev:move-ref s "main" "aaaa0001" :expected "bbbb0002"))))
      (ok (rev:revision-ref-p r))
      (ok (equal "aaaa0001" (rev:ref-target r))))
    (ok (equal "aaaa0001" (%id s (rev:rev-ref "main"))))))

(deftest force-commit-revision
  (let* ((s (%force-store))
         (r (%forcing (%commit s :id "dddd0004" :ref "main" :expected "aaaa0001"
                                 :at 400))))
    (ok (rev:revision-p r))
    (ok (equal "dddd0004" (rev:revision-id r)))
    (ok (equal '("cccc0003") (rev:revision-parent-ids r)) "defaulted parent = tip")
    (ok (equal "dddd0004" (%id s (rev:rev-ref "main"))))))

(deftest force-delete-ref
  (let ((s (%force-store)))
    (ok (eq t (%forcing (rev:delete-ref s "main" :expected "aaaa0001"))))
    (ok (null (rev:find-ref s "main")))))
