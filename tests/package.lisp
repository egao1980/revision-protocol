(defpackage #:revision-protocol/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:rev #:revision-protocol)
                    (#:dt #:datetime-protocol)))

(in-package #:revision-protocol/tests)

;;; Shared helpers. Every test builds its own IN-MEMORY-STORE.

(defun %t (seconds)
  "Instant at unix SECONDS."
  (dt:make-instant seconds))

(defun %store ()
  (rev:make-in-memory-store))

(defun %commit (store &rest args &key (at 1000) (content nil content-p)
                &allow-other-keys)
  "COMMIT-REVISION under a fixed clock at unix AT. Other keys pass through."
  (let ((keys (loop for (k v) on args by #'cddr
                    unless (member k '(:at :content))
                      append (list k v))))
    (dt:with-fixed-clock ((%t at))
      (apply #'rev:commit-revision store
             (if content-p content (list :content at))
             keys))))

(defun %id (store designator)
  (rev:revision-id (rev:resolve-revision store designator)))

(defun %ids (revisions)
  (mapcar #'rev:revision-id revisions))

(defun %set= (a b)
  (and (subsetp a b :test #'equal)
       (subsetp b a :test #'equal)))

(defun %before-p (list x y)
  "X occurs strictly before Y in LIST (by EQUAL)."
  (let ((px (position x list :test #'equal))
        (py (position y list :test #'equal)))
    (and px py (< px py))))

(defun %parents-first-p (store revisions)
  "Every revision in REVISIONS comes after each of its parents that is also in
REVISIONS."
  (declare (ignore store))
  (let ((ids (%ids revisions)))
    (every (lambda (r)
             (every (lambda (p)
                      (or (not (member p ids :test #'equal))
                          (%before-p ids p (rev:revision-id r))))
                    (rev:revision-parent-ids r)))
           revisions)))

(defmacro %capture ((type) &body body)
  "Run BODY; return the condition of TYPE it signalled, or NIL."
  (let ((c (gensym "C")))
    `(handler-case (progn ,@body nil)
       (,type (,c) ,c))))

;;; Graph fixtures. Each returns the store; ids are caller-chosen so
;;; assertions can name them.

(defun %linear-store ()
  "root a ← b ← c on \"main\", recorded at 100, 200, 300."
  (let ((s (%store)))
    (%commit s :id "aaaa0001" :ref "main" :at 100 :message "a")
    (%commit s :id "bbbb0002" :ref "main" :at 200 :message "b")
    (%commit s :id "cccc0003" :ref "main" :at 300 :message "c")
    s))

(defun %merge-store ()
  "a ← b ← m (merge of b and f) on main; a ← f on feature.
Times: a 100, f 150, b 200, m 300."
  (let ((s (%store)))
    (%commit s :id "aaaa0001" :ref "main" :at 100)
    (%commit s :id "ffff0002" :parents '("aaaa0001") :ref "feature" :at 150)
    (%commit s :id "bbbb0003" :ref "main" :at 200)
    (%commit s :id "eeee0004" :parents '("bbbb0003" "ffff0002") :ref "main"
               :at 300 :message "merge feature")
    s))

(defun %criss-cross-store ()
  "a; b and c from a; d = merge(b, c); e = merge(c, b)."
  (let ((s (%store)))
    (%commit s :id "aaaa0001" :at 100)
    (%commit s :id "bbbb0002" :parents '("aaaa0001") :at 200)
    (%commit s :id "cccc0003" :parents '("aaaa0001") :at 210)
    (%commit s :id "dddd0004" :parents '("bbbb0002" "cccc0003") :at 300)
    (%commit s :id "eeee0005" :parents '("cccc0003" "bbbb0002") :at 310)
    s))
