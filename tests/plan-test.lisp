(in-package #:revision-protocol/tests)

;;; R0b plans: plan-transition, describe-plan, execute-plan with stamps,
;;; drift, irreversible steps, step failures, resume.

(deftest-parametrize plan-transition-shapes
    ((primary from to shape base) :ids (plan-shape-ids) :rows (plan-shape-rows))
  (let* ((s (%fixture-store primary))
         (tg (%target-at s from))
         (heads (length (rev:heads s)))
         (p (rev:plan-transition s tg to)))
    (ok (rev:transition-plan-p p))
    (ok (eq s (rev:plan-store p)))
    (ok (equal (and from (list from)) (rev:plan-start p)))
    (ok (equal (list to) (rev:plan-goal p)))
    (ok (equal base (rev:plan-base p)))
    (ok (equal shape (%shape p)) "down to the merge-base, up to TO")
    (testing "each step's diff takes its FROM state to its TO state"
      (dolist (step (rev:plan-steps p))
        (ok (%st= (%fixture-state (rev:step-to step))
                  (stack-diff:apply-diff (%fixture-state (rev:step-from step))
                                         (rev:step-diff step))))
        (ok (equal (stack-diff:diff-footprint (rev:step-diff step))
                   (rev:step-footprint step)))
        (ok (rev:step-reversible-p step))))
    (testing "planning is pure"
      (ok (equal (and from (list from)) (rev:target-revisions tg)))
      (ok (%st= (%fixture-state from) (rev:target-state tg)))
      (ok (= heads (length (rev:heads s)))))))

(deftest plan-transition-errors
  (let ((s (%fixture-store)))
    (ok (signals (rev:plan-transition s (%target-at s "c1000000") "ffff9999")
                 'rev:revision-not-found))
    (let ((c (%capture (rev:revision-error)
               (rev:plan-transition
                s (rev:make-in-memory-target :revisions '("c1000000" "b1000000"))
                "c3000000"))))
      (ok (typep c 'rev:revision-error) "several stamped revisions")
      (ok (not (typep c 'rev:revision-not-found))))
    (testing "plan lists are fresh"
      (let ((p (rev:plan-transition s (%target-at s "c1000000") "c3000000")))
        (setf (first (rev:plan-steps p)) nil
              (first (rev:plan-goal p)) "zzzz")
        (ok (typep (first (rev:plan-steps p)) 'rev:plan-step))
        (ok (equal '("c3000000") (rev:plan-goal p)))))))

(deftest describe-plan-format
  (let* ((s (rev:make-in-memory-store)))
    (%commit s :id "r1000000" :ref "main" :at 100 :content (list :a 1))
    (%commit s :id "r2000000" :ref "main" :at 200 :content (list :a 2 :b 1))
    (let ((up (rev:plan-transition s (%target-at s "r1000000") "r2000000"))
          (down (rev:plan-transition s (%target-at s "r2000000") "r1000000"))
          (fresh (rev:plan-transition s (rev:make-in-memory-target) "r1000000"))
          (none (rev:plan-transition s (%target-at s "r1000000") "r1000000")))
      (ok (equal (format nil "up r1000000 -> r2000000 sexp:/a sexp:/b~%")
                 (rev:describe-plan up nil)))
      (ok (equal (format nil "down r2000000 -> r1000000 sexp:/a sexp:/b~%")
                 (rev:describe-plan down nil)))
      (ok (equal (format nil "up - -> r1000000 sexp:/a~%")
                 (rev:describe-plan fresh nil)))
      (ok (equal "" (rev:describe-plan none nil)))
      (testing "stream T writes to *standard-output* and returns NIL"
        (let ((ret :unset))
          (ok (equal (format nil "up r1000000 -> r2000000 sexp:/a sexp:/b~%")
                     (with-output-to-string (*standard-output*)
                       (setf ret (rev:describe-plan up t)))))
          (ok (null ret))))
      (testing "a stream argument is written to"
        (ok (equal (rev:describe-plan up nil)
                   (with-output-to-string (out) (rev:describe-plan up out)))))))
  (testing "irreversible steps are marked"
    (let* ((s (%opaque-fixture))
           (p (rev:plan-transition s (%target-at s "o1000000") "o0000000")))
      (ok (null (rev:step-reversible-p (first (rev:plan-steps p)))))
      (ok (equal (format nil "down o1000000 -> o0000000 sexp:/col (irreversible)~%")
                 (rev:describe-plan p nil)))
      (ok (rev:step-reversible-p
           (first (rev:plan-steps
                   (rev:plan-transition s (%target-at s "o0000000") "o1000000"))))
          "up steps are reversible"))))

(deftest execute-plan-stamps-each-step
  (let* ((s (%fixture-store))
         (log (list nil))
         (tg (%target-at s "c3000000" :stamp-hook (%recording-hook log)))
         (p (rev:plan-transition s tg "b2000000")))
    (multiple-value-bind (revs skipped) (rev:execute-plan tg p)
      (ok (equal '("b2000000") revs))
      (ok (null skipped)))
    (ok (equal '("b2000000") (rev:target-revisions tg)))
    (ok (%st= (%fixture-state "b2000000") (rev:target-state tg)))
    (ok (equal '(("c2000000") ("c1000000") ("b1000000") ("b2000000"))
               (mapcar #'car (reverse (car log)))))
    (testing "executing a finished plan again is a no-op"
      (setf (car log) nil)
      (ok (equal '("b2000000") (rev:execute-plan tg p)))
      (ok (null (car log)))))
  (testing "through the empty state between unrelated lines"
    (let* ((s (%fixture-store))
           (log (list nil))
           (tg (%target-at s "l1000000" :stamp-hook (%recording-hook log))))
      (rev:execute-plan tg (rev:plan-transition s tg "c0000000"))
      (ok (equal '(("l0000000") nil ("c0000000"))
                 (mapcar #'car (reverse (car log)))))
      (ok (%st= (%fixture-state "c0000000") (rev:target-state tg))))))

(deftest execute-plan-refuses-a-foreign-stamp
  (let* ((s (%fixture-store))
         (tg (%target-at s "c1000000"))
         (p (rev:plan-transition s tg "c3000000")))
    (rev:target-stamp tg '("b1000000"))
    (setf (rev:target-state tg) (rev:revision-state s "b1000000"))
    (ok (signals (rev:execute-plan tg p) 'rev:revision-error))
    (ok (equal '("b1000000") (rev:target-revisions tg)))))

(defun %drifted (s)
  "Store, target at c1 drifted to (:junk 1), and a plan from c1 to c3."
  (let* ((tg (%target-at s "c1000000"))
         (p (rev:plan-transition s tg "c3000000")))
    (setf (rev:target-state tg) (list :junk 1))
    (values tg p)))

(deftest execute-plan-drift-restarts
  (testing "the condition"
    (let ((s (%fixture-store)))
      (multiple-value-bind (tg p) (%drifted s)
        (let ((c (%capture (rev:target-drift-detected) (rev:execute-plan tg p))))
          (ok (typep c 'rev:target-drift-detected))
          (ok (typep c 'rev:revision-error))
          (ok (eq tg (rev:target-drift-detected-target c)))
          (ok (%st= '(:junk 1)
                    (stack-diff:apply-diff (%fixture-state "c1000000")
                                           (rev:target-drift-detected-drift c))))))))
  (testing "capture-and-continue"
    (let ((s (%fixture-store)))
      (multiple-value-bind (tg p) (%drifted s)
        (let ((before (%ids (rev:revision-children s "c1000000"))))
          (ok (equal '("c3000000")
                     (handler-bind ((rev:target-drift-detected
                                      (lambda (c)
                                        (declare (ignore c))
                                        (invoke-restart 'rev:capture-and-continue))))
                       (rev:execute-plan tg p))))
          (let ((new (set-difference (%ids (rev:revision-children s "c1000000"))
                                     before :test #'equal)))
            (ok (= 1 (length new)) "the drift was captured as a child of the stamp")
            (ok (%st= '(:junk 1) (rev:revision-state s (first new)))))
          (ok (%st= (%fixture-state "c3000000") (rev:target-state tg)))))))
  (testing "discard-drift"
    (let ((s (%fixture-store)))
      (multiple-value-bind (tg p) (%drifted s)
        (let ((count (length (rev:walk-revisions s (rev:heads s)))))
          (ok (equal '("c3000000")
                     (handler-bind ((rev:target-drift-detected
                                      (lambda (c)
                                        (declare (ignore c))
                                        (invoke-restart 'rev:discard-drift))))
                       (rev:execute-plan tg p))))
          (ok (= count (length (rev:walk-revisions s (rev:heads s))))
              "nothing committed")
          (ok (%st= (%fixture-state "c3000000") (rev:target-state tg)))))))
  (testing "abort"
    (let ((s (%fixture-store)))
      (multiple-value-bind (tg p) (%drifted s)
        (ok (null (handler-bind ((rev:target-drift-detected
                                   (lambda (c)
                                     (declare (ignore c))
                                     (invoke-restart 'abort))))
                    (rev:execute-plan tg p))))
        (ok (equal '("c1000000") (rev:target-revisions tg)))
        (ok (%st= '(:junk 1) (rev:target-state tg)) "nothing changed")))))

(defun %on-irreversible (restart)
  (lambda (c)
    (declare (ignore c))
    (invoke-restart restart)))

(deftest execute-plan-irreversible-step
  (testing "the condition and its restarts"
    (let* ((s (%opaque-fixture))
           (tg (%target-at s "o1000000"))
           (p (rev:plan-transition s tg "o0000000"))
           (seen nil))
      ;; The handler sits inside OK: Rove's OK binds its own ERROR handler
      ;; around the form, which would run first if this one were outside.
      (ok (null (handler-bind ((rev:irreversible-step
                                 (lambda (c)
                                   (setf seen (list (rev:irreversible-step-step c)
                                                    (and (find-restart 'rev:skip-step c) t)
                                                    (and (find-restart 'rev:stamp-only c) t)
                                                    (and (find-restart 'rev:abort-transition c) t)))
                                   (invoke-restart 'rev:abort-transition))))
                  (rev:execute-plan tg p)))
          "abort-transition returns NIL")
      (ok (eq (first (rev:plan-steps p)) (first seen)))
      (ok (equal '(t t t) (rest seen)))
      (ok (equal '("o1000000") (rev:target-revisions tg)))
      (ok (%st= '(:t 1 :col "x") (rev:target-state tg)))))
  (testing "skip-step"
    (let* ((s (%opaque-fixture))
           (tg (%target-at s "o1000000"))
           (p (rev:plan-transition s tg "o0000000")))
      (multiple-value-bind (revs skipped)
          (handler-bind ((rev:irreversible-step (%on-irreversible 'rev:skip-step)))
            (rev:execute-plan tg p))
        (ok (equal '("o1000000") revs))
        (ok (equal (rev:plan-steps p) skipped)))
      (ok (%st= '(:t 1 :col "x") (rev:target-state tg)))))
  (testing "stamp-only"
    (let* ((s (%opaque-fixture))
           (tg (%target-at s "o1000000"))
           (p (rev:plan-transition s tg "o0000000")))
      (multiple-value-bind (revs skipped)
          (handler-bind ((rev:irreversible-step (%on-irreversible 'rev:stamp-only)))
            (rev:execute-plan tg p))
        (ok (equal '("o0000000") revs))
        (ok (null skipped)))
      (ok (%st= '(:t 1 :col "x") (rev:target-state tg)) "state unchanged")
      (ok (rev:target-drift tg s) "so the target drifts"))))

(deftest execute-plan-step-failed
  (testing "the condition and retry-step"
    (let* ((s (%fixture-store))
           (tg (%target-at s "c1000000" :fail-step (%fail-on "c2000000")))
           (p (rev:plan-transition s tg "c3000000"))
           (seen nil))
      (ok (equal '("c3000000")
                 (handler-bind ((rev:step-failed
                                  (lambda (c)
                                    (push c seen)
                                    (invoke-restart 'rev:retry-step))))
                   (rev:execute-plan tg p))))
      (ok (= 1 (length seen)) "the second attempt succeeded")
      (let ((c (first seen)))
        (ok (typep c 'rev:revision-error))
        (ok (= 1 (rev:step-failed-attempt c)))
        (ok (equal "c2000000" (rev:step-to (rev:step-failed-step c))))
        (ok (typep (rev:step-failed-cause c) 'simple-error)))
      (ok (%st= (%fixture-state "c3000000") (rev:target-state tg)))))
  (testing "all three restarts are offered"
    (let* ((s (%fixture-store))
           (tg (%target-at s "c1000000" :fail-step (%fail-on "c2000000")))
           (found nil))
      (handler-bind ((rev:step-failed
                       (lambda (c)
                         (setf found (mapcar (lambda (r) (and (find-restart r c) t))
                                             '(rev:retry-step rev:skip-step
                                               rev:rollback-to-start)))
                         (invoke-restart 'rev:rollback-to-start))))
        (rev:execute-plan tg (rev:plan-transition s tg "c3000000")))
      (ok (equal '(t t t) found))))
  (testing "skip-step"
    (let* ((s (%fixture-store))
           (tg (%target-at s "c1000000" :fail-step (%fail-on "c3000000" :attempts t)))
           (p (rev:plan-transition s tg "c3000000")))
      (multiple-value-bind (revs skipped)
          (handler-bind ((rev:step-failed
                           (lambda (c)
                             (declare (ignore c))
                             (invoke-restart 'rev:skip-step))))
            (rev:execute-plan tg p))
        (ok (equal '("c2000000") revs))
        (ok (equal (last (rev:plan-steps p)) skipped)))
      (ok (%st= (%fixture-state "c2000000") (rev:target-state tg)))))
  (testing "rollback-to-start"
    (let* ((s (%fixture-store))
           (tg (%target-at s "c1000000" :fail-step (%fail-on "c3000000" :attempts t)))
           (p (rev:plan-transition s tg "c3000000")))
      (ok (null (handler-bind ((rev:step-failed
                                 (lambda (c)
                                   (declare (ignore c))
                                   (invoke-restart 'rev:rollback-to-start))))
                  (rev:execute-plan tg p))))
      (ok (equal '("c1000000") (rev:target-revisions tg)))
      (ok (%st= (%fixture-state "c1000000") (rev:target-state tg))))))

(deftest execute-plan-resumes-after-a-failure
  (let* ((s (%fixture-store))
         (flag (list t))
         (log (list nil))
         (tg (%target-at s "c0000000"
                         :fail-step (%fail-on "c2000000" :attempts t :flag flag)
                         :stamp-hook (%recording-hook log)))
         (p (rev:plan-transition s tg "c3000000")))
    (ok (signals (rev:execute-plan tg p) 'rev:step-failed) "unhandled, it propagates")
    (ok (equal '("c1000000") (rev:target-revisions tg))
        "the stamp names the last completed step")
    (ok (%st= (%fixture-state "c1000000") (rev:target-state tg)))
    (setf (car flag) nil
          (car log) nil)
    (ok (equal '("c3000000") (rev:execute-plan tg p)))
    (ok (equal '(("c2000000") ("c3000000")) (mapcar #'car (reverse (car log))))
        "only the remaining steps ran")))
