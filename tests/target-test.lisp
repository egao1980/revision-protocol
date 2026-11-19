(in-package #:revision-protocol/tests)

;;; R0b target layer: in-memory target, stamps, drift, capture, transactions.

(deftest in-memory-target-basics
  (let ((tg (rev:make-in-memory-target)))
    (ok (rev:revision-target-p tg))
    (ok (rev:in-memory-target-p tg))
    (ok (null (rev:target-revisions tg)))
    (ok (null (rev:target-state tg)))
    (ok (equal '(:exact :shadow :model) (rev:target-fidelities tg)))
    (ok (rev:target-transactional-p tg)))
  (testing "fidelities are kept best first"
    (ok (equal '(:exact :model)
               (rev:target-fidelities
                (rev:make-in-memory-target :fidelities '(:model :exact))))))
  (ok (null (rev:target-transactional-p
             (rev:make-in-memory-target :transactional nil))))
  (testing "the initial state is copied; target-state returns a fresh copy"
    (let* ((state (list :a (list 1 2)))
           (tg (rev:make-in-memory-target :state state)))
      (setf (second (second state)) 99)
      (ok (%st= '(:a (1 2)) (rev:target-state tg)))
      (let ((read (rev:target-state tg)))
        (setf (second read) :clobbered)
        (ok (%st= '(:a (1 2)) (rev:target-state tg)))))))

(deftest target-stamp-records-without-applying
  (let* ((s (%fixture-store))
         (tg (rev:make-in-memory-target :state (list :x 1))))
    (ok (equal '("c1000000") (rev:target-stamp tg (list "c1000000"))))
    (ok (equal '("c1000000") (rev:target-revisions tg)))
    (ok (%st= '(:x 1) (rev:target-state tg)) "state untouched")
    (testing "revisions or ids, in order, duplicates removed"
      (ok (equal '("c3000000" "b2000000")
                 (rev:target-stamp tg (list (rev:resolve-revision s "c3000000")
                                            "b2000000" "c3000000"))))
      (ok (equal '("c3000000" "b2000000") (rev:target-revisions tg))))
    (testing "returned lists are fresh"
      (let ((ids (rev:target-revisions tg)))
        (setf (first ids) "zzzz")
        (ok (equal '("c3000000" "b2000000") (rev:target-revisions tg)))))
    (ok (null (rev:target-stamp tg nil)))
    (ok (null (rev:target-revisions tg)))))

(deftest target-drift-detection
  (let* ((s (%fixture-store))
         (tg (%target-at s "c1000000")))
    (ok (null (rev:target-drift tg s)) "no drift at a clean stamp")
    (setf (rev:target-state tg) (list :name "edited"))
    (ok (equal '("c1000000") (rev:target-revisions tg)) "setf leaves the stamp")
    (let ((d (rev:target-drift tg s)))
      (ok (typep d 'stack-diff:diff))
      (ok (%st= '(:name "edited")
                (stack-diff:apply-diff (%fixture-state "c1000000") d))
          "drift goes from the stamped state to the actual state")))
  (testing "an unstamped target drifts from the empty state"
    (let ((s (%fixture-store)))
      (ok (null (rev:target-drift (rev:make-in-memory-target) s)))
      (let ((d (rev:target-drift (rev:make-in-memory-target :state (list :a 1)) s)))
        (ok (%st= '(:a 1) (stack-diff:apply-diff nil d))))))
  (testing "several stamped revisions"
    (let* ((s (%fixture-store))
           (tg (rev:make-in-memory-target :revisions '("c1000000" "b1000000"))))
      (ok (signals (rev:target-drift tg s) 'rev:revision-error)))))

(deftest capture-revision-commits-the-target-state
  (let* ((s (%fixture-store))
         (tg (%target-at s "c1000000")))
    (setf (rev:target-state tg) (list :captured 1))
    (let ((r (rev:capture-revision tg s :message "wip")))
      (ok (rev:revision-p r))
      (ok (equal '("c1000000") (rev:revision-parent-ids r)) "parents default to the stamp")
      (ok (equal "wip" (rev:revision-message r)))
      (ok (%st= '(:captured 1) (rev:revision-state s r)))
      (ok (equal (list (rev:revision-id r)) (rev:target-revisions tg)))
      (ok (null (rev:target-drift tg s))))
    (testing "explicit parents"
      (setf (rev:target-state tg) (list :captured 2))
      (let ((r (rev:capture-revision tg s :parents '("c0000000"))))
        (ok (equal '("c0000000") (rev:revision-parent-ids r)))
        (ok (%st= '(:captured 2) (rev:revision-state s r)))))
    (testing "a fresh target captures a root"
      (let* ((fresh (rev:make-in-memory-target :state (list :new t)))
             (r (rev:capture-revision fresh s)))
        (ok (null (rev:revision-parent-ids r)))
        (ok (equal (list (rev:revision-id r)) (rev:target-revisions fresh)))))))

(deftest with-target-transaction-rolls-back
  (let* ((s (%fixture-store))
         (tg (%target-at s "c1000000")))
    (testing "normal exit keeps the changes and returns the body's values"
      (ok (equal '(1 2)
                 (multiple-value-list
                  (rev:with-target-transaction (tg)
                    (setf (rev:target-state tg) (list :kept t))
                    (values 1 2)))))
      (ok (%st= '(:kept t) (rev:target-state tg))))
    (testing "a non-local exit restores state and stamp"
      (setf (rev:target-state tg) (rev:revision-state s "c1000000"))
      (ok (eq :escaped
              (catch :out
                (rev:with-target-transaction (tg)
                  (setf (rev:target-state tg) (list :lost t))
                  (rev:target-stamp tg '("c3000000"))
                  (throw :out :escaped)))))
      (ok (%st= (%fixture-state "c1000000") (rev:target-state tg)))
      (ok (equal '("c1000000") (rev:target-revisions tg))))
    (testing "an error inside rolls back too"
      (ok (signals (rev:with-target-transaction (tg)
                     (rev:target-stamp tg '("c0000000"))
                     (error "boom"))
                   'simple-error))
      (ok (equal '("c1000000") (rev:target-revisions tg)))))
  (testing "a non-transactional target refuses without running the body"
    (let ((tg (rev:make-in-memory-target :transactional nil))
          (ran nil))
      (ok (signals (rev:with-target-transaction (tg) (setf ran t))
                   'rev:revision-error))
      (ok (null ran)))))
