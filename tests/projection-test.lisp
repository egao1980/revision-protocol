(in-package #:revision-protocol/tests)

;;; R0b projections: project-state, project-plan (fidelity restarts,
;;; staleness), project-merge (merge-conflict restarts), commit, discard.

(deftest project-state-basics
  (let* ((s (%fixture-store))
         (p (rev:project-state s "c2000000")))
    (ok (rev:projection-p p))
    (ok (eq :exact (rev:projection-fidelity p)))
    (ok (%st= (%fixture-state "c2000000") (rev:projection-view p)))
    (ok (%st= (%fixture-state "c2000000") (rev:projection-state p)))
    (ok (equal '("c2000000") (rev:projection-parents p)))
    (ok (null (rev:projection-conflicts p)))
    (ok (null (rev:projection-writable-p p)))
    (ok (null (rev:projection-discarded-p p)))
    (testing "the view is a copy"
      (let ((view (rev:projection-view p)))
        (setf (second view) "clobbered")
        (ok (%st= (%fixture-state "c2000000") (rev:projection-view p)))))
    (testing "a read-only projection refuses writes"
      (ok (signals (setf (rev:projection-state p) (list :x 1)) 'rev:revision-error))
      (ok (%st= (%fixture-state "c2000000") (rev:projection-state p)))))
  (let ((s (%fixture-store)))
    (ok (equal '("c3000000") (rev:projection-parents
                              (rev:project-state s (rev:rev-ref "main")))))
    (ok (eq :exact (rev:projection-fidelity
                    (rev:project-state s "c1000000" :fidelity :model)))
        "the in-memory store always projects exactly")
    (ok (signals (rev:project-state s "ffff9999") 'rev:revision-not-found))))

(deftest commit-writable-projection
  (let* ((s (%fixture-store))
         (p (rev:project-state s "c2000000" :writable t)))
    (ok (rev:projection-writable-p p))
    (setf (rev:projection-state p) (list :edited t))
    (ok (%st= '(:edited t) (rev:projection-view p)))
    (let ((r (rev:commit-projection s p :message "edit" :ref "edits")))
      (ok (rev:revision-p r))
      (ok (equal '("c2000000") (rev:revision-parent-ids r)))
      (ok (equal "edit" (rev:revision-message r)))
      (ok (%st= '(:edited t) (rev:revision-state s r)))
      (ok (equal (rev:revision-id r) (rev:ref-target (rev:find-ref s "edits")))))
    (testing "explicit parents"
      (let ((r (rev:commit-projection s p :parents '("c0000000"))))
        (ok (equal '("c0000000") (rev:revision-parent-ids r)))))))

(deftest project-plan-exact
  (let* ((s (%fixture-store))
         (tg (%target-at s "c1000000"))
         (plan (rev:plan-transition s tg "c3000000"))
         (p (rev:project-plan tg plan)))
    (ok (eq :exact (rev:projection-fidelity p)) "default: the best supported")
    (ok (%st= (%fixture-state "c3000000") (rev:projection-state p)))
    (ok (equal '("c3000000") (rev:projection-parents p)))
    (testing "the target is untouched"
      (ok (equal '("c1000000") (rev:target-revisions tg)))
      (ok (%st= (%fixture-state "c1000000") (rev:target-state tg))))
    (let ((r (rev:commit-projection s p :message "projected")))
      (ok (equal '("c3000000") (rev:revision-parent-ids r)))
      (ok (%st= (%fixture-state "c3000000") (rev:revision-state s r)))))
  (testing "default fidelity follows the target's list"
    (let* ((s (%fixture-store))
           (tg (%target-at s "c1000000" :fidelities '(:model :shadow))))
      (ok (eq :shadow (rev:projection-fidelity
                       (rev:project-plan tg (rev:plan-transition s tg "c3000000"))))))))

(defun %unsupported (fidelities requested restart)
  "Project c1 → c3 at REQUESTED on a target supporting FIDELITIES, invoking
RESTART. → (values projection condition offered-restarts target)."
  (let* ((s (%fixture-store))
         (tg (%target-at s "c1000000" :fidelities fidelities))
         (plan (rev:plan-transition s tg "c3000000"))
         (seen nil)
         (offered nil))
    (let ((p (handler-bind ((rev:projection-unsupported
                              (lambda (c)
                                (setf seen c
                                      offered (remove-if-not
                                               (lambda (r) (find-restart r c))
                                               '(rev:use-shadow
                                                 rev:accept-model-fidelity
                                                 rev:apply-for-real)))
                                (invoke-restart restart))))
               (rev:project-plan tg plan :fidelity requested))))
      (values p seen offered tg))))

(deftest project-plan-unsupported-fidelity
  (testing "use-shadow"
    (multiple-value-bind (p c offered tg) (%unsupported '(:shadow) :exact 'rev:use-shadow)
      (ok (typep c 'rev:revision-error))
      (ok (eq :exact (rev:projection-unsupported-fidelity c)))
      (ok (equal '(:shadow) (rev:projection-unsupported-supported c)))
      (ok (equal '(rev:use-shadow rev:apply-for-real) offered))
      (ok (eq :shadow (rev:projection-fidelity p)))
      (ok (%st= (%fixture-state "c3000000") (rev:projection-state p)))
      (ok (equal '("c1000000") (rev:target-revisions tg)))))
  (testing "accept-model-fidelity"
    (multiple-value-bind (p c offered) (%unsupported '(:model) :exact
                                                     'rev:accept-model-fidelity)
      (ok (typep c 'rev:projection-unsupported))
      (ok (equal '(rev:accept-model-fidelity rev:apply-for-real) offered))
      (ok (eq :model (rev:projection-fidelity p)))
      (ok (%st= (%fixture-state "c3000000") (rev:projection-state p)))))
  (testing "apply-for-real"
    (multiple-value-bind (p c offered tg) (%unsupported '(:model) :shadow
                                                        'rev:apply-for-real)
      (ok (typep c 'rev:projection-unsupported))
      (ok (member 'rev:apply-for-real offered))
      (ok (eq :exact (rev:projection-fidelity p)))
      (ok (equal '("c3000000") (rev:target-revisions tg)) "the plan really ran")
      (ok (%st= (%fixture-state "c3000000") (rev:target-state tg)))
      (ok (%st= (%fixture-state "c3000000") (rev:projection-view p)))))
  (testing "unhandled"
    (let* ((s (%fixture-store))
           (tg (%target-at s "c1000000" :fidelities '(:model))))
      (ok (signals (rev:project-plan tg (rev:plan-transition s tg "c3000000")
                                     :fidelity :exact)
                   'rev:projection-unsupported))
      (ok (equal '("c1000000") (rev:target-revisions tg))))))

(deftest project-plan-staleness
  (let* ((s (%fixture-store))
         (tg (%target-at s "c1000000"))
         (p (rev:project-plan tg (rev:plan-transition s tg "c3000000"))))
    (rev:target-stamp tg '("c0000000"))
    (let ((c (%capture (rev:projection-stale) (rev:projection-view p))))
      (ok (typep c 'rev:projection-stale))
      (ok (typep c 'rev:revision-error))
      (ok (eq p (rev:projection-stale-projection c))))
    (ok (signals (rev:commit-projection s p) 'rev:projection-stale)))
  (testing "a state change makes it stale too"
    (let* ((s (%fixture-store))
           (tg (%target-at s "c1000000"))
           (p (rev:project-plan tg (rev:plan-transition s tg "c3000000"))))
      (setf (rev:target-state tg) (list :changed t))
      (ok (signals (rev:projection-state p) 'rev:projection-stale)))))

(deftest project-merge-clean
  (let* ((s (%merge-fixture))
         (heads (length (rev:heads s))))
    (multiple-value-bind (p conflicts) (rev:project-merge s (rev:rev-ref "ours")
                                                          (rev:rev-ref "theirs"))
      (ok (rev:projection-p p))
      (ok (null conflicts))
      (ok (null (rev:projection-conflicts p)))
      (ok (eq :exact (rev:projection-fidelity p)))
      (ok (equal '("ours0000" "thrs0000") (rev:projection-parents p)))
      (ok (%st= '(:a 2 :b 2 :c 1 :d 4) (rev:projection-state p)))
      (ok (= heads (length (rev:heads s))) "nothing committed")
      (let ((r (rev:commit-projection s p :message "merge")))
        (ok (equal '("ours0000" "thrs0000") (rev:revision-parent-ids r)))
        (ok (%st= '(:a 2 :b 2 :c 1 :d 4) (rev:revision-state s r)))))))

(defun %conflicting-merge ()
  (let ((s (%merge-fixture)))
    (values s (rev:project-merge s "ours0000" "cnfl0000"))))

(defun %commit-resolving (restart)
  (multiple-value-bind (s p) (%conflicting-merge)
    (let ((seen nil) (offered nil))
      (values (handler-bind ((rev:merge-conflict
                               (lambda (c)
                                 (setf seen c
                                       ;; Restarts exist only while the call runs,
                                       ;; so record them here, not after return.
                                       offered (remove-if-not
                                                (lambda (r) (find-restart r c))
                                                '(rev:take-ours rev:take-theirs
                                                  rev:use-merged abort)))
                                 (invoke-restart restart))))
                (rev:commit-projection s p))
              s p seen offered))))

(deftest project-merge-conflicts
  (multiple-value-bind (s p) (%conflicting-merge)
    (let ((conflicts (rev:projection-conflicts p)))
      (ok (= 1 (length conflicts)))
      (ok (equal '("sexp:/a") (stack-diff:conflict-footprint (first conflicts)))))
    (ok (%st= '(:a 1 :b 1 :c 1) (rev:projection-state p)) "base value kept")
    (ok (signals (rev:commit-projection s p) 'rev:merge-conflict)))
  (testing "the condition"
    (multiple-value-bind (r s p c offered) (%commit-resolving 'abort)
      (declare (ignore s))
      (ok (null r) "abort commits nothing")
      (ok (typep c 'rev:revision-error))
      (ok (eq p (rev:merge-conflict-projection c)))
      (ok (= 1 (length (rev:merge-conflict-conflicts c))))
      (ok (equal '(rev:take-ours rev:take-theirs rev:use-merged abort) offered))))
  (testing "abort leaves the store alone"
    (multiple-value-bind (r s) (%commit-resolving 'abort)
      (declare (ignore r))
      (ok (= 3 (length (rev:heads s))))))
  (testing "take-ours"
    (multiple-value-bind (r s) (%commit-resolving 'rev:take-ours)
      (ok (equal '("ours0000" "cnfl0000") (rev:revision-parent-ids r)))
      (ok (%st= '(:a 2 :b 1 :c 1) (rev:revision-state s r)))))
  (testing "take-theirs"
    (multiple-value-bind (r s) (%commit-resolving 'rev:take-theirs)
      (ok (%st= '(:a 3 :b 1 :c 1) (rev:revision-state s r)))))
  (testing "use-merged"
    (multiple-value-bind (r s) (%commit-resolving 'rev:use-merged)
      (ok (%st= '(:a 1 :b 1 :c 1) (rev:revision-state s r))))))

(deftest discard-projection-releases
  (let* ((s (%fixture-store))
         (p (rev:project-state s "c1000000")))
    (ok (eq t (rev:discard-projection p)))
    (ok (rev:projection-discarded-p p))
    (ok (eq t (rev:discard-projection p)) "idempotent")
    (ok (signals (rev:projection-view p) 'rev:revision-error))
    (ok (signals (rev:projection-state p) 'rev:revision-error))
    (ok (signals (rev:commit-projection s p) 'rev:revision-error))))
