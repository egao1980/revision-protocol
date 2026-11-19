(in-package #:revision-protocol/tests)

;;; R0b content layer: primary form, revision-state / revision-delta,
;;; payloads, delta-inverse / delta-footprint, state-diff, :footprint walks.

(deftest content-primary-and-engine
  (ok (eq :state (rev:content-primary (rev:make-in-memory-store))))
  (ok (eq :delta (rev:content-primary (rev:make-in-memory-store :primary :delta))))
  (ok (typep (rev:content-engine (rev:make-in-memory-store))
             'stack-diff:sexp-diff-engine))
  (let ((e (make-instance 'stack-diff:sexp-diff-engine)))
    (ok (eq e (rev:content-engine (rev:make-in-memory-store :engine e)))))
  (ok (signals (rev:make-in-memory-store :primary :both) 'type-error)))

(deftest-parametrize revision-state-of-fixture
    ((primary) (:state) (:delta))
  (let ((s (%fixture-store primary)))
    (dolist (id (%fixture-ids))
      (ok (%st= (%fixture-state id) (rev:revision-state s id))
          (format nil "state of ~a" id)))
    (testing "designators other than ids"
      (ok (%st= (%fixture-state "c3000000") (rev:revision-state s (rev:rev-ref "main"))))
      (ok (%st= (%fixture-state "c2000000")
                (rev:revision-state s (rev:rev-parent (rev:rev-ref "main")))))
      (ok (%st= (%fixture-state "b2000000")
                (rev:revision-state s (rev:rev-parent (rev:rev-ref "merged") 2)))))
    (testing "revision-content is still the content as given"
      (ok (equal (%fixture-state "c1000000")
                 (rev:revision-content (rev:resolve-revision s "c1000000")))))
    (ok (signals (rev:revision-state s "ffff9999") 'rev:revision-not-found))))

(deftest-parametrize revision-delta-of-fixture
    ((primary) (:state) (:delta))
  (let ((s (%fixture-store primary)))
    (testing "a root's delta is against the empty state"
      (let ((d (rev:revision-delta s "c0000000")))
        (ok (typep d 'stack-diff:diff))
        (ok (%st= (%fixture-state "c0000000") (stack-diff:apply-diff nil d)))))
    (testing "a child's delta is against its first parent"
      (let ((d (rev:revision-delta s "c2000000")))
        (ok (%st= (%fixture-state "c2000000")
                  (stack-diff:apply-diff (%fixture-state "c1000000") d)))))
    (testing "a merge's delta is against its first parent"
      (let ((d (rev:revision-delta s "m0000000")))
        (ok (%st= (%fixture-state "m0000000")
                  (stack-diff:apply-diff (%fixture-state "c3000000") d)))
        (ok (equal '("sexp:/branch") (rev:delta-footprint d)))))
    (ok (signals (rev:revision-delta s "ffff9999") 'rev:revision-not-found))))

(deftest-parametrize commit-diff-as-content
    ((primary) (:state) (:delta))
  (let* ((s (rev:make-in-memory-store :primary primary))
         (d0 (stack-diff:compute-diff *sexp* nil '(:a 1)))
         (r0 (%commit s :id "r0000000" :ref "main" :content d0))
         (d1 (stack-diff:compute-diff *sexp* '(:a 1) '(:a 2 :b 3)))
         (r1 (%commit s :id "r1000000" :ref "main" :content d1 :at 2000)))
    (ok (eq d0 (rev:revision-content r0)) "revision-content as given")
    (ok (eq d1 (rev:revision-content r1)))
    (ok (%st= '(:a 1) (rev:revision-state s "r0000000")))
    (ok (%st= '(:a 2 :b 3) (rev:revision-state s "r1000000")))
    (ok (%st= '(:a 2 :b 3)
              (stack-diff:apply-diff '(:a 1) (rev:revision-delta s "r1000000"))))
    (when (eq primary :delta)
      (ok (eq d1 (rev:revision-delta s "r1000000")) "a stored delta is returned as stored"))))

(deftest-parametrize payload-content
    ((primary) (:state) (:delta))
  (let* ((s (%opaque-fixture primary))
         (delta (rev:revision-delta s "o1000000")))
    (testing "a payload state wins over its opaque delta"
      (ok (%st= '(:t 1 :col "x") (rev:revision-state s "o1000000"))))
    (testing "the payload delta is returned as given"
      (ok (rev:revision-payload-p (rev:revision-content
                                   (rev:resolve-revision s "o1000000"))))
      (ok (eq (rev:payload-delta (rev:revision-content
                                  (rev:resolve-revision s "o1000000")))
              delta))
      (ok (equal '("table:t") (rev:delta-footprint delta)))
      (ok (null (rev:delta-inverse delta)) "an opaque delta has no inverse"))
    (testing "a payload with only a delta is applied to the parent"
      (%commit s :id "o2000000" :ref "main" :at 300
                 :content (rev:make-payload
                           :delta (stack-diff:compute-diff
                                   *sexp* '(:t 1 :col "x") '(:t 2 :col "x"))))
      (ok (%st= '(:t 2 :col "x") (rev:revision-state s "o2000000"))))
    (testing "a payload with a NIL state is the empty state"
      (%commit s :id "o3000000" :ref "main" :at 400
                 :content (rev:make-payload :state nil))
      (ok (null (rev:revision-state s "o3000000"))))
    (ok (signals (rev:make-payload) 'rev:revision-error))))

(deftest delta-inverse-and-footprint
  (let* ((a (%fixture-state "c0000000"))
         (b (%fixture-state "c3000000"))
         (d (stack-diff:compute-diff *sexp* a b))
         (inv (rev:delta-inverse d)))
    (ok (typep inv 'stack-diff:diff))
    (ok (%st= a (stack-diff:apply-diff b inv)) "the inverse round-trips")
    (ok (equal (stack-diff:diff-footprint d) (rev:delta-footprint d)))
    (ok (equal '("sexp:/cfg/x" "sexp:/cfg/y" "sexp:/extra" "sexp:/name" "sexp:/tags/1")
               (rev:delta-footprint d))))
  (testing "no inverse → NIL, never IRREVERSIBLE-CHANGE"
    (ok (null (handler-case (rev:delta-inverse (%opaque-diff))
                (stack-diff:irreversible-change () :signalled)))))
  (testing "empty deltas"
    (ok (null (rev:delta-footprint (stack-diff:make-diff nil))))
    (ok (null (rev:delta-footprint nil)))
    (ok (stack-diff:diff-empty-p (rev:delta-inverse (stack-diff:make-diff nil))))))

(deftest-parametrize state-diff-between-revisions
    ((primary) (:state) (:delta))
  (let ((s (%fixture-store primary)))
    (let ((d (rev:state-diff s "c0000000" (rev:rev-ref "main"))))
      (ok (typep d 'stack-diff:diff))
      (ok (eq :changes (stack-diff:diff-level d)) "default level :changes")
      (ok (%st= (%fixture-state "c3000000")
                (stack-diff:apply-diff (%fixture-state "c0000000") d))))
    (ok (eq :content (stack-diff:diff-level
                      (rev:state-diff s "c0000000" "c3000000" :level :content))))
    (ok (stack-diff:diff-empty-p (rev:state-diff s "b1000000" "b1000000")))
    (testing ":footprint keeps overlapping changes only"
      (let ((d (rev:state-diff s "c0000000" "c3000000" :footprint '("sexp:/name"))))
        (ok (equal '("sexp:/name") (stack-diff:diff-footprint d))))
      (let ((d (rev:state-diff s "c0000000" "c3000000" :footprint '("sexp:/cfg"))))
        (ok (equal '("sexp:/cfg/x" "sexp:/cfg/y") (stack-diff:diff-footprint d))))
      (ok (stack-diff:diff-empty-p
           (rev:state-diff s "c0000000" "c3000000" :footprint '("table:none")))))
    (ok (signals (rev:state-diff s "c0000000" "ffff9999") 'rev:revision-not-found))))

(deftest state-diff-over-targets-plans-projections
  (let* ((s (%fixture-store))
         (tg (%target-at s "c1000000")))
    (testing "a target designates its current state"
      (ok (stack-diff:diff-empty-p (rev:state-diff s "c1000000" tg)))
      (setf (rev:target-state tg) (list :drifted t))
      (let ((d (rev:state-diff s "c1000000" tg)))
        (ok (%st= '(:drifted t) (stack-diff:apply-diff (%fixture-state "c1000000") d))))
      (setf (rev:target-state tg) (rev:revision-state s "c1000000")))
    (testing "a plan designates the state of its goal"
      (let ((p (rev:plan-transition s tg "c3000000")))
        (ok (%st= (%fixture-state "c3000000")
                  (stack-diff:apply-diff (%fixture-state "c1000000")
                                         (rev:state-diff s tg p))))
        (ok (stack-diff:diff-empty-p (rev:state-diff s p "c3000000")))))
    (testing "a projection designates its projected state"
      (let ((p (rev:project-state s "c2000000")))
        (ok (stack-diff:diff-empty-p (rev:state-diff s p "c2000000")))
        (ok (stack-diff:diff-empty-p (rev:state-diff s "c2000000" p)))))))

(deftest-parametrize walk-revisions-footprint
    ((primary) (:state) (:delta))
  (let ((s (%fixture-store primary)))
    (ok (equal '("c3000000")
               (%ids (rev:walk-revisions s (rev:rev-ref "main")
                                         :footprint '("sexp:/extra")))))
    (ok (equal '("c0000000" "c1000000" "c2000000")
               (%ids (rev:walk-revisions s (rev:rev-ref "main")
                                         :footprint '("sexp:/cfg")))))
    (ok (equal '("c3000000" "c0000000")
               (%ids (rev:walk-revisions s (rev:rev-ref "main") :reverse t
                                         :footprint '("sexp:/name")))))
    (ok (equal '("b1000000" "m0000000")
               (%ids (rev:walk-revisions s (rev:rev-ref "merged")
                                         :until "c3000000"
                                         :footprint '("sexp:/branch")))))
    (testing "no footprint keeps the R0a walk"
      (ok (equal (%ids (rev:walk-revisions s (rev:rev-ref "main")))
                 (%ids (rev:walk-revisions s (rev:rev-ref "main") :footprint nil)))))))
