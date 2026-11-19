(in-package #:revision-protocol/tests)

;;; Laws 3, 5, 6 (brief "Laws") over the R0b fixture corpus, for both content
;;; primaries. Law 3 is diff-protocol's law suite lifted to revisions.

;;; Law 3: apply-diff(state(a), diff(a, b)) = state(b); inverses round-trip;
;;; diff(a, a) is empty; compose-diffs is associative.

(deftest-parametrize law-3-apply-state-diff
    ((primary a b level) :ids (law3-pair-ids) :rows (law3-pair-rows))
  (let* ((s (%fixture-store primary))
         (d (rev:state-diff s a b :level level)))
    (ok (%st= (rev:revision-state s b)
              (stack-diff:apply-diff (rev:revision-state s a) d))
        "apply-diff(state(a), state-diff(a, b)) = state(b)")
    (let ((inv (rev:delta-inverse d)))
      (ok inv "sexp diffs between revisions are invertible")
      (ok (%st= (rev:revision-state s a)
                (stack-diff:apply-diff (rev:revision-state s b) inv))
          "the inverse round-trips"))))

(deftest-parametrize law-3-self-diff-empty
    ((primary id) :ids (revision-ids) :rows (revision-rows))
  (let ((s (%fixture-store primary)))
    (ok (stack-diff:diff-empty-p (rev:state-diff s id id)))
    (ok (null (rev:delta-footprint (rev:state-diff s id id))))))

(deftest-parametrize law-3-revision-delta
    ((primary id) :ids (revision-ids) :rows (revision-rows))
  (let* ((s (%fixture-store primary))
         (parent (%fixture-parent id))
         (delta (rev:revision-delta s id)))
    (ok (%st= (rev:revision-state s id)
              (stack-diff:apply-diff (and parent (rev:revision-state s parent)) delta))
        "state(r) = apply-diff(state(first parent), delta(r))")
    (ok (%st= (and parent (rev:revision-state s parent))
              (stack-diff:apply-diff (rev:revision-state s id) (rev:delta-inverse delta)))
        "the delta's inverse goes back to the first parent")
    (ok (equal (stack-diff:diff-footprint delta) (rev:delta-footprint delta)))))

(deftest-parametrize law-3-compose
    ((primary a b c) :ids (law3-triple-ids) :rows (law3-triple-rows))
  (let* ((s (%fixture-store primary))
         (d1 (rev:state-diff s a b))
         (d2 (rev:state-diff s b c))
         (d3 (rev:state-diff s c a))
         (sa (rev:revision-state s a)))
    (ok (%st= (rev:revision-state s c)
              (stack-diff:apply-diff sa (stack-diff:compose-diffs d1 d2)))
        "compose(diff(a, b), diff(b, c)) applied to state(a) = state(c)")
    (ok (%st= (stack-diff:apply-diff sa (stack-diff:compose-diffs
                                         (stack-diff:compose-diffs d1 d2) d3))
              (stack-diff:apply-diff sa (stack-diff:compose-diffs
                                         d1 (stack-diff:compose-diffs d2 d3))))
        "(d1 . d2) . d3 = d1 . (d2 . d3) on state(a)")))

;;; Law 5: after execute-plan without error, target-revisions = plan goal; the
;;; stamp after each step names the revision reached.

(deftest-parametrize law-5-execute-plan-reaches-goal
    ((primary from to) :ids (transition-ids) :rows (transition-rows))
  (let* ((s (%fixture-store primary))
         (log (list nil))
         (tg (%target-at s from :stamp-hook (%recording-hook log)))
         (p (rev:plan-transition s tg to)))
    (ok (equal (rev:plan-goal p) (rev:execute-plan tg p)))
    (ok (equal (rev:plan-goal p) (rev:target-revisions tg)) "target-revisions = goal")
    (ok (%st= (rev:revision-state s to) (rev:target-state tg)))
    (let ((stamps (reverse (car log))))
      (ok (equal (mapcar (lambda (step) (let ((to (rev:step-to step)))
                                          (and to (list to))))
                         (rev:plan-steps p))
                 (mapcar #'car stamps))
          "one stamp per step, naming the step's TO")
      (ok (every (lambda (entry)
                   (destructuring-bind (ids . state) entry
                     (%st= (and ids (rev:revision-state s (first ids))) state)))
                 stamps)
          "at each stamp the state is the stamped revision's"))))

;;; Law 6: an :exact (and in-memory :shadow) projection equals execution;
;;; :model equals it on the modelled part.

(deftest-parametrize law-6-projection-equals-execution
    ((primary from to fidelity) :ids (law6-ids) :rows (law6-rows))
  (let* ((s (%fixture-store primary))
         (projected (%target-at s from :fidelities (list fidelity)))
         (executed (%target-at s from))
         (p (rev:plan-transition s projected to))
         (proj (rev:project-plan projected p :fidelity fidelity)))
    (ok (eq fidelity (rev:projection-fidelity proj)))
    (rev:execute-plan executed p)
    (ok (stack-diff:diff-empty-p
         (rev:state-diff s proj executed
                         :footprint (and (eq fidelity :model) *model-footprint*)))
        "state-diff(project-plan(t, p), t after execute-plan) is empty")
    (ok (equal (and from (list from)) (rev:target-revisions projected))
        "projecting left the target alone")))
