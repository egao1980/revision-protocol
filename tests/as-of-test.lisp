(in-package #:revision-protocol/tests)

;;; rev-as-of on the :recorded and :valid axes, and law 2 (monotonicity).

(defun %as-of-store ()
  "First-parent line on main: a ← b ← c ← m; side f (from b) merged at m.
         recorded  valid
    a      100      50
    b      200      30   (backdated: valid time is not monotonic)
    f      250      260  (side branch, second parent of m)
    c      300      —    (valid falls back to recorded = 300)
    m      400      350"
  (let ((s (%store)))
    (%commit s :id "aaaa0001" :ref "main" :at 100 :valid-at (%t 50))
    (%commit s :id "bbbb0002" :ref "main" :at 200 :valid-at (%t 30))
    (%commit s :id "ffff0003" :parents '("bbbb0002") :ref "side" :at 250
               :valid-at (%t 260))
    (%commit s :id "cccc0004" :ref "main" :at 300)
    (%commit s :id "eeee0005" :parents '("cccc0004" "ffff0003") :ref "main"
               :at 400 :valid-at (%t 350))
    s))

(defun %as-of (store seconds axis &optional (on "main"))
  (rev:resolve-revision store (rev:rev-as-of (%t seconds) :on on :axis axis)))

(deftest-parametrize as-of-recorded
    ((seconds expected)
     :ids ("at-first" "between-a-b" "at-b" "skips-side-branch" "at-c" "after-tip")
     (100 "aaaa0001")
     (150 "aaaa0001")
     (200 "bbbb0002")
     (260 "bbbb0002")
     (300 "cccc0004")
     (1000 "eeee0005"))
  (ok (equal expected (rev:revision-id (%as-of (%as-of-store) seconds :recorded)))))

(deftest-parametrize as-of-valid
    ((seconds expected)
     :ids ("backdated-b" "b-before-a-on-line" "c-falls-back-to-recorded" "at-m")
     (40 "bbbb0002")
     (60 "bbbb0002")
     (320 "cccc0004")
     (350 "eeee0005"))
  (ok (equal expected (rev:revision-id (%as-of (%as-of-store) seconds :valid)))))

(deftest as-of-default-axis-is-recorded
  (let ((s (%as-of-store)))
    (ok (equal "bbbb0002"
               (%id s (rev:rev-as-of (%t 260) :on "main"))))))

(deftest as-of-before-line-start-is-not-found
  (let ((s (%as-of-store)))
    (ok (signals (%as-of s 50 :recorded) 'rev:revision-not-found))
    (ok (signals (%as-of s 20 :valid) 'rev:revision-not-found))))

(deftest as-of-on-other-designators
  (let ((s (%as-of-store)))
    (rev:tag-revision s "release" "cccc0004")
    (ok (equal "bbbb0002" (rev:revision-id (%as-of s 299 :recorded (rev:rev-tag "release")))))
    (ok (equal "ffff0003" (rev:revision-id (%as-of s 300 :recorded "side"))))
    (ok (equal "aaaa0001"
               (rev:revision-id
                (%as-of s 199 :recorded (rev:resolve-revision s "cccc0004")))))))

(deftest as-of-missing-ref
  (let ((s (%as-of-store)))
    (ok (signals (%as-of s 300 :recorded "nope") 'rev:revision-not-found))))

;;; Law 2: as-of is monotonic along its line. For T1 <= T2 on the same axis
;;; and line, as-of(T1) is as-of(T2) or a first-parent ancestor of it.

(deftest-parametrize law-2-as-of-monotonic
    ((axis t1 t2)
     (:recorded 100 150)
     (:recorded 100 260)
     (:recorded 150 399)
     (:recorded 200 300)
     (:recorded 260 1000)
     (:valid 30 60)
     (:valid 40 320)
     (:valid 300 349)
     (:valid 60 1000))
  (let* ((s (%as-of-store))
         (r1 (%as-of s t1 axis))
         (r2 (%as-of s t2 axis))
         (line (%ids (rev:walk-revisions s r2 :order :first-parent))))
    (ok (rev:ancestor-p s r1 r2))
    (ok (member (rev:revision-id r1) line :test #'equal)
        "on the first-parent line of the later answer")))
