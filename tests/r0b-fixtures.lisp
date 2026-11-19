(in-package #:revision-protocol/tests)

;;; R0b fixtures: a sexp corpus committed into an in-memory store, for the
;;; content, target, projection and composite suites and laws 3, 5, 6.
;;; DEFTEST-PARAMETRIZE splices each row value into a LET as a form, so rows
;;; carry quoted states (%Q), keywords, strings and NIL only.
;;;
;;; Graph (ids caller-chosen; recorded at the time shown):
;;;
;;;   main:    c0 (100) ← c1 (200) ← c2 (300) ← c3 (400)
;;;   feature:             c1 ← b1 (250) ← b2 (350)
;;;   merged:  m (500) = merge(c3, b2), explicit state
;;;   other:   l0 (110) ← l1 (210) ← l2 (310) ← l3 (410)   (unrelated line:
;;;            lists, atoms, strings)

(defvar *sexp* (make-instance 'stack-diff:sexp-diff-engine))

(defun %q (x) (list 'quote x))

(defun %st= (a b)
  "State equality every R0b assertion uses (sexp engine)."
  (stack-diff:state-equal *sexp* a b))

(defparameter *fixture*
  ;; (id parents ref at state)
  '(("c0000000" () "main" 100
     (:name "w" :cfg (:x 1 :y (:p 1 :q 2)) :tags ("a" "b")))
    ("l0000000" () "other" 110 (1 2 3 4))
    ("c1000000" ("c0000000") "main" 200
     (:name "w" :cfg (:x 1 :y (:p 1 :q 3)) :tags ("a" "b" "c")))
    ("l1000000" ("l0000000") "other" 210 (1 (:k 1) "s" (2 3)))
    ("b1000000" ("c1000000") "feature" 250
     (:name "w" :cfg (:x 1 :y (:p 1 :q 3)) :tags ("a" "b" "c") :branch "on"))
    ("c2000000" ("c1000000") "main" 300
     (:name "w" :cfg (:x 2) :tags ("a")))
    ("l2000000" ("l1000000") "other" 310 42)
    ("b2000000" ("b1000000") "feature" 350
     (:name "w" :cfg (:x 1 :y (:p 9 :q 3)) :tags ("a" "b" "c") :branch "on"))
    ("c3000000" ("c2000000") "main" 400
     (:name "v" :cfg (:x 2) :tags ("a") :extra 42))
    ("l3000000" ("l2000000") "other" 410 "hello")
    ("m0000000" ("c3000000" "b2000000") "merged" 500
     (:name "v" :cfg (:x 2) :tags ("a") :extra 42 :branch "on"))))

(defparameter *primaries* '(:state :delta))

(defun %fixture-ids ()
  (mapcar #'first *fixture*))

(defun %fixture-state (id)
  "Expected state of fixture revision ID; NIL for NIL (the empty state)."
  (and id
       (let ((row (assoc id *fixture* :test #'equal)))
         (assert row () "No fixture revision ~S" id)
         (fifth row))))

(defun %fixture-parent (id)
  (first (second (assoc id *fixture* :test #'equal))))

(defun %fixture-store (&optional (primary :state))
  "Fresh in-memory store of PRIMARY holding *FIXTURE* (states as content)."
  (let ((s (rev:make-in-memory-store :primary primary)))
    (loop for (id parents ref at state) in *fixture*
          do (%commit s :id id :parents parents :ref ref :at at
                        :content (copy-tree state)))
    s))

(defun %target-at (store id &rest keys)
  "In-memory target realizing revision ID of STORE (fresh target for NIL).
KEYS pass to MAKE-IN-MEMORY-TARGET."
  (apply #'rev:make-in-memory-target
         :state (and id (rev:revision-state store id))
         :revisions (and id (list id))
         keys))

(defun %shape (plan)
  "Steps of PLAN as (direction from to) lists."
  (mapcar (lambda (s) (list (rev:step-direction s) (rev:step-from s) (rev:step-to s)))
          (rev:plan-steps plan)))

(defun %recording-hook (log-cell)
  "STAMP-HOOK pushing (ids . state) onto (CAR LOG-CELL)."
  (lambda (target ids)
    (push (cons (copy-list ids) (rev:target-state target)) (car log-cell))))

;;; Law 3 rows: (primary a b level).

(defparameter *state-pairs*
  '(("c0000000" "c3000000") ("c3000000" "c0000000") ("c1000000" "b2000000")
    ("b2000000" "c2000000") ("m0000000" "c0000000") ("c0000000" "m0000000")
    ("l0000000" "l1000000") ("l1000000" "l3000000") ("l3000000" "c0000000")
    ("c3000000" "l2000000")))

(defparameter *state-triples*
  '(("c0000000" "c1000000" "c2000000") ("c3000000" "b2000000" "m0000000")
    ("l0000000" "l3000000" "c1000000") ("b1000000" "c3000000" "l2000000")))

(defparameter *levels* '(:summary :changes :content))

(defun law3-pair-rows ()
  (loop for primary in *primaries*
        nconc (loop for (a b) in *state-pairs*
                    nconc (loop for level in *levels*
                                collect (list primary a b level)))))

(defun law3-pair-ids ()
  (loop for primary in *primaries*
        nconc (loop for (a b) in *state-pairs*
                    nconc (loop for level in *levels*
                                collect (format nil "~(~a~):~a>~a@~(~a~)"
                                                primary (subseq a 0 2)
                                                (subseq b 0 2) level)))))

(defun law3-triple-rows ()
  (loop for primary in *primaries*
        nconc (loop for (a b c) in *state-triples*
                    collect (list primary a b c))))

(defun law3-triple-ids ()
  (loop for primary in *primaries*
        nconc (loop for (a b c) in *state-triples*
                    collect (format nil "~(~a~):~a>~a>~a" primary (subseq a 0 2)
                                    (subseq b 0 2) (subseq c 0 2)))))

(defun revision-rows ()
  (loop for primary in *primaries*
        nconc (loop for id in (%fixture-ids) collect (list primary id))))

(defun revision-ids ()
  (loop for primary in *primaries*
        nconc (loop for id in (%fixture-ids)
                    collect (format nil "~(~a~):~a" primary (subseq id 0 2)))))

;;; Transitions for plans and laws 5, 6: (from to expected-shape expected-base).
;;; FROM NIL = a fresh target.

(defparameter *transitions*
  '((nil "c3000000"
     ((:up nil "c0000000") (:up "c0000000" "c1000000")
      (:up "c1000000" "c2000000") (:up "c2000000" "c3000000"))
     nil)
    ("c3000000" "c1000000"
     ((:down "c3000000" "c2000000") (:down "c2000000" "c1000000"))
     "c1000000")
    ("c3000000" "b2000000"
     ((:down "c3000000" "c2000000") (:down "c2000000" "c1000000")
      (:up "c1000000" "b1000000") (:up "b1000000" "b2000000"))
     "c1000000")
    ("b2000000" "c2000000"
     ((:down "b2000000" "b1000000") (:down "b1000000" "c1000000")
      (:up "c1000000" "c2000000"))
     "c1000000")
    ("c0000000" "m0000000"
     ((:up "c0000000" "c1000000") (:up "c1000000" "c2000000")
      (:up "c2000000" "c3000000") (:up "c3000000" "m0000000"))
     "c0000000")
    ("m0000000" "b1000000"
     ((:down "m0000000" "b2000000") (:down "b2000000" "b1000000"))
     "b1000000")
    ("l2000000" "c1000000"
     ((:down "l2000000" "l1000000") (:down "l1000000" "l0000000")
      (:down "l0000000" nil)
      (:up nil "c0000000") (:up "c0000000" "c1000000"))
     nil)
    ("c2000000" "c2000000" () "c2000000")))

(defun %short (id) (if id (subseq id 0 2) "-"))

(defun plan-shape-rows ()
  (loop for primary in *primaries*
        nconc (loop for (from to shape base) in *transitions*
                    collect (list primary from to (%q shape) base))))

(defun plan-shape-ids ()
  (loop for primary in *primaries*
        nconc (loop for (from to) in *transitions*
                    collect (format nil "~(~a~):~a>~a" primary (%short from)
                                    (%short to)))))

(defun transition-rows ()
  (loop for primary in *primaries*
        nconc (loop for (from to) in *transitions*
                    collect (list primary from to))))

(defun transition-ids () (plan-shape-ids))

(defparameter *fidelities* '(:exact :shadow :model))

(defparameter *model-footprint* '("sexp:/cfg"))

(defun law6-rows ()
  (loop for fidelity in *fidelities*
        nconc (loop for (primary from to) in (transition-rows)
                    collect (list primary from to fidelity))))

(defun law6-ids ()
  (loop for fidelity in *fidelities*
        nconc (loop for id in (transition-ids)
                    collect (format nil "~a@~(~a~)" id fidelity))))

;;; Small fixtures

(defun %merge-fixture ()
  "base ← ours (:a 2); base ← thrs (:b 2 :d 4); base ← cnfl (:a 3)."
  (let ((s (rev:make-in-memory-store)))
    (%commit s :id "base0000" :ref "main" :at 100 :content (list :a 1 :b 1 :c 1))
    (%commit s :id "ours0000" :parents '("base0000") :ref "ours" :at 200
               :content (list :a 2 :b 1 :c 1))
    (%commit s :id "thrs0000" :parents '("base0000") :ref "theirs" :at 210
               :content (list :a 1 :b 2 :c 1 :d 4))
    (%commit s :id "cnfl0000" :parents '("base0000") :ref "conflict" :at 220
               :content (list :a 3 :b 1 :c 1))
    s))

(defun %opaque-diff (&optional (footprint "table:t"))
  (stack-diff:make-diff
   (list (stack-diff:make-opaque-change :modify :footprint (list footprint)
                                        :description "backfill"))))

(defun %opaque-fixture (&optional (primary :state))
  "o0 (:t 1) ← o1: payload with state (:t 1 :col \"x\") and an opaque delta."
  (let ((s (rev:make-in-memory-store :primary primary)))
    (%commit s :id "o0000000" :ref "main" :at 100 :content (list :t 1))
    (%commit s :id "o1000000" :ref "main" :at 200
               :content (rev:make-payload :state (list :t 1 :col "x")
                                          :delta (%opaque-diff)))
    s))

(defun %fail-on (to &key (attempts '(1)) (flag (list t)))
  "FAIL-STEP hook: signal a SIMPLE-ERROR when the step's TO is TO, the attempt
is in ATTEMPTS (T = every attempt), and (CAR FLAG) is true."
  (lambda (step attempt)
    (when (and (car flag)
               (equal to (rev:step-to step))
               (or (eq attempts t) (member attempt attempts)))
      (error "injected failure at ~a (attempt ~d)" to attempt))))
