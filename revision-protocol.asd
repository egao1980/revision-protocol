(defsystem "revision-protocol"
  :version "0.3.0"
  :description "CLOS revision protocol for cl-stack: immutable revision DAG, movable refs and immutable tags with CAS updates, revision designators (as-of on recorded/valid time), graph walks; content (states and deltas on diff-protocol), targets and transition plans, projections, composite revisions"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "datetime-protocol" "0.1.3")
               (:version "bordeaux-threads" "0.9.4")
               (:version "diff-protocol" "0.1.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "content")
               (:file "target")
               (:file "projection")
               (:file "composite")
               (:file "memory")
               (:file "memory-content")
               (:file "memory-target"))
  :in-order-to ((test-op (test-op "revision-protocol/tests"))))

(defsystem "revision-protocol/tests"
  :depends-on ("revision-protocol" "diff-protocol" "bordeaux-threads" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "graph-test")
               (:file "walk-test")
               (:file "as-of-test")
               (:file "designator-test")
               (:file "ref-test")
               (:file "law-test")
               ;; R0b
               (:file "r0b-fixtures")
               (:file "content-test")
               (:file "target-test")
               (:file "plan-test")
               (:file "projection-test")
               (:file "composite-test")
               (:file "force-test")
               (:file "law-r0b-test")
               ;; 0.3.0
               (:file "v03-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
