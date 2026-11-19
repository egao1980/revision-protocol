(defsystem "revision-protocol"
  :version "0.0.1"
  :description "CLOS revision protocol for cl-stack: immutable revision DAG, movable refs and immutable tags with CAS updates, revision designators (as-of on recorded/valid time), graph walks"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "datetime-protocol" "0.1.3")
               (:version "bordeaux-threads" "0.9.4"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "memory"))
  :in-order-to ((test-op (test-op "revision-protocol/tests"))))

(defsystem "revision-protocol/tests"
  :depends-on ("revision-protocol" "bordeaux-threads" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "graph-test")
               (:file "walk-test")
               (:file "as-of-test")
               (:file "designator-test")
               (:file "ref-test")
               (:file "law-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
