(in-package #:revision-protocol/tests)

;;; R0b composite revisions: manifests of revisions in other stores, restore
;;; in dependency order, skip-part / rebuild-from-recipe.

(defun %part-stores ()
  "→ (values files-store db-store composite-store)."
  (let ((files (%fixture-store))
        (db (rev:make-in-memory-store)))
    (%commit db :id "d0000000" :ref "main" :at 100 :content (list :tables (list "t")))
    (%commit db :id "d1000000" :ref "main" :at 200
                :content (list :tables (list "t" "u")))
    (values files db (rev:make-in-memory-store))))

(deftest manifest-parts
  (multiple-value-bind (files db) (%part-stores)
    (let ((p (rev:make-manifest-part :files files (rev:rev-ref "main")
                                     :depends-on '(:db))))
      (ok (eq :files (rev:part-role p)))
      (ok (eq files (rev:part-store p)))
      (ok (equal "c3000000" (rev:part-revision-id p)) "resolved when made")
      (ok (equal '(:db) (rev:part-depends-on p)))
      (ok (null (rev:part-recipe p)))
      (rev:move-ref files "main" "c0000000")
      (ok (equal "c3000000" (rev:part-revision-id p)) "and kept"))
    (testing "a part without a store keeps the id string"
      (let ((p (rev:make-manifest-part :image nil "img-0001")))
        (ok (null (rev:part-store p)))
        (ok (equal "img-0001" (rev:part-revision-id p)))))
    (testing "argument checks"
      (ok (signals (rev:make-manifest-part :db db "ffff9999") 'rev:revision-not-found))
      (ok (signals (rev:make-manifest-part "db" db "d0000000") 'type-error))
      (ok (signals (rev:make-manifest-part :image nil (rev:rev-ref "main")) 'type-error))
      (ok (signals (rev:make-manifest-part :db db "d0000000" :recipe 42) 'type-error)))))

(deftest manifest-validation
  (multiple-value-bind (files db) (%part-stores)
    (let ((f (rev:make-manifest-part :files files "c3000000" :depends-on '(:db)))
          (d (rev:make-manifest-part :db db "d1000000")))
      (let ((m (rev:make-manifest (list f d))))
        (ok (equal (list f d) (rev:manifest-parts m)) "manifest order")
        (setf (first (rev:manifest-parts m)) nil)
        (ok (eq f (first (rev:manifest-parts m))) "fresh list"))
      (ok (signals (rev:make-manifest (list d (rev:make-manifest-part :db db "d0000000")))
                   'rev:revision-error)
          "duplicate role")
      (ok (signals (rev:make-manifest (list f)) 'rev:revision-error) "unknown dependency")
      (ok (signals (rev:make-manifest
                    (list (rev:make-manifest-part :files files "c3000000" :depends-on '(:db))
                          (rev:make-manifest-part :db db "d1000000" :depends-on '(:files))))
                   'rev:revision-error)
          "dependency cycle"))))

(defun %composite ()
  "→ (values composite-store revision files db); files depends on db."
  (multiple-value-bind (files db cs) (%part-stores)
    (let ((r (rev:commit-composite
              cs (list (rev:make-manifest-part :files files "c3000000" :depends-on '(:db))
                       (rev:make-manifest-part :db db "d1000000"))
              :ref "main" :message "checkpoint")))
      (values cs r files db))))

(deftest commit-composite-and-manifest
  (multiple-value-bind (cs r files) (%composite)
    (ok (rev:revision-p r))
    (ok (equal "checkpoint" (rev:revision-message r)))
    (ok (equal (rev:revision-id r) (rev:ref-target (rev:find-ref cs "main"))))
    (let ((m (rev:composite-manifest cs (rev:rev-ref "main"))))
      (ok (typep m 'rev:revision-manifest))
      (ok (equal '(:files :db) (mapcar #'rev:part-role (rev:manifest-parts m))))
      (ok (equal '("c3000000" "d1000000")
                 (mapcar #'rev:part-revision-id (rev:manifest-parts m))))
      (ok (eq m (rev:revision-content r)))
      (ok (eq m (rev:revision-state cs r))))
    (ok (null (rev:composite-manifest files "c3000000")) "not a composite")
    (ok (signals (rev:commit-composite cs (list (rev:make-manifest-part
                                                 :files files "c3000000"
                                                 :depends-on '(:db))))
                 'rev:revision-error))))

(deftest plan-composite-order
  (multiple-value-bind (cs r files db) (%composite)
    (let* ((tf (rev:make-in-memory-target))
           (td (rev:make-in-memory-target))
           (plans (rev:plan-composite cs r (list :files tf :db td))))
      (ok (equal '(:db :files) (mapcar #'car plans)) "dependencies first")
      (ok (equal '("d1000000") (rev:plan-goal (cdr (assoc :db plans)))))
      (ok (eq db (rev:plan-store (cdr (assoc :db plans)))))
      (ok (eq files (rev:plan-store (cdr (assoc :files plans)))))
      (ok (null (rev:target-revisions tf)) "planning is pure"))
    (testing "a part without a target has no plan"
      (let ((plans (rev:plan-composite cs r (list :files (rev:make-in-memory-target)))))
        (ok (equal '(:db :files) (mapcar #'car plans)))
        (ok (null (cdr (assoc :db plans))))))
    (ok (signals (rev:plan-composite files "c3000000" nil) 'rev:revision-error)
        "not a composite")))

(deftest restore-composite-parts
  (multiple-value-bind (cs r) (%composite)
    (let ((tf (rev:make-in-memory-target))
          (td (rev:make-in-memory-target)))
      (ok (equal '(:db ("d1000000") :files ("c3000000"))
                 (rev:restore-composite cs r (list :files tf :db td))))
      (ok (%st= (%fixture-state "c3000000") (rev:target-state tf)))
      (ok (%st= '(:tables ("t" "u")) (rev:target-state td))))))

(deftest restore-composite-unavailable-parts
  (testing "no target: skip-part, no recipe restart"
    (multiple-value-bind (cs r) (%composite)
      (let* ((tf (rev:make-in-memory-target))
             (seen nil)
             (result (handler-bind ((rev:composite-part-unavailable
                                      (lambda (c)
                                        (setf seen (list (rev:part-role
                                                          (rev:composite-part-unavailable-part c))
                                                         (rev:composite-part-unavailable-reason c)
                                                         (and (find-restart 'rev:skip-part c) t)
                                                         (and (find-restart 'rev:rebuild-from-recipe c) t)))
                                        (invoke-restart 'rev:skip-part))))
                       (rev:restore-composite cs r (list :files tf)))))
        (ok (equal '(:db :no-target t nil) seen))
        (ok (equal '(:db :skipped :files ("c3000000")) result))
        (ok (equal '("c3000000") (rev:target-revisions tf))))))
  (testing "no store: rebuild-from-recipe"
    (multiple-value-bind (files db cs) (%part-stores)
      (declare (ignore db))
      (let* ((calls nil)
             (recipe (lambda (part target)
                       (push (rev:part-role part) calls)
                       (setf (rev:target-state target) (list :rebuilt t))))
             (r (rev:commit-composite
                 cs (list (rev:make-manifest-part :files files "c1000000")
                          (rev:make-manifest-part :image nil "img-0001" :recipe recipe))))
             (ti (rev:make-in-memory-target))
             (reason nil)
             (result (handler-bind ((rev:composite-part-unavailable
                                      (lambda (c)
                                        (setf reason (rev:composite-part-unavailable-reason c))
                                        (invoke-restart 'rev:rebuild-from-recipe))))
                       (rev:restore-composite cs r (list :files (rev:make-in-memory-target)
                                                         :image ti)))))
        (ok (eq :no-store reason))
        (ok (equal '(:image) calls))
        (ok (equal '(:files ("c1000000") :image ("img-0001")) result))
        (ok (equal '("img-0001") (rev:target-revisions ti)))
        (ok (%st= '(:rebuilt t) (rev:target-state ti))))))
  (testing "unhandled, the condition propagates"
    (multiple-value-bind (cs r) (%composite)
      (let ((c (%capture (rev:composite-part-unavailable)
                 (rev:restore-composite cs r nil))))
        (ok (typep c 'rev:composite-part-unavailable))
        (ok (typep c 'rev:revision-error))))))
