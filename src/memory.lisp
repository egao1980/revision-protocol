(in-package #:revision-protocol)

;;; In-memory reference store.
;;;
;;; Contract (see the brief, docs/capabilities/revision.md in cl-stack):
;;;  - default ids: 40 lowercase hex characters, unique within the store;
;;;    COMMIT-REVISION :ID overrides (any string);
;;;  - RECORDED-AT is (DATETIME-PROTOCOL:NOW) at commit, so tests pin it with
;;;    DATETIME-PROTOCOL:WITH-FIXED-CLOCK;
;;;  - safe for concurrent use from several threads (one store lock via
;;;    bordeaux-threads bt2); ref CAS is linearizable;
;;;  - the listener runs after the update, outside the lock.
;;;
;;; Locking discipline:
;;;  - every table access happens inside %ATTEMPT (one lock acquisition);
;;;  - code under the lock calls only %-helpers, never a public generic
;;;    function (bt2 locks are not recursive);
;;;  - no condition is signalled while the lock is held: a failure detected
;;;    under the lock is THROWn out of the critical section as a condition
;;;    object (%FAIL) and signalled with ERROR after release, inside the
;;;    operation's RESTART-CASE;
;;;  - a ref compare-and-swap reads, compares and installs in one critical
;;;    section.
;;;
;;; Invariant: %ORDER lists every id once, each after all of its parents (a
;;; commit's parents exist before it is appended, and cycles are refused).
;;; A :TOPO walk is therefore the reachable set in insertion order.

(defclass in-memory-store (revision-store)
  ((listener :initform nil :accessor %listener)
   (lock :initform (bt2:make-lock :name "revision-protocol in-memory-store")
         :reader %lock)
   (revisions :initform (make-hash-table :test 'equal) :reader %revisions
              :documentation "id → REVISION.")
   (positions :initform (make-hash-table :test 'equal) :reader %positions
             :documentation "id → insertion index into ORDER.")
   (order :initform (make-array 64 :adjustable t :fill-pointer 0)
          :reader %order
          :documentation "Ids in insertion (= topological) order.")
   (children :initform (make-hash-table :test 'equal) :reader %children
             :documentation "id → list of child ids (each child once).")
   (refs :initform (make-hash-table :test 'equal) :reader %refs
         :documentation "name → immutable REVISION-REF snapshot.")
   (random-state :initform (make-random-state t) :reader %random-state
                 :documentation "Used only under LOCK, for default ids."))
  (:documentation "Reference REVISION-STORE kept in Lisp memory."))

(defun in-memory-store-p (x)
  "T when X is a IN-MEMORY-STORE."
  (typep x 'in-memory-store))

(defun make-in-memory-store ()
  "A fresh, empty IN-MEMORY-STORE."
  (make-instance 'in-memory-store))

;;; ------------------------------------------------------------------
;;; Locking core
;;; ------------------------------------------------------------------

(defun %fail (type &rest initargs)
  "Abort the current critical section with a fresh condition of TYPE.
Never signals: %ATTEMPT's caller signals it after the lock is released."
  (throw '%store-failure (apply #'make-condition type initargs)))

(defun %attempt (store thunk)
  "Call THUNK with STORE's lock held.
→ (values T list-of-THUNK-values) on success, (values NIL condition) when THUNK
called %FAIL."
  (let ((failure nil)
        (vals '()))
    (bt2:with-lock-held ((%lock store))
      (setf failure (catch '%store-failure
                      (setf vals (multiple-value-list (funcall thunk)))
                      nil)))
    (if failure
        (values nil failure)
        (values t vals))))

(defmacro %with-store ((store) &body body)
  "Run BODY under STORE's lock. A %FAIL is signalled with ERROR after release."
  (let ((ok (gensym "OK"))
        (result (gensym "RESULT")))
    `(multiple-value-bind (,ok ,result)
         (%attempt ,store (lambda () ,@body))
       (if ,ok
           (values-list ,result)
           (error ,result)))))

(defun %notify (store event)
  "Call STORE's listener with EVENT (when both are non-NIL). Lock not held."
  (let ((fn (%listener store)))
    (when (and fn event)
      (funcall fn event))))

(defun %read-new-tag-name ()
  (format *query-io* "New tag name: ")
  (force-output *query-io*)
  (list (read-line *query-io*)))

;;; ------------------------------------------------------------------
;;; Lookups and resolution (lock held)
;;; ------------------------------------------------------------------

(defun %by-id (store id)
  (gethash id (%revisions store)))

(defun %first-parent (store revision)
  (let ((p (first (%revision-parent-ids revision))))
    (and p (%by-id store p))))

(defun %not-found (designator &optional message)
  (%fail 'revision-not-found :designator designator :message message))

(defun %resolve-string (store string top)
  (or (%by-id store string)
      (let ((n (length string)))
        (when (< n +minimum-id-prefix+)
          (%not-found top (format nil "~s is not an id and is shorter than ~d"
                                  string +minimum-id-prefix+)))
        (let ((hits (loop for id across (%order store)
                          when (and (>= (length id) n)
                                    (string= string id :end2 n))
                            collect id)))
          (cond ((null hits) (%not-found top))
                ((rest hits)
                 (%fail 'ambiguous-revision :designator string
                                            :candidates hits))
                (t (%by-id store (first hits))))))))

(defun %resolve (store designator &optional (top designator))
  "REVISION named by DESIGNATOR, lock held. TOP is the designator the caller
passed; it fills REVISION-NOT-FOUND's designator slot, also for a failure in a
nested designator."
  (etypecase designator
    (string (%resolve-string store designator top))
    (revision
     (or (%by-id store (revision-id designator))
         (%not-found top "revision is not in this store")))
    (tag-designator
     (let ((entry (gethash (designator-name designator) (%refs store))))
       (if (and entry (eq (ref-kind entry) :tag))
           (%by-id store (ref-target entry))
           (%not-found top))))
    (ref-designator
     (let ((entry (gethash (designator-name designator) (%refs store))))
       (if entry
           (%by-id store (ref-target entry))
           (%not-found top))))
    (as-of-designator
     (let ((time (designator-time designator))
           (axis (designator-axis designator)))
       ;; No early stop: valid times are not monotonic along the line.
       (loop for r = (%resolve store (designator-on designator) top)
               then (%first-parent store r)
             while r
             when (dt:<= (revision-time r axis) time)
               return r
             finally (%not-found top "no revision at or before that time"))))
    (parent-designator
     (let* ((base (%resolve store (designator-base designator) top))
            (pid (nth (1- (designator-n designator))
                      (%revision-parent-ids base))))
       (if pid
           (%by-id store pid)
           (%not-found top "parent index out of range"))))
    (ancestor-designator
     (let ((r (%resolve store (designator-base designator) top)))
       (dotimes (i (designator-n designator) r)
         (declare (ignorable i))
         (setf r (or (%first-parent store r)
                     (%not-found top "ran past the root"))))))))

(defun %resolve-id (store designator)
  (revision-id (%resolve store designator)))

(defun %designator-list (x)
  "X as a list of designators: NIL → (), a list → itself, else (X)."
  (if (listp x) x (list x)))

(defun %check-designators (x)
  (mapc #'%check-designator (%designator-list x))
  x)

(defun %ids-of (store designators)
  (mapcar (lambda (d) (%resolve-id store d)) (%designator-list designators)))

;;; ------------------------------------------------------------------
;;; Graph helpers (lock held)
;;; ------------------------------------------------------------------

(defun %closure (store ids &optional stop)
  "Hash set of IDS and their ancestors, not entering ids in the STOP set."
  (let ((seen (make-hash-table :test 'equal))
        (stack (copy-list ids)))
    (loop while stack
          do (let ((id (pop stack)))
               (unless (or (gethash id seen)
                           (and stop (gethash id stop)))
                 (setf (gethash id seen) t)
                 (dolist (p (%revision-parent-ids (%by-id store id)))
                   (push p stack)))))
    seen))

(defun %ancestor-id-p (store a b)
  "T when id A is id B or an ancestor of it. A need not exist."
  (let ((sa (gethash a (%positions store)))
        (sb (gethash b (%positions store))))
    (and sa sb (<= sa sb)
         (values (gethash a (%closure store (list b)))))))

(defun %sort-by-sequence (store set)
  (let ((ids (loop for id being the hash-keys of set collect id))
        (seq (%positions store)))
    (mapcar (lambda (id) (%by-id store id))
            (sort ids #'< :key (lambda (id) (gethash id seq))))))

(defun %time-before-p (a b)
  "Older RECORDED-AT first; equal times by STRING< id."
  (let ((ta (revision-recorded-at a))
        (tb (revision-recorded-at b)))
    (or (dt:< ta tb)
        (and (dt:= ta tb)
             (string< (revision-id a) (revision-id b))
             t))))

(defun %kahn (store set before-p)
  "Topological sort of the id set SET; the ready set is ordered by BEFORE-P."
  (let ((pending (make-hash-table :test 'equal))
        (ready '())
        (out '()))
    (loop for id being the hash-keys of set
          do (let ((n (count-if (lambda (p) (gethash p set))
                                (remove-duplicates
                                 (%revision-parent-ids (%by-id store id))
                                 :test #'equal))))
               (if (zerop n)
                   (push (%by-id store id) ready)
                   (setf (gethash id pending) n))))
    (setf ready (sort ready before-p))
    (loop while ready
          do (let ((r (pop ready)))
               (push r out)
               (dolist (c (gethash (revision-id r) (%children store)))
                 (when (and (gethash c set)
                            (zerop (decf (gethash c pending))))
                   (setf ready (merge 'list (list (%by-id store c)) ready
                                      before-p))))))
    (nreverse out)))

(defun %walk-locked (store from order until)
  (let ((stop (%closure store (%ids-of store until)))
        (from-ids (%ids-of store from)))
    (ecase order
      (:first-parent
       ;; A list FROM walks the first-parent chain of its first element.
       (let ((out '()))
         (loop for r = (and from-ids (%by-id store (first from-ids)))
                 then (%first-parent store r)
               while (and r (not (gethash (revision-id r) stop)))
               do (push r out))
         out))
      (:topo (%sort-by-sequence store (%closure store from-ids stop)))
      (:time (%kahn store (%closure store from-ids stop) #'%time-before-p)))))

(defun %merge-bases-locked (store a b)
  "Best common ancestors of ids A and B, newest RECORDED-AT first, ties by
smallest id."
  (let* ((ca (%closure store (list a)))
         (cb (%closure store (list b)))
         (common (loop for id being the hash-keys of ca
                       when (gethash id cb) collect id))
         ;; Proper ancestors of a common revision are not best.
         (dominated (%closure store
                              (loop for c in common
                                    append (%revision-parent-ids
                                            (%by-id store c)))))
         (best (loop for c in common
                     unless (gethash c dominated)
                       collect (%by-id store c))))
    (sort best (lambda (x y)
                 (let ((tx (revision-recorded-at x))
                       (ty (revision-recorded-at y)))
                   (or (dt:< ty tx)
                       (and (dt:= tx ty)
                            (string< (revision-id x) (revision-id y))
                            t)))))))

(defun %ref-tip-locked (store name)
  "Revision ref NAME points at, or NIL."
  (let ((entry (and name (gethash name (%refs store)))))
    (and entry (%by-id store (ref-target entry)))))

(defun %fresh-id (store)
  "40 lowercase hex characters, unused in STORE. Lock held."
  (loop for id = (format nil "~(~40,'0x~)"
                         (random (expt 2 160) (%random-state store)))
        unless (%by-id store id)
          return id))

;;; ------------------------------------------------------------------
;;; Resolution and graph queries
;;; ------------------------------------------------------------------

(defmethod resolve-revision ((store in-memory-store) designator)
  (%check-designator designator)
  (%with-store (store) (%resolve store designator)))

(defmethod revision-parents ((store in-memory-store) designator)
  (%check-designator designator)
  (%with-store (store)
    (mapcar (lambda (id) (%by-id store id))
            (%revision-parent-ids (%resolve store designator)))))

(defmethod revision-children ((store in-memory-store) designator)
  (%check-designator designator)
  (%with-store (store)
    (mapcar (lambda (id) (%by-id store id))
            (gethash (revision-id (%resolve store designator))
                     (%children store)))))

(defmethod heads ((store in-memory-store))
  (%with-store (store)
    (loop for id across (%order store)
          unless (gethash id (%children store))
            collect (%by-id store id))))

(defmethod walk-revisions ((store in-memory-store) from
                           &key (order :topo) until reverse)
  (check-type order (member :topo :time :first-parent))
  (%check-designators from)
  (%check-designators until)
  (let ((walk (%with-store (store) (%walk-locked store from order until))))
    (if reverse (reverse walk) walk)))

(defmethod revision-range ((store in-memory-store) include exclude)
  (%check-designators include)
  (%check-designators exclude)
  (when include
    (%with-store (store) (%walk-locked store include :topo exclude))))

(defmethod ancestor-p ((store in-memory-store) a b)
  (%check-designator a)
  (%check-designator b)
  (%with-store (store)
    (%ancestor-id-p store (%resolve-id store a) (%resolve-id store b))))

(defmethod merge-base ((store in-memory-store) a b &key all)
  (%check-designator a)
  (%check-designator b)
  (let ((best (%with-store (store)
                (%merge-bases-locked store (%resolve-id store a)
                                     (%resolve-id store b)))))
    (if all best (first best))))

;;; ------------------------------------------------------------------
;;; Commit
;;; ------------------------------------------------------------------

(defun %commit-locked (store content parents parents-p ref expected expected-p
                       message provenance valid-at id recorded-at)
  "Validate and store one revision. → (values revision event). Every check
runs before the first mutation, so a %FAIL leaves the store unchanged."
  (let* ((entry (and ref (gethash ref (%refs store))))
         (tip (and entry (ref-target entry))))
    (when (and entry (eq (ref-kind entry) :tag))
      (%fail 'tag-immutable :name ref :target tip))
    (let ((parent-ids (if parents-p
                          (mapcar (lambda (d) (%resolve-id store d)) parents)
                          (and tip (list tip)))))
      (when id
        ;; The cycle check comes first: it is the more specific condition.
        (when (some (lambda (p) (%ancestor-id-p store id p)) parent-ids)
          (%fail 'cycle-refused :id id :parents (copy-list parent-ids)))
        (when (%by-id store id)
          (%fail 'revision-error
                 :message (format nil "revision ~s exists already" id))))
      (when ref
        (let ((want (cond ((not expected-p) tip)
                          ((null expected) nil)
                          (t (%resolve-id store expected)))))
          (unless (equal want tip)
            (%fail 'ref-conflict :name ref :expected want :actual tip))))
      (let* ((new-id (or id (%fresh-id store)))
             (revision (make-instance 'revision
                                      :id new-id
                                      :parent-ids parent-ids
                                      :recorded-at recorded-at
                                      :valid-at valid-at
                                      :provenance provenance
                                      :message message
                                      :content content)))
        (setf (gethash new-id (%revisions store)) revision
              (gethash new-id (%positions store)) (fill-pointer (%order store)))
        (vector-push-extend new-id (%order store))
        (dolist (p (remove-duplicates parent-ids :test #'equal))
          (push new-id (gethash p (%children store))))
        (when ref
          (setf (gethash ref (%refs store))
                (make-instance 'revision-ref :name ref :kind :movable
                                             :target new-id)))
        (values revision (and ref (list :ref-moved ref tip new-id)))))))

(defmethod commit-revision ((store in-memory-store) content
                            &key (parents nil parents-p) ref
                              (expected nil expected-p) message provenance
                              valid-at id)
  (check-type ref (or null string))
  (check-type id (or null string))
  (check-type valid-at (or null dt:instant))
  (check-type parents list)
  (mapc #'%check-designator parents)
  (when expected
    (%check-designator expected))
  (let ((ref (and ref (copy-seq ref)))
        (id (and id (copy-seq id)))
        (provenance (copy-list provenance)))
    (loop
      (let ((recorded-at (dt:now)))     ; the caller's thread and clock
        (multiple-value-bind (ok result)
            (%attempt store
                      (lambda ()
                        (%commit-locked store content parents parents-p ref
                                        expected expected-p message provenance
                                        valid-at id recorded-at)))
          (cond
            (ok
             (destructuring-bind (revision event) result
               (%notify store event)
               (return revision)))
            ((typep result 'ref-conflict)
             (restart-case (error result)
               (use-current ()
                 :report (lambda (s)
                           (format s "Return the current tip of ~s; commit nothing."
                                   ref))
                 (return (%with-store (store) (%ref-tip-locked store ref))))
               (retry ()
                 :report (lambda (s)
                           (format s "Commit again with ~s's current target as expected."
                                   ref))
                 ;; Defaulted parents stay defaulted: the next attempt re-reads
                 ;; them from the new tip.
                 (setf expected (ref-conflict-actual result)
                       expected-p t))))
            (t (error result))))))))

;;; ------------------------------------------------------------------
;;; Refs and tags
;;; ------------------------------------------------------------------

(defun %move-locked (store name designator expected expected-p)
  "→ (values revision-ref event)."
  (let* ((entry (gethash name (%refs store)))
         (current (and entry (ref-target entry))))
    (when (and entry (eq (ref-kind entry) :tag))
      (%fail 'tag-immutable :name name :target current))
    (let ((target (%resolve-id store designator)))
      (when expected-p
        (let ((want (and expected (%resolve-id store expected))))
          (unless (equal want current)
            (%fail 'ref-conflict :name name :expected want :actual current))))
      (if (equal target current)
          (values entry nil)
          (let ((new (make-instance 'revision-ref :name name :kind :movable
                                                  :target target)))
            (setf (gethash name (%refs store)) new)
            (values new (list :ref-moved name current target)))))))

(defmethod move-ref ((store in-memory-store) name designator
                     &key (expected nil expected-p))
  (check-type name string)
  (%check-designator designator)
  (when expected
    (%check-designator expected))
  (let ((name (copy-seq name)))
    (loop
      (multiple-value-bind (ok result)
          (%attempt store (lambda ()
                            (%move-locked store name designator
                                          expected expected-p)))
        (cond
          (ok
           (destructuring-bind (ref event) result
             (%notify store event)
             (return ref)))
          ((typep result 'ref-conflict)
           (restart-case (error result)
             (use-current ()
               :report (lambda (s)
                         (format s "Leave ~s unchanged and return it." name))
               (return (find-ref store name)))
             (retry ()
               :report (lambda (s)
                         (format s "Move ~s with its current target as expected."
                                 name))
               (setf expected (ref-conflict-actual result)
                     expected-p t))))
          ((typep result 'tag-immutable)
           (restart-case (error result)
             (create-new-tag (new-name)
               :report "Create a new tag at the designator instead."
               :interactive %read-new-tag-name
               (return (tag-revision store new-name designator)))))
          (t (error result)))))))

(defun %tag-locked (store name designator annotation)
  "→ (values revision-ref event)."
  (let ((entry (gethash name (%refs store)))
        (target (%resolve-id store designator)))
    (cond
      ((null entry)
       (let ((new (make-instance 'revision-ref :name name :kind :tag
                                               :target target
                                               :annotation annotation)))
         (setf (gethash name (%refs store)) new)
         (values new (list :ref-moved name nil target))))
      ((eq (ref-kind entry) :movable)
       (%fail 'ref-conflict :name name :expected nil :actual (ref-target entry)
                            :message "a movable ref has that name"))
      ((and (equal target (ref-target entry))
            (equal annotation (ref-annotation entry)))
       (values entry nil))
      (t
       (%fail 'tag-immutable :name name :target (ref-target entry))))))

(defmethod tag-revision ((store in-memory-store) name designator
                         &key annotation)
  (check-type name string)
  (%check-designator designator)
  (let ((name (copy-seq name)))
    (multiple-value-bind (ok result)
        (%attempt store (lambda () (%tag-locked store name designator annotation)))
      (cond
        (ok
         (destructuring-bind (ref event) result
           (%notify store event)
           ref))
        ((typep result 'tag-immutable)
         (restart-case (error result)
           (create-new-tag (new-name)
             :report "Create a new tag with another name instead."
             :interactive %read-new-tag-name
             (tag-revision store new-name designator :annotation annotation))))
        (t (error result))))))

(defun %delete-locked (store name expected expected-p)
  "→ (values T event)."
  (let ((entry (gethash name (%refs store))))
    (unless entry
      (%not-found name "no ref of that name"))
    (let ((current (ref-target entry)))
      (when (eq (ref-kind entry) :tag)
        (%fail 'tag-immutable :name name :target current))
      (when expected-p
        (let ((want (and expected (%resolve-id store expected))))
          (unless (equal want current)
            (%fail 'ref-conflict :name name :expected want :actual current))))
      (remhash name (%refs store))
      (values t (list :ref-moved name current nil)))))

(defmethod delete-ref ((store in-memory-store) name &key (expected nil expected-p))
  (check-type name string)
  (when expected
    (%check-designator expected))
  (loop
    (multiple-value-bind (ok result)
        (%attempt store (lambda () (%delete-locked store name expected expected-p)))
      (cond
        (ok
         (destructuring-bind (done event) result
           (%notify store event)
           (return done)))
        ((typep result 'ref-conflict)
         (restart-case (error result)
           (use-current ()
             :report (lambda (s) (format s "Keep ~s; return NIL." name))
             (return nil))
           (retry ()
             :report (lambda (s)
                       (format s "Delete ~s whatever it points at now." name))
             (setf expected (ref-conflict-actual result)
                   expected-p t))))
        (t (error result))))))

(defmethod find-ref ((store in-memory-store) name)
  (check-type name string)
  (%with-store (store) (values (gethash name (%refs store)))))

(defmethod list-refs ((store in-memory-store) &key kind prefix)
  (check-type kind (member nil :movable :tag))
  (check-type prefix (or null string))
  (let ((refs (%with-store (store)
                (loop for ref being the hash-values of (%refs store)
                      when (and (or (null kind) (eq kind (ref-kind ref)))
                                (or (null prefix)
                                    (let ((n (length prefix))
                                          (name (ref-name ref)))
                                      (and (>= (length name) n)
                                           (string= prefix name :end2 n)))))
                        collect ref))))
    (sort refs #'string< :key #'ref-name)))

;;; ------------------------------------------------------------------
;;; Listener
;;; ------------------------------------------------------------------

(defmethod ref-listener ((store in-memory-store))
  (%listener store))

(defmethod (setf ref-listener) (function (store in-memory-store))
  (check-type function (or null function))
  (bt2:with-lock-held ((%lock store))
    (setf (%listener store) function)))
