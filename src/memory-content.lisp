(in-package #:revision-protocol)

;;; R0b methods of IN-MEMORY-STORE and of diff values: content, state-diff,
;;; plans, projections, composite revisions.
;;;
;;; Contract stubs: every method signals a plain SIMPLE-ERROR through
;;; %R0B-STUB (never a REVISION-ERROR) so the R0b suite loads and fails.
;;; The implementation replaces the bodies; it may add slots, helper
;;; functions, internal projection classes and files (keep :SERIAL T order in
;;; the .asd). Code under the store lock must not call these public GFs
;;; (see the locking discipline in memory.lisp).

;;; Capability queries

(defmethod content-primary ((store in-memory-store))
  (%r0b-stub 'content-primary))

(defmethod content-engine ((store in-memory-store))
  (%r0b-stub 'content-engine))

(defmethod permit-force-p ((store in-memory-store) name)
  (declare (ignore name))
  (%r0b-stub 'permit-force-p))

;;; Content

(defmethod revision-state ((store in-memory-store) designator)
  (declare (ignore designator))
  (%r0b-stub 'revision-state))

(defmethod revision-delta ((store in-memory-store) designator)
  (declare (ignore designator))
  (%r0b-stub 'revision-delta))

(defmethod delta-inverse ((delta diff:diff))
  (%r0b-stub 'delta-inverse))

(defmethod delta-footprint ((delta diff:diff))
  (%r0b-stub 'delta-footprint))

(defmethod delta-footprint ((delta null))
  (%r0b-stub 'delta-footprint))

(defmethod state-diff ((store in-memory-store) a b &key footprint level)
  (declare (ignore a b footprint level))
  (%r0b-stub 'state-diff))

;;; Plans (store side)

(defmethod plan-transition ((store in-memory-store) target to)
  (declare (ignore target to))
  (%r0b-stub 'plan-transition))

(defmethod step-footprint ((step plan-step))
  (%r0b-stub 'step-footprint))

(defmethod describe-plan ((plan transition-plan) stream)
  (declare (ignore stream))
  (%r0b-stub 'describe-plan))

;;; Projections (store side)

(defmethod project-state ((store in-memory-store) designator
                          &key fidelity writable)
  (declare (ignore designator fidelity writable))
  (%r0b-stub 'project-state))

(defmethod project-merge ((store in-memory-store) a b)
  (declare (ignore a b))
  (%r0b-stub 'project-merge))

(defmethod commit-projection ((store in-memory-store) projection
                              &key parents message ref)
  (declare (ignore projection parents message ref))
  (%r0b-stub 'commit-projection))

;;; Composite revisions

(defmethod commit-composite ((store in-memory-store) parts
                             &key parents ref expected message)
  (declare (ignore parts parents ref expected message))
  (%r0b-stub 'commit-composite))

(defmethod composite-manifest ((store in-memory-store) designator)
  (declare (ignore designator))
  (%r0b-stub 'composite-manifest))

(defmethod plan-composite ((store in-memory-store) designator targets)
  (declare (ignore designator targets))
  (%r0b-stub 'plan-composite))

(defmethod restore-composite ((store in-memory-store) designator targets)
  (declare (ignore designator targets))
  (%r0b-stub 'restore-composite))
