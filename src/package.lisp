(defpackage #:revision-protocol
  (:use #:cl)
  (:nicknames #:stack-rev)
  (:local-nicknames (#:dt #:datetime-protocol)
                    (#:diff #:diff-protocol))
  (:export
   ;; conditions
   #:revision-error
   #:revision-error-message
   #:revision-not-found
   #:revision-not-found-designator
   #:ambiguous-revision
   #:ambiguous-revision-designator
   #:ambiguous-revision-candidates
   #:ref-conflict
   #:ref-conflict-name
   #:ref-conflict-expected
   #:ref-conflict-actual
   #:tag-immutable
   #:tag-immutable-name
   #:tag-immutable-target
   #:cycle-refused
   #:cycle-refused-id
   #:cycle-refused-parents
   ;; restart names
   #:use-current
   #:retry
   #:create-new-tag

   ;; revisions (immutable values)
   #:revision
   #:revision-p
   #:revision-id
   #:revision-parent-ids
   #:revision-recorded-at
   #:revision-valid-at
   #:revision-provenance
   #:revision-message
   #:revision-content
   #:revision-time
   #:+minimum-id-prefix+

   ;; refs (snapshots)
   #:revision-ref
   #:revision-ref-p
   #:ref-name
   #:ref-kind
   #:ref-target
   #:ref-annotation

   ;; designators (values)
   #:revision-designator
   #:revision-designator-p
   #:ref-designator
   #:tag-designator
   #:as-of-designator
   #:parent-designator
   #:ancestor-designator
   #:rev-ref
   #:rev-tag
   #:rev-as-of
   #:rev-parent
   #:rev-ancestor
   #:designator-name
   #:designator-time
   #:designator-on
   #:designator-axis
   #:designator-base
   #:designator-n

   ;; store protocol
   #:revision-store
   #:revision-store-p
   #:resolve-revision
   #:commit-revision
   #:move-ref
   #:tag-revision
   #:delete-ref
   #:find-ref
   #:list-refs
   #:revision-parents
   #:revision-children
   #:walk-revisions
   #:merge-base
   #:ancestor-p
   #:revision-range
   #:heads
   #:ref-listener

   ;; in-memory store
   #:in-memory-store
   #:in-memory-store-p
   #:make-in-memory-store

   ;;; ---------------- R0b ----------------

   ;; conditions (R0b)
   #:merge-conflict
   #:merge-conflict-conflicts
   #:merge-conflict-projection
   #:irreversible-step
   #:irreversible-step-step
   #:target-drift-detected
   #:target-drift-detected-target
   #:target-drift-detected-drift
   #:step-failed
   #:step-failed-step
   #:step-failed-cause
   #:step-failed-attempt
   #:projection-unsupported
   #:projection-unsupported-fidelity
   #:projection-unsupported-supported
   #:projection-stale
   #:projection-stale-projection
   #:composite-part-unavailable
   #:composite-part-unavailable-part
   #:composite-part-unavailable-reason
   ;; restart names (R0b); ABORT is CL:ABORT
   #:force
   #:take-ours
   #:take-theirs
   #:use-merged
   #:skip-step
   #:stamp-only
   #:abort-transition
   #:capture-and-continue
   #:discard-drift
   #:retry-step
   #:rollback-to-start
   #:use-shadow
   #:accept-model-fidelity
   #:apply-for-real
   #:skip-part
   #:rebuild-from-recipe

   ;; capability queries (R0b; capability-protocol adapter later)
   #:content-primary
   #:content-engine
   #:permit-force-p
   #:target-fidelities
   #:target-transactional-p

   ;; content layer
   #:revision-payload
   #:revision-payload-p
   #:make-payload
   #:payload-state
   #:payload-state-p
   #:payload-delta
   #:revision-state
   #:revision-delta
   #:delta-inverse
   #:delta-footprint
   #:state-diff

   ;; target layer
   #:revision-target
   #:revision-target-p
   #:target-revisions
   #:target-stamp
   #:target-state
   #:target-drift
   #:capture-revision
   #:call-with-target-transaction
   #:with-target-transaction

   ;; plans
   #:transition-plan
   #:transition-plan-p
   #:plan-store
   #:plan-start
   #:plan-goal
   #:plan-base
   #:plan-steps
   #:plan-step
   #:step-direction
   #:step-from
   #:step-to
   #:step-diff
   #:step-footprint
   #:step-reversible-p
   #:plan-transition
   #:describe-plan
   #:execute-plan

   ;; projections
   #:projection
   #:projection-p
   #:project-state
   #:project-plan
   #:project-merge
   #:projection-fidelity
   #:projection-view
   #:projection-state
   #:projection-parents
   #:projection-conflicts
   #:projection-writable-p
   #:projection-discarded-p
   #:commit-projection
   #:discard-projection

   ;; composite revisions
   #:manifest-part
   #:make-manifest-part
   #:part-role
   #:part-store
   #:part-revision-id
   #:part-depends-on
   #:part-recipe
   #:revision-manifest
   #:make-manifest
   #:manifest-parts
   #:commit-composite
   #:composite-manifest
   #:plan-composite
   #:restore-composite

   ;; in-memory target
   #:in-memory-target
   #:in-memory-target-p
   #:make-in-memory-target))
