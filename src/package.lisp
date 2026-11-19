(defpackage #:revision-protocol
  (:use #:cl)
  (:nicknames #:stack-rev)
  (:local-nicknames (#:dt #:datetime-protocol))
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
   #:make-in-memory-store))
