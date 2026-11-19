# revision-protocol

CLOS revision protocol for [cl-stack](https://github.com/egao1980/cl-stack) (`stack-rev`): an abstract revision DAG that git, jj, SQL migrations, agent sessions and Lisp images can all back.

Capability brief: [`cl-stack/docs/capabilities/revision.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/revision.md).

Version **0.1.0** — milestone R0a, the graph layer, with a thread-safe `in-memory-store` (one lock; conditions are signalled only after it is released, so handlers may call back into the store). Content, targets, plans, projections and the diff integration are R0b.

**R0b (contract on this branch, in progress):** the content layer on [`diff-protocol`](https://github.com/egao1980/diff-protocol) (`revision-state`, `revision-delta`, `delta-inverse`, `delta-footprint`, `state-diff`, `content-primary` `:state`|`:delta`, `walk-revisions :footprint`), targets and plans (`target-revisions`, `target-stamp`, `target-drift`, `capture-revision`, `plan-transition`, `describe-plan`, `execute-plan`, `with-target-transaction`), projections (`project-state`, `project-plan`, `project-merge`, `commit-projection`, `discard-projection`), composite revisions (`commit-composite`, `plan-composite`, `restore-composite`), an `in-memory-target` over sexp states, the rest of the condition tree and the permission-gated `force` restart. The methods are stubs; `tests/` (content, target, plan, projection, composite, force and law-r0b) is the spec.

## R0a surface

- **Revisions** are immutable: `revision-id`, `revision-parent-ids` (first parent first; merges allowed), `revision-recorded-at` (store clock, a `datetime-protocol` instant), `revision-valid-at` (optional, caller-supplied), `revision-provenance`, `revision-message`, `revision-content` (opaque until R0b).
- **Refs** are movable (`:movable`) or immutable tags (`:tag`), one namespace. Every update is compare-and-swap through `:expected`.
- **Designators** are values: a string id or id prefix (≥ 4 chars), a `revision`, `(rev-ref name)`, `(rev-tag name)`, `(rev-as-of instant :on ref :axis :recorded|:valid)`, `(rev-parent d n)`, `(rev-ancestor d n)`.
- **Store GFs:** `resolve-revision`, `commit-revision`, `move-ref`, `tag-revision`, `delete-ref`, `find-ref`, `list-refs`, `revision-parents`, `revision-children`, `walk-revisions` (`:topo` `:time` `:first-parent`, `:until`, `:reverse`), `merge-base` (`:all`), `ancestor-p`, `revision-range`, `heads`, `ref-listener`.
- **Conditions:** `revision-error` → `revision-not-found`, `ambiguous-revision`, `ref-conflict` (restarts `use-current`, `retry`), `tag-immutable` (restart `create-new-tag`), `cycle-refused`.

```lisp
(let ((s (stack-rev:make-in-memory-store)))
  (stack-rev:commit-revision s '(:text "hello") :ref "main" :message "first")
  (stack-rev:commit-revision s '(:text "hello, world") :ref "main")
  (stack-rev:resolve-revision s (stack-rev:rev-ancestor (stack-rev:rev-ref "main") 1)))
```

## Tests

```sh
cl-repo test --system revision-protocol
```

## License

MIT
