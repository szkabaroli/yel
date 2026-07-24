# track-progress-in-markdown

> Every decision, contract, number, and stage status lives in versioned markdown — agents have no memory, the files are the memory

## Why It Matters

A multi-month rewrite driven by subagents has a specific structural problem: no
participant remembers anything. Each agent starts cold, does its stage, and
disappears. The orchestrator's context window is not durable either. Whatever is
not written down is re-derived — usually differently — by the next agent that
needs it.

The things that get lost are exactly the expensive ones: why a seam is shaped
the way it is, which two goldens were legitimately re-blessed and why, what the
fuzzer number was three stages ago, which anti-spec rule was added after review
caught a violation. Reconstructing any of these costs more than writing them
down would have.

So: **the rewrite's state is a directory of markdown, committed alongside the
code.** Not an issue tracker, not chat history, not the orchestrator's memory —
files in the repo, updated in the same PR as the work they describe, reviewable
in the same diff.

## Bad

```
plans/rewrite.md   (written once at kickoff, never updated)

Everything else in agent transcripts and PR descriptions. Stage 4's agent asks
"why does the HIR seam carry spans separately?" — nobody knows; the decision was
made in a conversation eleven weeks ago. It gets redesigned.
```

## Good

```
plans/rewrite/
  README.md            Status board: stage table, what's in flight, what's next.
                       The single entry point — every agent brief links here.
  anti-spec.md         Shapes the rewrite may not reproduce. Append-only;
                       grows when review finds a new one.
  keep-list.md         What is carried over intact (diagnostics, interning, ids).
  ratchet.md           Append-only table of measured numbers per stage.
  seam-changes.md      Log of contract changes: request, options, decision, date.
  corpus.md            How the frozen corpus was generated; seed range; known
                       failures at baseline; regeneration procedure.
  goldens-changed.md   Every re-blessed golden, one line each, with justification.
  stage-1-parser.md    Per stage: brief, contract, definition of done, agent
  stage-2-hir.md       assignments, numbers at completion, decision log,
  stage-3-thir.md      surprises found. Written BEFORE the stage starts and
  stage-4-lir.md       closed out when it lands.
  stage-5-codegen.md
```

A stage file has a fixed shape so agents can be pointed at it directly:

```markdown
# Stage 2 — HIR                                    status: in-flight
Started 2026-08-11 · base main@a1b2c3d

## Brief            (what the agent was told; links to contract + anti-spec)
## Contract         (in/out types + invariants — or a link to the merged seam file)
## Reference        (which ark files, which frozen yel files encode the behaviour)
## Definition of done   (the 7 gates from stage-gate-sequential, as checkboxes)
## Numbers          (filled in at close: tests, execution, fuzz, divergences, ignored)
## Decision log     (dated one-liners: what was decided, why, by whom)
## Surprises        (behaviour discovered in the old compiler that nobody knew about
                     — the highest-value section, and the one most often skipped)
```

Discipline that makes it work rather than rot:

- **Written before, closed after.** A stage file created at kickoff and filled
  in at the end is a report; one written first is a brief the agent can execute.
- **Append-only for `ratchet.md`, `seam-changes.md`, `anti-spec.md`.** Editing
  history destroys the thing that makes them useful.
- **Updated in the same PR as the code.** A docs-catchup PR is a PR that does not
  happen.
- **`README.md` is the index and stays short.** If it exceeds a screen, move
  detail into a stage file. Nobody reads a status board they have to scroll.
- **Record surprises even when they change nothing.** "The old typeck silently
  widens `i32` to `f64` in this one position" is the kind of fact that costs a
  week when rediscovered at stage 5 and thirty seconds when written at stage 3.

## See Also

- [verify-ratchet-never-down](verify-ratchet-never-down.md) - What lives in `ratchet.md`
- [anti-spec-from-tech-debt](anti-spec-from-tech-debt.md) - What lives in `anti-spec.md`
- [orchestrate-integrator-owns-seams](orchestrate-integrator-owns-seams.md) - What lives in `seam-changes.md`
- [oracle-never-rebless](oracle-never-rebless.md) - Why `goldens-changed.md` exists at all
