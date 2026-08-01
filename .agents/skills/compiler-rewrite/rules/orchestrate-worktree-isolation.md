# orchestrate-worktree-isolation

> Agents working simultaneously get their own worktree; the integrator owns `main`

## Why It Matters

Stages run sequentially (see [`stage-gate-sequential`](stage-gate-sequential.md)),
but *within* a stage several agents are usefully concurrent — one implementing,
one extending the test slice, one reviewing against the anti-spec. Point all of
them at the same checkout and they interleave edits in the same files, each one
building on a tree the others are mutating. The symptom is agents "fixing"
compile errors that another agent introduced thirty seconds earlier, in a loop
that can burn a lot of tokens producing nothing.

Isolation makes concurrency safe and, more importantly, makes it *reviewable*:
each agent's output is a self-contained diff against a known base, which can be
read, tested, and accepted or rejected on its own.

The corollary is that no agent merges. Merging is where the anti-spec is
enforced, where seam violations are caught, and where the ratchet is measured —
it belongs to the integrator, who is the only participant with the whole stage
in view.

## Bad

```
Three agents, one checkout, same stage:
  agent A rewrites the lowering
  agent B extends the execution tests
  agent C reviews and "fixes what it finds"

A's cargo check fails on B's half-written test; C reverts a change A was
mid-way through; all three report confusing results.
```

## Good

Give each concurrent agent an isolated tree, from the same base commit:

```
Stage 3 — typeck. Base: main@<sha> (contract merged, stage 2 deleted)

  agent: implement    isolation: worktree   → diff: crates/yel-core/src/thir/**
  agent: test-slice   isolation: worktree   → diff: tests/** only
  agent: adversary    read-only, no worktree — reviews the other two diffs
                      against the anti-spec; reports, does not edit
```

Then:

- **The integrator applies diffs in order** — implementation, then tests, then
  review fixes — running the suite between each. A conflict is information about
  the split, not a chore: two agents touching the same file usually means the
  work was not actually independent.
- **The reviewing agent never has write access.** An adversarial reviewer that
  can fix what it finds stops reporting and starts patching, and you lose the
  report — which was the valuable artifact.
- **Every agent reports its diff and its numbers**, not a narrative. "Done,
  cleaned it up" is not a handoff.
- **Rebase forward, never sideways.** If the base moves, the integrator says so
  explicitly; agents do not pull each other's work.
- **Worktrees are for concurrent writers only.** A single agent working alone on
  a stage should just work on a branch — the isolation costs setup time and disk
  for no benefit when there is nobody to collide with.

## See Also

- [orchestrate-one-agent-one-stage](orchestrate-one-agent-one-stage.md) - What each agent owns
- [orchestrate-integrator-owns-seams](orchestrate-integrator-owns-seams.md) - The other thing agents may not touch
- [stage-gate-sequential](stage-gate-sequential.md) - Concurrency within a stage, never across stages
