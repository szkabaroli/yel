# orchestrate-one-agent-one-stage

> One agent owns one stage end-to-end — implementation, its tests, and its differential run

## Why It Matters

The instinct with a 93k-line rewrite is to shard by file or by size: three
agents on the 8.5k-line lowering file, one on each half of codegen. This
produces the worst possible outcome. Sub-stage shards share mutable state and
private helpers, so their edits collide; none of them can run a meaningful test,
because a third of a lowering pass has no observable behaviour; and no single
agent is accountable when the assembled result miscompiles.

The stage is the right unit because it is the only unit with a **testable
contract**: a stage has a defined input, a defined output, and a slice of the
conformance corpus that exercises it. An agent that owns one can answer "is it
correct?" without integration.

Ownership is end-to-end. The agent that writes the stage also writes its unit
tests, runs the differential sweep for its seam, and reports the number. Handing
a "finished" stage to someone else for testing means the tests are written by
someone who does not know what was cut.

## Bad

```
Agent 1: rewrite blocks.rs lines 1–3000
Agent 2: rewrite blocks.rs lines 3000–6000
Agent 3: rewrite blocks.rs lines 6000–8500
Integrator: assemble, fix conflicts, hope
```

Three agents editing one file, no runnable intermediate, no independent
verification, and a merge that is harder than the original task.

## Good

```
Stage 3 — typeck (HIR → THIR)              [one agent, one worktree]
  Input contract:   HirModule + invariants H1–H5   (merged, frozen)
  Output contract:  ThirModule + invariants T1–T7  (merged, frozen)
  Anti-spec:        §1, §2, §6 shapes — no god pass, no duplicate walkers
  Corpus slice:     23 diagnostic fixtures (exact meaning preserved)
                    91 positive fixtures (must typeck clean)
                    2000-seed corpus (must not newly reject anything)
  Definition of done:
    - cargo test -p yel-core passes
    - all 23 diagnostic fixtures reject, for the same reason
    - differential: zero new rejections across corpus/src/*.yel
    - report the numbers; do not re-bless any golden
```

Sizing heuristics that hold up:

- **If a shard cannot run the corpus, it is not a shard.** That is the test for
  whether you have split too far.
- **A stage too big for one agent is a stage with an undiscovered internal
  seam.** Find it, contract it, and you have two stages — run one after the
  other, not three shards run together.
- **One stage in flight, ever.** Stage N+1 is not briefed until stage N is
  complete and deleted — see [`stage-gate-sequential`](stage-gate-sequential.md).
  Concurrency lives *inside* a stage (implementer, test author, adversarial
  reviewer), never across stages.
- **Give the agent the old implementation as reference, explicitly framed**: it
  is the behavioural specification, not a structural model. Say so in the brief,
  or you get a transliteration of the mess you are trying to escape.

## See Also

- [stage-gate-sequential](stage-gate-sequential.md) - Stages run one at a time; this rule is about who owns the one in flight
- [contract-before-fanout](contract-before-fanout.md) - What makes a stage independently implementable
- [orchestrate-worktree-isolation](orchestrate-worktree-isolation.md) - Keeping parallel stages off each other
- [verify-differential-not-review](verify-differential-not-review.md) - The number the agent must report
