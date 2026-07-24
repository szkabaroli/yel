# review-adversarial-panel

> Review agents are read-only, one lens each, and they check the things the differential cannot

## Why It Matters

[`verify-differential-not-review`](verify-differential-not-review.md) says review
cannot clear a rewritten stage for correctness. That is true and it is often
misread as "review doesn't matter here." The opposite: the differential covers
*only* correctness, and correctness is not what a rewrite is for. If it were,
you would keep the old code — it is already correct.

The rewrite exists to produce code that is clean and cheap to develop on. That
property has no automated oracle. Nothing in CI catches a new god pass, a
reinvented side channel, a fourth copy of a walker, a contract quietly widened,
or the diagnostics infrastructure being replaced by something worse. Those are
review's job, and they are the *only* things that determine whether the rewrite
was worth doing.

A single "review this stage" agent does this badly. Handed 4000 lines and asked
to find problems, an agent produces a general impression weighted toward
whatever it read last. Splitting review into **one lens per agent**, each with a
narrow checklist and no authority to edit, produces specific findings that can
be acted on or rejected individually.

## Bad

```
Agent: "Review the stage 3 diff and report any issues."

→ "Overall the code is well structured and idiomatic. A few naming nits.
   The error handling could be more consistent. LGTM with minor comments."
```

Nothing here is checkable, nothing is attributable to a rule, and the two things
that actually mattered — a `ThirExpr` field added at the seam, and 40 goldens
re-blessed in one commit — were not looked for, so they were not found.

## Good

Run a panel over the stage diff. Each agent is read-only, gets one lens, and
returns findings against a named rule — never a narrative.

| Lens | Checks | Fails the stage if |
|---|---|---|
| **freeze** | `git diff --name-only` against the frozen tree | Any file under the frozen crates changed ([greenfield-never-touch-old-code](greenfield-never-touch-old-code.md)) |
| **contract** | Seam files vs. the merged contract; invariants actually established | A seam type changed, or a documented invariant is unenforced ([contract-before-fanout](contract-before-fanout.md)) |
| **anti-spec** | The listed shapes: god pass, side channel, duplicate walker, permanent bridge, silent fallback | Any shape reproduced ([anti-spec-from-tech-debt](anti-spec-from-tech-debt.md)) |
| **keep-list** | Diagnostics, interning, typed ids, context threading | Carried-over infrastructure was redesigned instead of reused ([keep-diagnostics-infrastructure](keep-diagnostics-infrastructure.md)) |
| **test-honesty** | Diff of `tests/`, fixtures, `.snap`, `#[ignore]` | A golden re-blessed without justification, an assertion weakened, a test newly ignored ([oracle-never-rebless](oracle-never-rebless.md)) |
| **reference-fidelity** | The new frontend vs. the ark design | Patterns claimed as ported are shape-only — e.g. a green tree that drops trivia, recovery sets that exist but are never used ([frontend-follow-ark-reference](frontend-follow-ark-reference.md)) |

How to run the panel:

- **Read-only, always.** A reviewer that can edit stops reporting and starts
  patching, and the report was the deliverable. Findings go back to the
  implementing agent or the integrator.
- **Findings cite a rule and a file:line.** "This is a god pass —
  `anti-god-pass`, `yel2-thir/src/check.rs:1..1900`, does resolution + inference
  + lowering in one impl" is actionable. "Consider splitting this up" is not.
- **The test-honesty lens is not optional and runs on every stage.** It is the
  cheapest agent in the panel and it guards the oracle, which is the one thing
  whose loss is unrecoverable.
- **Run the panel concurrently, in worktrees or read-only** — the lenses are
  independent by construction ([orchestrate-worktree-isolation](orchestrate-worktree-isolation.md)).
- **Prompt for refutation, not approval.** "Find where this violates X" beats
  "does this look good?" — the second reliably returns yes.
- **A finding the implementer disputes goes to the integrator**, who decides and
  writes the outcome into the stage file. Reviewers do not have the last word;
  they have the first.
- **New failure shapes get appended to `anti-spec.md`.** A lens that finds
  something no rule named has just improved every future stage — that is the
  panel's compounding value.

Panel findings gate the *quality* half of the definition of done; the
differential numbers gate the correctness half. A stage needs both.

## See Also

- [verify-differential-not-review](verify-differential-not-review.md) - The half of verification review cannot do
- [stage-gate-sequential](stage-gate-sequential.md) - Where panel findings gate the merge
- [track-progress-in-markdown](track-progress-in-markdown.md) - Where findings and their resolutions are recorded
