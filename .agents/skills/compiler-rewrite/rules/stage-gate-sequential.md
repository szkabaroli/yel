# stage-gate-sequential

> One stage at a time, finished to a written definition of done, before the next one starts

## Why It Matters

Parallelising stages is the obvious way to go faster and it is the reason
compiler rewrites die. Two stages in flight means the seam between them is being
implemented from two sides against a contract nobody has yet exercised — and
contracts are always wrong in small ways that only show up when real data flows
through. Discovering that with one stage built costs an afternoon. Discovering
it with three costs a re-merge of three branches whose authors are gone.

There is also a compounding effect specific to a rewrite under a differential
oracle: **the oracle only works if exactly one thing changed.** If HIR and
typeck are both new, a corpus divergence tells you the pair is wrong and nothing
about which one. The bisection that makes differential testing cheap depends on
serialisation. Two stages in flight throws away the single most valuable
property of the whole approach.

So: strictly sequential. Stage N is *complete* — merged to `main`, old path
deleted, corpus green, numbers recorded — before stage N+1 is briefed. This is
slower per week and much faster to the end.

## Bad

```
Week 1: brief agents for parser, HIR, and typeck in parallel — "they're
        independent stages, and we have the contracts written"
Week 4: all three report done
Week 5: nothing composes. The HIR agent needed spans the parser stopped
        emitting; typeck was written against contract v1 while HIR shipped v3.
        A corpus diff shows 400 divergences and no way to attribute them.
```

## Good

```
Stage 1 — parser        [brief → build → verify → merge → delete old → record]
Stage 2 — HIR           (not briefed until stage 1 is recorded)
Stage 3 — typeck/THIR
Stage 4 — LIR + lowering
Stage 5 — codegen
```

A stage is **complete** when every one of these is true — no partial credit, no
"finish it in the next stage":

1. `cargo test --workspace` passes; no test is `#[ignore]`d that was not
   ignored before.
2. The 91 positive, 23 diagnostic, and known-bug fixtures behave exactly as the
   baseline records — with any golden change individually justified in
   `goldens-changed.md`.
3. The 85 execution tests pass unmodified.
4. Differential sweep over the frozen corpus: zero unexplained divergences.
5. Fuzzer pass rate ≥ the recorded baseline, from a clean release build.
6. **The frozen tree is untouched** — `git diff --name-only` shows no change
   under the old crates — and **every throwaway adapter this stage introduced is
   deleted**, in the same PR series that introduced it.
7. The [review panel](review-adversarial-panel.md) is clean: no unresolved
   finding on any lens.
8. The stage file in `plans/rewrite/` is closed out — numbers, decisions,
   surprises — and `ratchet.md` has its new row.

Item 6 is the one that gets negotiated away, and it is the one that matters. An
adapter left in place "until the next stage needs it anyway" is how a two-week
bridge becomes a two-year one — precisely the pattern documented in
`TECH_DEBT.md §1`. (The *old compiler* is not deleted per stage; it is frozen
reference until the final cutover — see
[`cutover-switch-then-delete`](cutover-switch-then-delete.md).)

**Within** a stage, parallel agents are fine and useful — one writing the
implementation, another building its test slice, a panel of read-only reviewers.
The gate is on *stages*, not on agents.

## See Also

- [cutover-switch-then-delete](cutover-switch-then-delete.md) - How a stage actually lands
- [verify-ratchet-never-down](verify-ratchet-never-down.md) - The numbers in the definition of done
- [review-adversarial-panel](review-adversarial-panel.md) - The quality half of the gate
- [orchestrate-one-agent-one-stage](orchestrate-one-agent-one-stage.md) - Who does the work inside the gate
- [track-progress-in-markdown](track-progress-in-markdown.md) - Where the closed-out stage is recorded
