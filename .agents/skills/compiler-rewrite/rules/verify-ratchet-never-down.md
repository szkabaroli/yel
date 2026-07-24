# verify-ratchet-never-down

> Every stage lands on a measured number that is ≥ the last recorded one, from a clean build

## Why It Matters

Over a multi-month rewrite, quality erodes in increments that each look
acceptable. "Two fuzzer seeds regressed, but the structure is so much better."
"Three execution tests are `#[ignore]`d, we'll come back to them." Each trade is
defensible in isolation; six stages of them and the new compiler is worse than
the one you replaced, with no single commit to blame.

A ratchet removes the per-stage judgement call. The numbers are recorded when a
stage lands, and the next stage must meet or beat them. Not "roughly the same",
not "better in the ways that matter" — the recorded number or higher, or it does
not merge.

The measurement has to be honest, which in practice means one thing: **clean
release build**. A stale `./target/release/yelc` reports the previous run's
behaviour, and there is no more demoralising way to lose a week than chasing a
regression that was fixed before you started measuring.

## Bad

```markdown
Stage 4 done. Fuzzer 71/100 (baseline was 73 — the two regressions are edge
cases in nested options, filed as issues). Two execution tests #[ignore]d
pending the stage-5 codegen work. Merging.
```

Two things went wrong here and only one is visible. The visible one: the
ratchet went down and the merge happened anyway. The invisible one: "pending
stage 5" is a promise made by whoever is not going to keep it, and the ignored
tests are now the baseline that stage 5 measures against — so the loss is
permanent and compounding.

## Good

Keep a ratchet table in the rewrite plan, appended to as each stage lands:

```markdown
## Ratchet (append-only; never edit a past row)

| Stage | Date | workspace tests | execution | fuzz/200 | corpus divergences | ignored |
|-------|------|-----------------|-----------|----------|--------------------|---------|
| baseline (pre-rewrite) | 2026-07-24 | 396 pass | 85/85 | 146/200 | — | 1 |
| 1 — parser | | ≥396 | 85/85 | ≥146 | 0 | ≤1 |
| 2 — HIR | | ≥ prev | 85/85 | ≥ prev | 0 | ≤ prev |
```

The rules that make it a ratchet rather than a dashboard:

- **Measured from a clean release build**, every time:
  `touch crates/yelc/src/main.rs && cargo build --release -p yelc -p yel-smith`.
- **The ignored-test count is a tracked metric.** It is the easiest number to
  game and the one that silently absorbs regressions.
- **Zero unexplained corpus divergences.** Explained ones are enumerated in the
  PR with a reason each.
- **A row is never edited after it lands.** If a past number turns out to have
  been measured wrong, add a corrective row; don't rewrite history you are now
  measuring against.
- **Going down requires an explicit, written decision** from whoever owns the
  rewrite, recorded as its own row with the justification. It should happen
  approximately never, and the point of the ceremony is that it feels heavy.

The fuzzer number in particular deserves attention: it is the only metric in the
table that improves on its own as the rewrite fixes latent bugs. If it is flat
across a stage, the stage probably preserved the old code's bugs faithfully —
worth asking why.

## See Also

- [verify-differential-not-review](verify-differential-not-review.md) - Where the divergence column comes from
- [oracle-never-rebless](oracle-never-rebless.md) - The other way a green suite lies
- [stage-gate-sequential](stage-gate-sequential.md) - The gate these numbers enforce
- [`fuzz-measure-clean-build`](../../fuzzer-debugging/rules/fuzz-measure-clean-build.md) - Why the `touch` is not optional
