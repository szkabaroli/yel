# Ratchet — measured numbers per stage

> **Append-only. Never edit a row after it lands.** If a past number was measured
> wrong, add a corrective row; do not rewrite the history you are now measuring
> against.
> Rule: [`verify-ratchet-never-down`](../../.agents/skills/compiler-rewrite/rules/verify-ratchet-never-down.md)

Every row must be produced from a **clean release build**:

```bash
touch crates/yelc/src/main.rs && cargo build --release -p yelc -p yel-smith
```

A stale `target/release/yelc` reports the *previous* run's behaviour. There is no
faster way to lose a week than chasing a regression that was fixed before you
started measuring.

## The table

| Stage | Date | freeze/base SHA | workspace tests | execution | fuzz/200 | corpus divergences | ignored |
|-------|------|-----------------|-----------------|-----------|----------|--------------------|---------|
| **baseline (pre-rewrite)** | 2026-07-24 | `ccf2086` | **315 passed / 0 failed** | **85 / 85** | **200 / 200** | — (corpus defined here) | **2** |
| 1 — syntax | | | ≥ 315 | 85 / 85 | ≥ 200 | 0 | ≤ 2 |
| 2 — HIR | | | ≥ prev | 85 / 85 | ≥ prev | 0 | ≤ prev |
| 3 — THIR | | | ≥ prev | 85 / 85 | ≥ prev | 0 | ≤ prev |
| 4 — LIR | | | ≥ prev | 85 / 85 | ≥ prev | 0 | ≤ prev |
| 5 — codegen | | | ≥ prev | 85 / 85 | ≥ prev | 0 | ≤ prev |

## Baseline detail (2026-07-24, `ccf2086de2750c3783fd6f930be4a766f2463adb`)

Measured after `touch crates/yelc/src/main.rs && cargo build --release -p yelc -p yel-smith`,
`wasm-tools 1.227.1`, on darwin 24.2.0.

### Workspace — `cargo test --workspace`

**315 passed, 0 failed, 2 ignored, 0 filtered.** Per-target:

| Target | Passed | Ignored |
|--------|--------|---------|
| `yel-core` lib | 164 | 0 |
| `yel-wasm-codegen` lib | 31 | 0 |
| `yel-wasm-codegen` `tests/execution.rs` | **85** | 0 |
| `yel-wasm-codegen` `tests/runtime.rs` | 9 | 0 |
| `yel-wasm-codegen` `tests/integration.rs` | 8 | 0 |
| `yel-wasm-codegen` `tests/dump_wasm.rs` | 6 | 0 |
| `yelc` `tests/compile.rs` | 4 | 0 |
| `yelc` `tests/snapshot.rs` | 3 | 0 |
| `yel-smith` lib | 4 | 0 |
| `yel-lsp` lib | 1 | 0 |
| doc-tests + remaining unit targets | balance of 315 | 2 |

### Execution — `cargo test -p yel-wasm-codegen --test execution`

**85 / 85 passed, 0 ignored.** This is the only semantic oracle in the suite —
real DOM-op behaviour under Wasmtime. A stage that passes WIT snapshots but
drops an execution test has miscompiled something; the snapshot just wasn't
looking. **These tests are never modified by the rewrite.**

### Fuzz — 200 seeds, compile to wasm + `wasm-tools validate`

**200 / 200.** Zero failing seeds at baseline. The wider 2000-seed corpus freeze
agrees: 2000/2000 generate, compile to WIT + DOT + WASM, and validate, with an
**empty** `corpus/known-failures.txt` (see [`corpus.md`](corpus.md)).

Consequence worth stating up front: the fuzz column **cannot improve**. It is
already saturated, so it can only detect regressions, not reward fixes. The
metric that carries the "did the rewrite find latent bugs?" signal is therefore
the **corpus divergence** column plus the `known_bugs` fixture directory — not
this number. If a stage wants credit for a real fix, it lands a `known_bugs`
fixture promotion, and the seed count is widened past 200 for that stage's
sweep.

The flip side is that the divergence column gets stricter: with no failing seeds
to hide behind, *any* artifact difference is a real behavioural change that must
be explained. There is no "that seed was already broken" escape hatch.

### Ignored tests — 2

Both are documentation-example doctests, not disabled behaviour:

1. `crates/yel-core/src/lir/layout.rs` — doctest at line 8
2. `crates/yel-smith/src/lib.rs` — doctest at line 18

The ignored count is the easiest number in this table to game and the one that
silently absorbs regressions. It is tracked for exactly that reason: a stage
that "passes" by adding `#[ignore]` fails the gate.

## Rules

- **Zero unexplained corpus divergences.** Explained ones are enumerated in the
  stage file with a reason each, and cross-linked from `goldens-changed.md`.
- **Going down requires an explicit written decision** by the rewrite owner,
  recorded as its own row with the justification. It should happen approximately
  never; the ceremony is the point.
- **Execution stays 85/85 exactly.** Not "≥ 85" in spirit — the same 85 tests,
  unmodified. Adding tests is fine; the 85 must all still pass as written.
