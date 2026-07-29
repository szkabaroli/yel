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
| **baseline (re-freeze)** | 2026-07-28 | `c51b51d` | **315 passed / 0 failed** | **85 / 85** | **200 / 200** | — (corpus regenerated) | **2** |
| **1 — syntax** | 2026-07-28 | `33e5c71` | **480 pass / 0 fail** | **85 / 85** | **200 / 200** | **0** | **2** |
| **baseline (re-freeze 2)** | 2026-07-28 | `3ef3568` | **315 passed / 0 failed** | **85 / 85** | **200 / 200** | — (corpus proved neutral, not regenerated) | **2** |
| **2 — driver** | 2026-07-28 | `2505f8d` | **480 pass / 0 fail** | **85 / 85** | **200 / 200** | **0** | **2** |
| **baseline (re-freeze 3)** | 2026-07-29 | `1d12250` | **480 pass / 0 fail** | **85 / 85** | **200 / 200** | — (corpus untouched; no frozen `src/` changed) | **2** |
| 2a — HIR build+resolve (incl. `yelc-sema`) | | | ≥ prev | 85 / 85 | ≥ prev | 0 | ≤ prev |
| 2b — HIR check | | | ≥ prev | 85 / 85 | ≥ prev | 0 | ≤ prev |
| 3a — LIR data model | | | ≥ prev | 85 / 85 | ≥ prev | 0 | ≤ prev |
| 3b — LIR lowering | | | ≥ prev | 85 / 85 | ≥ prev | 0 | ≤ prev |
| 4 — codegen | | | ≥ prev | 85 / 85 | ≥ prev | 0 | ≤ prev |

**Numbers are contiguous.** HIR and THIR merged into one IR with two phases on
2026-07-28 ([`seam-changes.md`](seam-changes.md)), and the remaining stages were
renumbered rather than left with a gap. Row labels here match
[`README.md`](README.md) § Status and the `stage-N-*.md` files exactly —
`2a`/`2b` are `yelc-hir`, `3a` is `yelc-lir`, `3b` is `yelc-lower`.

**`yelc-sema` lost its own row on 2026-07-29**, when it became phase 1 of stage
2a rather than a separate landing. Its measurement did not disappear with the
row: the builtin `Definitions` table is comparable against the frozen one before
any source is parsed, and 2a owes that comparison in its Numbers. Worth naming
the cost — a line in a stage's Numbers is weaker than a row that cannot be passed
silently ([A19](anti-spec.md#a19--a-number-is-produced-by-a-command)).

**Stage 2's workspace count is flat at 480 on purpose.** `yelc-driver` adds no
tests, because nothing in `tests/` may assert on a dump's text — the moment
something does, the driver becomes a thing that must not change, and it is
supposed to be the cheap-to-change one
([`stage-2-driver.md`](stage-2-driver.md)). Its verification is the corpus run
recorded there: 2000 / 2000 programs round-tripped byte-identically through the
shipping binary, 0 driver failures. That is the first check of invariant S1 from
*outside* `yelc-syntax`.

**The 2026-07-29 re-freeze is the one kind that costs nothing.** `1d12250`
changed *fixture data only* — `global_filter_default.yel` moved from `positive/`
to `known_bugs/` ([`goldens-changed.md`](goldens-changed.md)). No file under any
frozen `src/` or `Cargo.*` changed, checked with `git status --porcelain`, so the
frozen binary is byte-identical and the corpus **cannot** have moved. This is
weaker evidence than `3ef3568`'s (which regenerated 8000 digests and compared
them) and it is weaker on purpose: there is nothing to compare when the compiler
is the same bytes. The claim to verify was "no source changed", and that is what
was verified.

Two counts moved and are recorded here so a later reader is not surprised:
**positive fixtures 91 → 90**, **known_bugs 3 → 4**. The workspace test count did
not move — `positive_fixtures` and `known_bugs_fixtures` are one test each,
looping over a directory.

**A row is per landing, not per crate.** 2a and 2b live in one crate but ratchet
separately, because each lands on its own measured number and the whole point is
that the number never goes down between them. Same for 3a/3b.

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

## A third baseline is owed (2026-07-28, keyword word boundary)

**The freeze point moved again, and the corrective row is not yet measured.**
`grammar.pest` gained a keyword word boundary — a **surface language change** on
the frozen tree, and therefore ordinary shipping work rather than rewrite work,
the same allowance `c51b51d` used. See
[`goldens-changed.md`](goldens-changed.md) and
[directions §7](directions.md#7--keywords-get-a-word-boundary--at-cutover-by-deletion).

What is already established, and what is not:

- **The corpus does not need regenerating.** It was regenerated as the gate on
  the change and all 8000 artifacts came back byte-identical to the committed
  digests, so `corpus/` still describes the compiler that exists. This is the one
  case where a moved freeze point costs nothing: the old artifacts are not stale,
  they are *provably* the same artifacts.
- **What is owed is a corrective baseline row** — the `315 / 0 / 2` /
  `85 / 85` / `200 / 200` sweep re-measured beside the new freeze SHA with
  `--exclude yelc-syntax --exclude yelc-base`, so stage 1's numbers stay
  comparable across the move exactly as they did for `c51b51d`. The whole-
  workspace numbers *were* measured (480 / 0 / 2, 85 / 85, 200 / 200 — unchanged
  from the stage-1 row); the frozen-only projection of them was not.

Do not regenerate the committed corpus on the strength of this note. The digests
proved neutral; regenerating anyway would replace a verified artifact set with an
unverified one for no gain.

## Why there are two baselines

The freeze point moved. `c51b51d` renamed `import component` to
`extern component` — a **surface language change** on the frozen tree, and
therefore ordinary shipping work rather than rewrite work
([`greenfield-never-touch-old-code`](../../.agents/skills/compiler-rewrite/rules/greenfield-never-touch-old-code.md)
allows exactly this, and says what it costs).

What it cost, done rather than promised:

1. The corpus was regenerated from `c51b51d`. The `ccf2086` artifacts described a
   compiler that no longer exists, so keeping them would have meant diffing
   against a moving target — the one thing the freeze exists to prevent.
2. This corrective baseline row, so stage 1's numbers are compared against the
   compiler it actually ran beside.

The two baselines are **identical on every column** — 315 / 0 / 2, 85/85, 200/200
— measured with `cargo test --workspace --exclude yelc-syntax --exclude yelc-base`.
That is the evidence the rename was behaviour-neutral for everything except the
keyword itself, and it is why stage 1's numbers stay comparable across the move.

(The first draft of this row said 395, reasoned from arithmetic rather than
measured. That is the A19 violation this file exists to prevent, caught before it
landed. Every number here comes from the command named beside it.)

## Why there are three baselines

The freeze point moved twice, both times for **shipping work on the frozen
tree**, which `greenfield-never-touch-old-code` permits and prices.

| SHA | change |
|---|---|
| `ccf2086` | original freeze |
| `c51b51d` | `import component` → `extern component` |
| `3ef3568` | the kebab lookahead and the keyword word boundary |

**All three baselines are identical on every column** — 315 / 0 / 2, 85/85,
200/200 — measured each time with
`cargo test --workspace --exclude yelc-syntax --exclude yelc-base`. That is the
evidence each change was behaviour-neutral apart from the construct it targeted,
and it is why stage 1's numbers stay comparable across both moves.

**The corpus was regenerated for `c51b51d` and deliberately NOT for `3ef3568`.**
The keyword/hyphen change was proved neutral a stronger way: the corpus was
regenerated into a scratch directory and all **8000 digests came back
byte-identical**, confirmed independently by compiling all 2000 sources through
both binaries side by side (0 WIT, 0 DOT, 0 WASM moved). Regenerating in place
would have rewritten 209 MB of git-lfs objects to identical content. The
committed corpus is still a faithful oracle for `3ef3568`; `corpus/MANIFEST`
names `33e5c71` because that is the tree its binary was built from, and the
frozen half is unchanged between the two.

## Rules

- **Zero unexplained corpus divergences.** Explained ones are enumerated in the
  stage file with a reason each, and cross-linked from `goldens-changed.md`.
- **Going down requires an explicit written decision** by the rewrite owner,
  recorded as its own row with the justification. It should happen approximately
  never; the ceremony is the point.
- **Execution stays 85/85 exactly.** Not "≥ 85" in spirit — the same 85 tests,
  unmodified. Adding tests is fine; the 85 must all still pass as written.
