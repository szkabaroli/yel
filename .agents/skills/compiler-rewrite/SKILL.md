---
name: compiler-rewrite
description: >
  The method for rewriting the yel compiler's internals with subagents while
  keeping the surface language and the AST→HIR→THIR→LIR→WASM stage boundaries
  intact. The existing compiler is frozen read-only reference; the rewrite grows
  in new crates beside it, strictly one stage at a time, each handed to a
  subagent as a written contract plus an anti-spec, verified differentially
  against the old binary over thousands of yel-smith seeds, and gated on a
  review panel and a pass-rate ratchet that never goes down. Progress lives in
  versioned markdown. Carries the frontend design from the szkabaroli/ark
  reference and keeps yel's existing diagnostics infrastructure. A workflow for
  orchestration and verification — not a design document for any particular IR.
  Use when planning or running the rewrite, when scoping a stage for a subagent,
  or when deciding whether a rewritten stage is allowed to land. Invoke with
  /compiler-rewrite.
license: Apache-2.0
metadata:
  author: yel
  version: "1.1.0"
  sources:
    - docs/ARCHITECTURE.md (stage boundaries that survive the rewrite)
    - docs/TECH_DEBT.md (the anti-spec — what must not be reproduced)
    - crates/yel-wasm-codegen/tests (the conformance corpus that becomes the oracle)
    - crates/yel-smith (the differential engine)
    - "github.com/szkabaroli/ark — arkc-parser / arkc-hir (frontend reference design)"
---

# Orchestrating the Yel Compiler Rewrite

A ~93k-line compiler is being rewritten internally. The **surface language**
(`LANGUAGE.md`) and the **stage decomposition** (`AST → HIR → THIR → LIR →
WASM`) are kept; every internal data structure, pass, and helper is up for
replacement. The work is fanned out to subagents.

That combination — big blast radius, parallel authors, an existing product that
must keep working — is what this skill is about. A compiler rewrite does not
fail because agents write bad Rust. It fails in three specific ways:

1. **The oracle rots.** Goldens get re-blessed from the new compiler, so the
   tests pass and the output is wrong.
2. **The seams don't meet.** Two agents each invent a plausible `Ty` and neither
   composes with the other.
3. **The debt reincarnates.** Nobody wrote down what was wrong with the old
   code, so the new code rediscovers the same shape.

Everything below exists to prevent one of those three.

## The three invariants

> **1. The existing compiler is never edited.** It is frozen, read-only
> reference: the differential baseline and the shipping product. The rewrite
> grows in new crates beside it.
>
> **2. Exactly one stage is in flight at a time.** Stage N+1 is not briefed
> until stage N is complete to its written definition of done.
>
> **3. The conformance number never goes down.** Every stage lands on a measured
> number ≥ the recorded baseline, or it does not land.

There is no "rewrite branch that will be merged when it's ready." The two trees
coexist, are continuously diffed against each other, and the old one is deleted
once — at the end, after parity.

## The loop — run it once per stage, in order

**freeze → contract → build → differentially verify → review → close out.**

| Step | Rule | One line |
|------|------|----------|
| freeze | [`greenfield-never-touch-old-code`](rules/greenfield-never-touch-old-code.md) | The old tree is read-only reference; new crates go beside it, never inside it |
| freeze | [`oracle-freeze-behaviour`](rules/oracle-freeze-behaviour.md) | Capture today's artifacts as a corpus *before* any new code exists |
| freeze | [`oracle-never-rebless`](rules/oracle-never-rebless.md) | Never regenerate a golden from the new compiler — that deletes the oracle |
| freeze | [`anti-spec-from-tech-debt`](rules/anti-spec-from-tech-debt.md) | Turn `TECH_DEBT.md` into an explicit list of shapes the rewrite may not reproduce |
| contract | [`scope-frozen-vs-free`](rules/scope-frozen-vs-free.md) | Write down what the rewrite may not change (syntax, stage names, WIT) vs what it must |
| contract | [`contract-before-fanout`](rules/contract-before-fanout.md) | Types + invariants at every seam land on `main` before agents start |
| contract | [`keep-diagnostics-infrastructure`](rules/keep-diagnostics-infrastructure.md) | Ship a keep-list too — `diagnostic.rs` and friends are good and stay |
| build | [`stage-gate-sequential`](rules/stage-gate-sequential.md) | One stage at a time, to a written definition of done — no partial credit |
| build | [`frontend-follow-ark-reference`](rules/frontend-follow-ark-reference.md) | Parser + HIR are ports of `szkabaroli/ark`, not fresh designs |
| build | [`orchestrate-one-agent-one-stage`](rules/orchestrate-one-agent-one-stage.md) | One agent owns the in-flight stage end-to-end, with its own fixture slice |
| build | [`orchestrate-integrator-owns-seams`](rules/orchestrate-integrator-owns-seams.md) | Agents never edit shared types; seam changes come back as a request |
| build | [`orchestrate-worktree-isolation`](rules/orchestrate-worktree-isolation.md) | Concurrent agents *within* a stage get their own worktree; the integrator merges |
| verify | [`verify-differential-not-review`](rules/verify-differential-not-review.md) | Diff old vs new *artifacts* over thousands of yel-smith seeds; review can't clear a rewrite |
| verify | [`verify-ratchet-never-down`](rules/verify-ratchet-never-down.md) | Land only on a measured number ≥ the recorded baseline |
| review | [`review-adversarial-panel`](rules/review-adversarial-panel.md) | Read-only reviewers, one lens each — they catch what the differential can't |
| close out | [`track-progress-in-markdown`](rules/track-progress-in-markdown.md) | `plans/rewrite/` is the project's memory; agents have none |
| cut over | [`cutover-switch-then-delete`](rules/cutover-switch-then-delete.md) | Coexist → parity → flip → **delete**, with phase 4 scheduled before phase 1 merges |

## Stage order (strictly sequential)

Follow the pipeline's own order — it is also the order in which artifact-level
differential testing becomes possible. Each row is briefed only after the row
above it is closed out.

| # | New crate | Replaces (frozen) | Must honour |
|---|-----------|-------------------|-------------|
| 1 | `yel2-syntax` | `yel-core/src/syntax/` | The `LANGUAGE.md` grammar exactly; spans preserved; lossless green tree ([ark reference](rules/frontend-follow-ark-reference.md)) |
| 2 | `yel2-hir` | `yel-core/src/hir/` | Register-then-lower ordering; `HirId ↔ NodeId` map; side tables, not fattened nodes |
| 3 | `yel2-thir` | `yel-core/src/thir/` | Bidirectional typeck; identical *meaning* on the 23 diagnostic fixtures |
| 4 | `yel2-lir` | `yel-core/src/{lir,lower_to_lir}/` | Frontend-agnostic LIR; arena traits are codegen's only entry; no UI vocabulary below the seam |
| 5 | `yel2-codegen` | `yel-wasm-codegen/` | Byte-identical WIT/DOT for the 91 positive fixtures; 85 execution tests unmodified |

Crate names are illustrative — pick them in stage 0 and record them in
`plans/rewrite/README.md`. What matters is that they are *new paths*: nothing
under the old crates is edited at any point
([`greenfield-never-touch-old-code`](rules/greenfield-never-touch-old-code.md)).

Stage 4 is the big one and the one carrying the existing north-star migration
(`docs/ARCHITECTURE.md §0`). If it will not fit in one agent's context, that is
a signal it contains an internal seam worth contracting — split it into 4a
(lowering) and 4b (LIR data model) and run them **in sequence**, not together.

## The fuzzer is the engine, not a side check

`yel-smith` already exists and already generates valid Yel. It is what makes
this rewrite verifiable at all, and it is used at three distinct points:

1. **Freeze** — generate a few thousand programs and record the current
   compiler's artifacts for each. That corpus is the oracle
   ([`oracle-freeze-behaviour`](rules/oracle-freeze-behaviour.md)).
2. **Verify** — after each stage, compile every corpus program both ways and
   diff the artifacts ([`verify-differential-not-review`](rules/verify-differential-not-review.md)).
   The 91 fixtures cover what someone thought of; the seeds cover what nobody did.
3. **Ratchet** — the 200-seed pass rate is a tracked number that may not go down
   ([`verify-ratchet-never-down`](rules/verify-ratchet-never-down.md)).

When a divergence appears, switch to [`/fuzzer-debugging`](../fuzzer-debugging/SKILL.md)
— measure, categorize, delta-minimize, locate. That skill is the debugging loop;
this one is the orchestration around it.

## The conformance corpus (today's numbers)

Measure these before touching anything and record them in the rewrite plan.
They are the oracle; the specific counts will move, the categories will not.

| Corpus | Where | What it pins |
|--------|-------|--------------|
| 91 positive fixtures | `yel-wasm-codegen/tests/fixtures/positive` | WIT + DOT byte-for-byte, WASM validates |
| 23 diagnostic fixtures | `.../fixtures/diagnostics` | Error text for programs that must be rejected |
| known-bugs fixtures | `.../fixtures/known_bugs` | Programs that *should* work and don't — the rewrite's free wins |
| 85 execution tests | `yel-wasm-codegen/tests/execution.rs` | Real DOM-op behaviour under Wasmtime — the only semantic oracle |
| insta snapshots | `yelc/tests/snapshot.rs` | CLI-level WIT/DOT |
| fuzzer seeds | `yel-smith`, 200-seed sweep | Coverage the fixtures don't have — see [`/fuzzer-debugging`](../fuzzer-debugging/SKILL.md) |

The execution tests are the ones that matter most and the ones most likely to be
quietly weakened. A rewritten stage that passes WIT snapshots but drops an
execution test has miscompiled something; the snapshot just wasn't looking.

## Commands

```bash
# Record the baseline (do this first, commit the output into the plan)
cargo test --workspace 2>&1 | tail -30
cargo test -p yel-wasm-codegen --test execution 2>&1 | tail -5

# Fuzzer baseline — clean release build or the number is last run's
touch crates/yelc/src/main.rs && cargo build --release -p yelc -p yel-smith
pass=0; for s in $(seq 1 200); do
  ./target/release/yel-smith --seed $s > /tmp/f.yel 2>/dev/null
  ./target/release/yelc compile -o wasm /tmp/f.yel > /tmp/f.wasm 2>/dev/null \
    && wasm-tools validate /tmp/f.wasm 2>/dev/null && pass=$((pass+1))
done; echo "BASELINE PASS=$pass/200"

# Differential sweep: old binary vs new binary, artifact-level
for s in $(seq 1 500); do
  ./target/release/yel-smith --seed $s > /tmp/f.yel 2>/dev/null
  ./target/release/yelc  compile -o wit /tmp/f.yel > /tmp/old.wit 2>/dev/null
  ./target/release/yelc2 compile -o wit /tmp/f.yel > /tmp/new.wit 2>/dev/null
  cmp -s /tmp/old.wit /tmp/new.wit || echo "DIVERGE seed=$s"
done

# Freeze check — run before reading any stage diff
git diff --name-only main... \
  | grep -E '^crates/(yel-core|yel-wasm-codegen|yelc)/' && echo "FREEZE VIOLATION"
```

## Stage 0 — before any code

Do these once, in order, and commit the results. Everything after depends on them.

1. Record the baseline numbers (commands above) into `plans/rewrite/ratchet.md`.
2. Generate and freeze the corpus; record known failures
   ([`oracle-freeze-behaviour`](rules/oracle-freeze-behaviour.md)).
3. Write `anti-spec.md` from `TECH_DEBT.md`, and `keep-list.md` from what is
   already good.
4. Write the frozen-vs-free table
   ([`scope-frozen-vs-free`](rules/scope-frozen-vs-free.md)).
5. Read the ark reference and decide the new crate layout.
6. Create `plans/rewrite/` with the stage files stubbed
   ([`track-progress-in-markdown`](rules/track-progress-in-markdown.md)).
7. Schedule the final deletion as a named task
   ([`cutover-switch-then-delete`](rules/cutover-switch-then-delete.md)).

## The mindset that makes this work

- **The old compiler is the specification.** It is bad code that encodes years
  of correct behaviour. Read it constantly, edit it never, and delete it only
  when something provably equivalent exists.
- **A green test suite proves nothing if you edited the tests.** The corpus is
  frozen input; the only honest signal is old-vs-new on inputs neither
  implementation was tuned for.
- **Contracts are cheaper than coordination.** An hour writing the seam types
  saves a week of merging two incompatible IRs.
- **Sequential stages are faster than parallel ones.** Not a compromise for
  safety — one stage in flight is what keeps every divergence attributable, and
  attribution is the whole reason the differential is cheap.
- **Write it down or it did not happen.** Agents are stateless; the markdown is
  the only continuity the project has.
- **The rewrite is for the code, not the behaviour.** Correctness is checked by
  the differential. Whether the result is worth having is checked by review —
  which is why the panel is a gate, not a formality.
