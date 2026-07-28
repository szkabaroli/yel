# The compiler internals rewrite — status board

> **Entry point.** Every agent brief links here first. Kept to one screen; detail
> lives in the stage files.
> Method: [`/compiler-rewrite`](../../.agents/skills/compiler-rewrite/SKILL.md)

Rewriting yel's compiler internals. The **surface language** (`LANGUAGE.md`) and
the **stage decomposition** (AST → HIR → THIR → LIR → WASM) are kept; every
internal data structure, pass, and helper is replaced.

## The three invariants

1. **The existing compiler is never edited.** `crates/{yel-core, yel-wasm-codegen, yelc}`
   are frozen, read-only reference: the differential baseline and the shipping
   product. The rewrite grows in new crates beside it.
2. **Exactly one stage is in flight at a time.** Stage N+1 is not briefed until
   stage N is closed out to its written definition of done.
3. **The conformance number never goes down.** Every stage lands on a measured
   number ≥ [`ratchet.md`](ratchet.md), or it does not land.

## Status

| # | Crate | Replaces (frozen) | Status | Agent | Landed |
|---|-------|-------------------|--------|-------|--------|
| 0 | — | — | ✅ **done** | orchestrator | 2026-07-24 |
| 1 | `yelc-syntax` | `yel-core/src/syntax/` | ✅ **landed** | agent + integrator | 2026-07-28 |
| 2 | `yelc-hir` | `yel-core/src/hir/` | 📝 brief written, blocked on 1 | — | — |
| 3 | `yelc-thir` | `yel-core/src/thir/` | ⬜ blocked on 2 | — | — |
| 4a | `yelc-lir` | `yel-core/src/lir/` | ⬜ blocked on 3 | — | — |
| 4b | `yelc-lower` | `yel-core/src/lower_to_lir/` | ⬜ blocked on 4a | — | — |
| 5 | `yelc-codegen` | `yel-wasm-codegen/` | ⬜ blocked on 4b | — | — |

Cutover phase: **1 — coexist**. Phase 4 (deletion) is a named task, scheduled
now: [`stage-5-codegen.md` § Final deletion](stage-5-codegen.md#final-deletion--cutover-phase-4).

## Crate layout

Named after the ark convention (`arkc-parser`, `arkc-hir`) — the reference is by
the same author. These are the **permanent** names; there is no rename at
cutover, because a `2` suffix is exactly the kind of transitional marker that
outlives its transition ([anti-spec A4](anti-spec.md#a4--no-permanent-bridge)).

```
crates/
  yelc-base/      diagnostics, SourceMap/Span, Interner/Name, ids, IndexVec   [keep-list]
  yelc-syntax/    lexer, green tree, AST, parser                      stage 1
  yelc-sema/      Ty interner, Definitions, CompilerContext, known/stdlib
  yelc-hir/       HIR + AST→HIR lowering                              stage 2
  yelc-thir/      THIR + typeck                                       stage 3
  yelc-lir/       LIR data model + arena traits + generic passes      stage 4a
  yelc-lower/     THIR → LIR lowering                                 stage 4b
  yelc-codegen/   LIR → WASM / WIT / DOT                              stage 5
  yelc-driver/    stage selection; binary `yelc2`, becomes `yelc` at flip
```

### The dependency graph is a load-bearing constraint

```
  base   ←  syntax  ←  hir  ←  thir  ←  lower
  base   ←  sema    ←  hir, thir, lower
  base   ←  lir     ←  lower, codegen

  yelc-codegen depends on { yelc-lir, yelc-base }   — and nothing else
  yelc-lir     depends on { yelc-base }             — and nothing else
```

`yelc-lir` and `yelc-codegen` have **no dependency path to any frontend crate**.
`use yelc_sema::Definitions` below the seam is a hard cargo error, not a
reviewer's judgement call — which is how
[anti-spec C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam)
stops being a matter of vigilance.

This is also why stage 4 is split: the LIR data model must not see frontend
vocabulary, while THIR→LIR lowering legitimately needs it. They run **in
sequence** — 4a then 4b — never together.

## The documents

| File | What it is |
|---|---|
| [`scope.md`](scope.md) | Frozen vs. free. **First thing in every brief.** |
| [`anti-spec.md`](anti-spec.md) | Shapes the rewrite may not reproduce. Append-only. |
| [`keep-list.md`](keep-list.md) | What carries over intact. May not be replaced. |
| [`directions.md`](directions.md) | Shapes we'd *like* to reach. Recorded intent, **not contract** — binding only once copied into a stage brief. Append-only. |
| [`ratchet.md`](ratchet.md) | Measured numbers per stage. Append-only, never edited. |
| [`corpus.md`](corpus.md) | The 2000-seed oracle: provenance, layout, how to sweep. |
| [`seam-changes.md`](seam-changes.md) | Contract-change log: request, options, decision, date. |
| [`goldens-changed.md`](goldens-changed.md) | Every re-blessed golden, one line, with justification. |
| `stage-N-*.md` | Per stage: brief, contract, definition of done, numbers, decisions, surprises. |

**This directory is the rewrite's architecture.** `docs/ARCHITECTURE.md` and
`docs/PIPELINE.md` describe the **frozen** compiler — read them as a description
of what the old code does, the same way you read the old source. They do not
constrain the new design, and they are rewritten at cutover rather than honoured.
`docs/TECH_DEBT.md` is different: it is the *input* to
[`anti-spec.md`](anti-spec.md), and it is accurate.

## Baseline (freeze `c51b51d`, 2026-07-28)

The freeze point **moved**: `c51b51d` renamed `import component` to
`extern component` on the frozen tree. Corpus regenerated, corrective baseline
row added — see [`ratchet.md`](ratchet.md) § Why there are two baselines.

| | baseline `c51b51d` | stage 1 `33e5c71` |
|---|---|---|
| workspace tests | **315 / 0 / 2 ignored** | **480 / 0 / 2** |
| execution tests | **85 / 85** — the only semantic oracle, never modified | **85 / 85** |
| fuzz 200 seeds | **200 / 200** | **200 / 200** |
| corpus | **2000 / 2000**; `known-failures.txt` **empty** | **0 divergences** |

The fuzz metric is **saturated** and cannot improve. Regressions show up as
**corpus divergences**; fixes show up as `known_bugs` promotions. See
[`ratchet.md`](ratchet.md).

## The loop, per stage

**freeze → contract → build → differentially verify → review → close out.**

1. Write the stage file **before** the stage starts — a file filled in at the end
   is a report, not a brief.
2. Land the seam types on `main` first. Agents implement against them and never
   invent them; a needed change is a request logged in `seam-changes.md`.
3. One agent owns the stage end-to-end. Concurrent agents *within* a stage get
   their own worktree; the integrator merges.
4. Verify by artifact-level differential over the corpus — review cannot clear a
   rewrite. On a divergence, switch to
   [`/fuzzer-debugging`](../../.agents/skills/fuzzer-debugging/SKILL.md).
5. Adversarial review panel, read-only, one lens each.
6. Close out: fill in Numbers, Decision log, and **Surprises**.

## Freeze check — run before reading any stage diff

```bash
scripts/freeze-check.sh              # working tree vs HEAD
scripts/freeze-check.sh <base-ref>   # also catches commits since <base-ref>
```

**Use the script, never an inline `git status … | wc -l`.** The obvious one-liner
is cwd-dependent and **fails open**: run from `crates/`, the pathspec
`crates/yel-core` resolves to `crates/crates/yel-core`, matches nothing, and
prints `0` — identical to "clean". git warns; `| wc -l` discards the warning.
That is a count-based assertion that passes vacuously
([A8](anti-spec.md#a8--an-invariant-is-asserted-not-observed) /
[A14](anti-spec.md#a14--test-inputs-are-verified-present-not-merely-counted)),
and it went undetected for the whole of stage 1 because the number it printed was
the number expected. The script anchors to the repo root and treats a missing
frozen path as fatal.

## Clone setup

The corpus is tracked via git-lfs. `git lfs install` before cloning, or
`git lfs pull` after.
