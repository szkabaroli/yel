# Stage 5 — `yelc-lir`, the data model              status: not started

Replaces (frozen, never edited): `crates/yel-core/src/lir/`.
Phase **5**; phase 6 is [`stage-6-lower.md`](stage-6-lower.md). Two crates,
run **in sequence** — 3 then 4 — never together.

Base: — · Started: — · Landed: —

> **Stub.** Written out fully before the phase is briefed.

## Why stages 5 and 6 are separate crates

`yelc-lir` depends only on `yelc-base`. It has **no dependency path to
`yelc-sema` or `yelc-hir`**, so `use yelc_sema::Definitions` inside the LIR or
the back-end is a hard cargo error.
[C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam) — no
`tree_shape`, `boundary`, `mount`, `component`, `signal`, `$Comp`, or
`yel:ui/dom` below the seam — stops being a matter of reviewer vigilance and
becomes a build failure.

[`yelc-lower`](stage-6-lower.md) sits above the seam, depends on `yelc-hir` +
`yelc-sema` + `yelc-lir`, and is where UI vocabulary legitimately lives. This is
the same substrate the visual flow language shares.

The split is also the honest answer to context size: if the LIR stage does not
fit in one agent, that is a signal it contains an internal seam worth
contracting.

## Brief

*To be written.* Frontend-agnostic block-based IR. The arena traits
(`lir/arena.rs` in the frozen tree — `LirResourceArena`) are codegen's **only**
entry point. Generic `LirOp`s only.

Watch for:

- [C2 — one representation, chosen at the seam](anti-spec.md#c2--one-representation-chosen-at-the-seam).
  The typed-GC migration is *done* in the frozen tree (`TECH_DEBT.md` §1.5, all
  boxes ticked); the new LIR starts from the finished state and must not
  reintroduce a second value representation. Carry the `WitBoundary` witness
  mechanism ([keep-list §10](keep-list.md#10--the-witboundary-witness-stage-7)).
- [C5 — no hard-coded sizing](anti-spec.md#c5--no-hard-coded-sizing)
  (`lir/layout.rs:160,166` — user-defined variant sizes are still uncomputed).
- [C6 — no classification placeholder](anti-spec.md#c6--no-classification-placeholder)
  (`lir/block.rs:157,159` "TODO #105: classify"; `FunctionRole`'s legacy
  catch-all variant).
- **Expr interning.** The frozen `intern_expr` "always adds — could deduplicate
  later", so identical exprs get distinct ids. Strings *are* deduped. Decide
  deliberately; either answer is fine, an accident is not.
- **Does LIR have a function value?**
  [directions §4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one).
  Answer it on **generic** grounds — the flow frontend wants callable values too.
  A closure representation admitted for `filter`'s sake is UI vocabulary below
  the seam ([C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam));
  a general function value that `filter` happens to use is substrate. The frozen
  `LirExprKind::Closure { params, body }` carries **no environment** and codegen
  panics on a captured local ([F6](findings.md#f6)) — so there is no output to
  preserve either way.

  §4's recommendation is **no funcrefs**: every function-valued position in yel
  resolves statically, and the canonical ABI has no function-reference type.
  Its anti-foreclosure requirement lands here — the frozen
  `Call { func: DefId, args }` bakes "callee is a known definition" into the op
  and every consumer reads the `DefId` directly. Resolve the callee through
  **one** place so an indirect case is one edit, *not* by adding a dead
  `Indirect` variant ([A9](anti-spec.md#a9--a-ported-construct-is-load-bearing-or-it-is-deleted)).
- **A `LirVisitor`**, or a documented decision that a linear arena scan is
  simpler now that every subexpression is its own arena entry
  ([A3](anti-spec.md#a3--no-duplicated-walkers)). The frozen
  `boundary_rewrite.rs` / `dedupe.rs` op-stream walkers are the remaining
  hand-rolled pair.
- **`else_if_branches` does not exist here.** 3 flattens `else if` into nested
  `If` ([D7](stage-3-hir-build.md#d7--flatten-else-if-chains)), so the LIR `If`
  drops its third field — the shape is removed from all three IRs
  ([F11](findings.md#f11)). Reconstructing the flat selector is
  [6](stage-6-lower.md)'s job, not a data-model concern.

## Contract · Reference · Definition of done · Numbers · Decision log · Surprises

*To be written.*
