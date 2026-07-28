# Stage 3 — `yelc-lir` (3a) then `yelc-lower` (3b)     status: not started

Replaces (frozen, never edited):
`crates/yel-core/src/lir/` → **3a**, `crates/yel-core/src/lower_to_lir/` → **3b**.
Base: — · Started: — · Landed: —

> **Stub.** The largest stage, and the one carrying the frontend-agnostic-LIR
> goal. Split into two crates run **in sequence** — 3a then 3b — never together.

## Why it is split

`yelc-lir` depends only on `yelc-base`. It has **no dependency path to
`yelc-sema` or `yelc-hir`**, so `use yelc_sema::Definitions` inside
the LIR or the back-end is a hard cargo error.
[Anti-spec C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam) —
no `tree_shape`, `boundary`, `mount`, `component`, `signal`, `$Comp`, or
`yel:ui/dom` below the seam — stops being a matter of reviewer vigilance and
becomes a build failure.

`yelc-lower` sits above the seam, depends on `yelc-hir` + `yelc-sema` +
`yelc-lir`, and is where UI vocabulary legitimately lives. This is the same
substrate the visual flow language shares.

The split is also the honest answer to context size: if stage 3 does not fit in
one agent, that is a signal it contains an internal seam worth contracting.

## 3a — `yelc-lir`: the data model

*To be written.* Frontend-agnostic block-based IR. The arena traits
(`lir/arena.rs` in the frozen tree — `LirResourceArena`) are codegen's **only**
entry point. Generic `LirOp`s only.

Watch for:
- [C2 — one representation, chosen at the seam](anti-spec.md#c2--one-representation-chosen-at-the-seam).
  The typed-GC migration is *done* in the frozen tree (`TECH_DEBT.md` §1.5, all
  boxes ticked); the new LIR starts from the finished state and must not
  reintroduce a second value representation. Carry the `WitBoundary` witness
  mechanism ([keep-list §10](keep-list.md#10--the-witboundary-witness-stage-4)).
- [C5 — no hard-coded sizing](anti-spec.md#c5--no-hard-coded-sizing)
  (`lir/layout.rs:160,166` — user-defined variant sizes are still uncomputed).
- [C6 — no classification placeholder](anti-spec.md#c6--no-classification-placeholder)
  (`lir/block.rs:157,159` "TODO #105: classify"; `FunctionRole`'s legacy
  catch-all variant).
- Expr interning: the frozen `intern_expr` "always adds — could deduplicate
  later", so identical exprs get distinct ids. Strings *are* deduped. Decide
  deliberately; either answer is fine, an accident is not.
- **Does LIR have a function value?**
  [directions §4](directions.md#4--closures-are-a-value-and-the-irs-are-shaped-for-one).
  Answer it on generic grounds — the flow frontend wants callable values too. A
  closure representation admitted for `filter`'s sake is UI vocabulary below the
  seam ([C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam)); a
  general function value that `filter` happens to use is substrate. The frozen
  `LirExprKind::Closure { params, body }` carries **no environment**, and codegen
  panics on a captured local — so there is no output to preserve either way.
- A `LirVisitor`, or a documented decision that a linear arena scan is simpler
  now that every subexpression is its own arena entry
  ([anti-spec A3](anti-spec.md#a3--no-duplicated-walkers)). The frozen
  `boundary_rewrite.rs` / `dedupe.rs` op-stream walkers are the remaining
  hand-rolled pair.

## 3b — `yelc-lower`: HIR → LIR

*To be written.* Replaces `lower_to_lir/blocks.rs` — **8,500 lines, one struct,
50+ fields**: output vectors, monotonic counters (`next_slot`, `next_block`,
`next_memory_offset`), `current_ops` + `ops_stack`, for-loop stacks,
deferred-body queues, signal layout, all mutating together.

This file is the single strongest reason the rewrite exists, and it is the
canonical [anti-spec A2](anti-spec.md#a2--no-god-pass) case. It was not written
by someone aiming for 8,500 lines; it grew one locally-reasonable decision at a
time. **An agent under the same local pressures will make the same decisions**
unless the brief forbids the shape explicitly.

Decomposition target — split by concern, the way `wasm/codegen/` already is:
slots · signals · control flow · deferred bodies. No pass over ~800 lines
without a written justification in this file.

Specific shortcuts inside not to reproduce:
- **Deferred emission** via `pending_block_id_override` — block ids pre-allocated
  and stashed so deferred handler/derived bodies can reference a block before it
  is emitted. Correct, but a subtle ordering dependency that must be modelled
  explicitly rather than re-derived.
- **Three deferred-body mechanisms where there is one concept** —
  `DeferredHandlerBody`, `DeferredDerivedBody`, and the inlined filter predicate.
  `DeferredHandlerBody`'s six env-snapshot fields *are* capture analysis, done
  here because the frontend declined to do it. See
  [directions §5](directions.md#5--handlers-and-closures-are-one-concept-split-by-trigger):
  one body node, one capture analysis, one lowering, trigger as a field. It emits
  the same blocks, so it changes no output — but the capture set must cover all
  six fields or it is a narrowing that presents as for-loop handlers breaking.
- `todo!()` cliffs at unsupported for-loop iterables ("no LIR classifier") — keep
  them **loud**; do not soften to a fallback
  ([anti-spec A5](anti-spec.md#a5--no-silent-fallback)).
- Hard-coded string/signal region sizing.

Open direction, to accept or reject when 3b is briefed:
[directions §1 — builtins are a table, not a field per builtin](directions.md#1--builtins-are-a-table-not-a-field-per-builtin).
It pairs with stage 2b — the lowering target and the type scheme come from one
row — so if 2b rejects it, 3b inherits the rejection rather than adopting half
of it. Whichever way it goes, it must not create a second path by which UI
names reach `yelc-lir` ([C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam)).

`resolve_global_triggers` is a **legitimate whole-module pass, not debt** — it
must run after every component is lowered because a global's fan-out targets do
not exist until then. It is a link step. It lives inside the shared module
spine, and the new design keeps that property.

## Contract · Reference · Definition of done · Numbers · Decision log · Surprises

*To be written.*
