# Stage 4 — `yelc-lir` (4a) then `yelc-lower` (4b)     status: not started

Replaces (frozen, never edited):
`crates/yel-core/src/lir/` → **4a**, `crates/yel-core/src/lower_to_lir/` → **4b**.
Base: — · Started: — · Landed: —

> **Stub.** The largest stage, and the one carrying the frontend-agnostic-LIR
> goal. Split into two crates run **in sequence** — 4a then 4b — never together.

## Why it is split

`yelc-lir` depends only on `yelc-base`. It has **no dependency path to
`yelc-sema`, `yelc-thir`, or `yelc-hir`**, so `use yelc_sema::Definitions` inside
the LIR or the back-end is a hard cargo error.
[Anti-spec C1](anti-spec.md#c1--no-domain-vocabulary-below-the-frontend-seam) —
no `tree_shape`, `boundary`, `mount`, `component`, `signal`, `$Comp`, or
`yel:ui/dom` below the seam — stops being a matter of reviewer vigilance and
becomes a build failure.

`yelc-lower` sits above the seam, depends on `yelc-thir` + `yelc-sema` +
`yelc-lir`, and is where UI vocabulary legitimately lives. This is the same
substrate the visual flow language shares.

The split is also the honest answer to context size: if stage 4 does not fit in
one agent, that is a signal it contains an internal seam worth contracting.

## 4a — `yelc-lir`: the data model

*To be written.* Frontend-agnostic block-based IR. The arena traits
(`lir/arena.rs` in the frozen tree — `LirResourceArena`) are codegen's **only**
entry point. Generic `LirOp`s only.

Watch for:
- [C2 — one representation, chosen at the seam](anti-spec.md#c2--one-representation-chosen-at-the-seam).
  The typed-GC migration is *done* in the frozen tree (`TECH_DEBT.md` §1.5, all
  boxes ticked); the new LIR starts from the finished state and must not
  reintroduce a second value representation. Carry the `WitBoundary` witness
  mechanism ([keep-list §10](keep-list.md#10--the-witboundary-witness-stage-5)).
- [C5 — no hard-coded sizing](anti-spec.md#c5--no-hard-coded-sizing)
  (`lir/layout.rs:160,166` — user-defined variant sizes are still uncomputed).
- [C6 — no classification placeholder](anti-spec.md#c6--no-classification-placeholder)
  (`lir/block.rs:157,159` "TODO #105: classify"; `FunctionRole`'s legacy
  catch-all variant).
- Expr interning: the frozen `intern_expr` "always adds — could deduplicate
  later", so identical exprs get distinct ids. Strings *are* deduped. Decide
  deliberately; either answer is fine, an accident is not.
- A `LirVisitor`, or a documented decision that a linear arena scan is simpler
  now that every subexpression is its own arena entry
  ([anti-spec A3](anti-spec.md#a3--no-duplicated-walkers)). The frozen
  `boundary_rewrite.rs` / `dedupe.rs` op-stream walkers are the remaining
  hand-rolled pair.

## 4b — `yelc-lower`: THIR → LIR

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
- `todo!()` cliffs at unsupported for-loop iterables ("no LIR classifier") — keep
  them **loud**; do not soften to a fallback
  ([anti-spec A5](anti-spec.md#a5--no-silent-fallback)).
- Hard-coded string/signal region sizing.

`resolve_global_triggers` is a **legitimate whole-module pass, not debt** — it
must run after every component is lowered because a global's fan-out targets do
not exist until then. It is a link step. It lives inside the shared module
spine, and the new design keeps that property.

## Contract · Reference · Definition of done · Numbers · Decision log · Surprises

*To be written.*
