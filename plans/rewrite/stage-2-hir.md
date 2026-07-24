# Stage 2 — `yelc-hir`                                 status: not started

Replaces (frozen, never edited): `crates/yel-core/src/hir/`
Base: — · Started: — · Landed: —

> **Stub.** Written out fully before the stage is briefed, and not before —
> stage 1's Surprises section is an input to this brief.

## Brief

*To be written.* Port `arkc-hir`'s structure to yel's item vocabulary.

Must honour:

- **Register-then-lower ordering.** All items registered before any body lowers,
  so forward references resolve. This is invariant H4 in the seam contract and
  the reason the old lowering worked at all.
- **Bidirectional `HirId ↔ NodeId` map** (ark's `hir_map.rs`). This is what lets
  a HIR-level diagnostic point back at source, and what the LSP needs.
- **Side tables, not fattened nodes** (ark's `NodeMap<V>` keyed by `HirId`, with
  `assert!(old.is_none())` on insert). Analysis results live beside the IR —
  [anti-spec B3](anti-spec.md#b3--no-analysis-result-stored-on-the-node-it-describes).
  `CompilerContext::signal_deps` keyed by `DefId` is the existing positive
  precedent and stays.
- **`ParsedType`-style unresolved types** — a lazily-filled cell, so name
  resolution can run after construction without a second tree. **Not** interning
  an unresolved named type as `Unknown` and hoping something overwrites it —
  [anti-spec B2](anti-spec.md#b2--no-deferred-name-resolution-encoded-as-a-lie).
  The frozen tree does exactly that at `types/interner.rs:331` and
  `hir/lower.rs:1068`.
- **Bodies separated from items by id** (ark's `Module { node_types, bodies, elements }`).
- One walker: `hir/visit.rs`, exhaustive, no `_` arm.
- Globals and components lower through **one uniform item spine** —
  [anti-spec D1](anti-spec.md#d1--the-compilation-unit-is-the-file-not-the-component).
  `HirItem` is already a real `{Component, Global}` enum with a symmetric
  accessor set in the frozen tree; that shape is correct and carries over.

## Contract

*To be landed on `main` before briefing.* Input: `yelc_syntax::ParsedFile`.
Output: `HirModule` + the `HirId ↔ NodeId` map. Invariants H1–H5.

## Reference

`ark/compiler/arkc-hir/src/{hir_map.rs, parsety.rs, hir/hir_id.rs, hir/hir_node.rs, hir/module.rs, hir/visit.rs}`;
frozen `yel-core/src/hir/lower.rs` (1.4k — also an [anti-spec A2](anti-spec.md#a2--no-god-pass) case).

## Definition of done · Numbers · Decision log · Surprises

*To be written.*
