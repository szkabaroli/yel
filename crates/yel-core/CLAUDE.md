# CLAUDE.md - yel-core

The compiler **front-end and all IRs**: `parse → HIR → THIR → LIR`. No target
code lives here (that's `yel-wasm-codegen`). Full picture in
[`docs/ARCHITECTURE.md`](../../docs/ARCHITECTURE.md); known hacks in
[`docs/TECH_DEBT.md`](../../docs/TECH_DEBT.md); patterns as a skill at
[`/compiler-skills`](../../.agents/skills/compiler-skills/SKILL.md).

## Module map

| Area | Files | What it is |
|------|-------|-----------|
| Driver | `compiler.rs` (`Compiler`), `context.rs` (`CompilerContext`, the `TyCtxt`-style shared state) | Phase methods + global state threaded through every phase |
| Shared state | `interner.rs` (`Name`), `types/` (`Ty`, `TypeInterner`, `InternedTyKind`), `definitions.rs` (`Definitions`, `Namespace`), `source.rs` (`Span`, `SourceMap`), `diagnostic.rs`, `known.rs` + `stdlib_lookup.rs` (builtins), `dom_imports.rs` | Interners, def tables, diagnostics, builtins |
| IDs/indices | `ids.rs` (`DefId`, `ExprId`, `NodeId`, `BlockId`, `ForId`, `IfId`, `TreeBoundaryId`, …), `index_vec.rs` (`IndexVec<I,T>`) | Typed `u32` newtype indices + typed arenas |
| AST | `syntax/` — `parser.rs` (pest, ~3.3k lines), `ast.rs` | Parse tree; names are strings, `Named` types unresolved |
| HIR | `hir/` — `lower.rs` (register-then-lower), `node.rs`, `expr.rs`, `local_scope.rs` | Tree IR; ids assigned; names still strings; no types |
| THIR | `thir/` — `typeck.rs` (bidirectional, ~2.8k lines), `signalck.rs` (reactivity analysis), `node.rs`, `expr.rs` | Typed tree; names resolved; `span→Ty` map for LSP |
| LIR | `lir/` — `module.rs` (`LirModule`), `node.rs` (`LirResource`), `block.rs` (`LirOp`, `LirTypeRef`), `expr.rs`, `arena.rs` (read traits), `signal*.rs`, `tree_shape.rs`, `layout.rs`, `struct_types.rs`, `boundary_rewrite.rs`, `dedupe.rs` | Block-based IR; interned exprs/strings; GC + memory layout |
| THIR→LIR | `lower_to_lir/` — `component.rs` (tree stage), `blocks.rs` (block stage, **~8.5k lines**), `signals_inline.rs`, `lifecycle_inline.rs` | Two-stage lowering; kept out of `lir/` so `lir/` has no THIR dep |

Public API re-exports are in `lib.rs`.

## Conventions specific to this crate

- **Bidirectional typeck**: `typeck.rs` dispatches on a `Mode` (`Infer`/`Check(Ty)`); recover from type errors with `Ty::ERROR` and keep traversing. Numeric literals stay polymorphic until the expected type is known.
- **Register-then-lower**: `hir/lower.rs` registers all defs (types, components, elements, globals) before lowering any body, so forward references resolve.
- **Two-stage LIR**: `component.rs` builds a tree (`TreeLirResource`, discovers signals/effects), then `blocks.rs` flattens to blocks + allocates/interns. Strings dedup; **exprs do not yet** (`blocks.rs` `intern_expr`).
- **Stable correlation ids** (`ForId`/`IfId`/`TreeBoundaryId`) must stay stable across tree→block lowering — later passes key off them.
- **Determinism**: sort+dedup dependency lists before use (e.g. `collect_dependencies`); output must be byte-stable for snapshots.
- **In-progress migrations** (expect mixed naming): typed `SlotId` ladder (`legacy_u32()` bridge), `LirComponent`→`LirResource` + `tree_shape` flatten. See `docs/TECH_DEBT.md` §1 and `plans/lir-resource-flatten.md`.

## No Silent Fallbacks

Never emit dummy/placeholder values (e.g. `Literal(Bool(false))`) as a fallback for unimplemented features during lowering. These cause silent type-incorrect IR where the expression kind doesn't match its declared type — leading to broken WASM that is extremely hard to diagnose.

```rust
// ❌ BAD — silent placeholder hides the missing feature
ThirExprKind::GlobalRead { .. } => {
    LirExprKind::Literal(LirLiteral::Bool(false))  // type says string, emits bool
}

// ✅ GOOD — fails loudly at compile time
ThirExprKind::GlobalRead { global, field, .. } => {
    todo!("GlobalRead not yet lowered to LIR: global={:?}, field={:?}", global, field)
}
```

Use `todo!()` with a descriptive message so unimplemented paths crash the compiler immediately with a clear location, instead of producing subtly broken output that takes hours to trace from hex dumps.
