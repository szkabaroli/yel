# CLAUDE.md - yel-wasm-codegen

The **back-end**: consumes `yel-core` LIR (through the `lir/arena.rs` read
traits, never a concrete type) and emits a WASM component, its WIT, and a DOT
debug graph. Full picture in [`docs/ARCHITECTURE.md`](../../docs/ARCHITECTURE.md) §5;
known shortcuts in [`docs/TECH_DEBT.md`](../../docs/TECH_DEBT.md) §4.

> **North star:** this crate is being generalized from a UI-specific back-end
> into one **shared by Yel and the flow language**. Depend only on the arena
> traits and generic `LirOp`s; don't add UI assumptions (`tree_shape`,
> `boundary`/`mount`, `$Comp` self-ref, `yel:ui/dom`). The already-generic entry
> is `generate_function_module`. See [`docs/ARCHITECTURE.md` §0](../../docs/ARCHITECTURE.md).

## Public API (`lib.rs`)

- `generate_wasm` / `generate_wasm_module` / `generate_wasm_with_wit` — component bytes.
- `generate_component` / `generate_function_module` — function-module path (used by the flow frontend).
- `generate_wit` (`WitOptions`) — the WIT world/interfaces.
- `generate_dot` (`DotOptions`) — Graphviz of the reactive dependency graph (debug/snapshots).
- Errors: typed `CodegenError` (`UnsupportedType`/`Expr`, `MissingDefinition`, `InvalidIR`, `LayoutMissing`, …). Never a silent fallback (see below).

## Module map

| Area | Files | What it is |
|------|-------|-----------|
| Encoder | `wasm/codegen/` — `build.rs` (type-section + fn-type interning), `op_emit.rs`, `block_fn.rs`, `dispatch.rs`, `signal_emit.rs`, `accessors.rs`, `record_list.rs`, `scratch.rs`, `name_section.rs`, `function_type.rs`, `constants.rs` | LIR-op → WASM instructions, split by concern |
| Types/repr | `wasm/gc_types.rs` (WASM-GC struct/array types from tree-shape boundaries), `wasm/repr.rs` (`InternalRepr`: scalar vs flat/GC), `wasm/expr.rs`, `wasm/functions.rs` | Value representation + emission entry |
| Runtime | `wasm/runtime/memory.rs` (linear-memory layout), `wasm/runtime/strings.rs` (`(ptr,len)` strings) | Memory model |
| WIT | `wit.rs`, `wit_ast.rs` | Exported component interface + imported `yel:ui/dom` |
| DOT | `dot.rs` | Reactive graph dump |
| Dead path | `lir_rust.rs` | LIR→Rust generator, currently commented out in `lib.rs` |

The host contract is `yel:ui/dom@0.1.0` — see `crates/yel-host` for a dev impl.
Tests (snapshots, diagnostic/known-bug fixtures, Wasmtime execution) live in
`tests/`; see `docs/ARCHITECTURE.md` §8.

## No Silent Fallbacks

Never emit dummy/placeholder WASM instructions (e.g. `i32.const 0`) as a fallback for unimplemented expression kinds or unknown function calls. These produce type-incorrect stack shapes that fail WASM validation or cause wrong runtime behavior — and the root cause is extremely hard to trace from hex dumps.

```rust
// ❌ BAD — wrong stack shape, impossible to debug
} else {
    func.instruction(&Instruction::I32Const(0));  // string needs (ptr, len) not one i32
}

// ✅ GOOD — returns a typed error
return Err(CodegenError::InvalidIR(
    format!("Unimplemented: emit_expr for {:?}", expr.kind)
));
```

Use `todo!()` with a descriptive message, or return `Err(CodegenError::...)`. Both crash the compiler immediately with a clear location and context, instead of producing subtly broken WASM.
