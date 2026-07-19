# host-import-roundtrip

> Give each host import a stable internal id and resolve it to a concrete wasm import index at codegen; never hardcode import indices

## Why It Matters

Wasm import indices are positional: the moment you add, remove, or reorder an import, every hardcoded index downstream points at the wrong function. The fix is indirection — refer to each host function by a stable internal id throughout the IR, and resolve that id to a concrete index once, at codegen, through a single round-trip helper. yel assigns every `yel:ui/dom` function a synthetic `DefId` (`dom_imports.rs::DomImports`: `create_element`, `create_comment`, `append_child`, …, registered by `register_dom_imports`); the LIR refers to imports by `DefId`, and `wasm/mod.rs::wasm_import_index_for_dom_def(ctx, def_id) -> Option<u32>` maps each one back to its index (`IMPORT_CREATE_ELEMENT`, …). Change the import list and only that one helper moves.

## Bad

```rust
// hardcode the import index at the call site
func.instruction(&Instruction::Call(8)); // append_child… until an import is inserted above it
```

## Good

```rust
// refer to the import by stable DefId; resolve to an index in one place
let def_id = ctx.dom_imports().append_child;
let import_idx = wasm_import_index_for_dom_def(ctx, def_id)
    .ok_or_else(|| CodegenError::MissingDefinition(def_id))?;
func.instruction(&Instruction::Call(import_idx));
```

## See Also

- [host-versioned-imports](host-versioned-imports.md) - The versioned interface these ids come from
- [abi-lift-lower-shims](abi-lift-lower-shims.md) - Canonical shims around imported calls
