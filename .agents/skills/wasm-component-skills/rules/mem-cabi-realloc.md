# mem-cabi-realloc

> Export the canonical-ABI `cabi_realloc` so the host can allocate into your linear memory when lowering lists/strings in

## Why It Matters

When the host lowers a `string` or `list` *into* the guest, the canonical ABI needs somewhere in the guest's linear memory to put the bytes — and it gets that space by calling the guest-exported `cabi_realloc(old_ptr, old_size, align, new_size) -> ptr`. Without that export, no incoming string/list can be materialized and the component can't be instantiated against a host that passes one. yel emits the function via `emit_cabi_realloc(alloc_func, free_func)` in `wasm/runtime/memory.rs` (allocate when `old_ptr == 0`, free when `new_size == 0`, otherwise alloc-copy-free), records its index on `AllocatorFuncs { alloc, free, cabi_realloc }` (`wasm/mod.rs`), and exports it by name in `wasm/codegen/build.rs`: `exports.export("cabi_realloc", ExportKind::Func, alloc_funcs.cabi_realloc)`. The same index is reused internally whenever the guest itself needs scratch heap (e.g. GC-list getters in `codegen/accessors.rs`).

## Bad

```rust
// emit cabi_realloc but never surface it in the export section
let realloc_idx = code.function(&runtime::emit_cabi_realloc(alloc, free));
// exports omit it — the host has nowhere to lower an incoming
// string/list into, so instantiation fails the moment one is passed
exports.export("memory", ExportKind::Memory, 0);
```

## Good

```rust
let alloc_funcs = AllocatorFuncs { alloc, free, cabi_realloc };
// canonical ABI looks up the export by this exact name
exports.export("memory", ExportKind::Memory, 0);
exports.export("cabi_realloc", ExportKind::Func, alloc_funcs.cabi_realloc);
```

## See Also

- [mem-fat-pointer-strings](mem-fat-pointer-strings.md) - The `(ptr,len)` values that land in realloc'd memory
- [mem-return-buffer-ownership](mem-return-buffer-ownership.md) - The other half of the allocation hand-off
- [comp-real-module-for-state](comp-real-module-for-state.md) - The core module that owns this linear memory
