# abi-flatten-at-boundary

> Keep values in their rich internal repr; flatten to canonical-ABI scalars only inside the export/import lift-lower shims

## Why It Matters

The component-model canonical ABI requires aggregates (records, tuples, lists, option/result) to be *flattened* into a sequence of core scalar `ValType`s before they cross a component boundary. But flattening everywhere internally is slower and bug-prone — every producer and consumer would have to agree on the field-by-field decomposition by hand. yel keeps values in a typed internal representation instead: `InternalRepr` in `wasm/repr.rs` classifies a record/tuple as a single `GcRef(u32)`, a scalar list as `GcArrayRef(u32)`, and an option/result/variant as `FlatGcStruct(u32)` — one typed WASM-GC ref slot, not N loose scalars. The canonical flattening lives only in `canonical_flat_valtypes` (`wasm/mod.rs`) and is confined to the WIT export/import boundary; `repr.rs` even documents this split as an invariant. (This is the component-model-specific sibling of compiler-skills' `cg-flatten-at-boundary`.)

## Bad

```rust
// every internal op decomposes a record into its canonical ABI scalars
fn push_record(&mut self, rec: &LirExpr) {
    for vt in self.canonical_flat_valtypes(rec.ty) {
        // each field on the stack as a loose scalar — slow, and every
        // caller/callee must re-agree on the flattening by hand
        self.push_field(vt);
    }
}
```

## Good

```rust
// internal: one typed GC ref, classified by InternalRepr
match self.internal_repr(rec.ty) {
    InternalRepr::GcRef(type_idx) => self.push_gc_ref(type_idx), // 1 slot
    InternalRepr::FatPointer => self.push_fat_ptr(rec),          // (ptr,len)
    _ => { /* … */ }
}
// flatten ONLY in the boundary shim:
fn lower_export_param(&mut self, ty: Ty) {
    for vt in self.canonical_flat_valtypes(ty) { /* WIT lift/lower */ }
}
```

## See Also

- [abi-lift-lower-shims](abi-lift-lower-shims.md) - Where the flattening is allowed to happen
- [gc-typed-internal](gc-typed-internal.md) - The typed internal repr you keep instead
- [anti-flatten-everywhere](anti-flatten-everywhere.md) - The failure mode this rule prevents
