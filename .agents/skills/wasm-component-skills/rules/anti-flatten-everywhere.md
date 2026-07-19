# anti-flatten-everywhere

> Don't thread flattened ABI scalars through internal calls; flatten once, at the boundary

## Why It Matters

The canonical ABI flattens a record/list/option into a run of scalars so it can cross the component boundary. That flattening is a boundary concern — pushing it inward, so every internal call passes records as their decomposed scalars, is both slow (N values pushed/popped per aggregate) and fragile (every caller and callee must agree on the flattening by hand, with no type to catch a mismatch). yel keeps a single typed GC ref internally (`InternalRepr::GcRef` / `GcArrayRef` / `FlatGcStruct`, one stack slot) and calls `WasmPackageBuilder::canonical_flat_valtypes(ty)` *only* in the boundary shims that lift/lower WIT exports and imports. This is the inverse of `abi-flatten-at-boundary`.

## Bad

```rust
// flatten an aggregate into canonical scalars for an internal call
let flat = self.canonical_flat_valtypes(rec_ty); // (i32, f64, i32, i32, …)
// push every field, re-pack in the callee — every site must agree by hand
for vt in &flat { /* push one scalar per field */ }
```

## Good

```rust
// internal calls pass one typed GC ref; flatten only in the WIT shim
match self.internal_repr(rec_ty) {
    InternalRepr::GcRef(_) => { /* push a single (ref null $rec) */ }
    _ => {}
}
// canonical_flat_valtypes stays confined to the lift/lower boundary shim
```

## See Also

- [abi-flatten-at-boundary](abi-flatten-at-boundary.md) - The rule this is the inverse of
- [gc-typed-internal](gc-typed-internal.md) - The one-ref internal repr to thread instead
