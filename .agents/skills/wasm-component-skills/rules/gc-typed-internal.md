# gc-typed-internal

> Use WASM-GC typed struct/array refs for internal aggregates — one ref slot, not a tuple of loose scalars

## Why It Matters

Inside the module, an aggregate value (record, tuple, list) wants to be *one thing* that flows through the IR, not N decomposed scalars that every producer and consumer must re-assemble in lockstep. WASM-GC gives you typed `struct`/`array` heap types, so a record becomes a single `(ref null $rec)` on the stack — cheaper to pass and impossible to mis-order. yel builds these types in `wasm/gc_types.rs` (`RecordGcTypes` holds `record_type_idx`, `tuple_struct_type_idx`, `list_array_type_idx`, `flat_gc_super_idx`) and `repr.rs::internal_repr` classifies the type as `InternalRepr::GcRef` / `GcArrayRef` / `FlatGcStruct` — always one stack slot (`internal_stack_slots` returns `1`). Canonical-ABI flattening is a separate concern that only happens at the WIT boundary.

## Bad

```rust
// represent a record as its decomposed fields everywhere internally
// every call site must agree on field order/count by hand
let fields: Vec<ValType> = self.canonical_flat_valtypes(rec_ty); // (i32, f64, i32, i32, …)
// push/pop N values per record, re-pack on every call — error-prone and slow
```

## Good

```rust
// one typed GC ref carries the whole aggregate internally
match self.internal_repr(rec_ty) {
    InternalRepr::GcRef(type_idx) => ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(type_idx), // $rec_record — 1 slot
    }),
    // GcArrayRef for lists, FlatGcStruct for option/result/variant
    _ => unreachable!(),
}
```

## See Also

- [gc-classify-once](gc-classify-once.md) - The single classifier that decides this repr
- [abi-flatten-at-boundary](abi-flatten-at-boundary.md) - Where the typed ref does get flattened
- [gc-hybrid-gc-and-memory](gc-hybrid-gc-and-memory.md) - GC refs vs the linear-memory boundary
