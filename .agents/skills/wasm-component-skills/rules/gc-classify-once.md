# gc-classify-once

> Decide scalar vs GC-ref vs linear-memory in one classifier consulted everywhere, never re-derived per emit site

## Why It Matters

"How is this value represented?" is the kind of question that *must* have exactly one answer: if two emit sites independently decide a record is a flat tuple here and a GC ref there, the producer and consumer disagree and you get a type-incorrect stack that only surfaces at component validation. yel funnels every representation decision through a single classifier — `WasmPackageBuilder::internal_repr(ty) -> InternalRepr` in `wasm/repr.rs`, returning `Zero` / `Scalar(ValType)` / `FatPointer` / `GcRef(u32)` / `GcArrayRef(u32)` / `FlatGcStruct(u32)`. Helpers like `internal_stack_slots`, `signal_storage_valtypes`, and `block_ty_for` all delegate to it rather than re-inspecting `Ty`. One source of truth keeps the representation consistent across the whole back-end.

## Bad

```rust
// re-derive "is this a pointer?" inline at an emit site
let slots = if matches!(self.ctx.ty_kind(ty), InternedTyKind::String) {
    2 // …but another site forgot lists are also fat pointers — now they disagree
} else {
    1
};
```

## Good

```rust
// ask the one classifier; every site gets the same answer
let slots = self.internal_stack_slots(ty); // delegates to internal_repr(ty)
match self.internal_repr(ty) {
    InternalRepr::FatPointer => { /* (i32, i32) */ }
    InternalRepr::GcRef(idx) => { /* one typed ref */ }
    _ => {}
}
```

## See Also

- [gc-typed-internal](gc-typed-internal.md) - The GC-ref reprs this classifier returns
- [gc-hybrid-gc-and-memory](gc-hybrid-gc-and-memory.md) - The scalar/GC/memory split it encodes
