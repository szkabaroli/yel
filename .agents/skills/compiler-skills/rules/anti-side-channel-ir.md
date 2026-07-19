# anti-side-channel-ir

> Don't make the backend read an out-of-band representation to interpret the IR — encode intent as explicit ops + typed types

## Why It Matters

When the *meaning* of an op lives in a side channel that codegen must cross-reference, the instruction stream no longer says what it does. Every consumer has to know to consult the side structure, and often to **reconstruct relationships at emit time** (walk a tree, resolve a symbolic pointer through a scope of locals). That coupling is slow, fragile, and impossible for a second frontend to reproduce. Put the intent in the op stream and the type system instead. yel's LIR carries a `tree_shape` mount-tree that codegen walks both to emit GC types and to resolve `LirSlotKind::BoundaryField { boundary_id, field_idx }` — a *symbolic* pointer resolved at codegen by a runtime chain walk over `current_boundary_locals`. The `lir-resource-flatten` plan replaces the side channel with explicit `StructGet`/`StructSet` ops and typed struct-ref params (`docs/TECH_DEBT.md §1.2`).

> Not the same as a *symbolic reference* resolved by a direct lookup — those are fine (see [cg-late-binding-refs](cg-late-binding-refs.md)). The smell is a separate data structure codegen must **traverse and join against** to recover what an op means.

## Bad

```rust
// The op carries a symbolic pointer; its meaning lives in a side `tree_shape`.
enum LirSlotKind { BoundaryField { boundary_id: TreeBoundaryId, field_idx: u32 } }

fn emit_read(&mut self, slot: LirSlotId) {
    let bid = self.slot_boundary(slot);
    let local = self.current_boundary_locals[&bid]; // chain walk to resolve…
    let ty = self.tree_shape.struct_ty_of(bid);     // …join against the side channel
    self.emit(local_get(local)); self.emit(struct_get(ty, field_idx));
}
```

## Good

```rust
// The op stream says exactly what happens; codegen just emits it.
enum LirOp {
    StructGet { ref_slot: LirSlotId, struct_ty: LirTypeRef, field_idx: u32, result: LirSlotId },
    StructSet { ref_slot: LirSlotId, struct_ty: LirTypeRef, field_idx: u32, value: LirSlotId },
}
// no tree_shape, no current_boundary_locals walk — a second frontend can emit these too
```

## See Also

- [ir-lower-away-domain](ir-lower-away-domain.md) - The side channel here is also a UI concept that should be lowered away
- [cg-late-binding-refs](cg-late-binding-refs.md) - Contrast: symbolic refs resolved by a direct lookup are fine
- [ir-handles-over-boxes](ir-handles-over-boxes.md) - Reference data by id in the op, don't reconstruct it
- [cg-no-domain-vocabulary](cg-no-domain-vocabulary.md) - A side channel is one way domain concepts leak into the backend
