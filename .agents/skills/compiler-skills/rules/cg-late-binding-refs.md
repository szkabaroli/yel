# cg-late-binding-refs

> Emit symbolic references in the IR and resolve them to concrete target indices at codegen time

## Why It Matters

The final WASM type-section indices depend on a layout pass that only runs in the backend, so the front-end cannot know them while lowering. yel's `LirTypeRef` (crates/yel-core/src/lir/block.rs) stores *intent* symbolically — `ComponentStruct`, `OtherComponentStruct(DefId)`, `TreeBoundary(TreeBoundaryId)`, `GlobalsStruct(DefId)`, `FlatGcCase(Ty, u32)` — and codegen resolves each to its assigned index. This decouples IR generation from target layout and lets references be emitted before any index exists.

## Bad

```rust
// Baking raw wasm type-section indices into the IR during lowering
enum LirTypeRef { Concrete(u32) } // index isn't known yet — layout runs later
op.struct_type = self.wasm_type_index_for(def); // front-end guessing backend layout
```

## Good

```rust
// Symbolic intent now; backend resolves to a concrete index later
enum LirTypeRef {
    ComponentStruct,
    OtherComponentStruct(DefId),
    TreeBoundary(TreeBoundaryId),
    GlobalsStruct(DefId),
    FlatGcCase(Ty, u32),
}
let idx = layout.resolve(&type_ref); // resolved at codegen time
```

## See Also

- [ir-handles-over-boxes](ir-handles-over-boxes.md) - Same idea: store handles/intent, dereference late
- [id-stable-across-passes](id-stable-across-passes.md) - Symbolic ids must stay stable until the backend resolves them
