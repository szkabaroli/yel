# ir-lower-away-domain

> Lower frontend/domain concepts out before the lowest IR — keep it target-generic

## Why It Matters

An IR stack earns its keep by *removing* a layer's vocabulary as you descend. The lowest IR should speak only the **target's** language — typed slots, blocks, ops, GC struct/array types — never the **source domain's** language: a UI `Node`, a reactive `Signal`, an `Effect` / dependency set. Those are AST/HIR/THIR-level concepts; by the lowest IR they must already be desugared into generic ops plus side tables. If a domain concept survives into the low IR, every downstream consumer (codegen, optimizer, a second frontend) is forced to understand it, and the IR stops being reusable. yel is mid-migration toward this: its `LirResource` still carries `signals`, `effects`, `body_tree`, and a `tree_shape` (the UI mount-tree), and codegen still reads them — the [`lir-resource-flatten`](../../../../plans/lir-resource-flatten.md) plan exists to push those *up* into a Yel-only lowering pass and *down* into generic `StructNew`/`StructGet` ops, leaving the LIR domain-neutral.

## Bad

```rust
// Low-level IR still speaking the UI/reactive frontend's vocabulary:
enum LirNodeKind { Element { tag, .. }, Text(..), If { .. }, For { .. } }   // UI tree
struct LirResource {
    blocks: Vec<LirBlock>,
    signals: Vec<LirSignal>,        // reactive concept
    effects: Vec<LirEffect>,        // dependency tracking — an HIR/THIR concern
    tree_shape: TreeShape,          // UI mount-tree side channel
}
// codegen and any other frontend must now understand UI + reactivity
```

## Good

```rust
// Low-level IR speaks only target-generic primitives:
struct LirResource {
    blocks: Vec<LirBlock>,          // flat op streams
    slots:  Vec<LirSlotInfo>,
    exprs:  Vec<LirExpr>,
    struct_types: Vec<LirStructTypeDecl>,
    array_types:  Vec<LirArrayTypeDecl>,
}
// A "signal" is just a typed slot + a struct field + update blocks.
// A UI "Node" became ops that emit host (DOM) calls.
// Reactivity "deps" became a dependency side table computed during THIR→LIR.
// No Node / Signal / Effect kinds remain in the IR.
```

## See Also

- [ir-layered-lowering](ir-layered-lowering.md) - Each layer removes ambiguity; this says it also removes vocabulary
- [ir-side-tables](ir-side-tables.md) - Where lowered-away analysis (deps, layout) goes instead of IR node kinds
- [cg-no-domain-vocabulary](cg-no-domain-vocabulary.md) - The backend payoff of a domain-free low IR
- [cg-lower-to-primitives](cg-lower-to-primitives.md) - Lower to generic ops, not a runtime library
