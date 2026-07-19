# cg-no-domain-vocabulary

> The backend must consume only generic ops/types — never branch on a source-domain concept

## Why It Matters

A backend that pattern-matches on frontend concepts — "is this a UI component?", "is this a reactive effect?" — is welded to that one frontend. If the vocabulary it consumes is domain-neutral (generic ops, typed slots, GC struct/array types, read through arena traits), the *same* backend serves any frontend that can produce those primitives. This is precisely how yel's `yel-wasm-codegen` is meant to be shared between the UI language and the visual flow language: `yel-flow-core` already drives `generate_function_module` through the `lir/arena.rs` traits, with no UI/mount-tree concepts. Today some codegen paths still reach for UI specifics (`tree_shape` walks, the `$Comp` self-ref, `yel:ui/dom` assumptions); those are being lowered away (`ARCHITECTURE.md §0`, `TECH_DEBT.md §1`). The rule: never add a backend branch that names a source concept.

## Bad

```rust
fn emit(res: &LirResource) {
    if res.is_ui_component {                 // backend knows about UI
        for b in res.tree_shape.boundaries { walk_mount_tree(b); }
    }
    for s in &res.signals { emit_reactive(s); } // ...and reactivity
}
// the flow frontend produces neither tree_shape nor signals → can't reuse this
```

## Good

```rust
// Backend sees only generic ops + types, via the arena traits:
fn emit(c: &impl LirComponentArena) {
    for block in c.blocks() {
        for op in block.ops() { emit_op(c, op); } // Call / StructNew / StructGet / If / ...
    }
}
// A UI mount tree was already lowered upstream into StructNew/StructGet ops,
// so the backend emits it like any other struct — no UI knowledge required.
```

## See Also

- [cg-arena-traits](cg-arena-traits.md) - Abstract *storage*; this rule abstracts *vocabulary*
- [ir-lower-away-domain](ir-lower-away-domain.md) - The producer-side discipline that makes this possible
- [cg-lower-to-primitives](cg-lower-to-primitives.md) - Lower domain constructs to generic target ops
