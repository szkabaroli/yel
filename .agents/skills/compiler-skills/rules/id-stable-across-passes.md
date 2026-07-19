# id-stable-across-passes

> Keep correlation ids stable across lowerings so later passes can tie output back to source constructs

## Why It Matters

When one IR lowers to another, the new pass emits many small artifacts (blocks, structs, tracking arrays) that must still be attributable to the control-flow construct that produced them. If ids are reassigned during lowering, a later pass has no way to correlate. yel assigns `ForId`, `IfId`, and `TreeBoundaryId` (`crates/yel-core/src/ids.rs:159-225`) that, per their doc comments, stay "stable across tree → block lowering" — so an effect's update block can fan out over the correct for-loop's tracking array, and each if-boundary struct can be keyed back to its `if`.

## Bad

```rust
// new id minted during lowering; the link to the source `for` is lost
for tree_for in trees {
    let block_id = BlockId::new(next_block());
    lower(tree_for, block_id); // which for-loop did this come from?
}
```

## Good

```rust
// ForId is assigned once and carried through every lowering
pub struct ForId(pub u32); // stable across tree -> block lowering

let for_id = node.for_id;            // assigned in the tree IR
let block = lower_for(node, for_id); // block tagged with the same ForId
// a later effect pass fans out over the right loop's tracking array:
update_block.fan_out(for_id);
```

## See Also

- [pass-deferred-emission](pass-deferred-emission.md) - Emitting artifacts that reference earlier ids
- [ctx-interior-mutability](ctx-interior-mutability.md) - Sharing the id-assigning context across passes
