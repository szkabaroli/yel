# ctx-interior-mutability

> Use `RefCell`/`Cell` for monotonic side tables and id counters that grow during otherwise-shared passes

## Why It Matters

Some context state must grow while a pass holds the context only by `&` — for instance a lowering pass borrows `ctx` immutably to read defs and types, yet still needs to allocate fresh ids or append debug entries. yel keeps that state behind interior mutability so `alloc_block_id(&self) -> BlockId` works through a shared reference: `block_id_counter: Cell<u32>`, `block_names: RefCell<HashMap<(DefId, BlockId), BlockDebugName>>`, plus monotonic side tables like `component_lifecycle_blocks` and `global_fanout_blocks`. Reserve this for genuinely append-only state; don't reach for `RefCell` to dodge real ownership design.

## Bad

```rust
// Forces &mut everywhere just to bump a counter, fighting immutable read borrows
fn alloc_block_id(&mut self) -> BlockId {
    self.block_id_counter += 1;
    BlockId(self.block_id_counter)
}
```

## Good

```rust
pub struct CompilerContext {
    block_id_counter: Cell<u32>,
    block_names: RefCell<HashMap<(DefId, BlockId), BlockDebugName>>,
    component_lifecycle_blocks: RefCell<HashMap<DefId, Vec<BlockId>>>,
}

impl CompilerContext {
    pub fn alloc_block_id(&self) -> BlockId {
        let id = self.block_id_counter.get();
        self.block_id_counter.set(id + 1);
        BlockId(id) // monotonic, works through &self
    }
}
```

## See Also

- [id-stable-across-passes](id-stable-across-passes.md) - Ids handed out here stay stable downstream
- [pass-deferred-emission](pass-deferred-emission.md) - Side tables collect work to emit later
