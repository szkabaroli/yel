# anti-god-pass

> Don't let one pass or lowering struct accrete dozens of fields and thousands of lines — split by concern

## Why It Matters

A lowering that owns *every* counter, buffer, stack, and cache becomes the single hardest thing in the compiler to change safely: state interactions are implicit, invariants span unrelated fields, and a tweak in one concern silently perturbs another. Past a point, "add it to the big struct" is never the right answer — split state by concern into small collaborators the pass composes. yel's `BlockLowering` is the cautionary case: one struct with **50+ fields** in an **~8,500-line file** (the largest in the repo) — output vecs, monotonic counters (`next_slot`/`next_block`/`next_memory_offset`), `current_ops` + an `ops_stack`, for-loop stacks, deferred-body queues, signal layout. The backend shows the alternative shape: `wasm/codegen/` is split into `op_emit` / `block_fn` / `dispatch` / `signal_emit` / `scratch` / … each owning one concern (`docs/TECH_DEBT.md §2`).

## Bad

```rust
struct BlockLowering<'a> {
    ctx: &'a CompilerContext,
    blocks: Vec<LirBlock>, slots: Vec<LirSlotInfo>, strings: Vec<String>, exprs: Vec<LirExpr>,
    next_slot: u32, next_block: u32, next_memory_offset: u32,
    current_ops: Vec<LirOp>, ops_stack: Vec<Vec<LirOp>>,
    for_stack: Vec<ForId>, deferred_handler_bodies: Vec<DeferredHandlerBody>,
    pending_block_id_override: Option<BlockId>, signal_layout: SignalLayout,
    /* …40+ more fields, all in one 8.5k-line impl… */
}
```

## Good

```rust
// Each concern is a small struct with one job; the pass composes them.
struct SlotAllocator { next: u32, slots: Vec<LirSlotInfo> }
struct StringPool    { map: HashMap<String, StringId>, strings: Vec<String> }
struct BlockBuilder  { current: Vec<LirOp>, stack: Vec<Vec<LirOp>> }
struct DeferredBodies { handlers: Vec<DeferredHandlerBody> }

struct BlockLowering<'a> { ctx: &'a CompilerContext, slots: SlotAllocator, strings: StringPool, body: BlockBuilder, deferred: DeferredBodies }
```

## See Also

- [pass-lowering-struct](pass-lowering-struct.md) - The right-sized version of this — encapsulate state, but per concern
- [ir-side-tables](ir-side-tables.md) - Move analysis results out of the pass into keyed tables
- [anti-duplicate-walker](anti-duplicate-walker.md) - Another "factor it out" structural rule
