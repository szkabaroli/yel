# pass-lowering-struct

> Encapsulate per-body lowering state (counters, scopes, output buffers) in a dedicated struct

## Why It Matters

Lowering a body threads a lot of mutable working state: monotonic id counters, scope tables, and output buffers. Passing all of it as free-function arguments is unreadable and error-prone. yel wraps each lowering in a struct holding a context ref plus that state: `HirLowering<'ctx> { ctx: &'ctx mut CompilerContext, locals: LocalScope, next_node_id: u32 }`, and `BlockLowering<'a>` which holds output `blocks/slots/strings/exprs` Vecs, monotonic counters (`next_slot`, `next_block`, `next_memory_offset`), a `current_ops` buffer with an `ops_stack` for nesting, and for-loop stacks. A struct plus methods keeps the state cohesive and the recursion clean.

## Bad

```rust
fn lower_expr(
    expr: &Expr,
    ctx: &mut Ctx,
    blocks: &mut Vec<Block>,
    ops: &mut Vec<Op>,
    next_slot: &mut u32,
    next_block: &mut u32,
    loop_stack: &mut Vec<LoopCtx>,
) { /* every recursive call repeats this argument list */ }
```

## Good

```rust
pub struct BlockLowering<'a> {
    ctx: &'a mut CompilerContext,
    blocks: Vec<Block>,
    slots: Vec<SlotInfo>,
    strings: Vec<String>,
    exprs: Vec<Expr>,
    current_ops: Vec<Op>,
    ops_stack: Vec<Vec<Op>>,
    next_slot: u32,
    next_block: u32,
    next_memory_offset: u32,
}

impl<'a> BlockLowering<'a> {
    fn lower_expr(&mut self, expr: &Expr) { /* state reached via self */ }
}
```

## See Also

- [pass-deferred-emission](pass-deferred-emission.md) - Output buffers enable deferring emission
- [ctx-interior-mutability](ctx-interior-mutability.md) - Sharing mutable compiler state
