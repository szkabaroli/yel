# ir-handles-over-boxes

> In flat/late IRs, reference children by id handles into a side table, not `Box`

## Why It Matters

Boxing is fine for a tree you traverse once, but a backend IR wants to dedup identical subexpressions, copy nodes cheaply, and serialize the whole program. Pointers do none of that. Storing nodes in one arena and referencing them by a `u32` id makes children copyable, comparable, and dedup-able, and turns recursion into a flat index instead of a pointer chase. yel's LIR keeps expressions in a `Vec<LirExpr>` on `LirResource` (`lir/node.rs`); ops reference them by `ExprId(pub u32)`, and codegen reads them through `trait LirExprArena { fn expr(&self, id: ExprId) -> &LirExpr; }` (`lir/arena.rs:40`). The type interner does the same with `List(Ty)` rather than `List(Box<Ty>)` (`types/interner.rs:67`), so recursion costs a single `u32`.

## Bad

```rust
// Pointer-linked backend IR: can't dedup, can't cheaply copy,
// can't serialize without chasing the heap.
enum LirExpr {
    Add(Box<LirExpr>, Box<LirExpr>),
    Const(i64),
}
struct LirOp { eval: Box<LirExpr> }
```

## Good

```rust
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(pub u32);

enum LirExpr {
    Add(ExprId, ExprId),          // children are handles into the arena
    Const(i64),
}

enum LirOp {
    EvalExpr { expr: ExprId, result: SlotHandle },
}

trait LirExprArena {
    fn expr(&self, id: ExprId) -> &LirExpr;   // resolve handle -> node
}
```

## See Also

- [id-newtype-index](id-newtype-index.md) - why `ExprId` is a newtype, not a bare `u32`
- [ir-box-large-variant](ir-box-large-variant.md) - the tree-IR counterpart that does use `Box`
- [intern-types](intern-types.md) - interning makes those handles dedup-able
- [cg-arena-traits](cg-arena-traits.md) - reading nodes through an arena trait in codegen
