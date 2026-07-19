# ir-box-large-variant

> Box recursive children in tree-shaped IRs to keep enum size small

## Why It Matters

An enum's size is the size of its largest variant. A directly-recursive variant like `Binary { lhs: Expr, rhs: Expr }` has no finite size at all, and even non-recursive-but-fat variants bloat *every* instance of the enum to the largest case — wasting memory and cache on the common small nodes. Boxing recursive children makes each child a single pointer, so the enum stays compact. yel's tree IRs do exactly this: `Binary { op, lhs: Box<HirExpr>, rhs: Box<HirExpr> }` (`hir/expr.rs:38`).

## Bad

```rust
enum HirExprKind {
    // does not compile: infinitely sized; even if it did, every
    // HirExpr would be as big as two whole HirExprs.
    Binary { op: BinOp, lhs: HirExpr, rhs: HirExpr },
    Lit(i64),
}
```

## Good

```rust
enum HirExprKind {
    Binary { op: BinOp, lhs: Box<HirExpr>, rhs: Box<HirExpr> },
    Lit(i64),
}
// Each variant is now pointer-sized; a `Lit` node no longer pays
// for the `Binary` payload.
```

## See Also

- [ir-handles-over-boxes](ir-handles-over-boxes.md) - the flat-IR alternative for late/backend IRs
- [id-newtype-index](id-newtype-index.md) - the id types those handles are built from
