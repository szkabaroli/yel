# ir-kind-span-struct

> Model a node as `{ kind: …Kind, span, … }` — separate the variant payload from per-node metadata

## Why It Matters

Every IR node needs uniform metadata — a source span, often a type and an id — but that data is orthogonal to the node's structural shape. Splitting the variant payload into a `…Kind` enum and wrapping it in a struct that holds the metadata means every node gets span/type/id for free, and visitors handle that metadata in exactly one place instead of in every match arm. yel uses `HirExpr { kind: HirExprKind, span: Span }` (`hir/expr.rs:10`) and `ThirExpr { id: ExprId, kind: ThirExprKind, ty: Ty, span: Span }` (`thir/expr.rs:11`).

## Bad

```rust
// Metadata smeared across every variant — repeated, easy to forget,
// impossible to access generically.
enum Expr {
    Binary { op: BinOp, lhs: Box<Expr>, rhs: Box<Expr>, span: Span, ty: Ty },
    Call   { callee: Box<Expr>, args: Vec<Expr>, span: Span, ty: Ty },
    Lit    { value: Lit, span: Span, ty: Ty },
}
// want every node's span? you must match all arms.
```

## Good

```rust
pub struct ThirExpr {
    pub id: ExprId,
    pub kind: ThirExprKind,
    pub ty: Ty,
    pub span: Span,
}

pub enum ThirExprKind {           // structure only
    Binary { op: BinOp, lhs: Box<ThirExpr>, rhs: Box<ThirExpr> },
    Call   { callee: Box<ThirExpr>, args: Vec<ThirExpr> },
    Lit(Lit),
}

fn span_of(e: &ThirExpr) -> Span { e.span }  // uniform, one line
```

## See Also

- [ir-preserve-spans](ir-preserve-spans.md) - why the `span` field must survive lowering
- [ir-box-large-variant](ir-box-large-variant.md) - boxing the recursive children inside `…Kind`
