# ty-record-typemap

> Record a `span → type` map during checking to power IDE hover, completion, and tooling

## Why It Matters

The type checker already visits every expression and computes its type — that is exactly the data an LSP needs for hover, completion, and go-to-type. Recomputing it in a second pass for tooling is wasteful and risks drift. In yel, `typeck` inserts `(Span, Ty)` into a `TypeMap` at each node as it checks, then returns that map alongside the THIR in `TypeCheckResult`, so a single traversal both validates the program and produces editor data. This is one of the concrete reasons spans must survive lowering.

## Bad

```rust
fn type_check(expr: &Expr) -> Result<Thir, Error> {
    let ty = infer(expr)?;       // type computed...
    Ok(lower(expr, ty))          // ...then discarded; LSP must re-infer later
}
```

## Good

```rust
struct TypeCheckResult {
    thir: Thir,
    type_map: TypeMap, // Span -> Ty, for hover / go-to-type
}

fn type_check_expr(&mut self, expr: &Expr, mode: Mode) -> (ThirKind, Ty) {
    let (kind, ty) = /* ... check / synthesize ... */;
    self.type_map.insert(expr.span, ty); // same pass feeds tooling
    (kind, ty)
}
```

## See Also

- [ir-preserve-spans](ir-preserve-spans.md) - Spans must be preserved through lowering for the map to be useful
- [ty-bidirectional](ty-bidirectional.md) - The checking traversal that populates the map
