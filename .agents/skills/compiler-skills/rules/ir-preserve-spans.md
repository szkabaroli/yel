# ir-preserve-spans

> Carry source spans through every IR so any later phase can still point at the user's code

## Why It Matters

A diagnostic is only useful if it can point at the exact source text that caused it — and the phase that detects an error is usually far downstream of parsing. If lowering drops spans, a type error found in THIR or a problem hit during codegen has nothing to render a `--> file:line:col` arrow against. Threading a span through every node keeps diagnostics precise at every stage and even powers IDE features. yel's `Span { source: SourceId, start: usize, end: usize }` (`source.rs:147`) rides on every `*Expr`/`*Node` from AST into HIR and THIR; the type checker additionally records a `span → Ty` map for editor hover.

## Bad

```rust
// Span discarded at lowering; downstream errors can only say "somewhere".
fn lower(ast: &AstExpr) -> HirExpr {
    HirExpr { kind: lower_kind(&ast.kind) }   // span dropped!
}

fn report_type_error(e: &ThirExpr) {
    eprintln!("type error");                  // no location to show
}
```

## Good

```rust
fn lower(ast: &AstExpr) -> HirExpr {
    HirExpr { kind: lower_kind(&ast.kind), span: ast.span }  // carried through
}

fn report_type_error(e: &ThirExpr, src: &SourceMap) {
    // span survived AST -> HIR -> THIR, so we can render the arrow.
    emit_diagnostic("type mismatch", e.span, src);  // --> file:line:col
}
```

## See Also

- [ir-kind-span-struct](ir-kind-span-struct.md) - where the `span` field lives on each node
- [diag-spans-everywhere](diag-spans-everywhere.md) - building diagnostics from those spans
- [ty-record-typemap](ty-record-typemap.md) - the `span -> Ty` map for IDE hover
