# ty-bidirectional

> Use bidirectional checking: an `Infer` (synthesize) mode and a `Check(expected)` mode

## Why It Matters

Pure bottom-up inference cannot type constructs that carry no information in isolation: an empty list `[]` has no element type, and a lambda's parameter types are unknown without context. Bidirectional checking solves this by threading an expected type inward where one exists, while still synthesizing types where they can be read off the syntax. In yel, `type_check_expr` takes a `Mode`: in `Infer` it builds a `(kind, ty)` pair bottom-up, and in `Check(expected)` it pushes the expected type top-down, getting the best of both directions.

## Bad

```rust
// Pure synthesis: no expected type to push inward.
fn infer(&mut self, expr: &Expr) -> Ty {
    match expr {
        Expr::List(items) if items.is_empty() => {
            // No element type anywhere — what is `[]`? Can't infer.
            panic!("cannot infer type of empty list")
        }
        // ...
    }
}
```

## Good

```rust
enum Mode {
    Infer,
    Check(Ty),
}

fn type_check_expr(&mut self, expr: &Expr, mode: Mode) -> (ThirKind, Ty) {
    match (expr, mode) {
        // Empty list: pull the element type from the expected type.
        (Expr::List(items), Mode::Check(expected)) if items.is_empty() => {
            let elem = self.list_elem_of(expected);
            (ThirKind::List(vec![]), self.intern_list(elem))
        }
        // Still synthesize where the syntax is self-describing.
        (Expr::IntLit(n), Mode::Infer) => self.synth_int(*n),
        // ...
    }
}
```

## See Also

- [ty-literal-polymorphism](ty-literal-polymorphism.md) - Check mode supplies the expected type that resolves polymorphic literals
- [pass-visitor-recurse](pass-visitor-recurse.md) - Both modes recurse over the same expression tree
