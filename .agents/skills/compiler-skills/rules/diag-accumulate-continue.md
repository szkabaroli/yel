# diag-accumulate-continue

> Collect diagnostics into a sink and keep going; don't abort on the first error

## Why It Matters

A compiler that bails on the first error forces users into a frustrating fix-one-recompile loop. yel's `Diagnostics { diagnostics: Vec<Diagnostic> }` is a sink with `push`, `error(span, msg)`, `has_errors()`, and `error_count()`: the type checker pushes errors and keeps traversing, so one run surfaces many problems at once. The driver only consults `has_errors()` between phases to decide whether to proceed.

## Bad

```rust
fn check_expr(&mut self, e: &Expr) -> Result<Ty, Error> {
    let lhs = self.check_expr(&e.lhs)?; // first mismatch aborts the whole run
    let rhs = self.check_expr(&e.rhs)?;
    ...
}
```

## Good

```rust
fn check_expr(&mut self, e: &Expr) -> Ty {
    let lhs = self.check_expr(&e.lhs);
    let rhs = self.check_expr(&e.rhs);
    if lhs != rhs {
        self.diags.error(e.span, "type mismatch"); // record and keep going
    }
    // ...continue checking the rest of the function
}
// driver:
if diags.has_errors() { return; } // gate between phases, not per-node
```

## See Also

- [diag-error-type-recovery](diag-error-type-recovery.md) - Poison failed nodes so continuing doesn't cascade
- [diag-spans-everywhere](diag-spans-everywhere.md) - Each accumulated diagnostic needs a location
