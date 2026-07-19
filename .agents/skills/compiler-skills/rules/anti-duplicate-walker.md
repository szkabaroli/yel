# anti-duplicate-walker

> Don't re-implement the IR-traversal `match` in every pass — factor it into one shared walker

## Why It Matters

The "walk an expression tree and collect/check something" shape is seductive to copy: each pass writes its own `match` over every variant, recursing into children. Do it more than once and you've built a maintenance trap — adding an IR variant means hand-editing every walker, and the one you forget **silently** skips the new node instead of failing. The recursion is mechanical; only the per-node action differs, so the recursion should live in exactly one place. yel currently has this exact debt: `collect_expr_reads` appears with the *same signature* in both `thir/signalck.rs` and `thir/typeck.rs`, and the same descent is re-spelled again as `collect_dependencies_inner` (`lower_to_lir/component.rs`) and `collect_deps_recursive` (`lower_to_lir/blocks.rs`) — four hand-written copies of one traversal (see `docs/TECH_DEBT.md §6`).

## Bad

```rust
// signalck.rs
fn collect_expr_reads(&self, e: &Expr, out: &mut HashSet<DefId>) {
    match &e.kind {
        ExprKind::Read(d) => { out.insert(*d); }
        ExprKind::Binary { lhs, rhs } => { self.collect_expr_reads(lhs, out); self.collect_expr_reads(rhs, out); }
        /* every variant, by hand */
    }
}
// typeck.rs — a second copy of the same descent
// component.rs — a third (collect_dependencies_inner)
// blocks.rs   — a fourth (collect_deps_recursive)
// add ExprKind::Index → 4 edits; miss one → that pass silently ignores indexing
```

## Good

```rust
// One walker owns the recursion; passes override only the arm they care about.
trait Visitor: Sized { fn visit_expr(&mut self, e: &Expr) { walk_expr(self, e); } }
fn walk_expr<V: Visitor>(v: &mut V, e: &Expr) {
    match &e.kind {
        ExprKind::Read(_) => {}
        ExprKind::Binary { lhs, rhs } => { v.visit_expr(lhs); v.visit_expr(rhs); }
        /* every variant — exactly once, here */
    }
}
struct ReadCollector { reads: HashSet<DefId> }
impl Visitor for ReadCollector {
    fn visit_expr(&mut self, e: &Expr) {
        if let ExprKind::Read(d) = &e.kind { self.reads.insert(*d); }
        walk_expr(self, e); // add a variant → it's covered everywhere at once
    }
}
```

## See Also

- [pass-visitor-recurse](pass-visitor-recurse.md) - The positive rule: read-only, match-based visitors with the walk/visit split
- [ir-side-tables](ir-side-tables.md) - What the (now-shared) walker collects into
