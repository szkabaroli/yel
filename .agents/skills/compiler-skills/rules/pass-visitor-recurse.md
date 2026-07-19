# pass-visitor-recurse

> Walk IR with recursive `match`-based visitors; keep analysis passes read-only

## Why It Matters

Separating read-only analysis from mutation makes a pass easy to reason about and reuse. yel's `crates/yel-core/src/thir/signalck.rs` is exactly such a pass: an `Analyzer` with `visit_node`, `visit_binding`, `visit_handler`, and `collect_expr_reads(expr, &mut HashSet<DefId>)` that recurses with a `match` over `expr.kind`, descending into children (`Binary { lhs, rhs }` visits both). It takes `&Component` and returns collected `SignalDependencies` without mutating the IR. Read-only analyses that take shared references can run in any order and never corrupt the IR.

**Factor the traversal once, don't re-spell it per pass.** The recursion skeleton — the `match` that descends into every child — should live in one shared place, not be copied into every analysis. The proven shape is the *walk/visit split* (rustc [`intravisit::Visitor`](https://rustc-dev-guide.rust-lang.org/hir.html#hir-visitors), [`syn::visit`](https://docs.rs/syn/latest/syn/visit/index.html)): a `Visitor` trait whose `visit_*` methods default to calling free `walk_*` functions that own the recursion; each analysis overrides only the arms it cares about. Re-implementing the `match` in every pass (as several yel passes currently do — see `docs/TECH_DEBT.md §6`) means adding an IR variant silently skips whichever walkers forgot it. So: keep the *read-only, match-based* discipline of this rule, but put the descent in a shared walker rather than `collect_*`-ing it by hand in each pass.

## Bad

```rust
// analysis that quietly rewrites the IR as it walks -> hard to reuse,
// order-dependent, and surprising to callers
fn analyze(&mut self, expr: &mut Expr) -> Deps {
    expr.kind = self.canonicalize(expr.kind); // mutation hidden in analysis
    /* ... */
}
```

## Good

```rust
// Recursion lives once, in `walk_expr` — every variant descended here.
trait Visitor: Sized {
    fn visit_expr(&mut self, e: &Expr) { walk_expr(self, e); } // default: just recurse
}
fn walk_expr<V: Visitor>(v: &mut V, e: &Expr) {
    match &e.kind {
        ExprKind::Read(_) => {}
        ExprKind::Binary { lhs, rhs } => { v.visit_expr(lhs); v.visit_expr(rhs); }
        /* ... every other variant descended here, once ... */
    }
}

// A read-only analysis = a Visitor that overrides only the arm it cares about.
#[derive(Default)]
struct ReadCollector { reads: HashSet<DefId> }
impl Visitor for ReadCollector {
    fn visit_expr(&mut self, e: &Expr) {
        if let ExprKind::Read(def) = &e.kind { self.reads.insert(*def); }
        walk_expr(self, e); // still descend — adding a variant can't silently skip us
    }
}
```

## See Also

- [ty-bidirectional](ty-bidirectional.md) - Type checking also recurses over expr kinds
- [pass-register-then-lower](pass-register-then-lower.md) - Phase ordering for whole-file walks
