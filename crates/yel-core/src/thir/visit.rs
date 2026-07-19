//! THIR traversal — a read-only visitor that splits *traversal* from *action*.
//!
//! The "match every `ThirExprKind` variant and recurse into children" skeleton
//! used to be hand-copied in every analysis that walks a typed expression tree
//! (signal-read collection, signal-write collection, LIR dependency tracking).
//! Each copy re-spelled `Binary { lhs, rhs }` / `Field { base }` / `Call { args }`
//! / … so adding a variant to [`ThirExprKind`] meant remembering to touch all of
//! them — a silent-incompleteness trap (see `docs/TECH_DEBT.md` §6).
//!
//! This module keeps the recursion in exactly one place. An analysis implements
//! [`ThirVisitor`], overrides only the arms it cares about, and calls the free
//! [`walk_expr`] / [`walk_stmt`] functions (directly or via the default method
//! bodies) to recurse. The `match` in `walk_expr` is exhaustive with **no**
//! `_` arm, so a new `ThirExprKind`/`ThirStatement` variant is a compile error
//! here — the single place to teach the new shape — never a silent skip.
//!
//! The visitor is read-only (`&ThirExpr`); the rewrite/lowering passes that
//! *produce* new trees are a separate concern and keep their own dispatch.

use super::expr::{ThirClosure, ThirExpr, ThirExprKind, ThirInterpolationPart, ThirStatement};

/// Read-only walk over a typed expression tree.
///
/// Every method defaults to the structural recursion in [`walk_expr`] /
/// [`walk_stmt`], so an implementor overrides only the cases whose *action* it
/// needs and lets the defaults carry the recursion. To act on a node **and**
/// keep descending, do the action then call `walk_expr(self, e)` (the pattern
/// the bundled collectors use).
pub trait ThirVisitor {
    /// Visit one expression. Defaults to recursing into its children.
    fn visit_expr(&mut self, expr: &ThirExpr) {
        walk_expr(self, expr);
    }

    /// Visit one statement. Defaults to recursing into its sub-statements and
    /// expressions.
    fn visit_stmt(&mut self, stmt: &ThirStatement) {
        walk_stmt(self, stmt);
    }

    /// Visit a closure encountered inside an expression. Defaults to walking
    /// the closure body's statements. Analyses that must *not* descend into
    /// closures (e.g. LIR dependency collection, which treats a closure body
    /// as opaque) override this to a no-op.
    fn visit_closure(&mut self, closure: &ThirClosure) {
        for stmt in &closure.body {
            self.visit_stmt(stmt);
        }
    }
}

/// Recurse into every child expression of `expr`, dispatching back through the
/// visitor. Exhaustive over [`ThirExprKind`] with no wildcard arm: a new
/// variant must be handled here.
pub fn walk_expr<V: ThirVisitor + ?Sized>(visitor: &mut V, expr: &ThirExpr) {
    match &expr.kind {
        ThirExprKind::Binary { lhs, rhs, .. } => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        ThirExprKind::Unary { operand, .. } => {
            visitor.visit_expr(operand);
        }
        ThirExprKind::Field { base, .. } | ThirExprKind::OptionalField { base, .. } => {
            visitor.visit_expr(base);
        }
        ThirExprKind::Index { base, index } => {
            visitor.visit_expr(base);
            visitor.visit_expr(index);
        }
        ThirExprKind::Call { args, .. } | ThirExprKind::GlobalCall { args, .. } => {
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        ThirExprKind::Range { start, end, .. } => {
            visitor.visit_expr(start);
            visitor.visit_expr(end);
        }
        ThirExprKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            visitor.visit_expr(condition);
            visitor.visit_expr(then_expr);
            visitor.visit_expr(else_expr);
        }
        ThirExprKind::Closure(closure) => {
            visitor.visit_closure(closure);
        }
        ThirExprKind::Interpolation(parts) => {
            for part in parts {
                if let ThirInterpolationPart::Expr(e) = part {
                    visitor.visit_expr(e);
                }
            }
        }
        ThirExprKind::VariantCtor {
            payload: Some(payload),
            ..
        } => {
            visitor.visit_expr(payload);
        }
        ThirExprKind::ListLiteral { elements, .. } | ThirExprKind::TupleLiteral { elements } => {
            for elem in elements {
                visitor.visit_expr(elem);
            }
        }
        ThirExprKind::RecordLiteral { fields, .. } => {
            for field in fields {
                visitor.visit_expr(field);
            }
        }
        // Leaves: no child expressions to recurse into.
        ThirExprKind::Local(_)
        | ThirExprKind::Def(_)
        | ThirExprKind::Literal(_)
        | ThirExprKind::EnumCase { .. }
        | ThirExprKind::VariantCtor { payload: None, .. }
        | ThirExprKind::GlobalRead { .. }
        | ThirExprKind::Error => {}
    }
}

/// Recurse into every child of a statement, dispatching back through the
/// visitor. Exhaustive over [`ThirStatement`] with no wildcard arm.
pub fn walk_stmt<V: ThirVisitor + ?Sized>(visitor: &mut V, stmt: &ThirStatement) {
    match stmt {
        ThirStatement::Expr(e) => visitor.visit_expr(e),
        ThirStatement::Assign { target, value } => {
            visitor.visit_expr(target);
            visitor.visit_expr(value);
        }
        ThirStatement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            visitor.visit_expr(condition);
            for s in then_branch {
                visitor.visit_stmt(s);
            }
            if let Some(els) = else_branch {
                for s in els {
                    visitor.visit_stmt(s);
                }
            }
        }
        ThirStatement::Let { value, .. } => visitor.visit_expr(value),
    }
}
