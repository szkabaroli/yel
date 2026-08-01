//! **The** HIR walker. One, exhaustive, no `_` arm — stage 4 shares it, and no
//! analysis gets a private one (A3; the frozen tree's `collect_children_slots`
//! is the counterexample this replaces).
//!
//! Shape follows the one place the frozen tree solved this well —
//! `thir/visit.rs`, the keep-list's standing example: a trait whose default
//! methods call `walk_*` free functions, so an implementor overrides what it
//! cares about and the walk order lives in exactly one place.
//!
//! Every `match` here is exhaustive over the node enums. A new variant is a
//! compile error in this file, not a silently-unvisited subtree.

use crate::expr::{
    HirBlock, HirBoundary, HirClosure, HirExpr, HirExprKind, HirInterpolationPart, HirStmt,
};
use crate::module::{HirBody, HirComponent, HirGlobal, HirItem};

/// A read-only walk over HIR. Default methods visit everything; override the
/// entry points you care about and call the matching `walk_*` to continue
/// below yourself.
pub trait Visitor: Sized {
    fn visit_item(&mut self, item: &HirItem) {
        walk_item(self, item);
    }

    fn visit_component(&mut self, component: &HirComponent) {
        walk_component(self, component);
    }

    fn visit_global(&mut self, global: &HirGlobal) {
        walk_global(self, global);
    }

    fn visit_body(&mut self, body: &HirBody) {
        walk_body(self, body);
    }

    fn visit_block(&mut self, block: &HirBlock) {
        walk_block(self, block);
    }

    fn visit_stmt(&mut self, stmt: &HirStmt) {
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &HirExpr) {
        walk_expr(self, expr);
    }

    fn visit_closure(&mut self, closure: &HirClosure) {
        walk_closure(self, closure);
    }
}

/// Items hold bodies by id, so walking an item does **not** walk its bodies —
/// the arena is the module's, and a whole-module pass iterates
/// `module.bodies` directly. What an item walk visits is its own structure.
pub fn walk_item<V: Visitor>(visitor: &mut V, item: &HirItem) {
    match item {
        HirItem::Component(component) => visitor.visit_component(component),
        HirItem::Global(global) => visitor.visit_global(global),
        // A root function's body is a body, reached by id like every other —
        // the item's own structure holds nothing more to walk.
        HirItem::Function { .. } => {}
    }
}

pub fn walk_component<V: Visitor>(_visitor: &mut V, _component: &HirComponent) {
    // Defaults, functions and the build are bodies, reached by id.
}

pub fn walk_global<V: Visitor>(_visitor: &mut V, _global: &HirGlobal) {
    // Same: functions are bodies, reached by id.
}

pub fn walk_body<V: Visitor>(visitor: &mut V, body: &HirBody) {
    visitor.visit_block(&body.block);
}

pub fn walk_block<V: Visitor>(visitor: &mut V, block: &HirBlock) {
    for stmt in &block.stmts {
        visitor.visit_stmt(stmt);
    }
    if let Some(tail) = &block.tail {
        visitor.visit_expr(tail);
    }
}

pub fn walk_stmt<V: Visitor>(visitor: &mut V, stmt: &HirStmt) {
    match stmt {
        HirStmt::Let { value, .. } => visitor.visit_expr(value),
        HirStmt::Assign { target, value, .. } => {
            visitor.visit_expr(target);
            visitor.visit_expr(value);
        }
        HirStmt::Expr(expr) => visitor.visit_expr(expr),
        HirStmt::Return { value, .. } => {
            if let Some(value) = value {
                visitor.visit_expr(value);
            }
        }
        HirStmt::For { iterable, body, .. } => {
            visitor.visit_expr(iterable);
            visitor.visit_block(body);
        }
        HirStmt::Error { .. } => {}
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &HirExpr) {
    match &expr.kind {
        HirExprKind::Local(_)
        | HirExprKind::Def(_)
        | HirExprKind::Prop { .. }
        | HirExprKind::Intrinsic(_)
        | HirExprKind::Unresolved(_)
        | HirExprKind::Literal(_)
        | HirExprKind::Error => {}
        HirExprKind::List(items) | HirExprKind::Tuple(items) => {
            for item in items {
                visitor.visit_expr(item);
            }
        }
        HirExprKind::Record { fields } => {
            for field in fields {
                visitor.visit_expr(&field.value);
            }
        }
        HirExprKind::Field { base, .. } | HirExprKind::OptionalField { base, .. } => {
            visitor.visit_expr(base);
        }
        HirExprKind::Index { base, index } => {
            visitor.visit_expr(base);
            visitor.visit_expr(index);
        }
        HirExprKind::Unary { operand, .. } => visitor.visit_expr(operand),
        HirExprKind::Binary { lhs, rhs, .. } => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        HirExprKind::Range { start, end, .. } => {
            visitor.visit_expr(start);
            visitor.visit_expr(end);
        }
        HirExprKind::Call { args, .. } => {
            // The callee is a resolution, not an expression — a reference in
            // callee position is a dependency question its *consumer* answers
            // by looking at the callee, not by walking into one.
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        HirExprKind::Closure(closure) => visitor.visit_closure(closure),
        HirExprKind::Interpolation(parts) => {
            for part in parts {
                match part {
                    HirInterpolationPart::Literal(_) => {}
                    HirInterpolationPart::Expr(expr) => visitor.visit_expr(expr),
                }
            }
        }
        HirExprKind::Match(node) => {
            visitor.visit_expr(&node.scrutinee);
            for arm in &node.arms {
                visitor.visit_expr(&arm.value);
            }
        }
        HirExprKind::Block(block) => visitor.visit_block(block),
        HirExprKind::Instantiate(node) => {
            for prop in &node.props {
                if let Some(getter) = &prop.getter {
                    visitor.visit_expr(getter);
                }
                if let Some(setter) = &prop.setter {
                    visitor.visit_closure(setter);
                }
            }
            for child in &node.children {
                visitor.visit_expr(child);
            }
        }
        HirExprKind::UiText(content) => visitor.visit_expr(content),
        HirExprKind::Boundary(boundary) => match &**boundary {
            HirBoundary::Conditional(node) => {
                visitor.visit_expr(&node.scrutinee);
                for arm in &node.arms {
                    visitor.visit_expr(&arm.value);
                }
            }
            HirBoundary::Repeat(node) => {
                visitor.visit_expr(&node.iterable);
                if let Some(key) = &node.key {
                    visitor.visit_expr(key);
                }
                for child in &node.children {
                    visitor.visit_expr(child);
                }
            }
            HirBoundary::Children => {}
        },
        HirExprKind::Fragment(children) => {
            for child in children {
                visitor.visit_expr(child);
            }
        }
    }
}

pub fn walk_closure<V: Visitor>(visitor: &mut V, closure: &HirClosure) {
    visitor.visit_block(&closure.block);
}
