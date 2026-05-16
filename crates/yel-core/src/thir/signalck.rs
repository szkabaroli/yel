//! Phase 1.1c-b: per-component signal dependency analysis.
//!
//! This is a read-only pass over THIR that runs after `typeck.rs`. It walks
//! every expression / statement in a component body and produces a
//! pre-resolved [`SignalDependencies`] structure that LIR lowering can read
//! directly instead of re-discovering signal reads/writes during expression
//! walks (Phase 1.1c-c will consume this in place of legacy
//! `TriggerEffects` op discovery).
//!
//! The analysis is purely additive — it does not mutate any THIR nodes. The
//! produced [`SignalDependencies`] is attached to [`ThirComponent`] via the
//! `signal_deps` field by [`check_component`].

use std::collections::{HashMap, HashSet};

use crate::hir::local_scope::LocalScope;
use crate::ids::DefId;

use super::expr::{ThirExpr, ThirExprKind, ThirInterpolationPart, ThirStatement};
use super::node::{ThirBinding, ThirComponent, ThirGlobal, ThirHandler, ThirNode, ThirNodeKind};

/// Per-component signal dependency analysis.
///
/// Produced by [`check_component`] after typeck. LIR lowering consumes
/// this to emit `CallBlock` sequences in place of legacy `TriggerEffects`
/// ops without re-discovering dependencies during expression walks.
#[derive(Debug, Clone, Default)]
pub struct SignalDependencies {
    /// Per-binding signal reads. Parallel to the linearized list of
    /// bindings produced by [`collect_bindings`] — i.e. a depth-first
    /// walk of the body that emits one entry per `ThirBinding` value
    /// expression.
    pub binding_reads: Vec<Vec<DefId>>,
    /// Per-handler signal writes. Parallel to the linearized list of
    /// handlers produced by [`collect_handlers`] — depth-first walk of
    /// the body that emits one entry per `ThirHandler`.
    pub handler_writes: Vec<Vec<DefId>>,
    /// Per-derived-signal dependencies
    /// (`total: s32 = price * quantity` reads `price`, `quantity`).
    ///
    /// Maps signal DefId to the set of signal DefIds its default
    /// expression reads.
    pub derived_signal_reads: HashMap<DefId, Vec<DefId>>,
    /// Inverted index: signal DefId → list of effect sources that
    /// depend on it. Used by LIR lowering to emit CallBlock sequences
    /// in TriggerEffects positions.
    pub effects_by_signal: HashMap<DefId, Vec<EffectSource>>,
}

/// An effect that needs to be re-run when a signal changes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EffectSource {
    /// Index into [`SignalDependencies::binding_reads`] — re-evaluate
    /// the corresponding binding expression.
    Binding(usize),
    /// Recompute the default expression of this derived signal.
    DerivedSignal(DefId),
}

/// Run signal-dependency analysis on a type-checked component and
/// attach the result to the component as `signal_deps`.
pub fn check_component(component: &mut ThirComponent, is_signal: &impl Fn(DefId) -> bool) {
    let deps = analyze(component, is_signal);
    component.signal_deps = deps;
}

/// Phase 1.1c-k: run signal-dependency analysis on a type-checked global
/// and attach the result as `signal_deps`. Globals have no body and no
/// handlers — only derived-signal default expressions contribute to the
/// dependency graph — so `binding_reads` and `handler_writes` stay empty
/// and `effects_by_signal` only contains `EffectSource::DerivedSignal`
/// entries.
pub fn check_global(global: &mut ThirGlobal, is_signal: &impl Fn(DefId) -> bool) {
    let deps = analyze_global(global, is_signal);
    global.signal_deps = deps;
}

/// Pure analysis: walk a `ThirGlobal` and produce a
/// [`SignalDependencies`]. Symmetrically named to [`analyze`].
pub fn analyze_global(
    global: &ThirGlobal,
    is_signal: &impl Fn(DefId) -> bool,
) -> SignalDependencies {
    // Globals don't carry a LocalScope (no body resolves Locals). Use an
    // empty scope so the analyzer's Local→DefId resolution short-circuits
    // safely — every signal read in a global default reaches the
    // `ThirExprKind::Def` arm.
    let locals = LocalScope::new();
    let mut state = Analyzer {
        locals: &locals,
        is_signal,
        deps: SignalDependencies::default(),
    };

    // Derived-signal default expressions are the only effect surface in
    // globals (`total: s32 = price * quantity` reads `price`, `quantity`).
    for (sig_def, default) in &global.signal_defaults {
        let reads = state.collect_reads(default);
        if !reads.is_empty() {
            state.deps.derived_signal_reads.insert(*sig_def, reads);
        }
    }

    // Build inverted index `effects_by_signal` (DerivedSignal-only).
    let derived_snapshot: Vec<(DefId, Vec<DefId>)> = state
        .deps
        .derived_signal_reads
        .iter()
        .map(|(k, v)| (*k, v.clone()))
        .collect();
    for (sig_def, reads) in derived_snapshot {
        for sig in reads {
            state
                .deps
                .effects_by_signal
                .entry(sig)
                .or_default()
                .push(EffectSource::DerivedSignal(sig_def));
        }
    }

    state.deps
}

/// Pure analysis: walk a `ThirComponent` and produce a
/// [`SignalDependencies`].
pub fn analyze(
    component: &ThirComponent,
    is_signal: &impl Fn(DefId) -> bool,
) -> SignalDependencies {
    let mut state = Analyzer {
        locals: &component.locals,
        is_signal,
        deps: SignalDependencies::default(),
    };

    // 1. Per-binding reads + per-handler writes, parallel to a
    //    depth-first walk of the body.
    for node in &component.body {
        state.visit_node(node);
    }

    // 2. Derived-signal default expressions.
    for (sig_def, default) in &component.signal_defaults {
        let reads = state.collect_reads(default);
        if !reads.is_empty() {
            state.deps.derived_signal_reads.insert(*sig_def, reads);
        }
    }

    // 3. Build inverted index `effects_by_signal`.
    let bindings_snapshot = state.deps.binding_reads.clone();
    for (idx, reads) in bindings_snapshot.iter().enumerate() {
        for sig in reads {
            state
                .deps
                .effects_by_signal
                .entry(*sig)
                .or_default()
                .push(EffectSource::Binding(idx));
        }
    }
    let derived_snapshot: Vec<(DefId, Vec<DefId>)> = state
        .deps
        .derived_signal_reads
        .iter()
        .map(|(k, v)| (*k, v.clone()))
        .collect();
    for (sig_def, reads) in derived_snapshot {
        for sig in reads {
            state
                .deps
                .effects_by_signal
                .entry(sig)
                .or_default()
                .push(EffectSource::DerivedSignal(sig_def));
        }
    }

    state.deps
}

struct Analyzer<'a, F: Fn(DefId) -> bool> {
    locals: &'a LocalScope,
    is_signal: &'a F,
    deps: SignalDependencies,
}

impl<'a, F: Fn(DefId) -> bool> Analyzer<'a, F> {
    fn visit_node(&mut self, node: &ThirNode) {
        match &node.kind {
            ThirNodeKind::Element {
                bindings,
                handlers,
                children,
                ..
            } => {
                for b in bindings {
                    self.visit_binding(b);
                }
                for h in handlers {
                    self.visit_handler(h);
                }
                for c in children {
                    self.visit_node(c);
                }
            }
            ThirNodeKind::Text(expr) => {
                let reads = self.collect_reads(expr);
                self.deps.binding_reads.push(reads);
            }
            ThirNodeKind::If {
                condition,
                then_branch,
                else_if_branches,
                else_branch,
            } => {
                let reads = self.collect_reads(condition);
                self.deps.binding_reads.push(reads);
                for n in then_branch {
                    self.visit_node(n);
                }
                for (cond, branch) in else_if_branches {
                    let reads = self.collect_reads(cond);
                    self.deps.binding_reads.push(reads);
                    for n in branch {
                        self.visit_node(n);
                    }
                }
                if let Some(els) = else_branch {
                    for n in els {
                        self.visit_node(n);
                    }
                }
            }
            ThirNodeKind::For {
                iterable,
                key,
                body,
                ..
            } => {
                let reads = self.collect_reads(iterable);
                self.deps.binding_reads.push(reads);
                if let Some(k) = key {
                    let reads = self.collect_reads(k);
                    self.deps.binding_reads.push(reads);
                }
                for n in body {
                    self.visit_node(n);
                }
            }
            ThirNodeKind::ChildrenSlot => {}
        }
    }

    fn visit_binding(&mut self, b: &ThirBinding) {
        if let Some(value) = &b.value {
            let reads = self.collect_reads(value);
            self.deps.binding_reads.push(reads);
        }
        // Setter bodies write to signals — treat them like handlers so
        // their writes are exposed.
        if let Some(setter) = &b.setter {
            let mut writes = HashSet::new();
            self.collect_stmt_writes(setter, &mut writes);
            let mut writes: Vec<DefId> = writes.into_iter().collect();
            writes.sort_by_key(|d| d.index());
            self.deps.handler_writes.push(writes);
        }
    }

    fn visit_handler(&mut self, h: &ThirHandler) {
        let mut writes = HashSet::new();
        self.collect_stmt_writes(&h.body, &mut writes);
        let mut writes: Vec<DefId> = writes.into_iter().collect();
        writes.sort_by_key(|d| d.index());
        self.deps.handler_writes.push(writes);
    }

    /// Collect the deduplicated, deterministically-ordered list of
    /// signal DefIds that `expr` reads.
    fn collect_reads(&self, expr: &ThirExpr) -> Vec<DefId> {
        let mut set = HashSet::new();
        self.collect_expr_reads(expr, &mut set);
        let mut out: Vec<DefId> = set.into_iter().collect();
        out.sort_by_key(|d| d.index());
        out
    }

    fn collect_expr_reads(&self, expr: &ThirExpr, reads: &mut HashSet<DefId>) {
        match &expr.kind {
            ThirExprKind::Def(def_id) => {
                if (self.is_signal)(*def_id) {
                    reads.insert(*def_id);
                }
            }
            ThirExprKind::Local(local_id) => {
                if let Some(def_id) = self.locals.get(*local_id).def_id {
                    if (self.is_signal)(def_id) {
                        reads.insert(def_id);
                    }
                }
            }
            ThirExprKind::Binary { lhs, rhs, .. } => {
                self.collect_expr_reads(lhs, reads);
                self.collect_expr_reads(rhs, reads);
            }
            ThirExprKind::Unary { operand, .. } => {
                self.collect_expr_reads(operand, reads);
            }
            ThirExprKind::Field { base, .. } | ThirExprKind::OptionalField { base, .. } => {
                self.collect_expr_reads(base, reads);
            }
            ThirExprKind::Index { base, index } => {
                self.collect_expr_reads(base, reads);
                self.collect_expr_reads(index, reads);
            }
            ThirExprKind::Call { args, .. } => {
                for arg in args {
                    self.collect_expr_reads(arg, reads);
                }
            }
            ThirExprKind::Range { start, end, .. } => {
                self.collect_expr_reads(start, reads);
                self.collect_expr_reads(end, reads);
            }
            ThirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.collect_expr_reads(condition, reads);
                self.collect_expr_reads(then_expr, reads);
                self.collect_expr_reads(else_expr, reads);
            }
            ThirExprKind::Closure { body, .. } => {
                // A closure body's reads are conservatively included as
                // dependencies of the surrounding expression context.
                for s in body {
                    self.collect_stmt_reads(s, reads);
                }
            }
            ThirExprKind::Interpolation(parts) => {
                for p in parts {
                    if let ThirInterpolationPart::Expr(e) = p {
                        self.collect_expr_reads(e, reads);
                    }
                }
            }
            ThirExprKind::VariantCtor {
                payload: Some(p), ..
            } => {
                self.collect_expr_reads(p, reads);
            }
            ThirExprKind::ListLiteral { elements, .. }
            | ThirExprKind::TupleLiteral { elements } => {
                for e in elements {
                    self.collect_expr_reads(e, reads);
                }
            }
            ThirExprKind::RecordLiteral { fields, .. } => {
                for f in fields {
                    self.collect_expr_reads(f, reads);
                }
            }
            ThirExprKind::GlobalCall { args, .. } => {
                for a in args {
                    self.collect_expr_reads(a, reads);
                }
            }
            ThirExprKind::Literal(_)
            | ThirExprKind::EnumCase { .. }
            | ThirExprKind::VariantCtor { payload: None, .. }
            | ThirExprKind::GlobalRead { .. }
            | ThirExprKind::Error => {}
        }
    }

    fn collect_stmt_reads(&self, stmt: &ThirStatement, reads: &mut HashSet<DefId>) {
        match stmt {
            ThirStatement::Expr(e) => self.collect_expr_reads(e, reads),
            ThirStatement::Assign { target: _, value } => {
                // Reading the assignment value (the assigned-to lvalue
                // is a write, not a read).
                self.collect_expr_reads(value, reads);
            }
            ThirStatement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_expr_reads(condition, reads);
                for s in then_branch {
                    self.collect_stmt_reads(s, reads);
                }
                if let Some(els) = else_branch {
                    for s in els {
                        self.collect_stmt_reads(s, reads);
                    }
                }
            }
            ThirStatement::Let { value, .. } => self.collect_expr_reads(value, reads),
        }
    }

    fn collect_stmt_writes(&self, stmts: &[ThirStatement], writes: &mut HashSet<DefId>) {
        for stmt in stmts {
            match stmt {
                ThirStatement::Assign { target, .. } => match &target.kind {
                    ThirExprKind::Def(def_id) => {
                        if (self.is_signal)(*def_id) {
                            writes.insert(*def_id);
                        }
                    }
                    ThirExprKind::Local(local_id) => {
                        if let Some(def_id) = self.locals.get(*local_id).def_id {
                            if (self.is_signal)(def_id) {
                                writes.insert(def_id);
                            }
                        }
                    }
                    _ => {}
                },
                ThirStatement::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    self.collect_stmt_writes(then_branch, writes);
                    if let Some(els) = else_branch {
                        self.collect_stmt_writes(els, writes);
                    }
                }
                ThirStatement::Expr(_) | ThirStatement::Let { .. } => {}
            }
        }
    }
}
