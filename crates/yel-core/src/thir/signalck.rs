//! Phase 1.1c-b: per-component signal dependency analysis.
//!
//! This is a read-only pass over THIR that runs after `typeck.rs`. It walks
//! every expression / statement in a component body and produces a
//! pre-resolved [`SignalDependencies`] structure that LIR lowering can read
//! directly instead of re-discovering signal reads/writes during expression
//! walks (Phase 1.1c-c will consume this in place of legacy
//! `TriggerEffects` op discovery).
//!
//! The analysis is purely additive — it does not mutate any THIR nodes. Each
//! `check_*` entry point returns the produced [`SignalDependencies`]; the
//! typeck driver stores it in the `CompilerContext` side table keyed by the
//! owning component/global `DefId` (`CompilerContext::set_signal_deps`).

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::hir::local_scope::LocalScope;
use crate::ids::DefId;

use super::expr::{ThirExpr, ThirExprKind, ThirStatement};
use super::node::{ThirBinding, ThirComponent, ThirGlobal, ThirHandler, ThirNode, ThirNodeKind};
use super::visit::{ThirVisitor, walk_expr, walk_stmt};

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

/// Run signal-dependency analysis on a type-checked component and return
/// the result. The caller stores it in the `CompilerContext` side table.
pub fn check_component(
    component: &ThirComponent,
    is_signal: &impl Fn(DefId) -> bool,
) -> SignalDependencies {
    analyze(component, is_signal)
}

/// Phase 1.1c-k: run signal-dependency analysis on a type-checked global
/// and return the result. Globals have no body and no handlers — only
/// derived-signal default expressions contribute to the dependency graph —
/// so `binding_reads` and `handler_writes` stay empty and `effects_by_signal`
/// only contains `EffectSource::DerivedSignal` entries.
pub fn check_global(global: &ThirGlobal, is_signal: &impl Fn(DefId) -> bool) -> SignalDependencies {
    analyze_global(global, is_signal)
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
    // Disjoint-borrow the source map and the inverted index — no snapshot.
    let SignalDependencies {
        derived_signal_reads,
        effects_by_signal,
        ..
    } = &mut state.deps;
    for (sig_def, reads) in derived_signal_reads.iter() {
        for sig in reads {
            effects_by_signal
                .entry(*sig)
                .or_default()
                .push(EffectSource::DerivedSignal(*sig_def));
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

    // 3. Build inverted index `effects_by_signal`. Disjoint-borrow the read
    //    maps and the inverted index so neither needs a snapshot clone.
    let SignalDependencies {
        binding_reads,
        derived_signal_reads,
        effects_by_signal,
        ..
    } = &mut state.deps;
    for (idx, reads) in binding_reads.iter().enumerate() {
        for sig in reads {
            effects_by_signal
                .entry(*sig)
                .or_default()
                .push(EffectSource::Binding(idx));
        }
    }
    for (sig_def, reads) in derived_signal_reads.iter() {
        for sig in reads {
            effects_by_signal
                .entry(*sig)
                .or_default()
                .push(EffectSource::DerivedSignal(*sig_def));
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
            let mut writes = HashSet::default();
            self.collect_stmt_writes(setter, &mut writes);
            let mut writes: Vec<DefId> = writes.into_iter().collect();
            writes.sort_by_key(|d| d.index());
            self.deps.handler_writes.push(writes);
        }
    }

    fn visit_handler(&mut self, h: &ThirHandler) {
        let mut writes = HashSet::default();
        self.collect_stmt_writes(&h.body, &mut writes);
        let mut writes: Vec<DefId> = writes.into_iter().collect();
        writes.sort_by_key(|d| d.index());
        self.deps.handler_writes.push(writes);
    }

    /// Collect the deduplicated, deterministically-ordered list of
    /// signal DefIds that `expr` reads.
    fn collect_reads(&self, expr: &ThirExpr) -> Vec<DefId> {
        let mut set = HashSet::default();
        self.collect_expr_reads(expr, &mut set);
        let mut out: Vec<DefId> = set.into_iter().collect();
        out.sort_by_key(|d| d.index());
        out
    }

    fn collect_expr_reads(&self, expr: &ThirExpr, reads: &mut HashSet<DefId>) {
        collect_expr_reads(expr, self.locals, self.is_signal, reads);
    }

    fn collect_stmt_writes(&self, stmts: &[ThirStatement], writes: &mut HashSet<DefId>) {
        collect_stmt_writes(stmts, self.locals, self.is_signal, writes);
    }
}

// ============================================================================
// Shared THIR read/write walkers
// ============================================================================
//
// Single source of truth for "which signals does this expression / statement
// read or write." Both the reactivity analysis above and the
// setter-overwrites-getter lint in `typeck` call these, so the two can never
// drift — they used to be hand-copied and the copies had already diverged (the
// `typeck` copy silently dropped `Closure` / `GlobalCall` reads). `is_signal`
// decides which `DefId`s count as signals.

/// Visitor that records every signal `DefId` read by an expression / statement.
/// `is_signal` decides which defs count; locals resolve to their backing def
/// via `locals`. Drives [`collect_expr_reads`].
struct ReadCollector<'a> {
    locals: &'a LocalScope,
    is_signal: &'a dyn Fn(DefId) -> bool,
    reads: &'a mut HashSet<DefId>,
}

impl ThirVisitor for ReadCollector<'_> {
    fn visit_expr(&mut self, expr: &ThirExpr) {
        match &expr.kind {
            ThirExprKind::Def(def_id) => {
                if (self.is_signal)(*def_id) {
                    self.reads.insert(*def_id);
                }
            }
            ThirExprKind::Local(local_id) => {
                if let Some(def_id) = self.locals.get(*local_id).def_id
                    && (self.is_signal)(def_id)
                {
                    self.reads.insert(def_id);
                }
            }
            // Everything else — including closures, whose bodies are
            // conservatively folded into the surrounding context by the
            // default `visit_closure` — recurses structurally.
            _ => walk_expr(self, expr),
        }
    }

    fn visit_stmt(&mut self, stmt: &ThirStatement) {
        // An assignment *reads* its value; the assigned-to lvalue is a write,
        // not a read, so it is deliberately not visited.
        if let ThirStatement::Assign { value, .. } = stmt {
            self.visit_expr(value);
        } else {
            walk_stmt(self, stmt);
        }
    }
}

/// Visitor that records every signal `DefId` *written* by a statement list.
/// A write happens only through an assignment's top-level lvalue, so this
/// never descends into expression interiors — only `Assign` targets and the
/// statements nested in `if` branches.
struct WriteCollector<'a> {
    locals: &'a LocalScope,
    is_signal: &'a dyn Fn(DefId) -> bool,
    writes: &'a mut HashSet<DefId>,
}

impl ThirVisitor for WriteCollector<'_> {
    fn visit_stmt(&mut self, stmt: &ThirStatement) {
        match stmt {
            ThirStatement::Assign { target, .. } => match &target.kind {
                ThirExprKind::Def(def_id) => {
                    if (self.is_signal)(*def_id) {
                        self.writes.insert(*def_id);
                    }
                }
                ThirExprKind::Local(local_id) => {
                    if let Some(def_id) = self.locals.get(*local_id).def_id
                        && (self.is_signal)(def_id)
                    {
                        self.writes.insert(def_id);
                    }
                }
                _ => {}
            },
            ThirStatement::If {
                then_branch,
                else_branch,
                ..
            } => {
                for s in then_branch {
                    self.visit_stmt(s);
                }
                if let Some(els) = else_branch {
                    for s in els {
                        self.visit_stmt(s);
                    }
                }
            }
            ThirStatement::Expr(_) | ThirStatement::Let { .. } => {}
        }
    }
}

pub(crate) fn collect_expr_reads(
    expr: &ThirExpr,
    locals: &LocalScope,
    is_signal: &dyn Fn(DefId) -> bool,
    reads: &mut HashSet<DefId>,
) {
    ReadCollector {
        locals,
        is_signal,
        reads,
    }
    .visit_expr(expr);
}

pub(crate) fn collect_stmt_writes(
    stmts: &[ThirStatement],
    locals: &LocalScope,
    is_signal: &dyn Fn(DefId) -> bool,
    writes: &mut HashSet<DefId>,
) {
    let mut collector = WriteCollector {
        locals,
        is_signal,
        writes,
    };
    for stmt in stmts {
        collector.visit_stmt(stmt);
    }
}
