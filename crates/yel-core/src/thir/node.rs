//! THIR UI node types.

use rustc_hash::FxHashMap as HashMap;

use crate::ids::{DefId, LocalId, NodeId};
use crate::interner::Name;
use crate::source::Span;
use crate::types::Ty;

use super::expr::{ThirExpr, ThirStatement};

/// A THIR component definition.
#[derive(Debug, Clone)]
pub struct ThirComponent {
    /// DefId of this component.
    pub def_id: DefId,
    /// Component name.
    pub name: Name,
    /// Source span.
    pub span: Span,
    /// Whether exported.
    pub is_export: bool,
    /// Local variables (including property/signal mappings).
    pub locals: crate::hir::local_scope::LocalScope,
    /// Type-checked signal default expressions.
    /// Maps signal DefId to its type-checked default ThirExpr.
    pub signal_defaults: HashMap<DefId, ThirExpr>,
    /// UI tree body.
    pub body: Vec<ThirNode>,
}

/// A THIR global-singleton definition — a first-class [`ThirItem`] variant,
/// the peer of [`ThirComponent`].
///
/// A global carries the subset of a component that applies to a host-boundary
/// singleton: its signals (properties) and their type-checked default
/// expressions. It has no UI body, no handlers, and no DOM mount/effects —
/// derived-signal fanout is its only effect surface — so it carries no node
/// tree (that is a component-specific shape, not a missing field). Its
/// signal-dependency analysis lives in the `CompilerContext::signal_deps`
/// side table keyed by `DefId`, exactly as a component's does: signalck is a
/// read-only analysis, so its output sits with the other analysis tables for
/// both kinds of item alike, not bolted onto the node.
#[derive(Debug, Clone)]
pub struct ThirGlobal {
    /// DefId of this global.
    pub def_id: DefId,
    /// Global block name.
    pub name: Name,
    /// Source span.
    pub span: Span,
    /// Whether the global is exported across packages (mirrors
    /// `GlobalDef::is_export`).
    pub is_export: bool,
    /// Property DefIds backing the global's signals (parallel to
    /// `ThirComponent::locals` but without local-scope wiring — globals
    /// don't have a body that resolves Locals).
    pub signals: Vec<DefId>,
    /// Type-checked default expressions per signal. Same shape as
    /// `ThirComponent::signal_defaults`.
    pub signal_defaults: HashMap<DefId, ThirExpr>,
}

/// A type-checked top-level compilation unit — the THIR counterpart of
/// [`crate::hir::HirItem`]. One `type_check` entry produces these, so the
/// driver iterates a single item list instead of running components and
/// globals through separate phases.
#[derive(Debug, Clone)]
pub enum ThirItem {
    Component(ThirComponent),
    Global(ThirGlobal),
}

impl ThirItem {
    /// The component, if this item is one.
    pub fn as_component(&self) -> Option<&ThirComponent> {
        match self {
            ThirItem::Component(c) => Some(c),
            ThirItem::Global(_) => None,
        }
    }

    /// The global, if this item is one.
    pub fn as_global(&self) -> Option<&ThirGlobal> {
        match self {
            ThirItem::Global(g) => Some(g),
            ThirItem::Component(_) => None,
        }
    }

    /// Consume the item, yielding the component if it is one.
    pub fn into_component(self) -> Option<ThirComponent> {
        match self {
            ThirItem::Component(c) => Some(c),
            ThirItem::Global(_) => None,
        }
    }

    /// Consume the item, yielding the global if it is one.
    pub fn into_global(self) -> Option<ThirGlobal> {
        match self {
            ThirItem::Global(g) => Some(g),
            ThirItem::Component(_) => None,
        }
    }
}

/// A THIR UI node.
#[derive(Debug, Clone)]
pub struct ThirNode {
    pub id: NodeId,
    pub kind: ThirNodeKind,
    pub span: Span,
}

impl ThirNode {
    pub fn new(id: NodeId, kind: ThirNodeKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

/// Kind of THIR UI node.
#[derive(Debug, Clone)]
pub enum ThirNodeKind {
    /// Element or component instantiation.
    Element {
        /// Resolved component DefId (None for built-in HTML elements).
        component: Option<DefId>,
        /// Element tag name.
        tag: String,
        /// Property bindings.
        bindings: Vec<ThirBinding>,
        /// Event handlers.
        handlers: Vec<ThirHandler>,
        /// Child nodes.
        children: Vec<ThirNode>,
    },
    /// Text content.
    Text(ThirExpr),
    /// Conditional rendering.
    If {
        condition: ThirExpr,
        then_branch: Vec<ThirNode>,
        else_if_branches: Vec<(ThirExpr, Vec<ThirNode>)>,
        else_branch: Option<Vec<ThirNode>>,
    },
    /// List rendering.
    For {
        /// Loop variable.
        item: LocalId,
        /// Loop variable name.
        item_name: Name,
        /// Loop variable span.
        item_span: Span,
        /// Item type (now resolved).
        item_ty: Ty,
        /// Iterable expression.
        iterable: ThirExpr,
        /// Optional key expression.
        key: Option<ThirExpr>,
        /// Loop body.
        body: Vec<ThirNode>,
    },
    /// Slot marker — caller's child nodes splice in at this position when
    /// the enclosing component is instantiated at a call site.
    ChildrenSlot,
}

/// A typed property binding with optional getter (value) and setter.
#[derive(Debug, Clone)]
pub struct ThirBinding {
    /// Property name.
    pub name: String,
    /// Property name span.
    pub name_span: Span,
    /// Resolved property DefId (if binding to a known property).
    pub prop_def: Option<DefId>,
    /// Getter expression (value to bind to the property).
    pub value: Option<ThirExpr>,
    /// Setter body (statements to execute when property changes from outside).
    pub setter: Option<Vec<ThirStatement>>,
}

/// A typed event handler.
#[derive(Debug, Clone)]
pub struct ThirHandler {
    /// Handler name.
    pub name: String,
    /// Handler name span.
    pub name_span: Span,
    /// Optional bound payload parameter (`drop: (payload) { … }`) — the
    /// body-scoped `LocalId` the dispatched event payload is written into.
    pub param: Option<crate::ids::LocalId>,
    /// Handler body statements.
    pub body: Vec<ThirStatement>,
}
