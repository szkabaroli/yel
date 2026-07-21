//! HIR UI node types.

use crate::ids::{DefId, LocalId, NodeId};
use crate::interner::Name;
use crate::source::Span;
use crate::types::Ty;
use serde::Serialize;

use super::expr::{HirExpr, HirStatement};

/// A HIR component definition.
#[derive(Debug, Clone, Serialize)]
pub struct HirComponent {
    /// DefId of this component.
    pub def_id: DefId,
    /// Component name.
    pub name: Name,
    /// Source span.
    pub span: Span,
    /// Whether exported.
    pub is_export: bool,
    /// UI tree body.
    pub body: Vec<HirNode>,
}

/// A HIR global-singleton definition.
///
/// A global is a host-boundary singleton: it has no UI body, so unlike
/// [`HirComponent`] it carries no node tree. Its property defaults and
/// callback signatures live in the `GlobalDef` registered during HIR
/// lowering (`register_global`); this handle is what lets a global flow
/// through the same `lower_to_hir → type_check → lower` spine as a
/// component instead of being pulled from a side table.
#[derive(Debug, Clone, Serialize)]
pub struct HirGlobal {
    /// DefId of this global.
    pub def_id: DefId,
    /// Global name.
    pub name: Name,
    /// Source span.
    pub span: Span,
    /// Whether exported across packages.
    pub is_export: bool,
}

/// A top-level compilation unit in HIR. Both variants are reactive units
/// the back-end lowers to a GC struct + init; a component additionally
/// has a UI body and a host-driven lifecycle, a global is a host-boundary
/// surface. They share one pipeline from here on.
#[derive(Debug, Clone, Serialize)]
pub enum HirItem {
    Component(HirComponent),
    Global(HirGlobal),
}

impl HirItem {
    /// The component, if this item is one.
    pub fn as_component(&self) -> Option<&HirComponent> {
        match self {
            HirItem::Component(c) => Some(c),
            HirItem::Global(_) => None,
        }
    }
}

/// A HIR UI node.
#[derive(Debug, Clone, Serialize)]
pub struct HirNode {
    pub id: NodeId,
    pub kind: HirNodeKind,
    pub span: Span,
}

impl HirNode {
    pub fn new(id: NodeId, kind: HirNodeKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

/// Kind of HIR UI node.
#[derive(Debug, Clone, Serialize)]
pub enum HirNodeKind {
    /// Element or component instantiation.
    Element {
        /// Element/component name.
        name: String,
        /// Property bindings.
        bindings: Vec<HirBinding>,
        /// Event handlers.
        handlers: Vec<HirHandler>,
        /// Child nodes.
        children: Vec<HirNode>,
    },
    /// Text content.
    Text(HirExpr),
    /// Conditional rendering.
    If {
        condition: HirExpr,
        then_branch: Vec<HirNode>,
        else_if_branches: Vec<(HirExpr, Vec<HirNode>)>,
        else_branch: Option<Vec<HirNode>>,
    },
    /// List rendering.
    For {
        /// Loop variable (LocalId allocated in scope).
        item: LocalId,
        /// Loop variable name (stored directly to avoid LocalScope lookup issues).
        item_name: Name,
        /// Loop variable span.
        item_span: Span,
        /// Item type (inferred or annotated).
        item_ty: Ty,
        /// Iterable expression.
        iterable: HirExpr,
        /// Optional key expression.
        key: Option<HirExpr>,
        /// Loop body.
        body: Vec<HirNode>,
    },
    /// Caller-children slot marker (`@children`). When the enclosing
    /// component is used at a call site, the caller's child nodes splice
    /// in at this position.
    ChildrenSlot,
}

/// A property binding with optional getter (value) and setter.
#[derive(Debug, Clone, Serialize)]
pub struct HirBinding {
    /// Property name.
    pub name: String,
    /// Property name span.
    pub name_span: Span,
    /// Getter expression (value to bind to the property).
    pub value: Option<HirExpr>,
    /// Setter body (statements to execute when property changes from outside).
    pub setter: Option<Vec<HirStatement>>,
}

/// An event handler.
#[derive(Debug, Clone, Serialize)]
pub struct HirHandler {
    /// Handler name (e.g., "onclick", "clicked").
    pub name: String,
    /// Handler name span.
    pub name_span: Span,
    /// Optional bound payload parameter (`drop: { payload -> … }`) — the
    /// interned param name and its span. HIR lowering defines a body-scoped
    /// `string` local under this name so references resolve; typeck
    /// re-defines it in its own scope (mirroring the for-loop `item`
    /// pattern) to produce the THIR `LocalId` with matching arena parity.
    pub param: Option<(Name, Span)>,
    /// Handler body statements.
    pub body: Vec<HirStatement>,
}
