//! THIR UI node types.

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
    /// UI tree body.
    pub body: Vec<ThirNode>,
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
    /// Handler body statements.
    pub body: Vec<ThirStatement>,
}
