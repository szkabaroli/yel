//! LIR UI node types.

use crate::ids::{DefId, LocalId, NodeId};
use crate::interner::Name;
use crate::source::Span;
use crate::types::Ty;

use super::block::{BlockId, LirBlock, LirBlockEffect, SlotInfo, StringId, ExprId};
use super::expr::{LirExpr, LirStatement};
use super::signal::LirSignal;

/// A LIR component definition (ready for codegen).
///
/// This is a block-based representation where:
/// - UI operations are explicit instructions (LirOp in blocks)
/// - Branches become separate blocks with mount/unmount operations
/// - Storage is pre-allocated (SlotId for temps and memory)
/// - Strings and expressions are interned (StringId, ExprId)
#[derive(Debug, Clone)]
pub struct LirComponent {
    /// DefId of this component.
    pub def_id: DefId,
    /// Component name.
    pub name: Name,
    /// Source span.
    pub span: Span,
    /// Whether exported.
    pub is_export: bool,

    // === Block-based structure ===
    /// All blocks in this component.
    pub blocks: Vec<LirBlock>,
    /// Entry point block for constructing/initializing the component.
    /// Initializes signals with default values and memory slots to zero.
    pub constructor_block: BlockId,
    /// Entry point block for mounting the component.
    pub mount_block: BlockId,
    /// Effects that connect signals to update blocks.
    pub effects: Vec<LirBlockEffect>,

    // === Pre-computed layout ===
    /// All slots (temps and memory locations).
    pub slots: Vec<SlotInfo>,
    /// Interned strings (tag names, attribute names, text content).
    pub strings: Vec<String>,
    /// Pre-lowered expressions (conditions, values, etc.).
    pub exprs: Vec<LirExpr>,

    // === Signal interface ===
    /// Signals (kept for external interface and WIT generation).
    pub signals: Vec<LirSignal>,
}

impl LirComponent {
    /// Get a string by its ID.
    pub fn get_string(&self, id: StringId) -> &str {
        &self.strings[id.0 as usize]
    }

    /// Get an expression by its ID.
    pub fn get_expr(&self, id: ExprId) -> &LirExpr {
        &self.exprs[id.0 as usize]
    }

    /// Get a block by its ID.
    pub fn get_block(&self, id: BlockId) -> &LirBlock {
        &self.blocks[id.0 as usize]
    }
}

/// A LIR UI node.
#[derive(Debug, Clone)]
pub struct LirNode {
    pub id: NodeId,
    pub kind: LirNodeKind,
    pub span: Span,
}

impl LirNode {
    pub fn new(id: NodeId, kind: LirNodeKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

/// Kind of LIR UI node.
#[derive(Debug, Clone)]
pub enum LirNodeKind {
    /// Element or component instantiation.
    Element {
        /// Resolved component DefId (None for built-in HTML elements).
        component: Option<DefId>,
        /// HTML tag name.
        tag: String,
        /// Static bindings (no reactivity).
        static_bindings: Vec<LirBinding>,
        /// Dynamic bindings (with reactivity - handled by effects).
        dynamic_binding_ids: Vec<u32>,
        /// Event handlers.
        handlers: Vec<LirHandler>,
        /// Child nodes.
        children: Vec<LirNode>,
    },
    /// Static text content.
    StaticText(String),
    /// Dynamic text content (handled by effect).
    DynamicText { effect_id: u32 },
    /// Conditional rendering.
    If {
        condition: LirExpr,
        then_branch: Vec<LirNode>,
        else_if_branches: Vec<(LirExpr, Vec<LirNode>)>,
        else_branch: Option<Vec<LirNode>>,
    },
    /// List rendering.
    For {
        /// Loop variable.
        item: LocalId,
        /// Loop variable name.
        item_name: Name,
        /// Loop variable span.
        item_span: Span,
        /// Item type.
        item_ty: Ty,
        /// Iterable expression.
        iterable: LirExpr,
        /// Optional key expression.
        key: Option<LirExpr>,
        /// Loop body.
        body: Vec<LirNode>,
    },
}

/// A static property binding (value known at creation).
#[derive(Debug, Clone)]
pub struct LirBinding {
    /// Attribute/property name.
    pub name: String,
    /// Static value expression.
    pub value: LirExpr,
}

/// An event handler.
#[derive(Debug, Clone)]
pub struct LirHandler {
    /// Event name (e.g., "click", "input").
    pub event: String,
    /// Handler body statements.
    pub body: Vec<LirStatement>,
}
