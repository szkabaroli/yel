//! LIR UI node types.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::ids::{BlockId, DefId, ForId, LocalId, NodeId};
use crate::interner::Name;
use crate::lir::block::{ComponentTreeShape, ForContext};
use crate::source::Span;
use crate::types::Ty;

use super::block::{LirBlock, LirBlockEffect, LirSlotId, LirSlotInfo, StringId, ExprId};
use super::expr::{LirExpr, LirStatement};
use super::signal::LirSignal;

/// A LIR component definition (ready for codegen).
///
/// This is a block-based representation where:
/// - UI operations are explicit instructions (LirOp in blocks)
/// - Branches become separate blocks with mount/unmount operations
/// - Storage is pre-allocated (SlotId for temps and memory)
/// - Strings and expressions are interned (StringId, ExprId)
#[derive(Debug, Clone, Serialize, Deserialize)]
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
    pub slots: Vec<LirSlotInfo>,
    /// Interned strings (tag names, attribute names, text content).
    pub strings: Vec<String>,
    /// Pre-lowered expressions (conditions, values, etc.).
    pub exprs: Vec<LirExpr>,

    // === Signal interface ===
    /// Signals (kept for external interface and WIT generation).
    pub signals: Vec<LirSignal>,

    /// SlotId whose DOM node the component's `@children` placeholder sits
    /// under — i.e. the *children-root* returned by `mount`. `None` for
    /// non-container components (no `@children` in the body).
    pub children_root_slot: Option<LirSlotId>,

    /// Handler blocks that were synthesized from a `set value: { body }`
    /// binding on a bindable `Input` element. Maps the block id to the
    /// target signal DefId whose value must be written from the DOM
    /// input-event payload before the user body runs. Used by codegen
    /// to emit the coercion + SignalWrite preamble in the handler
    /// block. Blocks absent from this map are regular (click/etc.)
    /// handlers and need no preamble.
    pub input_binding_handlers: HashMap<BlockId, DefId>,

    /// One entry per `for` loop, keyed by `ForId`. Carries the for's
    /// DOM parent/anchor memory slots, optional range-item scratch buf,
    /// and the set of effect / nested-for ids whose state is hoisted
    /// into its GC iteration record. Consumed by GC type emission and
    /// fan-out update_blocks.
    pub for_contexts: Vec<ForContext>,

    /// Inverted index of effect dependencies: for each signal DefId,
    /// the IDs of effects that should re-fire when the signal changes.
    /// Built during lowering; codegen reads instead of linear-scanning
    /// `effects` on every signal write. Map value is `Vec<u32>` of
    /// `LirBlockEffect::id`. Codegen still resolves
    /// `effect.id → update_block` via the existing path since
    /// `update_block` is a codegen-time concept after `block_lower`.
    pub effects_by_signal: HashMap<DefId, Vec<u32>>,
    /// The component body as a tree of `LirNode`s — the same tree that
    /// `lower::lower_component_to_tree` produced before `block_lower`
    /// flattened it into block ops. Preserved here so downstream passes
    /// (mount-tree GC type synthesis, retargeting backends) can walk
    /// the structural shape directly without reconstructing it from
    /// the block ops.
    pub body_tree: Vec<LirNode>,

    /// Concrete-typed mount-tree shape: one boundary per emitted GC
    /// struct type for the component (root + each `if` anchor + each
    /// `if` branch + each `for` anchor + each `for` iter-body).
    /// Synthesized from `body_tree` by `tree_shape::synthesize`.
    pub tree_shape: ComponentTreeShape,
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
    ///
    /// After structural dedupe, `BlockId.0` is no longer guaranteed
    /// to equal the block's index in `self.blocks` (some duplicate
    /// blocks have been spliced out and their `CallBlock` references
    /// rewritten to a canonical survivor). Fall back to a linear
    /// scan when the fast-path index doesn't match.
    pub fn get_block(&self, id: BlockId) -> &LirBlock {
        let idx = id.0 as usize;
        if let Some(b) = self.blocks.get(idx) {
            if b.id == id {
                return b;
            }
        }
        self.blocks
            .iter()
            .find(|b| b.id == id)
            .unwrap_or_else(|| panic!("get_block: BlockId {:?} not found in component", id))
    }

    /// An empty component used as a carrier for module-scope expression
    /// emission (e.g. lowering global-singleton property defaults).
    ///
    /// The `signals` vector is empty, so any `SignalRead`/`SignalWrite` that
    /// leaks into module scope falls through to the module's
    /// `global_property_addrs` lookup. `blocks` contains a single empty
    /// placeholder block so `constructor_block` / `mount_block` remain valid
    /// indices — nothing actually executes them in module scope.
    pub fn empty_module_carrier(name: Name) -> Self {
        use super::block::LirBlock;
        let placeholder = LirBlock::new(BlockId(0));
        Self {
            def_id: DefId::INVALID,
            name,
            span: Span::default(),
            is_export: false,
            blocks: vec![placeholder],
            constructor_block: BlockId(0),
            mount_block: BlockId(0),
            effects: Vec::new(),
            slots: Vec::new(),
            strings: Vec::new(),
            exprs: Vec::new(),
            signals: Vec::new(),
            children_root_slot: None,
            input_binding_handlers: HashMap::new(),
            for_contexts: Vec::new(),
            effects_by_signal: HashMap::new(),
            body_tree: Vec::new(),
            tree_shape: super::block::ComponentTreeShape::default(),
        }
    }
}

/// A LIR UI node.
#[derive(Debug, Clone, Serialize, Deserialize)]
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
#[derive(Debug, Clone, Serialize, Deserialize)]
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
        /// Stable identifier for this for-loop, minted at tree lowering.
        /// Effects whose expressions are lowered while this for was in
        /// scope are tagged with the same id so their update_blocks can
        /// fan out across this for's tracking array at runtime.
        for_id: ForId,
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
    /// Caller-children slot marker. The parent slot at this position
    /// becomes the component's exported children-root; caller's children
    /// (at each instantiation) append under that parent at mount time.
    /// Semantics land in Phase 3 — Phase 1 just threads the AST through.
    ChildrenSlot,
}

/// A static property binding (value known at creation).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirBinding {
    /// Attribute/property name.
    pub name: String,
    /// Static value expression.
    pub value: LirExpr,
}

/// An event handler.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirHandler {
    /// Event name (e.g., "click", "input").
    pub event: String,
    /// Handler body statements.
    pub body: Vec<LirStatement>,
    /// For `input`-family handlers synthesized from a `set value: { ... }`
    /// binding on a bindable element (e.g. `Input`), the DefId of the
    /// target signal that the DOM's current value must be coerced and
    /// written into before the user body runs. `None` for handler
    /// sources that don't participate in input-binding auto-sync
    /// (`clicked`, `hovered`, freestanding `input` handlers etc.).
    pub input_binding_target: Option<DefId>,
}
