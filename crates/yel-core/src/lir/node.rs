//! LIR UI node types.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::ids::{BlockId, DefId, ForId, LocalId, NodeId};
use crate::interner::Name;
use crate::lir::block::{ComponentTreeShape, ForContext};
use crate::lir::struct_types::{LirArrayTypeDecl, LirStructTypeDecl};
use crate::source::Span;
use crate::types::Ty;

use super::arena::{LirComponentArena, LirExprArena, LirSlotArena, LirStringArena};
use super::block::{LirBlock, LirBlockEffect, LirSlotId, LirSlotInfo, StringId, ExprId};
use super::expr::{LirExpr, LirStatement};
use super::signal::LirSignal;
use super::signal_layout::SignalLayout;

/// Cached per-valtype flat-scratch counts for the codegen-synthesized
/// internal lifecycle wrappers (constructor / mount / unmount).
///
/// Each tuple is `(i32, i64, f32, f64)` and maps directly onto the
/// declared local block of the emitted internal-lifecycle WASM
/// function. Populated during post-lowering by
/// `populate_internal_lifecycle_scratch` (Phase 0.3d of the
/// lir-resource-flatten plan), so codegen can read these instead of
/// re-walking each component's signal types. Phase 0.3e will use these
/// values when synthesizing the internal lifecycle bodies as
/// `LirBlock`s to populate their `max_flat_scratch_counts` directly.
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize)]
pub struct InternalLifecycleScratch {
    /// Constructor wrapper scratch (i32, i64, f32, f64). Sized to the
    /// max per-valtype canonical-ABI flat count across all non-Func
    /// signals on the component, matching the historical
    /// `flatten_core_slots(s.ty)` walk in
    /// `generate_constructor_internal_for`.
    pub ctor: (u32, u32, u32, u32),
    /// Mount wrapper scratch (i32, i64, f32, f64). Today already
    /// equal to `LirBlock.max_flat_scratch_counts` on the mount block;
    /// cached here for symmetry / Phase 0.3e consumers.
    pub mount: (u32, u32, u32, u32),
    /// Unmount wrapper scratch (i32, i64, f32, f64). The current
    /// internal unmount body uses zero scratch — the field exists for
    /// shape parity with ctor/mount.
    pub unmount: (u32, u32, u32, u32),
}

impl LirExprArena for LirResource {
    fn expr(&self, id: ExprId) -> &LirExpr {
        &self.exprs[id.0 as usize]
    }
}

impl LirStringArena for LirResource {
    fn string(&self, id: StringId) -> &str {
        &self.strings[id.0 as usize]
    }
}

impl LirSlotArena for LirResource {
    fn slots(&self) -> &[LirSlotInfo] {
        &self.slots
    }
}

impl LirComponentArena for LirResource {
    fn def_id(&self) -> DefId {
        self.def_id
    }
    fn name(&self) -> Name {
        self.name
    }
    fn is_export(&self) -> bool {
        self.is_export
    }
    fn blocks(&self) -> &[LirBlock] {
        &self.blocks
    }
    fn struct_types(&self) -> &[LirStructTypeDecl] {
        &self.struct_types
    }
    fn array_types(&self) -> &[LirArrayTypeDecl] {
        &self.array_types
    }
}

/// A LIR component definition (ready for codegen).
///
/// This is a block-based representation where:
/// - UI operations are explicit instructions (LirOp in blocks)
/// - Branches become separate blocks with mount/unmount operations
/// - Storage is pre-allocated (SlotId for temps and memory)
/// - Strings and expressions are interned (StringId, ExprId)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirResource {
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
    /// Phase 0.3e: synthesized internal constructor block. Wraps
    /// `constructor_block` with the per-component GC struct allocation,
    /// tree-root init, and memory-slot zero-init that codegen's
    /// `generate_constructor_internal_for` used to inline. When `Some`,
    /// codegen sources its op stream from this block instead of
    /// re-doing the inline emission. Populated by
    /// `synth_internal_constructor_block` during lowering; `None` on
    /// the empty-module carrier or when component has no signals.
    pub internal_constructor_block: Option<BlockId>,
    /// Phase 0.3g: companion to `internal_constructor_block`. Slot
    /// holding the freshly-allocated `(ref null $Comp_<i>)` so codegen
    /// can emit a trailing `local.get $self_ref` for the function
    /// return. `LirSlotKind::Temp` + `LirSlotValType::RefNullForComponent(def_id)`.
    pub internal_constructor_self_ref_slot: Option<LirSlotId>,

    /// Phase 0.3h: synthesized internal unmount block. Contains the
    /// per-detach sequence (DOM handle load + `call $remove`) for every
    /// memory-backed slot and DomHandle-role BoundaryField slot that's
    /// reachable through the typed `$self.tree` chain. Codegen walks
    /// this block instead of generating the detach loop inline.
    pub internal_unmount_block: Option<BlockId>,

    /// Phase 0.3m: synthesized export-wrapper blocks (host-facing
    /// entry points). `None` for non-exported components — codegen
    /// falls back to the inline ctor wrapper for those.
    pub export_constructor_block: Option<BlockId>,
    pub export_mount_block: Option<BlockId>,
    pub export_unmount_block: Option<BlockId>,
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

    /// Stage 2 (lir-resource-flatten plan): flat-list registry of GC
    /// struct types this resource owns. Today populated alongside
    /// `tree_shape` (one entry per `TreeBoundary`); both are read by
    /// downstream code, with `tree_shape` still authoritative. Stage 3
    /// will rewrite codegen to read this directly; Stage 5 deletes
    /// `tree_shape`.
    pub struct_types: Vec<LirStructTypeDecl>,
    /// Stage 2: flat-list registry of GC array types this resource
    /// owns. Today only for-anchor children arrays land here.
    pub array_types: Vec<LirArrayTypeDecl>,

    /// Phase 1.1a: per-signal storage layout (where each signal lives —
    /// in the component's `$Comp_<i>` GC struct or in linear memory).
    /// Computed at the end of THIR→LIR lowering by
    /// `signal_layout::compute_signal_layout`. Codegen reads this
    /// instead of rederiving the same information in
    /// `GcTypeLayout::signal_field_paths` + `MemoryLayout::signal_offsets`.
    pub signal_layout: SignalLayout,

    /// Phase 0.3d (lir-resource-flatten plan): cached flat-scratch
    /// counts for the codegen-synthesized internal lifecycle
    /// wrappers. See [`InternalLifecycleScratch`]. Populated by
    /// `populate_internal_lifecycle_scratch` at the end of
    /// `BlockLowering::lower_component`. Codegen cross-checks its
    /// per-function recomputation against this in debug builds.
    pub internal_lifecycle_scratch: InternalLifecycleScratch,

    /// Phase 0.3f: lifted component-struct (`$Comp_<i>`) field layout
    /// — moved out of codegen's `GcTypeLayout` so the LIR can name
    /// component-struct fields by index without consulting codegen
    /// state. Field-layout invariant in order:
    ///   1. signal fields (`signal_layout` knows the count)
    ///   2. parent-retention fields (`parent_retention_count` entries
    ///      starting at `parent_retention_field_base`)
    ///   3. self-handle field at `self_handle_field_idx`
    ///   4. tree-root field at `tree_root_field_idx` (when set)
    pub comp_struct_layout: ComponentStructLayout,
}

/// Component-struct (`$Comp_<i>`) field-index layout. Computed during
/// lowering, mirrors the field order `emit_component_struct_type` will
/// produce. Lets neutral LIR ops reference component-struct fields by
/// index without reaching into codegen-side `GcTypeLayout`.
///
/// Field order (from `gc_types::emit_component_struct_type`):
/// signals → parent-retention region → self-handle → tree-root.
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize)]
pub struct ComponentStructLayout {
    /// Number of `MountComponent` sites (outside for-loop bodies)
    /// summed across this component's blocks. Mirrors
    /// `compute_mount_retention_counts` in codegen.
    pub parent_retention_count: u32,
    /// First struct-field index of the parent-retention region in
    /// `$Comp_<i>`. `None` when `parent_retention_count == 0`.
    pub parent_retention_field_base: Option<u32>,
    /// Index of the trailing `(mut i32)` field on `$Comp_<i>` that
    /// caches the host's resource handle returned by `[resource-new]`.
    /// Always present.
    pub self_handle_field_idx: u32,
    /// Index of the trailing `(mut (ref null <comp>_tree_root))` field
    /// on `$Comp_<i>`. `None` when the component has no body tree
    /// (e.g. `empty_module_carrier`).
    pub tree_root_field_idx: Option<u32>,
}

impl LirResource {
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
            internal_constructor_block: None,
            internal_constructor_self_ref_slot: None,
            internal_unmount_block: None,
            export_constructor_block: None,
            export_mount_block: None,
            export_unmount_block: None,
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
            struct_types: Vec::new(),
            array_types: Vec::new(),
            signal_layout: SignalLayout::default(),
            internal_lifecycle_scratch: InternalLifecycleScratch::default(),
            comp_struct_layout: ComponentStructLayout::default(),
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
