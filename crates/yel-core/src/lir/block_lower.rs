//! Block Lowering Pass
//!
//! Converts tree-based LIR (`TreeLirComponent`) to block-based LIR (`LirComponent`).
//!
//! The lowering process:
//! 1. Walks the UI tree and emits LirOp instructions
//! 2. Interns strings (tag names, text content, attributes)
//! 3. Interns expressions (conditions, values)
//! 4. Allocates slots (temps for DOM handles, memory for persistent state)
//! 5. Creates blocks for branches (if/else) and handlers

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use super::block::{
    BoundaryDepIndex, ExprId, LirBlock, LirBlockEffect, LirOp, LirSlotId, LirSlotInfo, LirSlotKind,
    PendingBinding, PendingBindingKind, LirSlotValType, StringId,
};
use super::expr::{LirExpr, LirExprKind, LirStatement};
use super::lower::TreeLirComponent;
use super::node::{LirComponent, LirHandler, LirNode, LirNodeKind};
use super::signal::{LirEffect, LirSignal, UpdateKind};
use crate::context::CompilerContext;
use crate::definitions::DefKind;
use crate::hir::expr::BinOp;
use crate::ids::{BlockId, DefId, ForId, LocalId, TreeBoundaryId};
use crate::lir::block::{
    ComponentTreeShape, ForContext, TreeBoundary, TreeBoundaryKind, TreeFieldDecl,
};
use crate::lir::dedupe::dedupe_update_blocks;
use crate::lir::tree_shape::{synthesize, IterSource};
use crate::lir::{LirBindingMode, LirLayoutContext};
use crate::types::{InternedTyKind, Ty};
use crate::{BlockDebugName, NodeId};

/// Phase 5e.1: structural DTR-record check, free-function form for use
/// inside closures that only capture `&CompilerContext`. Mirrors the
/// `is_dtr_record_ty` method on `BlockLowering`.
fn is_dtr_record_struct(
    ctx: &crate::context::CompilerContext,
    def_id: crate::DefId,
) -> bool {
    let mut seen = std::collections::HashSet::new();
    is_dtr_record_struct_inner(ctx, def_id, &mut seen)
}

fn is_dtr_record_struct_inner(
    ctx: &crate::context::CompilerContext,
    def_id: crate::DefId,
    seen: &mut std::collections::HashSet<crate::DefId>,
) -> bool {
    let record = match ctx.defs.kind(def_id) {
        DefKind::Record(r) => r.clone(),
        _ => return false,
    };
    if !seen.insert(def_id) {
        return true;
    }
    let result = (|| {
        for &field_def_id in &record.fields {
            let field_ty = match ctx.defs.kind(field_def_id) {
                DefKind::Field(f) => f.ty,
                _ => return false,
            };
            if !is_dtr_field_ty_struct(ctx, field_ty, seen) {
                return false;
            }
        }
        true
    })();
    seen.remove(&def_id);
    result
}

fn is_dtr_field_ty_struct(
    ctx: &crate::context::CompilerContext,
    ty: crate::types::Ty,
    seen: &mut std::collections::HashSet<crate::DefId>,
) -> bool {
    if matches!(
        ctx.ty_kind(ty),
        InternedTyKind::Bool
        | InternedTyKind::S8 | InternedTyKind::S16 | InternedTyKind::S32
        | InternedTyKind::U8 | InternedTyKind::U16 | InternedTyKind::U32
        | InternedTyKind::S64 | InternedTyKind::U64
        | InternedTyKind::F32 | InternedTyKind::F64
        | InternedTyKind::Char
    ) || matches!(ctx.ty_kind(ty), InternedTyKind::Adt(d) if matches!(ctx.defs.kind(*d), DefKind::Enum(_)))
    {
        return true;
    }
    match ctx.ty_kind(ty) {
        InternedTyKind::String => true,
        InternedTyKind::List(_) => is_scalar_list_ty_struct(ctx, ty, seen),
        InternedTyKind::Adt(d) => match ctx.defs.kind(*d) {
            DefKind::Record(_) => is_dtr_record_struct_inner(ctx, *d, seen),
            _ => false,
        },
        _ => false,
    }
}

fn is_scalar_list_ty_struct(
    ctx: &crate::context::CompilerContext,
    ty: crate::types::Ty,
    seen: &mut std::collections::HashSet<crate::DefId>,
) -> bool {
    let elem = match ctx.ty_kind(ty) {
        InternedTyKind::List(e) => *e,
        _ => return false,
    };
    if matches!(
        ctx.ty_kind(elem),
        InternedTyKind::Bool
        | InternedTyKind::S8 | InternedTyKind::S16 | InternedTyKind::S32
        | InternedTyKind::U8 | InternedTyKind::U16 | InternedTyKind::U32
        | InternedTyKind::S64 | InternedTyKind::U64
        | InternedTyKind::F32 | InternedTyKind::F64
        | InternedTyKind::Char
    ) || matches!(ctx.ty_kind(elem), InternedTyKind::Adt(d) if matches!(ctx.defs.kind(*d), DefKind::Enum(_)))
    {
        return true;
    }
    if matches!(ctx.ty_kind(elem), InternedTyKind::List(_))
        && is_scalar_list_ty_struct(ctx, elem, seen)
    {
        return true;
    }
    if matches!(ctx.ty_kind(elem), InternedTyKind::String) {
        return true;
    }
    if let InternedTyKind::Option(inner) = ctx.ty_kind(elem) {
        let inner_ty = *inner;
        let inner_fits = matches!(
            ctx.ty_kind(inner_ty),
            InternedTyKind::Bool
            | InternedTyKind::S8 | InternedTyKind::S16 | InternedTyKind::S32
            | InternedTyKind::U8 | InternedTyKind::U16 | InternedTyKind::U32
            | InternedTyKind::F32 | InternedTyKind::Char
        ) || matches!(
            ctx.ty_kind(inner_ty),
            InternedTyKind::Adt(d) if matches!(ctx.defs.kind(*d), DefKind::Enum(_))
        );
        if inner_fits {
            return true;
        }
    }
    if let InternedTyKind::Adt(d) = ctx.ty_kind(elem) {
        if matches!(ctx.defs.kind(*d), DefKind::Record(_)) {
            return is_dtr_record_struct_inner(ctx, *d, seen);
        }
    }
    false
}

/// Classification of a for-loop iterable expression.
enum IterableKind {
    /// Iterable is a signal (state variable) - reactive, creates update effects.
    Signal(DefId),
    /// Iterable is an expression that evaluates to a list.
    /// Includes literals like `[0, 1]` and field accesses like `item.children`.
    /// Evaluated at mount time, produces (ptr, len).
    Expr { expr_id: ExprId },
    /// Iterable is a range expression (start..end or start..=end).
    /// Generates integers from start to end (exclusive or inclusive).
    Range {
        start: ExprId,
        end: ExprId,
        inclusive: bool,
    },
    /// Unsupported iterable expression.
    Unsupported,
}

/// State for the block lowering pass.
pub(crate) struct BlockLowering<'a> {
    ctx: &'a CompilerContext,
    /// DefId of the component being lowered (for block naming).
    component_id: DefId,

    // Input - tree-based effects and signals for lookup
    tree_effects: &'a [LirEffect],
    tree_signals: &'a [LirSignal],

    // Output
    blocks: Vec<LirBlock>,
    effects: Vec<LirBlockEffect>,
    slots: Vec<LirSlotInfo>,
    strings: Vec<String>,
    string_map: HashMap<String, StringId>,
    exprs: Vec<LirExpr>,

    // Allocation counters
    next_slot: u32,
    /// Running compacted WASM-local index. Bumped only by Temp-slot
    /// allocation; Memory slots do NOT consume a local slot. This
    /// decouples `SlotId` (unique id, may have gaps from memory slots)
    /// from the emitted WASM local index space (dense 0..N).
    next_local_idx: u32,
    next_block: u32,
    next_memory_offset: u32,

    // Current block being built
    current_ops: Vec<LirOp>,
    // Stack for nested block creation
    ops_stack: Vec<Vec<LirOp>>,

    // For-loop item bindings: LocalId -> (SlotId containing item value/ptr,
    // item type, BindingMode).
    //
    // The mode controls how the codegen reads the binding: `Ptr` (today's
    // default for every entry) means the slot holds a byte address and a
    // typed load is emitted after `local.get`; `Value` means the slot
    // already holds the scalar value. Phase 5b-v.2 only inserts `Ptr`;
    // 5b-v.3 will switch migrated-list iter bindings to `Value`.
    local_bindings: HashMap<LocalId, (LirSlotId, Ty, LirBindingMode)>,

    // Active outer item field slots for nested blocks.
    // Maps LocalId -> (Type, BoundaryField SlotId, BindingMode).
    // BindingMode::Ptr for memory/range items; Value for GC-list items.
    outer_item_field_slots: HashMap<LocalId, (Ty, LirSlotId, LirBindingMode)>,

    // Locals defined in the current block being built.
    // Used to populate block's local_to_slot when finishing.
    current_block_locals: Vec<LocalId>,
    // Stack for nested block creation (mirrors ops_stack).
    block_locals_stack: Vec<Vec<LocalId>>,

    // Records the parent SlotId at the position where `@children` appears
    // in the component body. Set when we lower `LirNodeKind::ChildrenSlot`;
    // read into `LirComponent.children_root_slot` after body lowering
    // finishes. `None` means non-container component.
    children_root_slot: Option<LirSlotId>,

    // Map of handler blocks synthesized from `set value: { ... }` binding
    // setters to the target signal that the DOM's input value must be
    // written into before the user body runs.
    input_binding_handlers: HashMap<BlockId, DefId>,

    // Enclosing-for context tracked while lowering the body.
    // Populated by `lower_for` (push on entry, pop on exit). Read by
    // helpers that need to know the innermost for at emission time
    // (e.g. `current_enclosing_boundary` for derived-signal effects).
    for_stack: Vec<ForId>,

    // Parallel stack to `for_stack`: the synthesized ForIterBody
    // boundary id of each enclosing for. Used by `lower_if` to
    // thread the iter-body ref into nested if-branch / if-update
    // block boundary_params, so the loop variable read inside the
    // if resolves via `BoundaryField` on the iter-body (rather
    // than the legacy linear-memory item slot).
    for_iter_body_stack: Vec<TreeBoundaryId>,

    // For each for-loop item LocalId currently in scope, the
    // synthesized `ForIterBody` boundary id whose field 0 stores
    // the loop variable. Populated in `create_for_item_mount_block`
    // when the for binds its item; consulted by nested for-loops
    // (via `lower_for`) to materialize a `BoundaryField` slot for
    // each enclosing for's loop variable instead of routing the
    // value through linear-memory storage.
    for_item_iter_body: HashMap<LocalId, TreeBoundaryId>,

    // One entry per for-loop lowered, keyed by ForId. Populated by
    // `lower_for` via a pre-pass over the for's body. Drained into
    // `LirComponent.for_contexts` when the component finishes.
    for_contexts: HashMap<ForId, ForContext>,

    // Monotonic counter for synthesizing stable if-label names. No
    // IfId exists in the IR (ifs aren't referenced after lowering),
    // so block_lower mints a running id for debug labels only.
    next_if_label_id: u32,

    // Concrete-typed mount-tree shape, synthesized before body
    // lowering. Used to allocate `SlotKind::BoundaryField` slots
    // whose `(boundary_id, field_idx)` pairs come from
    // `tree_shape.node_field` keyed by `LirNode.id`. Populated in
    // `lower_component` and moved into the resulting `LirComponent`
    // when lowering finishes.
    tree_shape: ComponentTreeShape,

    // Dynamic-binding metadata collected during body lowering. Each
    // dynamic site (attr/text/structural) appends one entry. The
    // collection drives both `build_boundary_dep_index` and the
    // per-(boundary, signal) update-fn emitter.
    binding_collector: Vec<PendingBinding>,

    // Monotonic id source for `PendingBinding.binding_id`. Started
    // above any plausible effect id (1_000_000) so the inline-binding
    // counter ids cannot collide with the real effect ids that
    // structural bindings still carry as their `binding_id`. The
    // dedupe pass in `emit_per_boundary_signal_updates` uses a single
    // id-set across both inline and structural kinds, so this offset
    // is load-bearing.
    next_binding_id: u32,

    // Per-component dependency index over tree boundaries. Built once
    // by `build_boundary_dep_index` after body lowering and consumed
    // by `emit_per_boundary_signal_updates`. `None` until built.
    boundary_dep_index: Option<BoundaryDepIndex>,

    // Per-AttrSet binding data captured at emission time, keyed by the
    // matching `PendingBinding.binding_id`. Holds the inputs needed to
    // inline a `LoadHandle` + `SetAttribute` pair into a
    // per-(boundary, signal) update fn body.
    attr_binding_data: HashMap<u32, AttrBindingInfo>,

    // Per-DynamicText binding data captured at emission time. Same
    // indexing scheme as `attr_binding_data`. Inline body is
    // `LoadHandle` + `SetTextContent`.
    dyntext_binding_data: HashMap<u32, DynTextBindingInfo>,

    // Structural bindings (if-cond reroute, for-list reroute,
    // derived-signal recompute) whose update_block is dispatched into
    // via `CallBlock` from the per-(boundary, signal) walker. Each
    // block carries the full branch-transition / list-diff /
    // derived-signal logic on its own; the walker just routes the
    // call site.
    ifcond_binding_data: HashMap<u32, StructuralBindingInfo>,
    forlist_binding_data: HashMap<u32, StructuralBindingInfo>,
    derivedsig_binding_data: HashMap<u32, StructuralBindingInfo>,
}

#[derive(Debug, Clone)]
struct AttrBindingInfo {
    /// Owning boundary recorded at emission time. Currently unused at
    /// the consumer (the emitter recovers the boundary by re-reading
    /// the slot's `BoundaryField` kind), but kept for symmetry with
    /// the corresponding `PendingBinding` and to enable cross-checking
    /// in future phases.
    owning_boundary: TreeBoundaryId,
    dependencies: Vec<DefId>,
    elem_mem_slot: LirSlotId,
    name_id: StringId,
    expr_id: ExprId,
}

#[derive(Debug, Clone)]
struct DynTextBindingInfo {
    owning_boundary: TreeBoundaryId,
    dependencies: Vec<DefId>,
    text_mem_slot: LirSlotId,
    expr_id: ExprId,
}

/// Shared envelope for structural bindings (if-cond reroute, for-list
/// diff, derived-signal recompute): an `update_block` already emitted
/// with its full mutation body, plus enough metadata for the
/// per-(boundary, signal) walker to dispatch into it. The block's
/// `boundary_params` describe what refs it expects in scope when
/// called; `emit_update_block_for_boundary_signal` binds them via
/// `BindBoundaryLocal` before issuing `CallBlock`.
#[derive(Debug, Clone)]
struct StructuralBindingInfo {
    owning_boundary: TreeBoundaryId,
    dependencies: Vec<DefId>,
    /// The update_block that contains the structural mutation logic
    /// (if-cond reroute, for-list diff, or derived-signal recompute).
    /// Invoked as-is via `CallBlock`.
    update_block: BlockId,
}

impl<'a> BlockLowering<'a> {
    pub(crate) fn new(ctx: &'a CompilerContext, tree: &'a TreeLirComponent) -> Self {
        Self {
            ctx,
            component_id: tree.def_id,
            tree_effects: &tree.effects,
            tree_signals: &tree.signals,
            blocks: Vec::new(),
            effects: Vec::new(),
            slots: Vec::new(),
            strings: Vec::new(),
            string_map: HashMap::new(),
            exprs: Vec::new(),
            next_slot: 0,
            next_local_idx: 0,
            next_block: 0,
            // Start at a high enough offset to avoid collisions with:
            // - Runtime scratch buffer (0-31)
            // - String data section (256+, varies by component)
            // - Signal storage (after strings)
            // Using 1024 (0x400) as a safe starting point for component memory slots.
            // TODO: Compute this dynamically in codegen based on actual string/signal sizes.
            next_memory_offset: 1024,
            current_ops: Vec::new(),
            ops_stack: Vec::new(),
            local_bindings: HashMap::new(),
            outer_item_field_slots: HashMap::new(),
            current_block_locals: Vec::new(),
            block_locals_stack: Vec::new(),
            children_root_slot: None,
            input_binding_handlers: HashMap::new(),
            for_stack: Vec::new(),
            for_iter_body_stack: Vec::new(),
            for_item_iter_body: HashMap::new(),
            for_contexts: HashMap::new(),
            next_if_label_id: 0,
            tree_shape: ComponentTreeShape::default(),
            binding_collector: Vec::new(),
            // Inline-binding ids start above any plausible effect id
            // so they cannot collide with the real effect ids that
            // structural bindings carry as their `binding_id`. The
            // dedupe set in `emit_per_boundary_signal_updates` mixes
            // both kinds, so disjoint id spaces are required.
            next_binding_id: 1_000_000,
            boundary_dep_index: None,
            attr_binding_data: HashMap::new(),
            dyntext_binding_data: HashMap::new(),
            ifcond_binding_data: HashMap::new(),
            forlist_binding_data: HashMap::new(),
            derivedsig_binding_data: HashMap::new(),
        }
    }

    /// Build a `BoundaryDepIndex` from the collected `PendingBinding`s.
    /// Consumed by `emit_per_boundary_signal_updates` to drive
    /// per-(boundary, signal) update-fn emission.
    ///
    /// Bindings whose `owning_boundary == TreeBoundaryId(u32::MAX)`
    /// (the unresolved-derivation sentinel) are excluded; the count
    /// is reported via `tracing::debug!` so we can see if any are
    /// still unresolved.
    pub(crate) fn build_boundary_dep_index(
        &self,
        tree_shape: &ComponentTreeShape,
    ) -> BoundaryDepIndex {
        let sentinel = TreeBoundaryId(u32::MAX);
        let mut idx = BoundaryDepIndex::default();

        // Pass 1: fold each PendingBinding into boundary_deps.
        let mut sentinel_count: u32 = 0;
        for pb in &self.binding_collector {
            if pb.owning_boundary == sentinel {
                sentinel_count += 1;
                continue;
            }
            let entry = idx.boundary_deps.entry(pb.owning_boundary).or_default();
            for dep in &pb.dependencies {
                entry.insert(*dep);
            }
        }
        #[cfg(debug_assertions)]
        if sentinel_count > 0 {
            eprintln!(
                "build_boundary_dep_index: {} PendingBinding(s) skipped due to unresolved owning_boundary sentinel",
                sentinel_count
            );
        }

        // Compute children index in one O(n) pass from parent_link.
        // Also stitch ForIterBody → ForAnchor edges since iter-bodies
        // intentionally have `parent_link = None` (they're reached via
        // the anchor's children-array). Without this stitch, deps in
        // an iter-body's subtree never propagate up past the anchor.
        let mut children: HashMap<TreeBoundaryId, Vec<TreeBoundaryId>> = HashMap::new();
        // Pseudo-parent-link map for iter-bodies, keyed by iter-body id
        // → its anchor id. Mirrors `parent_link` for the deps walk.
        let mut iter_to_anchor: HashMap<TreeBoundaryId, TreeBoundaryId> = HashMap::new();
        let mut depth: HashMap<TreeBoundaryId, u32> = HashMap::new();
        for b in &tree_shape.boundaries {
            if let Some((parent, _)) = b.parent_link {
                children.entry(parent).or_default().push(b.id);
            }
            if let TreeBoundaryKind::ForAnchor { iter_body_idx, .. } = b.kind {
                let iter_id = TreeBoundaryId(iter_body_idx);
                children.entry(b.id).or_default().push(iter_id);
                iter_to_anchor.insert(iter_id, b.id);
            }
        }
        // Compute depth via parent walk (memoized). Treat
        // iter_to_anchor as a parent edge too.
        fn compute_depth(
            id: TreeBoundaryId,
            boundaries: &[TreeBoundary],
            iter_to_anchor: &HashMap<TreeBoundaryId, TreeBoundaryId>,
            depth: &mut HashMap<TreeBoundaryId, u32>,
        ) -> u32 {
            if let Some(d) = depth.get(&id) {
                return *d;
            }
            let d = if let Some((p, _)) = boundaries[id.index()].parent_link {
                1 + compute_depth(p, boundaries, iter_to_anchor, depth)
            } else if let Some(&p) = iter_to_anchor.get(&id) {
                1 + compute_depth(p, boundaries, iter_to_anchor, depth)
            } else {
                0
            };
            depth.insert(id, d);
            d
        }
        for b in &tree_shape.boundaries {
            compute_depth(b.id, &tree_shape.boundaries, &iter_to_anchor, &mut depth);
        }

        // Pass 2: bottom-up over boundaries by depth desc.
        let mut order: Vec<TreeBoundaryId> = tree_shape.boundaries.iter().map(|b| b.id).collect();
        order.sort_by_key(|id| std::cmp::Reverse(*depth.get(id).unwrap_or(&0)));

        for b in order {
            let mut subtree: BTreeSet<DefId> =
                idx.boundary_deps.get(&b).cloned().unwrap_or_default();
            if let Some(cs) = children.get(&b) {
                for c in cs {
                    if let Some(child_subtree) = idx.subtree_deps.get(c) {
                        for d in child_subtree {
                            subtree.insert(*d);
                        }
                    }
                }
            }
            idx.subtree_deps.insert(b, subtree);
        }

        // Pass 3: invert subtree_deps to signal_to_path.
        for (b, deps) in &idx.subtree_deps {
            for s in deps {
                idx.signal_to_path.entry(*s).or_default().insert(*b);
            }
        }

        idx
    }

    /// Emit one update block per `(boundary, signal)` pair where the
    /// signal has at least one inline binding (AttrSet or DynamicText)
    /// and `boundary ∈ signal_to_path[signal]`. Structural bindings
    /// (IfCondReroute, ForListReroute, DerivedSignal) reuse their
    /// pre-emitted update_block via `CallBlock` from the root's
    /// per-signal update fn — those blocks self-fan-out over
    /// enclosing for-loops, so calling them once per signal trigger
    /// suffices.
    ///
    /// After emission, every migrated effect is dropped and replaced
    /// by a single per-signal effect dispatching into
    /// `update_b<root>_s<signal>` (or a no-op block if the root isn't
    /// on the path).
    fn emit_per_boundary_signal_updates(&mut self, dep_index: &BoundaryDepIndex) {
        // Inline-kind bindings indexed by signal: for each signal, the
        // set of (owning_boundary, binding_id) pairs.
        let mut attr_signals: BTreeMap<DefId, Vec<(TreeBoundaryId, u32)>> = BTreeMap::new();
        let mut dyntext_signals: BTreeMap<DefId, Vec<(TreeBoundaryId, u32)>> = BTreeMap::new();
        // Structural-kind bindings indexed by signal: for each signal,
        // the set of (owning_boundary, structural_block_id, binding_id)
        // tuples. These get fired once per signal trigger from the root.
        let mut structural_signals: BTreeMap<DefId, Vec<(TreeBoundaryId, BlockId, u32)>> =
            BTreeMap::new();
        // Every successfully migrated binding_id (across kinds).
        let mut migrated_binding_ids: BTreeSet<u32> = BTreeSet::new();
        let sentinel = TreeBoundaryId(u32::MAX);
        for pb in &self.binding_collector {
            if pb.owning_boundary == sentinel {
                continue;
            }
            match pb.kind {
                PendingBindingKind::AttrSet => {
                    let info = match self.attr_binding_data.get(&pb.binding_id) {
                        Some(info) => info,
                        None => continue,
                    };
                    for s in &info.dependencies {
                        attr_signals
                            .entry(*s)
                            .or_default()
                            .push((pb.owning_boundary, pb.binding_id));
                    }
                    migrated_binding_ids.insert(pb.binding_id);
                }
                PendingBindingKind::DynamicText => {
                    let info = match self.dyntext_binding_data.get(&pb.binding_id) {
                        Some(info) => info,
                        None => continue,
                    };
                    for s in &info.dependencies {
                        dyntext_signals
                            .entry(*s)
                            .or_default()
                            .push((pb.owning_boundary, pb.binding_id));
                    }
                    migrated_binding_ids.insert(pb.binding_id);
                }
                PendingBindingKind::IfCondReroute => {
                    let info = match self.ifcond_binding_data.get(&pb.binding_id) {
                        Some(info) => info.clone(),
                        None => continue,
                    };
                    for s in &info.dependencies {
                        structural_signals.entry(*s).or_default().push((
                            pb.owning_boundary,
                            info.update_block,
                            pb.binding_id,
                        ));
                    }
                    migrated_binding_ids.insert(pb.binding_id);
                }
                PendingBindingKind::ForListReroute => {
                    let info = match self.forlist_binding_data.get(&pb.binding_id) {
                        Some(info) => info.clone(),
                        None => continue,
                    };
                    for s in &info.dependencies {
                        structural_signals.entry(*s).or_default().push((
                            pb.owning_boundary,
                            info.update_block,
                            pb.binding_id,
                        ));
                    }
                    migrated_binding_ids.insert(pb.binding_id);
                }
                PendingBindingKind::DerivedSignal => {
                    let info = match self.derivedsig_binding_data.get(&pb.binding_id) {
                        Some(info) => info.clone(),
                        None => continue,
                    };
                    for s in &info.dependencies {
                        structural_signals.entry(*s).or_default().push((
                            pb.owning_boundary,
                            info.update_block,
                            pb.binding_id,
                        ));
                    }
                    migrated_binding_ids.insert(pb.binding_id);
                }
            }
        }

        if migrated_binding_ids.is_empty() {
            return;
        }

        let root_id = TreeBoundaryId(self.tree_shape.root_idx);

        // For every signal s with at least one migrated binding, emit
        // per-(b, s) blocks for every b ∈ signal_to_path[s]. Build child
        // blocks before parents so each block can reference its child
        // block ids.
        let mut new_effects: Vec<LirBlockEffect> = Vec::new();
        // Union of all three signal maps (deterministic via BTreeSet).
        let mut signal_order_set: BTreeSet<DefId> = BTreeSet::new();
        signal_order_set.extend(attr_signals.keys().copied());
        signal_order_set.extend(dyntext_signals.keys().copied());
        signal_order_set.extend(structural_signals.keys().copied());
        let signal_order: Vec<DefId> = signal_order_set.into_iter().collect();
        for sig in signal_order {
            // Boundaries on the path for this signal (deterministic).
            let path_set: BTreeSet<TreeBoundaryId> = dep_index
                .signal_to_path
                .get(&sig)
                .cloned()
                .unwrap_or_default();
            if path_set.is_empty() {
                // No boundaries on the path — emit a no-op effect block
                // (a trigger fires but does nothing).
                let no_op = self.alloc_no_op_update_block();
                new_effects.push(LirBlockEffect {
                    id: 0, // assigned below
                    dependencies: vec![sig],
                    update_block: no_op,
                });
                continue;
            }

            // Build pseudo parent edges for ForIterBody → ForAnchor
            // (mirrors `build_boundary_dep_index`), then compute depth
            // along the combined edge set so leaves have higher depth
            // than ancestors. Children must be emitted before parents
            // so parent walks can reference each child's BlockId.
            let mut iter_to_anchor: HashMap<TreeBoundaryId, TreeBoundaryId> = HashMap::new();
            for tb in &self.tree_shape.boundaries {
                if let TreeBoundaryKind::ForAnchor { iter_body_idx, .. } = tb.kind {
                    iter_to_anchor.insert(TreeBoundaryId(iter_body_idx), tb.id);
                }
            }
            let mut depth: HashMap<TreeBoundaryId, u32> = HashMap::new();
            for &b in &path_set {
                let mut d = 0u32;
                let mut cur = b;
                loop {
                    let pl = self.tree_shape.boundaries[cur.index()].parent_link;
                    let next = match pl {
                        Some((p, _)) => Some(p),
                        None => iter_to_anchor.get(&cur).copied(),
                    };
                    match next {
                        Some(p) => {
                            d += 1;
                            cur = p;
                        }
                        None => break,
                    }
                }
                depth.insert(b, d);
            }

            // Per-(b, sig) BlockId map, populated as we emit.
            let mut update_blocks: HashMap<TreeBoundaryId, BlockId> = HashMap::new();

            // Index of attr-binding ids by owning_boundary for this
            // signal — only the bindings whose deps include `sig`.
            let mut attr_bindings_at: HashMap<TreeBoundaryId, Vec<u32>> = HashMap::new();
            if let Some(pairs) = attr_signals.get(&sig) {
                for (b, leg) in pairs {
                    attr_bindings_at.entry(*b).or_default().push(*leg);
                }
            }
            let mut dyntext_bindings_at: HashMap<TreeBoundaryId, Vec<u32>> = HashMap::new();
            if let Some(pairs) = dyntext_signals.get(&sig) {
                for (b, leg) in pairs {
                    dyntext_bindings_at.entry(*b).or_default().push(*leg);
                }
            }
            // Structural update blocks (if-cond reroute, for-list
            // reroute, derived-signal recompute) fire from the
            // per-(boundary, signal) walker at their binding's
            // `owning_boundary` level. The if-cond block declares
            // `boundary_params = [owning_boundary]` so the walker passes
            // the typed ref via `CallBlock`'s emit_boundary_ref; for-list
            // and derived-signal blocks declare no boundary_params and
            // are called as bare CallBlocks (their bodies self-walk
            // through `$self.tree`).
            //
            // Index by owning_boundary for the walker.
            let mut structural_calls_at: HashMap<TreeBoundaryId, Vec<BlockId>> = HashMap::new();

            // Boundaries that need a walker block emitted purely to host
            // structural calls (no inline bindings, not naturally on the
            // path via subtree_deps but they own a structural binding
            // for `sig`).
            let mut extra_path: BTreeSet<TreeBoundaryId> = BTreeSet::new();
            if let Some(pairs) = structural_signals.get(&sig) {
                for (b, blk, _) in pairs {
                    structural_calls_at.entry(*b).or_default().push(*blk);
                    if !path_set.contains(b) {
                        extra_path.insert(*b);
                    }
                }
            }

            // Augment order with any extra owning boundaries not already
            // on the path AND every ancestor needed to reach them from
            // the root, so the root's dispatcher can descend to each
            // structural binding's owning boundary. Mirrors the depth
            // walk in `build_boundary_dep_index` for consistency.
            let mut augmented_path: BTreeSet<TreeBoundaryId> = path_set.clone();
            for b in &extra_path {
                let mut cur = *b;
                let mut d = 0u32;
                augmented_path.insert(cur);
                loop {
                    if !depth.contains_key(&cur) {
                        depth.insert(cur, d);
                    }
                    let pl = self.tree_shape.boundaries[cur.index()].parent_link;
                    let next = match pl {
                        Some((p, _)) => Some(p),
                        None => iter_to_anchor.get(&cur).copied(),
                    };
                    match next {
                        Some(p) => {
                            d += 1;
                            cur = p;
                            augmented_path.insert(cur);
                        }
                        None => break,
                    }
                }
            }

            // Recompute depth for everything in augmented_path so the
            // depth-desc sort produces leaves-first order across both
            // the path_set and structural-only boundaries.
            depth.clear();
            for &b in &augmented_path {
                let mut d = 0u32;
                let mut cur = b;
                loop {
                    let pl = self.tree_shape.boundaries[cur.index()].parent_link;
                    let next = match pl {
                        Some((p, _)) => Some(p),
                        None => iter_to_anchor.get(&cur).copied(),
                    };
                    match next {
                        Some(p) => {
                            d += 1;
                            cur = p;
                        }
                        None => break,
                    }
                }
                depth.insert(b, d);
            }
            // Path-set augmented for structural-only boundaries — used
            // by SubBoundary descent inside the walker to know which
            // children to visit.
            let path_set_for_walk = augmented_path.clone();

            // Re-sort with extras included.
            let mut order: Vec<TreeBoundaryId> = augmented_path.iter().copied().collect();
            order.sort_by(|a, b| depth.get(b).cmp(&depth.get(a)).then(a.0.cmp(&b.0)));

            for b in &order {
                let structural_calls = structural_calls_at
                    .get(b)
                    .map(|v| v.as_slice())
                    .unwrap_or(&[]);
                let block_id = self.emit_update_block_for_boundary_signal(
                    *b,
                    sig,
                    &path_set_for_walk,
                    &update_blocks,
                    attr_bindings_at.get(b).map(|v| v.as_slice()).unwrap_or(&[]),
                    dyntext_bindings_at
                        .get(b)
                        .map(|v| v.as_slice())
                        .unwrap_or(&[]),
                    structural_calls,
                );
                update_blocks.insert(*b, block_id);
            }

            // Effect dispatches into root's update fn if it's on the path,
            // else no-op.
            let dispatch = update_blocks
                .get(&root_id)
                .copied()
                .unwrap_or_else(|| self.alloc_no_op_update_block());
            new_effects.push(LirBlockEffect {
                id: 0,
                dependencies: vec![sig],
                update_block: dispatch,
            });
        }

        // Splice new per-signal effects in place: each signal's new
        // effect replaces the FIRST migrated effect for that signal
        // (by its position in `self.effects`), preserving the original
        // ordering as much as possible. Other migrated effects (extra
        // AttrSet effects collapsed into the same per-signal effect)
        // are dropped. After splicing, we renumber all effect ids
        // contiguously so they remain dense and stable.
        // Index new_effects by their (sole) dependency signal.
        let new_by_signal: HashMap<DefId, LirBlockEffect> = new_effects
            .into_iter()
            .map(|e| (e.dependencies[0], e))
            .collect();
        // Map binding_id → signal that absorbs it (by deps lookup
        // through PendingBinding metadata).
        let mut binding_to_signal: HashMap<u32, DefId> = HashMap::new();
        for pb in &self.binding_collector {
            if !migrated_binding_ids.contains(&pb.binding_id) {
                continue;
            }
            // Pick any signal in deps that has a new effect — they all
            // get folded into the same per-signal effect, so any one
            // suffices for "first occurrence" placement. We use the
            // smallest DefId for determinism.
            let mut chosen: Option<DefId> = None;
            for d in &pb.dependencies {
                if new_by_signal.contains_key(d) {
                    chosen = Some(match chosen {
                        Some(prev) if prev.0 < d.0 => prev,
                        _ => *d,
                    });
                }
            }
            if let Some(s) = chosen {
                binding_to_signal.insert(pb.binding_id, s);
            }
        }
        let mut signals_used: std::collections::HashSet<DefId> = std::collections::HashSet::new();
        let old_effects = std::mem::take(&mut self.effects);
        let mut spliced: Vec<LirBlockEffect> = Vec::with_capacity(old_effects.len());
        for e in old_effects {
            if migrated_binding_ids.contains(&e.id) {
                if let Some(sig) = binding_to_signal.get(&e.id) {
                    if signals_used.insert(*sig) {
                        if let Some(new_e) = new_by_signal.get(sig).cloned() {
                            spliced.push(new_e);
                        }
                    }
                }
                continue;
            }
            spliced.push(e);
        }
        // Append any new effects whose signal didn't match a
        // migrated effect's primary slot (e.g. signals that only had
        // structural bindings whose effects were migrated under a
        // different signal's primary slot, or signals with no prior
        // effect to anchor against). Iterate in DefId order for
        // determinism — the
        // surrounding HashMap iteration is otherwise nondeterministic
        // and the resulting effect ordering shows up in DOT snapshots.
        let mut leftover_signals: Vec<DefId> = new_by_signal
            .keys()
            .copied()
            .filter(|s| !signals_used.contains(s))
            .collect();
        leftover_signals.sort_by_key(|s| s.0);
        for sig in leftover_signals {
            if let Some(e) = new_by_signal.get(&sig).cloned() {
                spliced.push(e);
            }
        }
        // Renumber.
        for (i, e) in spliced.iter_mut().enumerate() {
            e.id = i as u32;
        }
        self.effects = spliced;
    }

    /// Emit a single `update_b<b.0>_s<sig>` block: walks the boundary's
    /// fields, inlining AttrSet binding ops and dispatching into child
    /// update blocks for SubBoundary fields whose target is on this
    /// signal's path. Children must be pre-emitted and present in
    /// `child_blocks`.
    fn emit_update_block_for_boundary_signal(
        &mut self,
        boundary_id: TreeBoundaryId,
        sig: DefId,
        path_set: &BTreeSet<TreeBoundaryId>,
        child_blocks: &HashMap<TreeBoundaryId, BlockId>,
        attr_binding_ids: &[u32],
        dyntext_binding_ids: &[u32],
        structural_call_blocks: &[BlockId],
    ) -> BlockId {
        self.start_block();

        // At the root boundary's update fn, dispatch into each
        // migrated structural binding's update_block via `CallBlock`.
        // These blocks self-fan-out over their enclosing for-loops, so
        // calling them once per signal trigger produces the correct
        // overall update semantics. Done up-front before the field
        // walk so structural changes (mount/unmount, list diff,
        // derived-signal recompute) settle before any attr/text
        // inline updates touch the resulting tree.
        for &structural_block in structural_call_blocks {
            let dummy_parent = self.alloc_temp_slot_named("structural_dispatch_parent");
            self.emit(LirOp::CallBlock {
                block: structural_block,
                parent: dummy_parent,
            });
        }

        // Walk the boundary's declared fields in order. Snapshot first
        // so we can release the borrow on `self.tree_shape`.
        let fields = self.tree_shape.boundaries[boundary_id.index()]
            .fields
            .clone();
        // Snapshot the kind for IfAnchor active-tag dispatch. We only need
        // to know whether this boundary has an ActiveTag field — branch
        // dispatch reads it via field index 2 (synthesizer-fixed).
        let active_tag_field_idx: Option<u32> =
            fields.iter().enumerate().find_map(|(i, f)| match f {
                TreeFieldDecl::ActiveTag { .. } => Some(i as u32),
                _ => None,
            });

        for (fi, field) in fields.iter().enumerate() {
            let field_idx = fi as u32;
            match field {
                TreeFieldDecl::DomHandle { .. } => {
                    // Inline an AttrSet binding if any of this boundary's
                    // attr bindings own a DomHandle at this field index
                    // and the binding's deps include `sig`.
                    for binding_id in attr_binding_ids {
                        let info = match self.attr_binding_data.get(binding_id) {
                            Some(info) => info.clone(),
                            None => continue,
                        };
                        // Find this binding's slot's field index.
                        let binding_field_idx = match self
                            .slots
                            .iter()
                            .find(|s| s.id == info.elem_mem_slot)
                            .map(|s| &s.kind)
                        {
                            Some(LirSlotKind::BoundaryField {
                                boundary_id: bb,
                                field_idx,
                            }) if *bb == boundary_id => *field_idx,
                            _ => continue,
                        };
                        if binding_field_idx != field_idx {
                            continue;
                        }
                        if !info.dependencies.contains(&sig) {
                            continue;
                        }
                        // Inline LoadHandle + SetAttribute.
                        let target = self.alloc_temp_slot_named("attr_target");
                        self.emit(LirOp::LoadHandle {
                            slot: info.elem_mem_slot,
                            to: target,
                        });
                        self.emit(LirOp::SetAttribute {
                            node: target,
                            name: info.name_id,
                            expr: info.expr_id,
                        });
                    }
                    // Inline DynamicText bindings the same way:
                    // LoadHandle + SetTextContent on this DomHandle
                    // field when the binding owns it for `sig`.
                    for binding_id in dyntext_binding_ids {
                        let info = match self.dyntext_binding_data.get(binding_id) {
                            Some(info) => info.clone(),
                            None => continue,
                        };
                        let binding_field_idx = match self
                            .slots
                            .iter()
                            .find(|s| s.id == info.text_mem_slot)
                            .map(|s| &s.kind)
                        {
                            Some(LirSlotKind::BoundaryField {
                                boundary_id: bb,
                                field_idx,
                            }) if *bb == boundary_id => *field_idx,
                            _ => continue,
                        };
                        if binding_field_idx != field_idx {
                            continue;
                        }
                        if !info.dependencies.contains(&sig) {
                            continue;
                        }
                        let target = self.alloc_temp_slot_named("text_target");
                        self.emit(LirOp::LoadHandle {
                            slot: info.text_mem_slot,
                            to: target,
                        });
                        self.emit(LirOp::SetTextContent {
                            node: target,
                            expr: info.expr_id,
                        });
                    }
                }
                TreeFieldDecl::SubBoundary { target_idx, .. } => {
                    let target_b = TreeBoundaryId(*target_idx);
                    if !path_set.contains(&target_b) {
                        continue;
                    }
                    let child_block = match child_blocks.get(&target_b).copied() {
                        Some(bid) => bid,
                        None => continue,
                    };
                    // Dispatch depends on the target boundary's kind.
                    let target_kind = self.tree_shape.boundaries[target_b.index()].kind.clone();
                    match target_kind {
                        TreeBoundaryKind::IfAnchor { branches, .. } => {
                            // Load active tag from THIS boundary's child IfAnchor.
                            // We need the IfAnchor's ActiveTag field — fixed at
                            // index 2 per the synthesizer (parent=0, anchor=1,
                            // active=2, then branch SubBoundary refs).
                            let if_anchor_active_slot = self.alloc_boundary_field_slot_named(
                                target_b,
                                2,
                                "walker_ifanchor_active",
                            );
                            let active_tag = self.alloc_temp_slot_named("walker_active_tag");
                            self.emit(LirOp::LoadI32 {
                                slot: if_anchor_active_slot,
                                to: active_tag,
                            });
                            // For each branch boundary, if it's on the
                            // path AND active tag matches, call its
                            // update fn.
                            for (b_idx, branch_idx) in branches.iter().enumerate() {
                                let branch_id = TreeBoundaryId(*branch_idx);
                                if !path_set.contains(&branch_id) {
                                    continue;
                                }
                                let branch_block = match child_blocks.get(&branch_id).copied() {
                                    Some(b) => b,
                                    None => continue,
                                };
                                let tag_value = (b_idx as i32) + 1;
                                // Load the branch ref via its parent_link
                                // (IfAnchor field N+3). We use a
                                // BoundaryField slot to fetch it.
                                let branch_field_idx = self.tree_shape.boundaries
                                    [branch_id.index()]
                                .parent_link
                                .map(|(_, f)| f)
                                .unwrap_or(0);
                                let branch_ref_slot = self.alloc_boundary_field_slot_named(
                                    target_b,
                                    branch_field_idx,
                                    "walker_branch_ref_field",
                                );
                                let branch_ref_temp = self.alloc_temp_slot_typed_named(
                                    LirSlotValType::RefNullForBoundary(branch_id),
                                    "walker_branch_ref",
                                );
                                let cmp = self.alloc_temp_slot_named("walker_active_eq");
                                self.emit(LirOp::I32EqConst {
                                    lhs: active_tag,
                                    rhs: tag_value,
                                    result: cmp,
                                });
                                let dummy_parent =
                                    self.alloc_temp_slot_named("walker_branch_dummy_parent");
                                let then_ops = vec![
                                    // Fetch the typed branch ref via the
                                    // BoundaryField slot. This emits a
                                    // single `struct.get` on the IfAnchor
                                    // ref already in scope (we're inside
                                    // a block whose boundary_param
                                    // includes the parent boundary, and
                                    // emit_boundary_ref walks down).
                                    // We re-load the branch ref into a
                                    // typed local and BindBoundaryLocal
                                    // it so CallBlock's emit_boundary_ref
                                    // resolves via current_boundary_locals.
                                    LirOp::LoadHandle {
                                        slot: branch_ref_slot,
                                        to: branch_ref_temp,
                                    },
                                    LirOp::BindBoundaryLocal {
                                        boundary_id: branch_id,
                                        slot: branch_ref_temp,
                                    },
                                    LirOp::CallBlock {
                                        block: branch_block,
                                        parent: dummy_parent,
                                    },
                                ];
                                let label = self.next_if_label();
                                self.emit(LirOp::If {
                                    cond: cmp,
                                    then_ops,
                                    else_ops: vec![],
                                    name: Some(format!("walker_if{}_branch{}", label, b_idx)),
                                });
                            }
                            let _ = child_block; // anchor's own block unused — branches are leaves
                        }
                        TreeBoundaryKind::ForAnchor { iter_body_idx, .. } => {
                            let iter_body_id = TreeBoundaryId(iter_body_idx);
                            let iter_block = match child_blocks.get(&iter_body_id).copied() {
                                Some(b) => b,
                                None => {
                                    // iter-body not on path; skip.
                                    let _ = child_block;
                                    continue;
                                }
                            };
                            // Load children-array (field 2 on ForAnchor),
                            // loop, dispatch each iter-body.
                            let children_field_slot = self.alloc_boundary_field_slot_named(
                                target_b,
                                2,
                                "walker_for_children",
                            );
                            let arr_slot = self.alloc_temp_slot_typed_named(
                                LirSlotValType::RefNullForChildrenArray(target_b),
                                "walker_for_arr",
                            );
                            let len_slot = self.alloc_temp_slot_named("walker_for_len");
                            let idx_slot = self.alloc_temp_slot_named("walker_for_idx");
                            let break_cond = self.alloc_temp_slot_named("walker_for_break");
                            self.emit(LirOp::LoadHandle {
                                slot: children_field_slot,
                                to: arr_slot,
                            });
                            self.emit(LirOp::ArrayLen {
                                arr: arr_slot,
                                result: len_slot,
                            });
                            self.emit(LirOp::SetSlot {
                                slot: idx_slot,
                                value: 0,
                            });
                            self.emit(LirOp::GeU {
                                index: idx_slot,
                                len: len_slot,
                                result: break_cond,
                            });

                            let iter_ref_slot = self.alloc_temp_slot_typed_named(
                                LirSlotValType::RefNullForBoundary(iter_body_id),
                                "walker_iter_ref",
                            );
                            let dummy_parent =
                                self.alloc_temp_slot_named("walker_for_dummy_parent");
                            let body_ops = vec![
                                LirOp::ChildrenArrayGet {
                                    anchor_boundary: target_b,
                                    arr: arr_slot,
                                    idx: idx_slot,
                                    result: iter_ref_slot,
                                },
                                LirOp::BindBoundaryLocal {
                                    boundary_id: iter_body_id,
                                    slot: iter_ref_slot,
                                },
                                LirOp::CallBlock {
                                    block: iter_block,
                                    parent: dummy_parent,
                                },
                                LirOp::IncrSlot { slot: idx_slot },
                                LirOp::GeU {
                                    index: idx_slot,
                                    len: len_slot,
                                    result: break_cond,
                                },
                            ];
                            self.emit(LirOp::Loop {
                                break_cond,
                                body_ops,
                                name: Some(format!("walker_for_b{}", target_b.0)),
                            });
                        }
                        TreeBoundaryKind::Root
                        | TreeBoundaryKind::IfBranch { .. }
                        | TreeBoundaryKind::ForIterBody { .. } => {
                            // SubBoundary should only point at IfAnchor
                            // or ForAnchor in practice; other kinds
                            // would be a synthesizer bug. Skip safely.
                        }
                    }
                }
                TreeFieldDecl::LoopVar { .. }
                | TreeFieldDecl::ChildrenArray { .. }
                | TreeFieldDecl::ActiveTag { .. } => {
                    // No-op for the update walk.
                }
            }
        }
        let _ = active_tag_field_idx; // reserved for IfAnchor self-walks (none today)

        // Dummy parent slot reserved as the `params` entry so callers
        // (TriggerEffects + CallBlock) that pass an i32 parent argument
        // match the block's signature. The slot's value is unused.
        let dummy_param = self.alloc_temp_slot_named("walker_param_unused");

        let block_id = self.finish_block_with_name(BlockDebugName::update(sig.0));
        if let Some(b) = self.blocks.iter_mut().find(|b| b.id == block_id) {
            b.params = vec![dummy_param];
            b.boundary_params = vec![boundary_id];
        }
        block_id
    }

    /// Emit an empty no-op block used as the update_block for signals
    /// whose root boundary isn't on the dependency path.
    fn alloc_no_op_update_block(&mut self) -> BlockId {
        self.start_block();
        let _dummy = self.alloc_temp_slot_named("walker_noop_param");
        self.finish_block_named("noop-update")
    }

    /// Derive the topmost enclosing boundary for bindings that don't
    /// have a direct DOM node handle in scope (derived-signal,
    /// if-cond reroute, for-list reroute). Falls back to the component
    /// root when not inside any for.
    fn current_enclosing_boundary(&self) -> TreeBoundaryId {
        match self.for_iter_body_stack.last().copied() {
            Some(b) => b,
            None => TreeBoundaryId(self.tree_shape.root_idx),
        }
    }

    /// Mint a fresh debug-label id for a new `LirOp::If`. Used purely
    /// for name-section labels; runtime semantics don't depend on it.
    fn next_if_label(&mut self) -> u32 {
        let id = self.next_if_label_id;
        self.next_if_label_id += 1;
        id
    }

    /// Find a tree-based effect by its ID.
    fn find_tree_effect(&self, effect_id: u32) -> Option<&LirEffect> {
        self.tree_effects.iter().find(|e| e.id == effect_id)
    }

    /// For every `UpdateKind::DerivedSignal(target)` in the tree,
    /// materialise an update block that re-evaluates the expression and
    /// writes it to the target signal, then push a `LirBlockEffect` so
    /// the standard dep-tracking machinery wires it up like any other
    /// reactive effect.
    ///
    /// Writing to the target via `SignalWriteExpr` emits a
    /// `TriggerEffects { signal: target }` op as part of its lowering,
    /// so derived→derived chains propagate automatically — each level's
    /// effect writes its slot, which triggers the next level's effect.
    fn register_derived_signal_effects(&mut self) {
        // Snapshot matching effects so the borrow on `tree_effects`
        // doesn't overlap with the mutable `self` calls below.
        let derived: Vec<(u32, Vec<DefId>, DefId, LirExpr)> = self
            .tree_effects
            .iter()
            .filter_map(|e| match &e.update_kind {
                UpdateKind::DerivedSignal(target) => {
                    Some((e.id, e.dependencies.clone(), *target, e.expr.clone()))
                }
                _ => None,
            })
            .collect();

        for (_tree_id, dependencies, target, expr) in derived {
            // Build the update block: SignalWriteExpr { signal: target, expr }
            // followed by TriggerEffects { signal: target }. The write stores
            // the new value but does NOT implicitly trigger observers —
            // LirStatement::SignalWrite lowering emits both ops, and the
            // derived-signal effect must do the same for chained
            // propagation (derived→derived→text) to work.
            let expr_id = self.intern_expr(&expr);
            self.start_block();
            self.emit(LirOp::SignalWriteExpr {
                signal: target,
                expr: expr_id,
            });
            self.emit(LirOp::TriggerEffects { signal: target });
            let update_block = self.finish_block_named("derived-update");

            self.effects.push(LirBlockEffect {
                id: self.effects.len() as u32,
                dependencies: dependencies.clone(),
                update_block,
            });
            // Register as a PendingBinding and capture the
            // update_block for per-(boundary, signal) dispatch.
            let binding_id = self.effects.len() as u32 - 1;
            let owning_boundary = self.current_enclosing_boundary();
            self.binding_collector.push(PendingBinding {
                owning_boundary,
                dependencies: dependencies.clone(),
                kind: PendingBindingKind::DerivedSignal,
                binding_id,
            });
            self.derivedsig_binding_data.insert(
                binding_id,
                StructuralBindingInfo {
                    owning_boundary,
                    dependencies,
                    update_block,
                },
            );
        }
    }

    /// Find a signal's type by its DefId.
    fn find_signal_type(&self, signal_def_id: DefId) -> Option<Ty> {
        self.tree_signals
            .iter()
            .find(|s| s.def_id == signal_def_id)
            .map(|s| s.ty)
    }

    /// Convert a Ty to SlotValType for WASM local declaration.
    fn ty_to_slot_val_type(&self, ty: Ty) -> LirSlotValType {
        // Phase 5b-v.3: scalar lists are typed GC array refs.
        if self.is_scalar_list_ty(ty) {
            return LirSlotValType::RefNullForListGc(ty);
        }
        // Phase 5b-v.3 / 5d preview: option<list<scalar>> collapses
        // to a single nullable ref of the inner list's array type.
        // Storage-wise this is the same as `list<scalar>` itself —
        // none == null, some(arr) == arr.
        if let InternedTyKind::Option(inner_ty) = self.ctx.ty_kind(ty) {
            if self.is_scalar_list_ty(*inner_ty) {
                return LirSlotValType::RefNullForListGc(*inner_ty);
            }
        }
        // Phase 5e.1: DTR records have a single GC ref internal repr.
        if let InternedTyKind::Adt(d) = self.ctx.ty_kind(ty) {
            if matches!(self.ctx.defs.kind(*d), DefKind::Record(_)) {
                let mut seen = HashSet::new();
                if self.is_dtr_record_ty(ty, &mut seen) {
                    return LirSlotValType::RefNullForRecord(ty);
                }
            }
        }
        match self.ctx.ty_kind(ty) {
            InternedTyKind::F32 => LirSlotValType::F32,
            InternedTyKind::F64 => LirSlotValType::F64,
            InternedTyKind::S64 | InternedTyKind::U64 => LirSlotValType::I64,
            _ => LirSlotValType::I32,
        }
    }

    pub(crate) fn lower_component(&mut self, tree: &TreeLirComponent) -> LirComponent {
        // Synthesize the typed mount-tree shape up-front so slot
        // allocation during body lowering can build
        // `SlotKind::BoundaryField` slots that reference fields the
        // synthesizer planned out.
        self.tree_shape = {
            let ctx = self.ctx;
            let tree_signals = self.tree_signals;

            synthesize(
                &tree.body,
                |ty, iter_src| {
                    // Phase 5b-v.3 / 5e.1: for GC-array lists, the LoopVar
                    // field stores the item VALUE directly. For records
                    // that gives a record GC ref; for scalars the unboxed
                    // primitive. For memory/range iterables it stays an
                    // I32 ptr.
                    match iter_src {
                        IterSource::ListGc => match ctx.ty_kind(ty) {
                            InternedTyKind::F32 => LirSlotValType::F32,
                            InternedTyKind::F64 => LirSlotValType::F64,
                            InternedTyKind::S64 | InternedTyKind::U64 => LirSlotValType::I64,
                            InternedTyKind::Adt(_) => {
                                // Record element → typed record ref;
                                // enum/variant elements still i32.
                                if let InternedTyKind::Adt(d) = ctx.ty_kind(ty) {
                                    if matches!(ctx.defs.kind(*d), DefKind::Record(_)) {
                                        LirSlotValType::RefNullForRecord(ty)
                                    } else {
                                        LirSlotValType::I32
                                    }
                                } else {
                                    LirSlotValType::I32
                                }
                            }
                            _ => LirSlotValType::I32,
                        },
                        IterSource::ListMemory | IterSource::Range => LirSlotValType::I32,
                    }
                },
                |iterable_expr| {
                    // Phase 5b-v.3 / 5e.1: GC-array-eligible lists →
                    // ListGc when backed by a component-local signal.
                    // Global properties and non-signal exprs stay
                    // ListMemory until Phase 6. Eligible elements:
                    // primitive scalars (5b-v.3), DTR records (5e.1).
                    let is_component_signal = match &iterable_expr.kind {
                        LirExprKind::SignalRead(def_id) => {
                            tree_signals.iter().any(|s| s.def_id == *def_id)
                        }
                        _ => true, // Non-signal exprs: allow GC if eligible
                    };

                    let elem_is_scalar = matches!(
                        ctx.ty_kind(iterable_expr.ty),
                        InternedTyKind::List(e) if matches!(
                            ctx.ty_kind(*e),
                            InternedTyKind::Bool
                            | InternedTyKind::S8 | InternedTyKind::S16 | InternedTyKind::S32
                            | InternedTyKind::U8 | InternedTyKind::U16 | InternedTyKind::U32
                            | InternedTyKind::S64 | InternedTyKind::U64
                            | InternedTyKind::F32 | InternedTyKind::F64
                            | InternedTyKind::Char
                        )
                    );
                    // Phase 5e.1: DTR records — recognise structurally,
                    // mirroring is_scalar_list_ty's record branch.
                    let elem_is_dtr_record = if let InternedTyKind::List(e) =
                        ctx.ty_kind(iterable_expr.ty)
                    {
                        let elem_ty = *e;
                        if let InternedTyKind::Adt(d) = ctx.ty_kind(elem_ty) {
                            matches!(ctx.defs.kind(*d), DefKind::Record(_))
                                && is_dtr_record_struct(ctx, *d)
                        } else {
                            false
                        }
                    } else {
                        false
                    };

                    if is_component_signal && (elem_is_scalar || elem_is_dtr_record) {
                        IterSource::ListGc
                    } else {
                        IterSource::ListMemory
                    }
                },
            )
        };

        // Generate constructor block first
        let constructor_block = self.generate_constructor_block(tree);

        // Start mount block
        self.start_block();

        // Lower the body - we need a parent slot
        // The mount function receives parent as parameter (slot 0)
        let parent_slot = self.alloc_temp_slot_named("parent_slot");

        for node in &tree.body {
            self.lower_node(node, parent_slot);
        }

        // Finish mount block
        let mount_block = self.finish_block_named("mount");

        // Register derived-signal effects: each `UpdateKind::DerivedSignal`
        // in the tree becomes a self-contained update block that
        // recomputes the expression and writes to the target signal. The
        // write then triggers downstream effects via the standard signal
        // write path (`SignalWriteExpr`), so A→B→C chains propagate
        // without any extra orchestration here.
        self.register_derived_signal_effects();

        // Build the BoundaryDepIndex once. Pure data construction;
        // the per-(boundary, signal) emitter consumes it.
        let dep_index = self.build_boundary_dep_index(&self.tree_shape);

        // Debug-only invariant: every collected `PendingBinding`'s
        // owning_boundary must appear in `signal_to_path[s]` for each
        // signal `s` it depends on. Sentinel-bearing bindings
        // (TreeBoundaryId(u32::MAX)) are skipped — they were never
        // resolved to a real boundary.
        #[cfg(debug_assertions)]
        {
            let sentinel = TreeBoundaryId(u32::MAX);
            for pb in &self.binding_collector {
                if pb.owning_boundary == sentinel {
                    continue;
                }
                for s in &pb.dependencies {
                    let in_path = dep_index
                        .signal_to_path
                        .get(s)
                        .map(|set| set.contains(&pb.owning_boundary))
                        .unwrap_or(false);
                    debug_assert!(
                        in_path,
                        "BoundaryDepIndex invariant: binding#{} signal {:?} not reachable via signal_to_path[{:?}] from owning_boundary {:?} (kind={:?})",
                        pb.binding_id, s, s, pb.owning_boundary, pb.kind,
                    );
                }
            }
        }

        // Emit per-(boundary, signal) update fns for inline bindings
        // (AttrSet, DynamicText) and route structural bindings
        // (IfCondReroute, ForListReroute, DerivedSignal) through their
        // update_blocks via `CallBlock` from the root's per-signal
        // update fn. Replaces every migrated binding's prior fan-out
        // effect with a single per-signal effect targeting the new
        // root update fn.
        self.emit_per_boundary_signal_updates(&dep_index);

        self.boundary_dep_index = Some(dep_index);

        let mut component = LirComponent {
            def_id: tree.def_id,
            name: tree.name,
            span: tree.span,
            is_export: tree.is_export,
            blocks: std::mem::take(&mut self.blocks),
            constructor_block,
            mount_block,
            effects: std::mem::take(&mut self.effects),
            slots: std::mem::take(&mut self.slots),
            strings: std::mem::take(&mut self.strings),
            exprs: std::mem::take(&mut self.exprs),
            signals: tree.signals.clone(),
            children_root_slot: self.children_root_slot,
            input_binding_handlers: std::mem::take(&mut self.input_binding_handlers),
            for_contexts: {
                // Sort by ForId so the component has a stable, id-ordered
                // list — simpler for codegen to walk / assert against.
                let map = std::mem::take(&mut self.for_contexts);
                let mut v: Vec<_> = map.into_values().collect();
                v.sort_by_key(|c| c.id.0);
                v
            },
            effects_by_signal: HashMap::new(),
            body_tree: tree.body.clone(),
            tree_shape: std::mem::take(&mut self.tree_shape),
        };

        // Bitwise structural dedupe of per-(boundary, signal)
        // update blocks. Two `update_b<b>_s<s>` blocks with identical
        // shape (after canonicalising slot ids and inner CallBlock
        // targets) collapse to one. CallBlock / CallBlock2 / effect
        // references across the whole component are rewritten to the
        // canonical survivor, and duplicate blocks are removed.
        dedupe_update_blocks(self.ctx, &mut component);

        // Populate per-block structural metadata that codegen would
        // otherwise recompute by re-walking the op tree on every emit.
        populate_block_structural_metadata(self.ctx, &mut component);

        // Build the inverted dependency index: signal DefId → effect ids.
        for effect in &component.effects {
            for &dep in &effect.dependencies {
                component
                    .effects_by_signal
                    .entry(dep)
                    .or_default()
                    .push(effect.id);
            }
        }

        component
    }

    /// Generate the constructor block that initializes signals and memory slots.
    fn generate_constructor_block(&mut self, tree: &TreeLirComponent) -> BlockId {
        self.start_block();

        // Initialize each signal
        for (i, signal) in tree.signals.iter().enumerate() {
            if let Some(default_expr) = &signal.default {
                // Signal has a default value - intern the expression and emit InitSignal
                let expr_id = self.intern_expr(default_expr);
                self.emit(LirOp::InitSignal {
                    signal_idx: i as u32,
                    expr: expr_id,
                });
            } else {
                // No default - emit InitSignalDefault to set zero/empty
                self.emit(LirOp::InitSignalDefault {
                    signal_idx: i as u32,
                });
            }
        }

        // Note: Memory slots will be initialized during codegen since they're
        // allocated during mount block lowering which happens after this.
        // The codegen will iterate through slots and initialize Memory slots.

        // ResourceNew is emitted during codegen based on whether component is exported

        self.finish_block_named("constructor")
    }

    /// Lower a single node, emitting ops to the current block.
    fn lower_node(&mut self, node: &LirNode, parent_slot: LirSlotId) {
        match &node.kind {
            LirNodeKind::Element {
                component,
                tag,
                static_bindings,
                dynamic_binding_ids,
                handlers,
                children,
            } => {
                // Check if this is a user-defined component (not a builtin element like VStack, Text)
                if let Some(component_def) = component {
                    // Only emit MountComponent for user-defined components, not builtin elements
                    if !self.ctx.known.elements.is_builtin(*component_def) {
                        // Container components: allocate a slot to receive
                        // the children-root id returned from `mount`, then
                        // lower caller's children under that slot.
                        let target_has_slot = match self.ctx.defs.kind(*component_def) {
                            DefKind::Component(c) => c.has_children_slot,
                            DefKind::ImportComponent(ic) => ic.has_children_slot,
                            _ => false,
                        };
                        let children_root = if target_has_slot {
                            Some(self.alloc_temp_slot_named("children_root"))
                        } else {
                            None
                        };
                        self.emit(LirOp::MountComponent {
                            component_def: *component_def,
                            parent: parent_slot,
                            children_root,
                        });
                        if let Some(cr) = children_root {
                            for child in children {
                                self.lower_node(child, cr);
                            }
                        }
                        return;
                    }
                }

                // Regular HTML element or builtin element (VStack, Text, Button, etc.)
                let elem_slot = self.alloc_temp_slot_named(format!("elem_{}", tag));
                let tag_id = self.intern_string(tag);
                self.emit(LirOp::CreateElement {
                    tag: tag_id,
                    result: elem_slot,
                });

                // Append to parent
                self.emit(LirOp::AppendChild {
                    parent: parent_slot,
                    child: elem_slot,
                });

                // Static bindings (attributes set at creation time)
                for binding in static_bindings {
                    let name_id = self.intern_string(&binding.name);
                    let expr_id = self.intern_expr(&binding.value);
                    self.emit(LirOp::SetAttribute {
                        node: elem_slot,
                        name: name_id,
                        expr: expr_id,
                    });
                }

                // Dynamic bindings: set initial value AND create effects for updates
                if !dynamic_binding_ids.is_empty() {
                    // Store element handle in the typed mount tree.
                    // Every Element with a synthesizer-issued
                    // `node_field` lands on its owning boundary's
                    // DomHandle field — root, iter-body, or if-branch.
                    // The boundary ref is in scope at every emit site
                    // (root via `$self.tree`, iter-body / if-branch via
                    // `boundary_params`), so `BoundaryField` reads /
                    // writes resolve via `local.get` or a single
                    // `struct.get` walk.
                    let elem_mem_slot = match self
                        .tree_shape
                        .node_field
                        .get(&node.id)
                        .copied()
                    {
                        Some(nfr) => self.alloc_boundary_field_slot_named(
                            nfr.owning_boundary,
                            nfr.field_idx,
                            "elem_handle",
                        ),
                        None => unreachable!(
                            "Element node {:?} missing tree_shape.node_field entry; synthesizer must allocate a BoundaryField for every Element",
                            node.id
                        ),
                    };
                    self.emit(LirOp::StoreHandle {
                        slot: elem_mem_slot,
                        from: elem_slot,
                    });

                    // Create effects for each dynamic binding
                    for &effect_id in dynamic_binding_ids {
                        if let Some(effect) = self.find_tree_effect(effect_id) {
                            let deps = effect.dependencies.clone();
                            let expr = effect.expr.clone();
                            let update_kind = effect.update_kind.clone();

                            // Get property name from update_kind
                            if let UpdateKind::Property(prop_name) = &update_kind {
                                let expr_id = self.intern_expr(&expr);
                                let name_id = self.intern_string(prop_name);

                                // Set initial attribute value during mount
                                self.emit(LirOp::SetAttribute {
                                    node: elem_slot,
                                    name: name_id,
                                    expr: expr_id,
                                });

                                // Effect-target handle is already
                                // persisted by the `StoreHandle
                                // elem_mem_slot, from elem_slot` above:
                                // `elem_mem_slot` is now a
                                // `BoundaryField` slot pinned to this
                                // node's `(owning_boundary, field_idx)`.
                                // The per-(boundary, signal) walker
                                // resolves it via `parent_link` after
                                // binding the iter-body in scope.

                                // Register the binding for the
                                // per-(boundary, signal) walker to
                                // inline. No `LirBlockEffect` is pushed
                                // — the walker emits LoadHandle +
                                // SetAttribute directly into the right
                                // boundary's update fn, looked up via
                                // `attr_binding_data`.
                                if !deps.is_empty() {
                                    let pb_id = self.next_binding_id;
                                    self.next_binding_id += 1;
                                    let owning_boundary = self
                                        .tree_shape
                                        .node_field
                                        .get(&node.id)
                                        .map(|nfr| nfr.owning_boundary)
                                        .unwrap_or(TreeBoundaryId(u32::MAX));
                                    self.binding_collector.push(PendingBinding {
                                        owning_boundary,
                                        dependencies: deps.clone(),
                                        kind: PendingBindingKind::AttrSet,
                                        binding_id: pb_id,
                                    });
                                    self.attr_binding_data.insert(
                                        pb_id,
                                        AttrBindingInfo {
                                            owning_boundary,
                                            dependencies: deps,
                                            elem_mem_slot,
                                            name_id,
                                            expr_id,
                                        },
                                    );
                                }
                            }
                        }
                    }
                }

                // Event handlers
                for handler in handlers {
                    let handler_block = self.lower_handler(handler);
                    let event_id = self.intern_string(&handler.event);
                    self.emit(LirOp::AddEventListener {
                        node: elem_slot,
                        event: event_id,
                        handler: handler_block,
                    });
                }

                // Children
                for child in children {
                    self.lower_node(child, elem_slot);
                }
            }

            LirNodeKind::StaticText(text) => {
                let text_slot = self.alloc_temp_slot_named("text_slot");
                let content_id = self.intern_string(text);
                self.emit(LirOp::CreateText {
                    content: content_id,
                    result: text_slot,
                });
                self.emit(LirOp::AppendChild {
                    parent: parent_slot,
                    child: text_slot,
                });
            }

            LirNodeKind::DynamicText { effect_id } => {
                // Dynamic text: create with initial content, store handle for effects
                let text_slot = self.alloc_temp_slot_named("text_slot");

                // Find the effect that owns this dynamic text to get the expression
                // Clone what we need before mutably borrowing self
                let (expr, dependencies) = self
                    .find_tree_effect(*effect_id)
                    .map(|e| (e.expr.clone(), Some(e.dependencies.clone())))
                    .unwrap_or_else(|| {
                        todo!(
                            "Dynamic text effect not found for effect_id={:?}",
                            effect_id
                        )
                    });

                // Persistent handle storage: pin to the synthesizer's
                // typed `BoundaryField` slot for this DynamicText's
                // owning boundary (root, iter-body, or if-branch). The
                // boundary ref is in scope at every emit site through
                // `boundary_params` plumbing, so reads/writes resolve
                // via `local.get` (in-scope) or a single `struct.get`
                // walk via `parent_link`.
                let text_mem_slot = match self
                    .tree_shape
                    .node_field
                    .get(&node.id)
                    .copied()
                {
                    Some(nfr) => self.alloc_boundary_field_slot_named(
                        nfr.owning_boundary,
                        nfr.field_idx,
                        "text_handle",
                    ),
                    None => unreachable!(
                        "DynamicText node {:?} missing tree_shape.node_field entry; synthesizer must allocate a BoundaryField for every DynamicText",
                        node.id
                    ),
                };
                let expr_id = self.intern_expr(&expr);

                // Create text node with initial dynamic content
                self.emit(LirOp::CreateTextDynamic {
                    expr: expr_id,
                    result: text_slot,
                });

                // Store handle in memory for effect updates
                self.emit(LirOp::StoreHandle {
                    slot: text_mem_slot,
                    from: text_slot,
                });

                // The text node's handle is already persisted by the
                // `StoreHandle text_mem_slot, from text_slot` above:
                // `text_mem_slot` is now a `BoundaryField` slot pinned
                // to this node's `(owning_boundary, field_idx)` from
                // `tree_shape.node_field`. Fan-out text-update walks
                // bind the iter-body in scope and fetch the target via
                // a `BoundaryField` `LoadHandle` — no iter-rec stash.

                // Append to parent
                self.emit(LirOp::AppendChild {
                    parent: parent_slot,
                    child: text_slot,
                });

                // Register the binding for the per-(boundary, signal)
                // walker to inline. No `LirBlockEffect` is pushed —
                // the walker emits LoadHandle + SetTextContent
                // directly into the right boundary's update fn,
                // looked up via `dyntext_binding_data`.
                if let Some(deps) = dependencies {
                    if !deps.is_empty() {
                        let pb_id = self.next_binding_id;
                        self.next_binding_id += 1;
                        let owning_boundary = self
                            .tree_shape
                            .node_field
                            .get(&node.id)
                            .map(|nfr| nfr.owning_boundary)
                            .unwrap_or(TreeBoundaryId(u32::MAX));
                        self.binding_collector.push(PendingBinding {
                            owning_boundary,
                            dependencies: deps.clone(),
                            kind: PendingBindingKind::DynamicText,
                            binding_id: pb_id,
                        });
                        self.dyntext_binding_data.insert(
                            pb_id,
                            DynTextBindingInfo {
                                owning_boundary,
                                dependencies: deps,
                                text_mem_slot,
                                expr_id,
                            },
                        );
                    }
                }
            }

            LirNodeKind::If {
                condition,
                then_branch,
                else_if_branches,
                else_branch,
            } => {
                self.lower_if(
                    condition,
                    then_branch,
                    else_if_branches,
                    else_branch.as_deref(),
                    parent_slot,
                    node.id,
                );
            }

            LirNodeKind::For {
                for_id,
                item,
                item_name: _,
                item_span: _,
                item_ty,
                iterable,
                key: _,
                body,
            } => {
                self.lower_for(
                    *for_id,
                    *item,
                    *item_ty,
                    iterable,
                    body,
                    parent_slot,
                    node.id,
                );
            }
            LirNodeKind::ChildrenSlot => {
                // Caller-children splice here. The parent slot at this
                // position IS the component's children-root — record it so
                // the mount function can `return` its DOM id. Nothing is
                // emitted at the slot itself; the caller appends under the
                // returned id at mount time.
                //
                // Phase 2 already rejects >1 slot per component at HIR
                // registration, so this branch runs at most once per
                // component body.
                self.children_root_slot = Some(parent_slot);
            }
        }
    }

    /// Lower an if node with potential else-if + else branches.
    ///
    /// Every if produces a typed `IfAnchor` boundary holding `parent`,
    /// `anchor`, `active` plus one `SubBoundary` field per branch. Each
    /// branch is its own `IfBranch` boundary holding the branch's
    /// `content` DOM-root handle (field 0) and the branch body fields.
    ///
    /// Branches are emitted as flat siblings — `then` (idx 0),
    /// each `else if` (idx 1..N), then optional `else` (last). State is
    /// tracked in `active`: 0 = none mounted, 1 = then, 2..N+1 = else_if_n,
    /// last = else.
    ///
    /// All persistent state lives on the typed boundary structs — no
    /// linear-memory slots are used for if-state.
    fn lower_if(
        &mut self,
        condition: &LirExpr,
        then_branch: &[LirNode],
        else_if_branches: &[(LirExpr, Vec<LirNode>)],
        else_branch: Option<&[LirNode]>,
        parent_slot: LirSlotId,
        if_node_id: NodeId,
    ) {
        // Every `lower_if` call corresponds to a real `LirNodeKind::If`
        // in the body tree — the synthesizer minted exactly one
        // IfAnchor boundary for it.
        let if_anchor_id = self.subboundary_target_for_node(if_node_id);

        // Recover this anchor's child branch boundary ids (in declared
        // order: then=0, else_if_n=1..N, else=last when present).
        let branches: Vec<TreeBoundaryId> =
            match &self.tree_shape.boundaries[if_anchor_id.index()].kind {
                TreeBoundaryKind::IfAnchor { branches, .. } => {
                    branches.iter().map(|i| TreeBoundaryId(*i)).collect()
                }
                other => panic!(
                    "lower_if: tree boundary {:?} kind is not IfAnchor: {:?}",
                    if_anchor_id, other
                ),
            };

        // Sanity: synthesizer must have produced one branch per branch
        // body the lowering iterates (then + else_if_n + optional else).
        let expected_branches =
            1 + else_if_branches.len() + if else_branch.is_some() { 1 } else { 0 };
        assert_eq!(
            branches.len(),
            expected_branches,
            "lower_if: synthesized branch count {} != lowered count {}",
            branches.len(),
            expected_branches
        );

        // BoundaryField slots — synthesizer's IfAnchor layout:
        //   field 0 = parent (DomHandle)
        //   field 1 = anchor (DomHandle)
        //   field 2 = active (ActiveTag)
        let parent_field_slot =
            self.alloc_boundary_field_slot_named(if_anchor_id, 0, "if_parent_handle");
        let anchor_field_slot =
            self.alloc_boundary_field_slot_named(if_anchor_id, 1, "if_anchor_handle");
        let active_flag = self.alloc_boundary_field_slot_named(if_anchor_id, 2, "if_active_tag");

        // Allocate the IfAnchor sub-boundary BEFORE any field writes.
        // The op stores the ref on the parent's SubBoundary field and
        // registers it in `current_boundary_locals` so subsequent
        // BoundaryField accesses resolve in O(1).
        let if_anchor_ref_slot = self.alloc_temp_slot_typed_named(
            LirSlotValType::RefNullForBoundary(if_anchor_id),
            "if_anchor_ref",
        );
        self.emit(LirOp::AllocSubBoundary {
            boundary_id: if_anchor_id,
            ref_slot: if_anchor_ref_slot,
        });

        // Create anchor comment — insertion point for branch content.
        let anchor_slot = self.alloc_temp_slot_named("anchor_slot");
        let anchor_text = self.intern_string("if");
        self.emit(LirOp::CreateComment {
            content: anchor_text,
            result: anchor_slot,
        });
        self.emit(LirOp::AppendChild {
            parent: parent_slot,
            child: anchor_slot,
        });
        self.emit(LirOp::StoreHandle {
            slot: anchor_field_slot,
            from: anchor_slot,
        });
        // Persist the if's DOM parent into the IfAnchor so update /
        // unmount can re-fetch it without a memory slot.
        self.emit(LirOp::StoreHandle {
            slot: parent_field_slot,
            from: parent_slot,
        });

        // Build the flat list of (cond_opt, body, branch_id) tuples.
        // `cond_opt = None` marks the trailing `else` branch.
        let mut flat_branches: Vec<(Option<&LirExpr>, &[LirNode], TreeBoundaryId)> =
            Vec::with_capacity(expected_branches);
        flat_branches.push((Some(condition), then_branch, branches[0]));
        for (i, (cond, body)) in else_if_branches.iter().enumerate() {
            flat_branches.push((Some(cond), body.as_slice(), branches[i + 1]));
        }
        if let Some(else_nodes) = else_branch {
            flat_branches.push((None, else_nodes, *branches.last().unwrap()));
        }

        // Create mount + unmount blocks for every branch. Mount blocks
        // take both IfAnchor and the branch's IfBranch as boundary
        // params (the IfAnchor for parent/anchor reads, the IfBranch
        // for `content` writes). Unmount blocks need only the IfBranch
        // — the caller fetches the right branch ref before invoking.
        //
        // The branch's `content` field index is 0 (synthesizer-fixed).
        // If this if is inside a for, thread the enclosing for's
        // iter-body boundary as an extra `boundary_params` entry on
        // every if-branch mount block. That gives the branch's
        // function the iter-body ref as a typed param, so loop-
        // variable reads inside the branch resolve via
        // `BoundaryField` on iter-body field 0 — the new path that
        // replaces the legacy linear-memory item slot.
        let mut branch_mount_unmount: Vec<(BlockId, BlockId)> =
            Vec::with_capacity(expected_branches);
        for (_, body, branch_id) in &flat_branches {
            let content_slot =
                self.alloc_boundary_field_slot_named(*branch_id, 0, "if_branch_content");
            let (mount, unmount) = self.create_branch_with_tracking(
                body,
                parent_slot,
                anchor_field_slot,
                content_slot,
            );
            // Mount block: needs IfAnchor + IfBranch in scope. When
            // nested under a for, append the iter-body ref so loop-
            // variable reads resolve via BoundaryField in the branch
            // function.
            if let Some(b) = self.blocks.iter_mut().find(|b| b.id == mount) {
                let mut bp = vec![if_anchor_id, *branch_id];
                // Append every enclosing for's iter-body (innermost-
                // first) so all outer-item BoundaryField slot reads
                // inside the branch resolve via `local.get`.
                for &ib in self.for_iter_body_stack.iter().rev() {
                    bp.push(ib);
                }
                b.boundary_params = bp;
            }
            // Unmount block: IfBranch needed (Remove uses content).
            // When nested under a for, also include the iter-body so
            // any vestigial loop-variable LoadHandle ops emitted at
            // unmount-block entry resolve their BoundaryField slots
            // (the unmount path snapshots `outer_item_field_slots` the
            // same way the mount path does).
            if let Some(b) = self.blocks.iter_mut().find(|b| b.id == unmount) {
                let mut bp = vec![*branch_id];
                for &ib in self.for_iter_body_stack.iter().rev() {
                    bp.push(ib);
                }
                b.boundary_params = bp;
            }
            branch_mount_unmount.push((mount, unmount));
        }

        // Register effect (when any condition has signal deps).
        let mut deps: Vec<DefId> = Vec::new();
        for (cond_opt, _, _) in &flat_branches {
            if let Some(cond) = cond_opt {
                for d in self.collect_dependencies(cond) {
                    if !deps.contains(&d) {
                        deps.push(d);
                    }
                }
            }
        }
        if !deps.is_empty() {
            // Resolve owning_boundary BEFORE creating the update block so
            // we can stamp it onto the block's `boundary_params`.
            let owning_boundary = self
                .tree_shape
                .node_field
                .get(&if_node_id)
                .map(|nfr| nfr.owning_boundary)
                .unwrap_or(TreeBoundaryId(u32::MAX));
            let update_block = self.create_if_update_block_flat(
                &flat_branches,
                &branch_mount_unmount,
                parent_field_slot,
                active_flag,
                owning_boundary,
            );
            self.effects.push(LirBlockEffect {
                id: self.effects.len() as u32,
                dependencies: deps.clone(),
                update_block,
            });
            // Register as a PendingBinding and capture the
            // update_block for per-(boundary, signal) dispatch. The
            // block body already encodes the full reroute logic and
            // is invoked via `CallBlock`. owning_boundary is the
            // parent boundary of the IfAnchor.
            let binding_id = self.effects.len() as u32 - 1;
            self.binding_collector.push(PendingBinding {
                owning_boundary,
                dependencies: deps.clone(),
                kind: PendingBindingKind::IfCondReroute,
                binding_id,
            });
            self.ifcond_binding_data.insert(
                binding_id,
                StructuralBindingInfo {
                    owning_boundary,
                    dependencies: deps,
                    update_block,
                },
            );
        }

        // Initial mount: evaluate each cond in order; mount the first
        // matching branch (or the trailing else if all conds were
        // false). Records `active = 1 + branch_idx` (1=then, 2+=else_if
        // n, last=else; 0 means nothing mounted).
        self.emit_if_initial_mount(
            &flat_branches,
            &branch_mount_unmount,
            if_anchor_id,
            parent_slot,
            active_flag,
        );
    }

    /// Emit the initial-mount sequence for an if at component-mount time.
    /// Walks branches in order; mounts the first whose condition is
    /// truthy (or the trailing else if all conds are false), then sets
    /// `active` to `1 + branch_idx`.
    fn emit_if_initial_mount(
        &mut self,
        flat_branches: &[(Option<&LirExpr>, &[LirNode], TreeBoundaryId)],
        branch_mount_unmount: &[(BlockId, BlockId)],
        if_anchor_id: TreeBoundaryId,
        parent_slot: LirSlotId,
        active_flag: LirSlotId,
    ) {
        // Build an `If`-tree from the bottom up so the deepest else
        // arm is the trailing branch (or a no-op if no `else`). For
        // each branch with a condition, generate a fresh ref-slot for
        // its IfBranch alloc so each instance is uniquely tagged.
        //
        // ops_for_branch_i: emit AllocSubBoundary for branch i's
        // IfBranch, then CallBlock to its mount fn, then StoreI32 for
        // the active tag (= 1 + i).
        let mut tail_ops: Vec<LirOp> = Vec::new();
        // If the last entry is an `else` branch (cond = None), seed
        // `tail_ops` with its mount sequence.
        let n = flat_branches.len();
        let has_trailing_else = matches!(flat_branches.last(), Some((None, _, _)));
        if has_trailing_else {
            let (_, _, branch_id) = flat_branches[n - 1];
            let (mount_block, _) = branch_mount_unmount[n - 1];
            let ref_slot = self.alloc_temp_slot_typed_named(
                LirSlotValType::RefNullForBoundary(branch_id),
                "if_branch_init_ref",
            );
            tail_ops.push(LirOp::AllocSubBoundary {
                boundary_id: branch_id,
                ref_slot,
            });
            tail_ops.push(LirOp::CallBlock {
                block: mount_block,
                parent: parent_slot,
            });
            tail_ops.push(LirOp::StoreI32 {
                slot: active_flag,
                value: n as i32,
            });
        } else {
            // No else: when no condition matches, active stays 0
            // (= "nothing mounted").
            tail_ops.push(LirOp::StoreI32 {
                slot: active_flag,
                value: 0,
            });
        }
        let _ = if_anchor_id;

        // Now wrap, from the LAST conditional branch up to the FIRST
        // (then), an If chain.
        let cond_count = if has_trailing_else { n - 1 } else { n };
        for ci in (0..cond_count).rev() {
            let (cond_opt, _, branch_id) = flat_branches[ci];
            let cond = cond_opt.expect("flat_branches conditional entry must have a cond");
            let (mount_block, _) = branch_mount_unmount[ci];
            let cond_slot = self.alloc_temp_slot_named("init_cond");
            let cond_expr = self.intern_expr(cond);
            self.emit(LirOp::EvalExpr {
                expr: cond_expr,
                result: cond_slot,
            });
            let ref_slot = self.alloc_temp_slot_typed_named(
                LirSlotValType::RefNullForBoundary(branch_id),
                "if_branch_init_ref",
            );
            let then_ops = vec![
                LirOp::AllocSubBoundary {
                    boundary_id: branch_id,
                    ref_slot,
                },
                LirOp::CallBlock {
                    block: mount_block,
                    parent: parent_slot,
                },
                LirOp::StoreI32 {
                    slot: active_flag,
                    value: (ci as i32) + 1,
                },
            ];
            let label = self.next_if_label();
            let prev_tail = std::mem::take(&mut tail_ops);
            tail_ops = vec![LirOp::If {
                cond: cond_slot,
                then_ops,
                else_ops: prev_tail,
                name: Some(format!("if{}_init_b{}", label, ci)),
            }];
        }
        for op in tail_ops {
            self.emit(op);
        }
    }

    /// Build the if-update block: evaluates branch conditions in order,
    /// computes the target branch index (1..N or 0 if none match and
    /// there's no else), and if it differs from `active`, unmounts the
    /// old branch and mounts the new one.
    ///
    /// The update block takes the IfAnchor as its boundary param so
    /// `parent`, `anchor`, `active` reads + branch-ref chains all
    /// resolve via in-scope locals.
    fn create_if_update_block_flat(
        &mut self,
        flat_branches: &[(Option<&LirExpr>, &[LirNode], TreeBoundaryId)],
        branch_mount_unmount: &[(BlockId, BlockId)],
        parent_field_slot: LirSlotId,
        active_flag: LirSlotId,
        owning_boundary: TreeBoundaryId,
    ) -> BlockId {
        self.start_block();

        // Load parent from the IfAnchor's `parent` field.
        let parent_slot = self.alloc_temp_slot_named("upd_parent");
        self.emit(LirOp::LoadHandle {
            slot: parent_field_slot,
            to: parent_slot,
        });

        // Compute target_idx = 1 + first matching branch index, or
        // (n_total if trailing else, else 0). Done with a chain of
        // EvalExpr+If ops.
        let target_slot = self.alloc_temp_slot_named("upd_target");
        let n = flat_branches.len();
        let has_trailing_else = matches!(flat_branches.last(), Some((None, _, _)));
        let cond_count = if has_trailing_else { n - 1 } else { n };

        // Default target value: n if trailing else, else 0.
        let default_value: i32 = if has_trailing_else { n as i32 } else { 0 };
        let mut tail_ops: Vec<LirOp> = vec![LirOp::StoreI32 {
            slot: target_slot,
            value: default_value,
        }];

        for ci in (0..cond_count).rev() {
            let (cond_opt, _, _) = flat_branches[ci];
            let cond = cond_opt.expect("flat_branches conditional entry must have a cond");
            let cond_slot = self.alloc_temp_slot_named("upd_cond");
            let cond_expr = self.intern_expr(cond);
            self.emit(LirOp::EvalExpr {
                expr: cond_expr,
                result: cond_slot,
            });
            let then_ops = vec![LirOp::StoreI32 {
                slot: target_slot,
                value: (ci as i32) + 1,
            }];
            let label = self.next_if_label();
            let prev_tail = std::mem::take(&mut tail_ops);
            tail_ops = vec![LirOp::If {
                cond: cond_slot,
                then_ops,
                else_ops: prev_tail,
                name: Some(format!("if{}_upd_pickb{}", label, ci)),
            }];
        }

        for op in tail_ops {
            self.emit(op);
        }

        // Load old active.
        let old_active = self.alloc_temp_slot_named("upd_old_active");
        self.emit(LirOp::LoadI32 {
            slot: active_flag,
            to: old_active,
        });

        // If old_active != target_idx: unmount the branch indexed by
        // (old_active - 1) if old_active > 0, then mount target_idx
        // branch (allocating fresh IfBranch first) if target_idx > 0.
        let neq_slot = self.alloc_temp_slot_named("upd_neq");
        self.emit(LirOp::I32Ne {
            lhs: old_active,
            rhs: target_slot,
            result: neq_slot,
        });

        // Build switch ops: for each possible (old_active value) we
        // emit `if old_active == k: unmount branch k-1`. Same for
        // target_idx -> mount. We chain via nested ifs. To keep
        // generated code small we emit one `if old_active == k`
        // per branch.
        let mut ops_when_changed: Vec<LirOp> = Vec::new();

        // Unmount the previously-active branch (if any).
        for k in 1..=n {
            let cmp = self.alloc_temp_slot_named("upd_old_eq");
            ops_when_changed.push(LirOp::I32EqConst {
                lhs: old_active,
                rhs: k as i32,
                result: cmp,
            });
            // Emit: if old_active == k: CallBlock unmount_block_{k-1}
            let (_, unmount_block) = branch_mount_unmount[k - 1];
            let label = self.next_if_label();
            ops_when_changed.push(LirOp::If {
                cond: cmp,
                then_ops: vec![LirOp::CallBlock {
                    block: unmount_block,
                    parent: parent_slot,
                }],
                else_ops: vec![],
                name: Some(format!("if{}_upd_unmb{}", label, k - 1)),
            });
        }

        // Mount the new target branch (if target > 0).
        for k in 1..=n {
            let cmp = self.alloc_temp_slot_named("upd_new_eq");
            ops_when_changed.push(LirOp::I32EqConst {
                lhs: target_slot,
                rhs: k as i32,
                result: cmp,
            });
            let (_, _, branch_id) = flat_branches[k - 1];
            let (mount_block, _) = branch_mount_unmount[k - 1];
            let ref_slot = self.alloc_temp_slot_typed_named(
                LirSlotValType::RefNullForBoundary(branch_id),
                "if_branch_upd_ref",
            );
            let label = self.next_if_label();
            ops_when_changed.push(LirOp::If {
                cond: cmp,
                then_ops: vec![
                    LirOp::AllocSubBoundary {
                        boundary_id: branch_id,
                        ref_slot,
                    },
                    LirOp::CallBlock {
                        block: mount_block,
                        parent: parent_slot,
                    },
                ],
                else_ops: vec![],
                name: Some(format!("if{}_upd_mntb{}", label, k - 1)),
            });
        }

        // Persist new active.
        ops_when_changed.push(LirOp::StoreI32Slot {
            slot: active_flag,
            from: target_slot,
        });

        let upd_label = self.next_if_label();
        self.emit(LirOp::If {
            cond: neq_slot,
            then_ops: ops_when_changed,
            else_ops: vec![],
            name: Some(format!("if{}_upd_diff", upd_label)),
        });

        // The per-(boundary, signal) walker dispatches this block via
        // `LirOp::CallBlock` which always pushes one i32 parent
        // argument. Reserve a dummy slot so the block signature is
        // `(self, parent_i32, owning_boundary_ref)` — matching the
        // walker's CallBlock shape.
        //
        // BoundaryField slots on the IfAnchor resolve via `parent_link`
        // walks rooted at `owning_boundary` (iter-body or root). No
        // fan-out wrap needed — the walker already binds every iter-
        // body on the descent path.
        let dummy_param = self.alloc_temp_slot_named("if_update_param_unused");
        let block = self.finish_block_named("if-update");
        if let Some(b) = self.blocks.iter_mut().find(|b| b.id == block) {
            b.params = vec![dummy_param];
            b.boundary_params = vec![owning_boundary];
        }
        block
    }

    /// Lower a for-loop node.
    /// Creates three blocks:
    /// - for-item-mount: renders body for each item (parent, item_ptr) -> ()
    /// - for-item-unmount: removes rendered node (node) -> ()
    /// - for-update: handles list updates (parent) -> ()
    ///
    /// Then emits inline loop ops in the current block to do initial mount.
    fn lower_for(
        &mut self,
        for_id: ForId,
        item: LocalId,
        item_ty: Ty,
        iterable: &LirExpr,
        body: &[LirNode],
        parent_slot: LirSlotId,
        for_node_id: NodeId,
    ) {
        // Resolve the synthesized ForAnchor boundary id and iter-body
        // boundary id for this for. The anchor lives at the for's NodeId
        // SubBoundary slot; the iter-body is named in `ForAnchor.kind`.
        let for_anchor_id = self.subboundary_target_for_node(for_node_id);
        let iter_body_id: TreeBoundaryId =
            match &self.tree_shape.boundaries[for_anchor_id.index()].kind {
                TreeBoundaryKind::ForAnchor { iter_body_idx, .. } => TreeBoundaryId(*iter_body_idx),
                other => panic!(
                    "lower_for: tree boundary {:?} kind is not ForAnchor: {:?}",
                    for_anchor_id, other
                ),
            };
        // BoundaryField slots — synthesizer's ForAnchor layout:
        //   field 0 = parent (DomHandle)
        //   field 1 = anchor (DomHandle)
        //   field 2 = children (ChildrenArray)
        let parent_field_slot = self.alloc_boundary_field_slot_named(
            for_anchor_id,
            0,
            format!("for{}_parent_field", for_id.0),
        );
        let anchor_field_slot = self.alloc_boundary_field_slot_named(
            for_anchor_id,
            1,
            format!("for{}_anchor_field", for_id.0),
        );

        // Allocate the ForAnchor sub-boundary BEFORE any field writes.
        // The op stores the ref on the parent's SubBoundary field and
        // registers it in `current_boundary_locals` so subsequent
        // BoundaryField accesses resolve in O(1).
        let for_anchor_ref_slot = self.alloc_temp_slot_typed_named(
            LirSlotValType::RefNullForBoundary(for_anchor_id),
            format!("for{}_anchor_ref", for_id.0),
        );
        self.emit(LirOp::AllocSubBoundary {
            boundary_id: for_anchor_id,
            ref_slot: for_anchor_ref_slot,
        });

        // Create anchor comment for this for-loop
        let anchor_slot = self.alloc_temp_slot_named("anchor_slot");
        let anchor_text = self.intern_string("for");
        self.emit(LirOp::CreateComment {
            content: anchor_text,
            result: anchor_slot,
        });
        self.emit(LirOp::AppendChild {
            parent: parent_slot,
            child: anchor_slot,
        });

        // Store parent and anchor for effects/updates
        self.emit(LirOp::StoreHandle {
            slot: parent_field_slot,
            from: parent_slot,
        });
        self.emit(LirOp::StoreHandle {
            slot: anchor_field_slot,
            from: anchor_slot,
        });

        // Calculate element size based on item type
        let element_size = self.compute_element_size(item_ty);

        // Classify the iterable expression
        let iterable_kind = self.classify_iterable(iterable);

        // Phase 5b-v.3: GC path only for component-local scalar-list signals
        // Phase 5b-v.3: only component-local list signals migrate to GC.
        // Literal ListConstruct iterables (e.g. `[a, b, c]`) and all
        // other Expr iterables stay memory-backed until Phase 5b-iv.
        // Phase 5e.4: list<string> uses GC array storage, but for-loop
        // iteration over strings stays on the legacy memory path
        // (item slot is fat-pointer ptr + companion len). When the
        // signal storage is GC and iteration is memory, the lower_for
        // path needs the materializer to produce (ptr, len) for the
        // legacy path. Easiest: only use ListGc when the element is a
        // *single-slot* GC value (not a string fat-pointer).
        let elem_is_string = matches!(
            self.ctx.ty_kind(item_ty),
            InternedTyKind::String
        );
        let is_gc_list = match &iterable_kind {
            IterableKind::Signal(def_id) => {
                self.is_scalar_list_ty(iterable.ty)
                    && !elem_is_string
                    && self.tree_signals.iter().any(|s| s.def_id == *def_id)
            }
            _ => false,
        };
        let list_ty = iterable.ty;

        // For ranges, reserve the scratch buf now — before pre-pass
        // and before lowering the body — so inner for-loops can
        // observe it on the outer's `ForContext` when they register
        // their update blocks (Phase 7 fan-out reads it to re-seed
        // the outer loop variable per-iter).
        let range_item_buf: Option<LirSlotId> = match &iterable_kind {
            IterableKind::Range { .. } => {
                Some(self.alloc_memory_slot_named(4, format!("for{}_range_item_buf", for_id.0)))
            }
            _ => None,
        };

        // Collect outer items from local_bindings (for nested for-loops)
        // These need to be stored to memory so inner blocks can access them
        let outer_items: Vec<(LocalId, LirSlotId, Ty, LirBindingMode)> = self
            .local_bindings
            .iter()
            .map(|(id, (slot, ty, mode))| (*id, *slot, *ty, *mode))
            .collect();

        // For each outer item currently in scope, materialize a
        // `BoundaryField` slot pointing at field 0 of that for's
        // synthesized `ForIterBody` boundary (the loop-variable slot
        // — see `tree_shape::synthesize`). Replaces the legacy
        // linear-memory stash + per-block reload — reads now go
        // through the iter-body ref bound in `current_boundary_locals`
        // (provided by the inner for-mount block's `boundary_params`
        // and by the fan-out wrap loop's `BindBoundaryLocal`).
        //
        // Outer items that aren't loop variables of an enclosing for
        // (e.g. handler-bound locals) have no iter-body to reference —
        // they're skipped here. Body code reading them resolves via
        // the per-block `local_to_slot` mechanism, unchanged.
        let mut outer_item_field_slots: HashMap<LocalId, (Ty, LirSlotId, LirBindingMode)> =
            HashMap::new();
        for (outer_id, _outer_slot, outer_ty, outer_mode) in &outer_items {
            if let Some(&outer_iter_body_id) = self.for_item_iter_body.get(outer_id) {
                let field_slot = self.alloc_boundary_field_slot_named(
                    outer_iter_body_id,
                    0,
                    "for_outer_item_field",
                );
                outer_item_field_slots.insert(*outer_id, (*outer_ty, field_slot, *outer_mode));
            }
        }

        let ctx = ForContext {
            id: for_id,
            parent: self.for_stack.last().copied(),
            range_item_buf,
        };
        self.for_contexts.insert(for_id, ctx);

        // Body lowering (inside the for-item-mount block) must know we
        // are inside this for so derived-signal `current_enclosing_boundary`
        // and nested-for diff inputs see the innermost iter-body.
        self.for_stack.push(for_id);
        self.for_iter_body_stack.push(iter_body_id);

        // Create for-item-mount block: (parent, item_ptr) -> ()
        // `for_ctx` was populated just above; pull it out of `for_contexts`
        // by id (safe — we inserted it in this function before the call).
        let for_ctx_snapshot = self
            .for_contexts
            .get(&for_id)
            .cloned()
            .expect("ForContext populated above");

        let item_binding_mode = if is_gc_list {
            LirBindingMode::Value
        } else {
            LirBindingMode::Ptr
        };

        let mount_block = self.create_for_item_mount_block(
            item,
            item_ty,
            body,
            &outer_item_field_slots,
            item_binding_mode,
            &for_ctx_snapshot,
            iter_body_id,
        );

        let popped = self.for_stack.pop();
        debug_assert_eq!(popped, Some(for_id), "block_lower for_stack mismatch");
        let popped_iter = self.for_iter_body_stack.pop();
        debug_assert_eq!(
            popped_iter,
            Some(iter_body_id),
            "block_lower for_iter_body_stack mismatch"
        );

        // Create for-item-unmount block: (node) -> ()
        let unmount_block = self.create_for_item_unmount_block();

        // Register an update effect for any iterable whose expression
        // transitively reads signals. This includes:
        //   - bare signal reads (`for x in items`)
        //   - list literals over signals (`for x in [a, b, c]`)
        //   - field accesses like `Store.items`
        //   - stdlib calls like `list.filter(..)` over signal-bearing args
        //   - ranges with signal bounds (`for i in 0..count`)
        // Iterables with no signal dependencies (constant ranges, pure
        // literals) skip effect registration — they can never go stale.
        let iterable_deps = self.collect_dependencies(iterable);
        if !iterable_deps.is_empty() {
            // Re-classify the iterable for the update block so it gets a
            // FRESH set of ExprIds (separate from the ones initial-mount
            // emitted). This matters for stdlib calls like `list.filter`
            // whose codegen assigns a monotonic call-site index per
            // encountered ExprId in component.exprs — sharing a single
            // ExprId between init and update would mint two filter calls
            // but register only one entry in `filter_calls`, tripping
            // `Filter N not found in filter_calls` at codegen time.
            let update_iterable_kind = self.classify_iterable(iterable);
            let update_block = self.create_for_update_block_reactive(
                &update_iterable_kind,
                element_size,
                is_gc_list,
                list_ty,
                mount_block,
                unmount_block,
                parent_field_slot,
                &for_ctx_snapshot,
                iter_body_id,
            );
            self.effects.push(LirBlockEffect {
                id: self.effects.len() as u32,
                dependencies: iterable_deps.clone(),
                update_block,
            });
            // Register as a PendingBinding and capture the
            // update_block for per-(boundary, signal) dispatch.
            // owning_boundary is the parent boundary of the ForAnchor.
            let binding_id = self.effects.len() as u32 - 1;
            let owning_boundary = self
                .tree_shape
                .node_field
                .get(&for_node_id)
                .map(|nfr| nfr.owning_boundary)
                .unwrap_or(TreeBoundaryId(u32::MAX));
            self.binding_collector.push(PendingBinding {
                owning_boundary,
                dependencies: iterable_deps.clone(),
                kind: PendingBindingKind::ForListReroute,
                binding_id,
            });
            self.forlist_binding_data.insert(
                binding_id,
                StructuralBindingInfo {
                    owning_boundary,
                    dependencies: iterable_deps,
                    update_block,
                },
            );
        }

        // === Emit inline loop ops for initial mount ===
        // Temp slots for loop variables
        // For list iteration: list_ptr = pointer to first element, list_len = count
        // For range iteration: list_ptr = start value (repurposed), list_len = count
        // GC-list: list_ptr unused; list_ref_slot holds the array ref instead.
        let list_ptr = self.alloc_temp_slot_named("init_list_ptr");
        let list_ref_slot = if is_gc_list {
            Some(self.alloc_temp_slot_typed_named(
                LirSlotValType::RefNullForListGc(list_ty),
                format!("for{}_init_list_ref", for_id.0),
            ))
        } else {
            None
        };
        let list_len = self.alloc_temp_slot_named("init_list_len");
        let index = self.alloc_temp_slot_named("init_index");
        // Phase 5e.1: when iterating a GC-array list of records, the
        // item slot holds a record GC ref (the result of `array.get`).
        // For scalar GC lists, it's an unboxed scalar (i32/i64/f32/f64
        // as appropriate). For memory/range lists it's an i32 ptr.
        let item_ptr = if is_gc_list {
            self.alloc_temp_slot_typed_named(self.ty_to_slot_val_type(item_ty), "init_item_ptr")
        } else {
            self.alloc_temp_slot_named("init_item_ptr")
        };
        let break_cond = self.alloc_temp_slot_named("init_break_cond");

        // `range_item_buf` was reserved up-front so inner for-loops
        // could see it on the outer's ForContext during their own
        // update-block emission.

        // Load list or range based on iterable kind
        match &iterable_kind {
            IterableKind::Signal(signal_def_id) => {
                if is_gc_list {
                    self.emit(LirOp::LoadListGc {
                        signal: *signal_def_id,
                        ref_result: list_ref_slot.unwrap(),
                        len_result: list_len,
                    });
                } else {
                    self.emit(LirOp::LoadList {
                        signal: *signal_def_id,
                        ptr_result: list_ptr,
                        len_result: list_len,
                    });
                }
            }
            IterableKind::Expr { expr_id } => {
                if is_gc_list {
                    self.emit(LirOp::EvalListExprGc {
                        expr: *expr_id,
                        ref_result: list_ref_slot.unwrap(),
                        len_result: list_len,
                    });
                } else {
                    self.emit(LirOp::EvalListExpr {
                        expr: *expr_id,
                        ptr_result: list_ptr,
                        len_result: list_len,
                    });
                }
            }
            IterableKind::Range {
                start,
                end,
                inclusive,
            } => {
                // Evaluate start and end expressions
                let end_slot = self.alloc_temp_slot_named("end_slot");
                self.emit(LirOp::EvalExpr {
                    expr: *start,
                    result: list_ptr, // Repurpose list_ptr to hold range start
                });
                self.emit(LirOp::EvalExpr {
                    expr: *end,
                    result: end_slot,
                });
                // Compute len = end - start
                self.emit(LirOp::SubSlots {
                    a: end_slot,
                    b: list_ptr,
                    result: list_len,
                });
                // For inclusive range, add 1 to len (start..=end has end-start+1 elements)
                if *inclusive {
                    self.emit(LirOp::IncrSlot { slot: list_len });
                }
            }
            IterableKind::Unsupported => {
                todo!("Unsupported iterable expression in for-loop (no LIR classifier)");
            }
        }

        // Allocate the typed children-array `(array (mut (ref null
        // <iter_body_struct>)))` and store its ref into the
        // `ForAnchor.children` BoundaryField slot (field 2). The
        // ForAnchor's parent_link is recorded at synthesis time so
        // BoundaryField writes resolve via the in-scope ForAnchor ref
        // for top-level fors (chains through `$self.tree`) or via the
        // outer iter-body for nested fors (chains through the bound
        // outer iter-body local).
        let gc_arr_slot = self.alloc_temp_slot_typed_named(
            LirSlotValType::RefNullForChildrenArray(for_anchor_id),
            format!("for{}_children_arr", for_id.0),
        );
        self.emit(LirOp::ChildrenArrayNewDefault {
            anchor_boundary: for_anchor_id,
            len: list_len,
            result: gc_arr_slot,
        });
        let children_field_slot = self.alloc_boundary_field_slot_named(
            for_anchor_id,
            2,
            format!("for{}_children_field", for_id.0),
        );
        self.emit(LirOp::StoreHandle {
            slot: children_field_slot,
            from: gc_arr_slot,
        });

        // Init index to 0 and compute initial break_cond (exit immediately if list is empty)
        self.emit(LirOp::SetSlot {
            slot: index,
            value: 0,
        });
        self.emit(LirOp::GeU {
            index,
            len: list_len,
            result: break_cond,
        });

        // Build loop body ops
        // Order: compute item ptr, do work, increment, then compute break condition for NEXT iteration
        let mut loop_body = Vec::new();

        // Compute item_ptr based on iterable kind. For ranges the
        // item VALUE is stashed in the record's `Item` field so the
        // update fan-out walk can recover the loop variable per iter
        // without relying on shared linear-memory scratch.
        let item_value_for_record = self.alloc_temp_slot_named("item_value_for_record");
        let mut have_item_value = false;
        match (&iterable_kind, range_item_buf) {
            (IterableKind::Range { .. }, Some(item_buf)) => {
                loop_body.push(LirOp::AddSlots {
                    a: list_ptr,
                    b: index,
                    result: item_value_for_record,
                });
                have_item_value = true;
                // Store item_value to the shared memory buffer; item_ptr
                // is its address. Kept during cutover so in-body reads
                // of `row`/`col` continue to resolve through item_ptr
                // dereference until the expression layer learns to
                // read from struct fields directly.
                loop_body.push(LirOp::StoreHandle {
                    slot: item_buf,
                    from: item_value_for_record,
                });
                loop_body.push(LirOp::GetSlotAddress {
                    mem_slot: item_buf,
                    result: item_ptr,
                });
            }
            _ if is_gc_list => {
                // Phase 5b-v.3: GC-list item — read element directly.
                loop_body.push(LirOp::ArrayGetItem {
                    arr: list_ref_slot.unwrap(),
                    idx: index,
                    list_ty,
                    result: item_ptr,
                });
            }
            _ => {
                // For lists: item_ptr = list_ptr + index * element_size
                loop_body.push(LirOp::ComputeItemPtr {
                    base: list_ptr,
                    index,
                    element_size,
                    result: item_ptr,
                });
            }
        }

        // Allocate a fresh ForIterBody boundary struct. Its
        // `parent_link` is `None` (synthesizer marks ForIterBody
        // as reachable only via the for-anchor's children-array, not
        // a static SubBoundary field), so we use the parent-less
        // `AllocBoundary` op rather than `AllocSubBoundary`. The
        // ref is plumbed to the mount block via `boundary_params`;
        // nested `if` AllocSubBoundary inside the for body resolves
        // its parent through this ref via `current_boundary_locals`.
        let iter_body_slot = self.alloc_temp_slot_typed_named(
            LirSlotValType::RefNullForBoundary(iter_body_id),
            format!("for{}_init_iter_body", for_id.0),
        );
        loop_body.push(LirOp::AllocBoundary {
            boundary_id: iter_body_id,
            ref_slot: iter_body_slot,
        });
        // Bind the freshly-allocated iter-body ref into scope so the
        // boundary_params plumbing in CallBlock2 finds it via
        // `current_boundary_locals`. (AllocBoundary already inserts
        // it; this BindBoundaryLocal is a no-op redundant guard but
        // keeps the contract explicit.) Also lets the loop-variable
        // store below resolve via the in-scope local.
        loop_body.push(LirOp::BindBoundaryLocal {
            boundary_id: iter_body_id,
            slot: iter_body_slot,
        });
        // Populate iter-body field 0 (LoopVar) with the per-iter
        // `item_ptr` — address-of-buf for ranges, pointer-into-list
        // for lists. Nested fors read this field via a
        // `BoundaryField` slot at block entry and treat it as the
        // address-style item_ptr so in-body deref expressions resolve
        // unchanged. For ranges the per-iter VALUE additionally
        // lands in field 2 (`loop_var_value`) below, which the
        // fan-out wrap reads to re-seed the shared scratch buffer.
        let init_loop_var_slot =
            self.alloc_boundary_field_slot_named(iter_body_id, 0, "for_init_loop_var");
        loop_body.push(LirOp::StoreHandle {
            slot: init_loop_var_slot,
            from: item_ptr,
        });
        if have_item_value {
            // Stash the per-iter range VALUE into iter-body field 2
            // (`loop_var_value`) so the fan-out wrap can recover it
            // after `range_item_buf` has been overwritten by later
            // iterations. iter-body field 1 is `root_handle`,
            // populated inside the mount block.
            let init_loop_var_value_slot =
                self.alloc_boundary_field_slot_named(iter_body_id, 2, "for_init_loop_var_value");
            loop_body.push(LirOp::StoreHandle {
                slot: init_loop_var_value_slot,
                from: item_value_for_record,
            });
        }
        loop_body.push(LirOp::CallBlock2 {
            block: mount_block,
            param0: parent_slot,
            param1: item_ptr,
            result: None,
        });
        // Publish the per-iteration iter-body ref into the for-anchor's
        // typed children-array. Fan-out walks read iter-body refs out
        // of this array directly (no iter-rec indirection).
        loop_body.push(LirOp::ChildrenArraySet {
            anchor_boundary: for_anchor_id,
            arr: gc_arr_slot,
            idx: index,
            value: iter_body_slot,
        });

        // Increment index
        loop_body.push(LirOp::IncrSlot { slot: index });

        // Check break condition for NEXT iteration: index >= list_len
        loop_body.push(LirOp::GeU {
            index,
            len: list_len,
            result: break_cond,
        });

        // Emit the loop
        self.emit(LirOp::Loop {
            break_cond,
            body_ops: loop_body,
            name: Some(format!("for{}_init", for_id.0)),
        });
    }

    /// Classify an iterable expression for a for-loop.
    fn classify_iterable(&mut self, expr: &LirExpr) -> IterableKind {
        match &expr.kind {
            // Signal reads are reactive - create update effects
            LirExprKind::SignalRead(def_id) => IterableKind::Signal(*def_id),
            // All other list-producing expressions: literals, field accesses,
            // calls returning lists (e.g. list.filter/map stdlib calls), etc.
            LirExprKind::ListConstruct { .. }
            | LirExprKind::ListStatic { .. }
            | LirExprKind::Field { .. }
            | LirExprKind::Call { .. }
            | LirExprKind::GlobalCall { .. } => {
                let expr_id = self.intern_expr(expr);
                IterableKind::Expr { expr_id }
            }
            // Range expressions: start..end or start..=end
            LirExprKind::Range {
                start,
                end,
                inclusive,
            } => {
                let start_id = self.intern_expr(start);
                let end_id = self.intern_expr(end);
                IterableKind::Range {
                    start: start_id,
                    end: end_id,
                    inclusive: *inclusive,
                }
            }
            _ => IterableKind::Unsupported,
        }
    }

    /// Phase 5b-v.3 / 5e.1: true iff `ty` is a `list<T>` where T migrates
    /// to a typed GC `(array (mut <elem>))`. Includes primitive scalars
    /// and DTR records.
    fn is_scalar_list_ty(&self, ty: Ty) -> bool {
        let elem = match self.ctx.ty_kind(ty) {
            InternedTyKind::List(e) => *e,
            _ => return false,
        };
        if matches!(
            self.ctx.ty_kind(elem),
            InternedTyKind::Bool
                | InternedTyKind::S8
                | InternedTyKind::S16
                | InternedTyKind::S32
                | InternedTyKind::U8
                | InternedTyKind::U16
                | InternedTyKind::U32
                | InternedTyKind::S64
                | InternedTyKind::U64
                | InternedTyKind::F32
                | InternedTyKind::F64
                | InternedTyKind::Char
        ) || matches!(self.ctx.ty_kind(elem), InternedTyKind::Adt(d) if matches!(self.ctx.defs.kind(*d), DefKind::Enum(_)))
        {
            return true;
        }
        // Phase 5e.2: nested lists — list<list<...>> where inner is
        // itself GC-eligible recursively.
        if matches!(self.ctx.ty_kind(elem), InternedTyKind::List(_))
            && self.is_scalar_list_ty(elem)
        {
            return true;
        }
        // Phase 5e.4: strings — element type is the shared $fat_value.
        if matches!(self.ctx.ty_kind(elem), InternedTyKind::String) {
            return true;
        }
        // Phase 5e.5: option<scalar-i32-fits> reuses $fat_value as box
        // (disc + payload, both i32). Conservative: only allow option
        // wrapping single-i32 inner types (bool/narrow ints/s32/u32/
        // char/enum/f32). The detailed canonical-flat check is on the
        // codegen side; here we approximate structurally.
        if let InternedTyKind::Option(inner) = self.ctx.ty_kind(elem) {
            let inner_ty = *inner;
            let inner_fits = matches!(
                self.ctx.ty_kind(inner_ty),
                InternedTyKind::Bool
                    | InternedTyKind::S8
                    | InternedTyKind::S16
                    | InternedTyKind::S32
                    | InternedTyKind::U8
                    | InternedTyKind::U16
                    | InternedTyKind::U32
                    | InternedTyKind::F32
                    | InternedTyKind::Char
            ) || matches!(
                self.ctx.ty_kind(inner_ty),
                InternedTyKind::Adt(d) if matches!(self.ctx.defs.kind(*d), DefKind::Enum(_))
            );
            if inner_fits {
                return true;
            }
        }
        let mut seen = std::collections::HashSet::new();
        self.is_dtr_record_ty(elem, &mut seen)
    }

    fn is_dtr_record_ty(&self, ty: Ty, seen: &mut HashSet<DefId>) -> bool {
        let def_id = match self.ctx.ty_kind(ty) {
            InternedTyKind::Adt(d) => *d,
            _ => return false,
        };
        let record = match self.ctx.defs.kind(def_id) {
            DefKind::Record(r) => r.clone(),
            _ => return false,
        };
        if !seen.insert(def_id) {
            return true;
        }
        let result = (|| {
            for &field_def_id in &record.fields {
                let field_ty = match self.ctx.defs.kind(field_def_id) {
                    DefKind::Field(f) => f.ty,
                    _ => return false,
                };
                if !self.is_dtr_field_ty(field_ty, seen) {
                    return false;
                }
            }
            true
        })();
        seen.remove(&def_id);
        result
    }

    fn is_dtr_field_ty(&self, ty: Ty, seen: &mut HashSet<DefId>) -> bool {
        if matches!(
            self.ctx.ty_kind(ty),
            InternedTyKind::Bool
                | InternedTyKind::S8
                | InternedTyKind::S16
                | InternedTyKind::S32
                | InternedTyKind::U8
                | InternedTyKind::U16
                | InternedTyKind::U32
                | InternedTyKind::S64
                | InternedTyKind::U64
                | InternedTyKind::F32
                | InternedTyKind::F64
                | InternedTyKind::Char
        ) || matches!(self.ctx.ty_kind(ty), InternedTyKind::Adt(d) if matches!(self.ctx.defs.kind(*d), DefKind::Enum(_)))
        {
            return true;
        }
        match self.ctx.ty_kind(ty) {
            InternedTyKind::String => true,
            // Phase 5e.6: extend DTR to any GC-array-eligible list, so
            // records with `list<string>`, `list<record>`, nested lists,
            // etc. can use a typed `(ref null $<rec>)` field on the
            // component struct instead of two i32 slots.
            InternedTyKind::List(_) => self.is_scalar_list_ty(ty),
            InternedTyKind::Adt(d) => match self.ctx.defs.kind(*d) {
                DefKind::Record(_) => self.is_dtr_record_ty(ty, seen),
                _ => false,
            },
            _ => false,
        }
    }

    /// Compute the size of an element type in bytes.
    fn compute_element_size(&self, ty: Ty) -> u32 {
        match self.ctx.ty_kind(ty) {
            InternedTyKind::S8 | InternedTyKind::U8 | InternedTyKind::Bool => 1,
            InternedTyKind::S16 | InternedTyKind::U16 => 2,
            InternedTyKind::S32
            | InternedTyKind::U32
            | InternedTyKind::F32
            | InternedTyKind::Char => 4,
            InternedTyKind::S64 | InternedTyKind::U64 | InternedTyKind::F64 => 8,
            InternedTyKind::String | InternedTyKind::List(_) => 8, // ptr + len
            InternedTyKind::Adt(def_id) => {
                // For records, compute total size from fields
                if let Some(record) = self.ctx.defs.as_record(*def_id) {
                    let mut size = 0u32;
                    for &field_def_id in &record.fields {
                        // Look up field type from its DefId
                        if let DefKind::Field(f) = self.ctx.defs.kind(field_def_id) {
                            size += self.compute_element_size(f.ty);
                        }
                    }
                    // Align to 4 bytes
                    (size + 3) & !3
                } else {
                    4 // Enum discriminant
                }
            }
            InternedTyKind::Tuple(elems) => {
                let mut size = 0u32;
                for elem_ty in elems {
                    size += self.compute_element_size(*elem_ty);
                }
                (size + 3) & !3
            }
            _ => 4, // Default
        }
    }

    /// Create the for-item-mount block.
    /// Called for each item with: (parent: i32, item_ptr: i32) -> ()
    fn create_for_item_mount_block(
        &mut self,
        item: LocalId,
        item_ty: Ty,
        body: &[LirNode],
        outer_item_field_slots: &HashMap<LocalId, (Ty, LirSlotId, LirBindingMode)>,
        item_binding_mode: LirBindingMode,
        _for_ctx: &ForContext,
        iter_body_id: TreeBoundaryId,
    ) -> BlockId {
        self.start_block();

        // Allocate fresh slots for the block's 2 params: (parent,
        // item_ptr). The per-iteration `ForIterBody` boundary ref is
        // plumbed via `boundary_params` (registered in scope at
        // function entry) so nested `if`s resolve their parent
        // boundary; no separate iter-record param is needed.
        let block_parent = self.alloc_temp_slot_named("parent");
        // Phase 5e.1: when item_binding_mode is Value AND item_ty is
        // a DTR record, item_ptr is a typed record GC ref (not i32).
        let item_ptr_slot = if matches!(item_binding_mode, LirBindingMode::Value) {
            self.alloc_temp_slot_typed_named(self.ty_to_slot_val_type(item_ty), "item_ptr")
        } else {
            self.alloc_temp_slot_named("item_ptr")
        };

        // Allocate the iteration's host-fragment wrapper element
        // (`yel-frag`). Body content gets appended into the wrapper
        // instead of the for's parent, so a single
        // `Remove(wrapper)` in the for-item-unmount block cascades
        // to detach every DOM node the iteration owns — regardless
        // of body shape (Element-first, If-first, For-first, or
        // multiple top-level siblings). Replaces the prior
        // "first DOM op = root_handle" heuristic.
        let wrapper_slot = self.alloc_temp_slot_named("iter_wrapper");
        self.emit(LirOp::CreateFragment {
            result: wrapper_slot,
        });
        self.emit(LirOp::AppendChild {
            parent: block_parent,
            child: wrapper_slot,
        });
        // Stash the wrapper into iter-body field 1 (the `wrapper`
        // DomHandle). The for-item-unmount block reads this field
        // and calls host `remove` on it.
        let wrapper_field_slot =
            self.alloc_boundary_field_slot_named(iter_body_id, 1, "for_iter_wrapper");
        self.emit(LirOp::StoreHandle {
            slot: wrapper_field_slot,
            from: wrapper_slot,
        });

        // The iter-body's loop-variable field 0 is populated by the
        // CALLER (init/update mount loops) before invoking this block —
        // they have both the item pointer and the range value at hand
        // and choose the right one based on iterable kind. The body of
        // this block resolves outer-item BoundaryField reads via the
        // iter-body ref passed as `boundary_params`.

        // Load outer items from boundary fields and update local_bindings.
        // Phase 5b-v.3: propagate BindingMode from the outer map so GC-list
        // items use Value (direct slot) and memory/range items use Ptr (deref).
        let mut outer_loaded_slots: HashMap<LocalId, LirSlotId> = HashMap::new();
        for (outer_id, (outer_ty, mem_slot, outer_mode)) in outer_item_field_slots {
            // Phase 5e.6: type the temp slot based on the outer item's
            // type. For typed records / GC arrays the boundary field
            // stores a typed ref, so the loaded local must match.
            let temp_slot = if matches!(*outer_mode, LirBindingMode::Value) {
                self.alloc_temp_slot_typed_named(
                    self.ty_to_slot_val_type(*outer_ty),
                    "temp_slot",
                )
            } else {
                self.alloc_temp_slot_named("temp_slot")
            };
            self.emit(LirOp::LoadHandle {
                slot: *mem_slot,
                to: temp_slot,
            });
            self.local_bindings
                .insert(*outer_id, (temp_slot, *outer_ty, *outer_mode));
            outer_loaded_slots.insert(*outer_id, temp_slot);
        }

        // Store the current item binding info for expression lowering.
        // Phase 5b-v.3: use `item_binding_mode` passed by the caller —
        // `Ptr` for memory/range lists, `Value` for GC scalar-list items.
        self.local_bindings
            .insert(item, (item_ptr_slot, item_ty, item_binding_mode));
        // Track the iter-body that holds this item's loop-variable
        // field so nested for-loops can recover it as an outer-item
        // BoundaryField slot.
        self.for_item_iter_body.insert(item, iter_body_id);

        // Update outer_item_field_slots so nested blocks (if-branches, etc.) can access for-loop items
        // Save old outer_item_field_slots and restore after lowering body
        let old_outer_item_field_slots = std::mem::take(&mut self.outer_item_field_slots);

        // Copy inherited outer items (they're already stored to memory)
        for (outer_id, (outer_ty, mem_slot, outer_mode)) in outer_item_field_slots {
            self.outer_item_field_slots
                .insert(*outer_id, (*outer_ty, *mem_slot, *outer_mode));
        }

        // The iter-body boundary's field 0 is the loop variable.
        // Allocate a BoundaryField slot pointing at it so nested
        // if-branches and fors inside the body resolve outer-item
        // reads via `LoadHandle` on this slot.
        let item_field_slot =
            self.alloc_boundary_field_slot_named(iter_body_id, 0, "for_item_field");
        self.outer_item_field_slots
            .insert(item, (item_ty, item_field_slot, item_binding_mode));

        // Lower each body node into the wrapper so all DOM the iter
        // creates becomes a child of the wrapper.
        for node in body {
            self.lower_node(node, wrapper_slot);
        }

        // Restore outer_item_field_slots
        self.outer_item_field_slots = old_outer_item_field_slots;

        // Clean up bindings
        self.local_bindings.remove(&item);
        self.for_item_iter_body.remove(&item);
        for outer_id in outer_item_field_slots.keys() {
            self.local_bindings.remove(outer_id);
        }

        let block_id = self.finish_block_named("for-item-mount");

        // Record params + captured_locals for this block. Params drive
        // the function signature: `(parent: i32, item_ptr: i32,
        // iter_body: (ref null $<iter_body>), <ancestor iter-bodies>...) -> ()`.
        // The boundary params are plumbed via `boundary_params` so the
        // dynamic per-block function-type emission picks them up and
        // codegen registers them in `current_boundary_locals` at function
        // entry. That makes nested `if` AllocSubBoundary inside the
        // for-iter body resolve its parent (the iter-body boundary).
        // The block is void — it stashes the root DOM handle into the
        // iter-body boundary's `root_handle` field (emitted above) instead
        // of returning it.
        if let Some(block) = self.blocks.iter_mut().find(|b| b.id == block_id) {
            block.params = vec![block_parent, item_ptr_slot];
            // Boundary chain: own iter-body first, followed by every
            // enclosing for's iter-body (innermost-first). Lets reads
            // of `outer_item_field_slots` BoundaryField slots resolve via
            // the function's typed boundary params instead of a
            // memory-buffer round-trip. The outer-most-first ordering
            // is fine — `current_boundary_locals` is keyed by id.
            let mut bp: Vec<TreeBoundaryId> = vec![iter_body_id];
            // The active for_iter_body_stack contains all enclosing
            // iter-bodies plus our own (pushed in `lower_for` before
            // this fn was called). Skip the trailing entry (== own id)
            // and add ancestors innermost-first.
            let stack = &self.for_iter_body_stack;
            if stack.last() != Some(&iter_body_id) {
                panic!(
                    "create_for_item_mount_block: top of for_iter_body_stack ({:?}) \
                     does not match own iter_body_id ({:?})",
                    stack.last(),
                    iter_body_id
                );
            }
            for &anc in stack[..stack.len() - 1].iter().rev() {
                bp.push(anc);
            }
            block.boundary_params = bp;
            block.captured_locals.insert(item, item_ptr_slot);
            block.local_to_slot = outer_loaded_slots.clone();
            // Phase 5b-v.3: item mode comes from the caller; outer modes
            // from the `outer_item_field_slots` BindingMode entries.
            block.local_modes.insert(item, item_binding_mode);
            for (outer_id, (_, _, outer_mode)) in outer_item_field_slots {
                block.local_modes.insert(*outer_id, *outer_mode);
            }
            block.return_slot = None;
        }

        block_id
    }

    /// Create the for-item-unmount block.
    /// Called to remove a rendered item: (node: i32) -> ()
    fn create_for_item_unmount_block(&mut self) -> BlockId {
        self.start_block();

        // Single-param block: takes the node to remove.
        let node_slot = self.alloc_temp_slot_named("node");
        self.emit(LirOp::Remove { node: node_slot });

        let block_id = self.finish_block_named("for-item-unmount");
        if let Some(block) = self.blocks.iter_mut().find(|b| b.id == block_id) {
            block.params = vec![node_slot];
        }
        block_id
    }

    /// Create the for-update block.
    /// Handles list updates: unmounts old items, mounts new items.
    ///
    /// Re-evaluates the iterable expression per-variant:
    /// - `IterableKind::Signal` — `LoadList` from the signal slot (fat ptr).
    /// - `IterableKind::Expr`   — `EvalListExpr` on the full expression, so
    ///   list literals (`[a, b, c]`), field accesses (`Store.items`), and
    ///   stdlib calls (`list.filter(..)`) all re-run on every signal write.
    /// - `IterableKind::Range`  — re-evaluate start/end expressions and
    ///   recompute `len = end - start (+1 if inclusive)`; the iteration
    ///   counter doubles as the "base" that the mount path adds index to.
    ///
    /// For `Unsupported` iterables the block_lower pipeline never reaches
    /// here with non-empty deps (classify_iterable returns `Unsupported`
    /// only for expression shapes that can't carry signal reads today);
    /// `unreachable!` fails loudly if that invariant is ever violated.
    // Args are heterogeneous (iterable shape, layout sizes, block ids, anchor
    // slots, for-context) and don't form a coherent group; bundling would
    // create a one-shot struct that obscures rather than clarifies.
    #[allow(clippy::too_many_arguments)]
    fn create_for_update_block_reactive(
        &mut self,
        iterable: &IterableKind,
        element_size: u32,
        is_gc_list: bool,
        list_ty: Ty,
        mount_block: BlockId,
        unmount_block: BlockId,
        parent_field_slot: LirSlotId,
        for_ctx: &ForContext,
        iter_body_id: TreeBoundaryId,
    ) -> BlockId {
        self.start_block();

        // If this for is nested, capture the diff body so we can wrap
        // it in an outer walk. The outer walk fans the entire diff
        // out across every outer iteration.
        let nested_parent = for_ctx.parent;

        // Temp slots — named for debuggability. When things go wrong
        // inside the diff, the WAT dump should read like the source.
        let parent_slot = self.alloc_temp_slot_named("upd_parent");
        let old_len = self.alloc_temp_slot_named("upd_old_len");
        let new_list_ptr = self.alloc_temp_slot_named("upd_new_list_ptr");
        let new_list_ref_slot = if is_gc_list {
            Some(self.alloc_temp_slot_typed_named(
                LirSlotValType::RefNullForListGc(list_ty),
                "upd_new_list_ref",
            ))
        } else {
            None
        };
        let new_len = self.alloc_temp_slot_named("upd_new_len");
        let index = self.alloc_temp_slot_named("upd_index");
        let break_cond = self.alloc_temp_slot_named("upd_break_cond");
        // Phase 5e.1: when iterating a GC-array list, the item slot
        // holds the typed array element directly (a record GC ref or
        // unboxed scalar). Otherwise it's a memory ptr (i32).
        let item_ptr = if is_gc_list {
            let item_ty = match self.ctx.ty_kind(list_ty) {
                InternedTyKind::List(e) => *e,
                _ => list_ty,
            };
            self.alloc_temp_slot_typed_named(
                self.ty_to_slot_val_type(item_ty),
                "upd_item_ptr",
            )
        } else {
            self.alloc_temp_slot_named("upd_item_ptr")
        };

        // REUSE the for-context's `range_item_buf` rather than
        // allocating a fresh one. Both mount and update paths must
        // share the same addressable storage so expression lowering
        // that dereferences `item_ptr` resolves consistently across
        // the initial-mount and diff-driven reflows.
        let range_item_buf = for_ctx.range_item_buf;

        // The for-anchor's typed children-array (element type =
        // ForIterBody struct) is the per-for tracking array. Old and
        // new arrays are typed `(ref null <children_arr_ty>)`.
        let for_anchor_id = self.for_anchor_id_for(for_ctx.id);
        let old_arr_slot = self.alloc_temp_slot_typed_named(
            LirSlotValType::RefNullForChildrenArray(for_anchor_id),
            "upd_old_arr",
        );
        let new_arr_slot = self.alloc_temp_slot_typed_named(
            LirSlotValType::RefNullForChildrenArray(for_anchor_id),
            "upd_new_arr",
        );

        // Build the diff body — a sequence of ops that assumes
        // `old_arr_slot` is valid (either a WASM global for top-level
        // fors, or paged in from the outer record for nested fors).
        // The top-level case emits the load inline before the body;
        // the nested case emits the load inside the outer-walk wrap.
        let diff_ops: Vec<LirOp> = {
            let mut ops: Vec<LirOp> = Vec::new();
            // Load parent from the ForAnchor's BoundaryField. For nested
            // fors this resolves through the outer iter-body ref bound
            // by the wrap loop's `BindBoundaryLocal`, so it must be
            // emitted INSIDE the diff body (wrapped by ancestor walks),
            // not at the top of the function.
            ops.push(LirOp::LoadHandle {
                slot: parent_field_slot,
                to: parent_slot,
            });
            // old_len = array.len old_arr
            ops.push(LirOp::ArrayLen {
                arr: old_arr_slot,
                result: old_len,
            });
            // Re-evaluate iterable for (new_list_ptr/ref, new_len).
            match iterable {
                IterableKind::Signal(signal) => {
                    if is_gc_list {
                        ops.push(LirOp::LoadListGc {
                            signal: *signal,
                            ref_result: new_list_ref_slot.unwrap(),
                            len_result: new_len,
                        });
                    } else {
                        ops.push(LirOp::LoadList {
                            signal: *signal,
                            ptr_result: new_list_ptr,
                            len_result: new_len,
                        });
                    }
                }
                IterableKind::Expr { expr_id } => {
                    if is_gc_list {
                        ops.push(LirOp::EvalListExprGc {
                            expr: *expr_id,
                            ref_result: new_list_ref_slot.unwrap(),
                            len_result: new_len,
                        });
                    } else {
                        ops.push(LirOp::EvalListExpr {
                            expr: *expr_id,
                            ptr_result: new_list_ptr,
                            len_result: new_len,
                        });
                    }
                }
                IterableKind::Range {
                    start,
                    end,
                    inclusive,
                } => {
                    let end_slot = self.alloc_temp_slot_named("end_slot");
                    ops.push(LirOp::EvalExpr {
                        expr: *start,
                        result: new_list_ptr,
                    });
                    ops.push(LirOp::EvalExpr {
                        expr: *end,
                        result: end_slot,
                    });
                    ops.push(LirOp::SubSlots {
                        a: end_slot,
                        b: new_list_ptr,
                        result: new_len,
                    });
                    if *inclusive {
                        ops.push(LirOp::IncrSlot { slot: new_len });
                    }
                }
                IterableKind::Unsupported => {
                    unreachable!(
                        "create_for_update_block_reactive called with \
                         IterableKind::Unsupported — classifier/collector \
                         disagree on whether the iterable carries signals"
                    );
                }
            }

            let min_len = self.alloc_temp_slot_named("min_len");
            let old_lt_new = self.alloc_temp_slot_named("old_lt_new");
            ops.push(LirOp::LtU {
                a: old_len,
                b: new_len,
                result: old_lt_new,
            });
            ops.push(LirOp::If {
                cond: old_lt_new,
                then_ops: vec![LirOp::CopySlot {
                    from: old_len,
                    to: min_len,
                }],
                else_ops: vec![LirOp::CopySlot {
                    from: new_len,
                    to: min_len,
                }],
                name: Some(format!("for{}_diff_min_len", for_ctx.id.0)),
            });

            // Allocate fresh typed children-array sized for new_len.
            ops.push(LirOp::ChildrenArrayNewDefault {
                anchor_boundary: for_anchor_id,
                len: new_len,
                result: new_arr_slot,
            });

            // Copy survivor entries [0, min_len) via array.copy.
            ops.push(LirOp::SetSlot {
                slot: index,
                value: 0,
            });
            ops.push(LirOp::ChildrenArrayCopy {
                anchor_boundary: for_anchor_id,
                dst: new_arr_slot,
                dst_idx: index,
                src: old_arr_slot,
                src_idx: index,
                count: min_len,
            });

            // Unmount tail [min_len, old_len): walk old_arr, fetch each
            // iter-body ref directly, bind it into scope, read field 1
            // (`root_handle`) as the iteration's detach target.
            ops.push(LirOp::CopySlot {
                from: min_len,
                to: index,
            });
            ops.push(LirOp::GeU {
                index,
                len: old_len,
                result: break_cond,
            });
            let unmount_root = self.alloc_temp_slot_named("unmount_root");
            let unmount_iter_body = self.alloc_temp_slot_typed_named(
                LirSlotValType::RefNullForBoundary(iter_body_id),
                format!("upd_unmount_for{}_iter_body", for_ctx.id.0),
            );
            let unmount_root_handle_slot =
                self.alloc_boundary_field_slot_named(iter_body_id, 1, "for_unmount_root_handle");
            let unmount_body = vec![
                LirOp::ChildrenArrayGet {
                    anchor_boundary: for_anchor_id,
                    arr: old_arr_slot,
                    idx: index,
                    result: unmount_iter_body,
                },
                LirOp::BindBoundaryLocal {
                    boundary_id: iter_body_id,
                    slot: unmount_iter_body,
                },
                LirOp::LoadHandle {
                    slot: unmount_root_handle_slot,
                    to: unmount_root,
                },
                LirOp::CallBlock {
                    block: unmount_block,
                    parent: unmount_root,
                },
                LirOp::IncrSlot { slot: index },
                LirOp::GeU {
                    index,
                    len: old_len,
                    result: break_cond,
                },
            ];
            ops.push(LirOp::Loop {
                break_cond,
                body_ops: unmount_body,
                name: Some(format!("for{}_diff_unmount_tail", for_ctx.id.0)),
            });

            // Publish the new children-array onto the for-anchor's
            // children-field so subsequent fan-outs see it. The
            // `children_field_slot` is a BoundaryField on the inner
            // for's anchor; for nested fors it resolves through the
            // outer iter-body bound by the wrap loop. For top-level
            // fors it resolves through `$self.tree`.
            let publish_children_slot = self.alloc_boundary_field_slot_named(
                for_anchor_id,
                2,
                format!("for{}_publish_children", for_ctx.id.0),
            );
            ops.push(LirOp::StoreHandle {
                slot: publish_children_slot,
                from: new_arr_slot,
            });

            // Mount tail [min_len, new_len): struct.new_default + call
            // mount block + array.set into new_arr.
            ops.push(LirOp::CopySlot {
                from: min_len,
                to: index,
            });
            ops.push(LirOp::GeU {
                index,
                len: new_len,
                result: break_cond,
            });
            let update_item_value = self.alloc_temp_slot_named("update_item_value");
            let mut update_have_item_value = false;
            let mut mount_body = Vec::new();
            match (iterable, range_item_buf) {
                (IterableKind::Range { .. }, Some(item_buf)) => {
                    mount_body.push(LirOp::AddSlots {
                        a: new_list_ptr,
                        b: index,
                        result: update_item_value,
                    });
                    update_have_item_value = true;
                    mount_body.push(LirOp::StoreHandle {
                        slot: item_buf,
                        from: update_item_value,
                    });
                    mount_body.push(LirOp::GetSlotAddress {
                        mem_slot: item_buf,
                        result: item_ptr,
                    });
                }
                _ if is_gc_list => {
                    mount_body.push(LirOp::ArrayGetItem {
                        arr: new_list_ref_slot.unwrap(),
                        idx: index,
                        list_ty,
                        result: item_ptr,
                    });
                }
                _ => {
                    mount_body.push(LirOp::ComputeItemPtr {
                        base: new_list_ptr,
                        index,
                        element_size,
                        result: item_ptr,
                    });
                }
            }
            // Allocate fresh iter-body boundary for this iteration (see
            // initial-mount path comment for rationale). The mount
            // block now takes the iter-body ref as its 4th param via
            // `boundary_params`, registering it in scope so nested
            // `if`s resolve their parent boundary.
            let upd_iter_body = self.alloc_temp_slot_typed_named(
                LirSlotValType::RefNullForBoundary(iter_body_id),
                "upd_new_iter_body",
            );
            mount_body.push(LirOp::AllocBoundary {
                boundary_id: iter_body_id,
                ref_slot: upd_iter_body,
            });
            mount_body.push(LirOp::BindBoundaryLocal {
                boundary_id: iter_body_id,
                slot: upd_iter_body,
            });
            // Populate iter-body field 0 (LoopVar) with `item_ptr` —
            // address-style for ranges (range_item_buf addr) /
            // pointer-into-list for lists. Mirrors the initial-mount
            // path so nested fors and outer-item access pattern stay
            // identical across init and diff-driven mounts.
            let upd_loop_var_slot =
                self.alloc_boundary_field_slot_named(iter_body_id, 0, "for_upd_loop_var");
            mount_body.push(LirOp::StoreHandle {
                slot: upd_loop_var_slot,
                from: item_ptr,
            });
            if update_have_item_value {
                // For ranges, also stash the per-iter VALUE into
                // field 2 (`loop_var_value`) so cols-grow-style
                // fan-out re-seeding survives `range_item_buf`
                // overwrites by later iterations.
                let upd_loop_var_value_slot =
                    self.alloc_boundary_field_slot_named(iter_body_id, 2, "for_upd_loop_var_value");
                mount_body.push(LirOp::StoreHandle {
                    slot: upd_loop_var_value_slot,
                    from: update_item_value,
                });
            }
            mount_body.push(LirOp::CallBlock2 {
                block: mount_block,
                param0: parent_slot,
                param1: item_ptr,
                result: None,
            });
            mount_body.push(LirOp::ChildrenArraySet {
                anchor_boundary: for_anchor_id,
                arr: new_arr_slot,
                idx: index,
                value: upd_iter_body,
            });
            mount_body.push(LirOp::IncrSlot { slot: index });
            mount_body.push(LirOp::GeU {
                index,
                len: new_len,
                result: break_cond,
            });
            ops.push(LirOp::Loop {
                break_cond,
                body_ops: mount_body,
                name: Some(format!("for{}_diff_mount_tail", for_ctx.id.0)),
            });

            ops
        };

        if nested_parent.is_none() {
            // Top-level for: load old_arr from the for-anchor's
            // children-field (resolves through `$self.tree`).
            let load_children_slot = self.alloc_boundary_field_slot_named(
                for_anchor_id,
                2,
                format!("for{}_load_children", for_ctx.id.0),
            );
            self.emit(LirOp::LoadHandle {
                slot: load_children_slot,
                to: old_arr_slot,
            });
            for op in diff_ops {
                self.emit(op);
            }
        } else {
            // Nested for: wrap the diff body in one outer-walk loop per
            // ancestor. Each wrap walks the ancestor's children-array,
            // fetching iter-body refs directly. After binding the
            // ancestor iter-body, this for's BoundaryField reads (incl.
            // the children-field on its own ForAnchor) chain through
            // the parent_link.

            #[derive(Clone)]
            struct WrapLevel {
                ancestor_id: ForId,
                ancestor_range_item_buf: Option<LirSlotId>,
            }

            let mut levels: Vec<WrapLevel> = Vec::new();
            let mut cur_parent = for_ctx.parent;
            while let Some(anc_id) = cur_parent {
                let anc = self
                    .for_contexts
                    .get(&anc_id)
                    .cloned()
                    .expect("ancestor ForContext registered before descendant");
                levels.push(WrapLevel {
                    ancestor_id: anc_id,
                    ancestor_range_item_buf: anc.range_item_buf,
                });
                cur_parent = anc.parent;
            }

            // Diff body: load old_arr from the inner for-anchor's
            // children-field (resolves through the just-bound outer
            // iter-body), then splice.
            let mut body: Vec<LirOp> = {
                let mut inner: Vec<LirOp> = Vec::new();
                let load_old_children_slot = self.alloc_boundary_field_slot_named(
                    for_anchor_id,
                    2,
                    format!("for{}_load_old_children", for_ctx.id.0),
                );
                inner.push(LirOp::LoadHandle {
                    slot: load_old_children_slot,
                    to: old_arr_slot,
                });
                inner.extend(diff_ops);
                inner
            };

            // Fold outward: levels[0] is the immediate parent (inner-
            // most wrap), levels[last] is the outermost. Each wrap
            // walks the ancestor's children-array, binding its
            // iter-body ref so the next-inner level can resolve its
            // boundary fields through it.
            for (i, level) in levels.iter().enumerate() {
                let anc_anchor_id = self.for_anchor_id_for(level.ancestor_id);
                let j = self.alloc_temp_slot_named("j");
                let outer_break = self.alloc_temp_slot_named("outer_break");
                let outer_len = self.alloc_temp_slot_named("outer_len");
                let anc_arr_slot = self.alloc_temp_slot_typed_named(
                    LirSlotValType::RefNullForChildrenArray(anc_anchor_id),
                    format!("upd_anc{}_children", level.ancestor_id.0),
                );

                let mut wrap = Vec::new();
                // Load this ancestor's children-array from its
                // for-anchor's children-field. For the outermost
                // ancestor this resolves via `$self.tree`; for inner
                // ancestors it resolves through the next-outer
                // already-bound iter-body.
                let anc_children_slot = self.alloc_boundary_field_slot_named(
                    anc_anchor_id,
                    2,
                    format!("upd_anc{}_load_children", level.ancestor_id.0),
                );
                wrap.push(LirOp::LoadHandle {
                    slot: anc_children_slot,
                    to: anc_arr_slot,
                });
                let _ = i;

                wrap.push(LirOp::ArrayLen {
                    arr: anc_arr_slot,
                    result: outer_len,
                });
                wrap.push(LirOp::SetSlot { slot: j, value: 0 });
                wrap.push(LirOp::GeU {
                    index: j,
                    len: outer_len,
                    result: outer_break,
                });

                let mut loop_body = Vec::new();
                let anc_iter_body_id = self.iter_body_id_for(level.ancestor_id);
                let anc_iter_body_slot = self.alloc_temp_slot_typed_named(
                    LirSlotValType::RefNullForBoundary(anc_iter_body_id),
                    format!("upd_anc{}_iter_body", level.ancestor_id.0),
                );
                loop_body.push(LirOp::ChildrenArrayGet {
                    anchor_boundary: anc_anchor_id,
                    arr: anc_arr_slot,
                    idx: j,
                    result: anc_iter_body_slot,
                });
                loop_body.push(LirOp::BindBoundaryLocal {
                    boundary_id: anc_iter_body_id,
                    slot: anc_iter_body_slot,
                });
                if let Some(buf) = level.ancestor_range_item_buf {
                    // Read the per-iter VALUE from the ancestor
                    // iter-body's `loop_var_value` field (index 2),
                    // not field 0 — field 0 holds the address-style
                    // `item_ptr` (range_item_buf addr) which would
                    // dereference to whatever the LAST-mounted
                    // iteration left in the shared buf, not this
                    // iteration's value.
                    let scratch = self.alloc_temp_slot_named("scratch");
                    let anc_loop_var_value_slot = self.alloc_boundary_field_slot_named(
                        anc_iter_body_id,
                        2,
                        "for_wrap_anc_loop_var_value",
                    );
                    loop_body.push(LirOp::LoadHandle {
                        slot: anc_loop_var_value_slot,
                        to: scratch,
                    });
                    loop_body.push(LirOp::StoreHandle {
                        slot: buf,
                        from: scratch,
                    });
                }
                loop_body.extend(body);
                loop_body.push(LirOp::IncrSlot { slot: j });
                loop_body.push(LirOp::GeU {
                    index: j,
                    len: outer_len,
                    result: outer_break,
                });
                wrap.push(LirOp::Loop {
                    break_cond: outer_break,
                    body_ops: loop_body,
                    name: Some(format!("for{}_wrap", level.ancestor_id.0)),
                });
                body = wrap;
            }

            for op in body {
                self.emit(op);
            }
        }

        self.finish_block_named("for-update")
    }

    /// Create mount and unmount blocks for a branch with node tracking.
    /// Returns (mount_block, unmount_block).
    ///
    /// The mount block:
    /// - Creates the branch's nodes
    /// - Stores the first node's handle in content_mem for later removal
    /// - Inserts nodes after the anchor
    ///
    /// The unmount block:
    /// - Loads the stored handle from content_mem
    /// - Removes the node from DOM
    fn create_branch_with_tracking(
        &mut self,
        nodes: &[LirNode],
        _parent_slot: LirSlotId, // Outer parent - not used in block functions
        anchor_mem: LirSlotId,
        content_mem: LirSlotId,
    ) -> (BlockId, BlockId) {
        // Capture outer_item_field_slots before starting the block
        // (These are for-loop items that need to be accessible in this block)
        let outer_items_snapshot: Vec<(LocalId, Ty, LirSlotId, LirBindingMode)> = self
            .outer_item_field_slots
            .iter()
            .map(|(id, (ty, slot, mode))| (*id, *ty, *slot, *mode))
            .collect();

        // === Mount block ===
        // Allocate a fresh slot for the parent param.
        self.start_block();
        let block_parent = self.alloc_temp_slot_named("parent");

        // Load for-loop items from memory at start of block
        let mut loaded_items: HashMap<LocalId, LirSlotId> = HashMap::new();
        for (local_id, _ty, mem_slot, _mode) in &outer_items_snapshot {
            let temp_slot = self.alloc_temp_slot_named("temp_slot");
            self.emit(LirOp::LoadHandle {
                slot: *mem_slot,
                to: temp_slot,
            });
            loaded_items.insert(*local_id, temp_slot);
        }

        if !nodes.is_empty() {
            // Load anchor for InsertAfter — used once to position the
            // branch's wrapper element as the if-anchor's sibling.
            let anchor_temp = self.alloc_temp_slot_named("anchor_temp");
            self.emit(LirOp::LoadHandle {
                slot: anchor_mem,
                to: anchor_temp,
            });

            // Allocate the branch's host-fragment wrapper element
            // (`yel-frag`). Body content gets appended into the
            // wrapper instead of directly under the if's parent, so a
            // single `Remove(wrapper)` in the branch-unmount block
            // cascades to detach every DOM node the branch owns —
            // regardless of body shape (Element-first, If-first,
            // For-first, or multiple top-level siblings).
            let wrapper_slot = self.alloc_temp_slot_named("branch_wrapper");
            self.emit(LirOp::CreateFragment {
                result: wrapper_slot,
            });
            self.emit(LirOp::InsertAfter {
                parent: block_parent,
                node: wrapper_slot,
                anchor: anchor_temp,
            });
            // Stash the wrapper into the IfBranch boundary's `wrapper`
            // field (BoundaryField slot, passed as `content_mem`).
            // The branch-unmount block reads this and calls Remove.
            self.emit(LirOp::StoreHandle {
                slot: content_mem,
                from: wrapper_slot,
            });

            // Lower body content with the wrapper as parent — every
            // Element / DynamicText / nested If / nested For becomes
            // a child (or descendant) of the wrapper.
            for node in nodes {
                self.lower_node(node, wrapper_slot);
            }
        }

        let mount_block = self.finish_block_named("if-branch-mount");

        if let Some(block) = self.blocks.iter_mut().find(|b| b.id == mount_block) {
            block.params = vec![block_parent];
            if !loaded_items.is_empty() {
                block.local_to_slot = loaded_items.clone();
                for (local_id, _, _, mode) in &outer_items_snapshot {
                    block.local_modes.insert(*local_id, *mode);
                }
            }
        }

        // === Unmount block ===
        self.start_block();
        let unmount_parent = self.alloc_temp_slot_named("parent");

        // Load for-loop items from memory (unmount might need them for cleanup)
        let mut unmount_loaded_items: HashMap<LocalId, LirSlotId> = HashMap::new();
        for (local_id, _ty, mem_slot, _mode) in &outer_items_snapshot {
            let temp_slot = self.alloc_temp_slot_named("temp_slot");
            self.emit(LirOp::LoadHandle {
                slot: *mem_slot,
                to: temp_slot,
            });
            unmount_loaded_items.insert(*local_id, temp_slot);
        }

        if !nodes.is_empty() {
            // Load the stored node handle
            let node_temp = self.alloc_temp_slot_named("node_temp");
            self.emit(LirOp::LoadHandle {
                slot: content_mem,
                to: node_temp,
            });

            // Remove it from DOM
            self.emit(LirOp::Remove { node: node_temp });
        }

        let unmount_block = self.finish_block_named("if-branch-unmount");

        if let Some(block) = self.blocks.iter_mut().find(|b| b.id == unmount_block) {
            block.params = vec![unmount_parent];
            if !unmount_loaded_items.is_empty() {
                for (local_id, _, _, mode) in &outer_items_snapshot {
                    block.local_modes.insert(*local_id, *mode);
                }
                block.local_to_slot = unmount_loaded_items;
            }
        }

        (mount_block, unmount_block)
    }

    /// Create the update block for an if statement with proper unmount handling.
    ///
    /// State machine:
    /// - active_flag: 0 = else/nothing mounted, 1 = then mounted
    ///
    /// Logic (called only after initial mount, so something is always mounted):
    /// - If cond true && old_state == 1: do nothing (already then)
    /// - If cond true && old_state == 0: unmount else (if exists), mount then
    /// - If cond false && old_state == 0: do nothing (already else)
    /// - If cond false && old_state == 1: unmount then, mount else
    // Args are mixed (cond expr, anchor/state slots, then/else block-id pairs)
    // and don't naturally cluster; a wrapper struct would just shadow each
    // parameter under a longer name.
    #[allow(clippy::too_many_arguments)]
    /// Lower an event handler to a block.
    fn lower_handler(&mut self, handler: &LirHandler) -> BlockId {
        self.start_block();

        // Lower handler body statements
        for stmt in &handler.body {
            self.lower_statement(stmt);
        }

        let block_id = self.finish_block_with_name(BlockDebugName::handle(&handler.event));
        // Record input-binding metadata so codegen emits the DOM-value
        // coercion + signal-write preamble at the top of the block.
        if let Some(target) = handler.input_binding_target {
            self.input_binding_handlers.insert(block_id, target);
        }
        block_id
    }

    /// Lower a statement.
    fn lower_statement(&mut self, stmt: &LirStatement) {
        match stmt {
            LirStatement::Expr(expr) => {
                // Expression statement: evaluate for side effects and drop
                // any stack values the expression produced. Using DropExpr
                // here avoids allocating a typed slot that would have to
                // match the expression's (possibly composite) flat shape
                // just to immediately discard the value.
                let expr_id = self.intern_expr(expr);
                self.emit(LirOp::DropExpr { expr: expr_id });
            }
            LirStatement::SignalWrite { signal, value } => {
                // For composite signal types (option, result, variant with
                // payload, record, tuple, enum) the canonical-ABI flat shape
                // is multi-slot. A single-SlotId `SignalWrite` op cannot hold
                // them, so emit via `SignalWriteExpr` which stores each flat
                // slot directly from the expression's stack values.
                let signal_ty = self.find_signal_type(*signal);
                // Only Option/Result/Variant-with-payload need the flat-slot
                // store path today. Records and tuples are pointer-passed
                // and already work through the single-slot SignalWrite path.
                // Enums (Adt without payload cases) also fit a single i32.
                let is_composite = match signal_ty.map(|ty| self.ctx.ty_kind(ty)) {
                    Some(InternedTyKind::Option(_)) => true,
                    Some(InternedTyKind::Result { .. }) => true,
                    Some(InternedTyKind::Adt(def_id)) => {
                        if let Some(var_def) = self.ctx.defs.as_variant(*def_id) {
                            let cases = var_def.cases.clone();
                            cases.iter().any(|&c| {
                                if let DefKind::VariantCase(case) = self.ctx.defs.kind(c) {
                                    case.payload.is_some()
                                } else {
                                    false
                                }
                            })
                        } else {
                            // Phase 2 GC migration: route record-typed
                            // SignalWrites through SignalWriteExpr so
                            // primitive-only records take the GC path
                            // (struct.new at the source, struct.set at
                            // the destination) — bypassing the i32
                            // slot allocation that would cause a type
                            // mismatch with the ref-typed component
                            // field. Non-POR records keep their legacy
                            // memory path via the same SignalWriteExpr
                            // route, which dispatches on signal_in_struct.
                            self.ctx.defs.as_record(*def_id).is_some()
                        }
                    }
                    _ => false,
                };
                if is_composite {
                    let value_expr = self.intern_expr(value);
                    self.emit(LirOp::SignalWriteExpr {
                        signal: *signal,
                        expr: value_expr,
                    });
                    self.emit(LirOp::TriggerEffects { signal: *signal });
                } else {
                    // Allocate typed slot based on signal type. String and
                    // list values are fat pointers (ptr, len) = 2 i32 slots;
                    // the EvalExpr store path writes to both `value_slot`
                    // and `value_slot + 1`, so we must RESERVE the second
                    // slot here or it collides with the next allocation's
                    // local — causing either silent data corruption (next
                    // slot also i32) or WASM validation failure (next slot
                    // typed f32/f64/i64).
                    // Phase 5b-v.3: scalar lists are now single-slot GC array refs
                    // (not fat pointers). Only string/non-scalar list still
                    // need the companion `len` slot.
                    let is_fat_ptr = matches!(
                        signal_ty.map(|ty| self.ctx.ty_kind(ty)),
                        Some(InternedTyKind::String)
                    ) || matches!(
                        signal_ty,
                        Some(ty) if matches!(self.ctx.ty_kind(ty), InternedTyKind::List(_))
                            && !self.is_scalar_list_ty(ty)
                    );
                    let val_ty = signal_ty
                        .map(|ty| self.ty_to_slot_val_type(ty))
                        .unwrap_or(LirSlotValType::I32);
                    let value_slot = self.alloc_temp_slot_typed(val_ty);
                    if is_fat_ptr {
                        // Companion slot for the len half of the fat pointer.
                        // Not referenced by name — EvalExpr computes
                        // `value_slot + 1` at emit time.
                        let _len_slot = self.alloc_temp_slot_typed(LirSlotValType::I32);
                    }
                    let value_expr = self.intern_expr(value);
                    self.emit(LirOp::EvalExpr {
                        expr: value_expr,
                        result: value_slot,
                    });
                    self.emit(LirOp::SignalWrite {
                        signal: *signal,
                        value: value_slot,
                    });
                    self.emit(LirOp::TriggerEffects { signal: *signal });
                }
            }
            LirStatement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_slot = self.alloc_temp_slot_named("cond_slot");
                let cond_expr = self.intern_expr(condition);
                self.emit(LirOp::EvalExpr {
                    expr: cond_expr,
                    result: cond_slot,
                });

                let mut then_ops = Vec::new();
                let mut else_ops = Vec::new();

                // Build then ops
                std::mem::swap(&mut self.current_ops, &mut then_ops);
                for s in then_branch {
                    self.lower_statement(s);
                }
                std::mem::swap(&mut self.current_ops, &mut then_ops);

                // Build else ops
                if let Some(else_stmts) = else_branch {
                    std::mem::swap(&mut self.current_ops, &mut else_ops);
                    for s in else_stmts {
                        self.lower_statement(s);
                    }
                    std::mem::swap(&mut self.current_ops, &mut else_ops);
                }

                let stmt_if_id = self.next_if_label();
                self.emit(LirOp::If {
                    cond: cond_slot,
                    then_ops,
                    else_ops,
                    name: Some(format!("if{}_stmt", stmt_if_id)),
                });
            }
            LirStatement::Let { local_id, value } => {
                // Check if value type is a fat pointer (list or string) - needs two slots
                let is_fat_ptr = matches!(
                    self.ctx.ty_kind(value.ty),
                    InternedTyKind::List(_) | InternedTyKind::String
                );

                // Allocate slot(s) for the local variable
                let local_slot = self.alloc_temp_slot_named("local_slot");
                if is_fat_ptr {
                    // Allocate second slot for len (consecutive to ptr slot)
                    self.alloc_temp_slot_named("local_slot_len");
                }

                let value_expr = self.intern_expr(value);
                self.emit(LirOp::EvalExpr {
                    expr: value_expr,
                    result: local_slot,
                });
                // Store the local_id -> slot mapping for expression reference.
                // `Let`-bound locals always materialize their value into a
                // memory-backed slot today, so register with `BindingMode::Ptr`.
                self.local_bindings
                    .insert(*local_id, (local_slot, value.ty, LirBindingMode::Ptr));
                // Track this local as belonging to the current block
                self.current_block_locals.push(*local_id);
            }
        }
    }

    /// Collect signal dependencies from an expression.
    fn collect_dependencies(&self, expr: &LirExpr) -> Vec<DefId> {
        let mut deps = Vec::new();
        self.collect_deps_recursive(expr, &mut deps);
        deps
    }

    fn collect_deps_recursive(&self, expr: &LirExpr, deps: &mut Vec<DefId>) {
        match &expr.kind {
            LirExprKind::SignalRead(def_id) => {
                if !deps.contains(def_id) {
                    deps.push(*def_id);
                }
            }
            LirExprKind::Binary { lhs, rhs, .. } => {
                self.collect_deps_recursive(lhs, deps);
                self.collect_deps_recursive(rhs, deps);
            }
            LirExprKind::Unary { operand, .. } => {
                self.collect_deps_recursive(operand, deps);
            }
            LirExprKind::Call { args, .. } => {
                for arg in args {
                    self.collect_deps_recursive(arg, deps);
                }
            }
            LirExprKind::Field { base, .. } => {
                self.collect_deps_recursive(base, deps);
            }
            LirExprKind::Index { base, index } => {
                self.collect_deps_recursive(base, deps);
                self.collect_deps_recursive(index, deps);
            }
            LirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.collect_deps_recursive(condition, deps);
                self.collect_deps_recursive(then_expr, deps);
                self.collect_deps_recursive(else_expr, deps);
            }
            LirExprKind::VariantCtor { payload, .. } => {
                if let Some(p) = payload {
                    self.collect_deps_recursive(p, deps);
                }
            }
            // List/Record constructs - collect deps from elements/fields
            LirExprKind::ListConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_deps_recursive(elem, deps);
                }
            }
            LirExprKind::RecordConstruct { fields, .. } => {
                for field in fields {
                    self.collect_deps_recursive(field, deps);
                }
            }
            LirExprKind::TupleConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_deps_recursive(elem, deps);
                }
            }
            LirExprKind::Range { start, end, .. } => {
                self.collect_deps_recursive(start, deps);
                self.collect_deps_recursive(end, deps);
            }
            LirExprKind::GlobalCall { args, .. } => {
                for arg in args {
                    self.collect_deps_recursive(arg, deps);
                }
            }
            // Closures capture state from the enclosing component — walk
            // their body statements so filter/map predicates contribute
            // their captured signals to the outer iterable's dep set.
            LirExprKind::Closure { body, .. } => {
                for stmt in body {
                    self.collect_deps_from_stmt(stmt, deps);
                }
            }
            // Leaf nodes - no dependencies
            LirExprKind::Local(_)
            | LirExprKind::Def(_)
            | LirExprKind::Literal(_)
            | LirExprKind::EnumCase { .. }
            | LirExprKind::ListStatic { .. } => {}
        }
    }

    /// Walk a `LirStatement` — used by closure-body dep collection so
    /// nested lets/ifs inside a predicate contribute their reads.
    fn collect_deps_from_stmt(&self, stmt: &LirStatement, deps: &mut Vec<DefId>) {
        match stmt {
            LirStatement::Expr(e) => self.collect_deps_recursive(e, deps),
            LirStatement::SignalWrite { value, .. } => {
                self.collect_deps_recursive(value, deps);
            }
            LirStatement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_deps_recursive(condition, deps);
                for s in then_branch {
                    self.collect_deps_from_stmt(s, deps);
                }
                if let Some(else_branch) = else_branch {
                    for s in else_branch {
                        self.collect_deps_from_stmt(s, deps);
                    }
                }
            }
            LirStatement::Let { value, .. } => {
                self.collect_deps_recursive(value, deps);
            }
        }
    }

    // === Helper methods ===

    fn start_block(&mut self) {
        // Save any in-progress ops to the stack
        if !self.current_ops.is_empty() {
            self.ops_stack.push(std::mem::take(&mut self.current_ops));
        }
        self.current_ops = Vec::new();

        // Save current block locals to the stack
        if !self.current_block_locals.is_empty() {
            self.block_locals_stack
                .push(std::mem::take(&mut self.current_block_locals));
        }
        self.current_block_locals = Vec::new();
    }

    fn finish_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;

        let ops = std::mem::take(&mut self.current_ops);

        // Build local_to_slot + local_modes from current_block_locals.
        // Every entry defaults to `BindingMode::Ptr` — Phase 5b-v.2 is a
        // pure-plumbing change.
        let mut local_to_slot = HashMap::new();
        let mut local_modes: HashMap<LocalId, LirBindingMode> = HashMap::new();
        for local_id in &self.current_block_locals {
            if let Some((slot, _ty, mode)) = self.local_bindings.get(local_id) {
                local_to_slot.insert(*local_id, *slot);
                local_modes.insert(*local_id, *mode);
            }
        }

        // Clean up locals from local_bindings that were defined in this block
        for local_id in &self.current_block_locals {
            self.local_bindings.remove(local_id);
        }

        // Don't apply captured_locals automatically - only for-item-mount blocks
        // get captured_locals set manually after creation
        self.blocks.push(LirBlock {
            id,
            ops,
            captured_locals: HashMap::new(),
            local_to_slot,
            local_modes,
            return_slot: None,
            params: Vec::new(),
            max_flat_scratch_counts: (0, 0, 0, 0),
            mount_component_count: 0,
            mount_component_children: Vec::new(),
            boundary_params: Vec::new(),
        });

        // Restore previous ops from stack
        if let Some(prev_ops) = self.ops_stack.pop() {
            self.current_ops = prev_ops;
        }

        // Restore previous block locals from stack
        self.current_block_locals = self.block_locals_stack.pop().unwrap_or_default();

        id
    }

    /// Finish the current block tagging it with a structured debug
    /// kind. Use `finish_block_with_name` for blocks that carry extra
    /// metadata (e.g. signal id on update blocks).
    fn finish_block_named(&mut self, kind: &'static str) -> BlockId {
        self.finish_block_with_name(BlockDebugName::kind(kind))
    }

    fn finish_block_with_name(&mut self, name: BlockDebugName) -> BlockId {
        let id = self.finish_block();
        self.ctx.set_block_name(self.component_id, id, name);
        id
    }

    fn emit(&mut self, op: LirOp) {
        self.current_ops.push(op);
    }

    #[allow(dead_code)]
    fn alloc_temp_slot(&mut self) -> LirSlotId {
        self.alloc_temp_slot_typed(LirSlotValType::I32)
    }

    /// Alloc a temp slot tagged with a debug name. The name flows
    /// into the WASM name section as `$<name>` so WAT output reads
    /// `local.get $iter_record_ptr` instead of `local.get 97`. Prefer
    /// this at every semantic allocation site.
    fn alloc_temp_slot_named(&mut self, name: impl Into<String>) -> LirSlotId {
        let id = self.alloc_temp_slot_typed(LirSlotValType::I32);
        if let Some(info) = self.slots.iter_mut().find(|s| s.id == id) {
            info.name = Some(format!("{}_{}", name.into(), id.0));
        }
        id
    }

    /// Alloc a temp slot with both a non-default WASM value type and a
    /// debug name. Used by GC-ref slots where both the `val_ty`
    /// (determining the WASM local type) and a readable name matter.
    #[allow(dead_code)]
    fn alloc_temp_slot_typed_named(
        &mut self,
        val_ty: LirSlotValType,
        name: impl Into<String>,
    ) -> LirSlotId {
        let id = self.alloc_temp_slot_typed(val_ty);
        if let Some(info) = self.slots.iter_mut().find(|s| s.id == id) {
            info.name = Some(format!("{}_{}", name.into(), id.0));
        }
        id
    }

    fn alloc_temp_slot_typed(&mut self, val_ty: LirSlotValType) -> LirSlotId {
        let id = LirSlotId(self.next_slot);
        self.next_slot += 1;
        let local_idx = self.next_local_idx;
        self.next_local_idx += 1;
        self.slots.push(LirSlotInfo {
            id,
            kind: LirSlotKind::Temp { local_idx },
            val_ty,
            name: None,
        });
        id
    }

    fn alloc_memory_slot(&mut self, size: u32) -> LirSlotId {
        let id = LirSlotId(self.next_slot);
        self.next_slot += 1;

        // Align to 4 bytes
        let align = 4;
        let offset = (self.next_memory_offset + align - 1) & !(align - 1);
        self.next_memory_offset = offset + size;

        self.slots.push(LirSlotInfo {
            id,
            kind: LirSlotKind::Memory { offset, size },
            val_ty: LirSlotValType::I32, // Memory slots are accessed as i32
            name: None,
        });
        id
    }

    /// Alloc a memory slot tagged with a debug name.
    fn alloc_memory_slot_named(&mut self, size: u32, name: impl Into<String>) -> LirSlotId {
        let id = self.alloc_memory_slot(size);
        if let Some(info) = self.slots.iter_mut().find(|s| s.id == id) {
            info.name = Some(name.into());
        }
        id
    }

    /// Allocate a `SlotKind::BoundaryField` slot referencing
    /// `(boundary_id, field_idx)` in the component's mount tree.
    /// Caller is responsible for ensuring the field exists at that
    /// boundary — typically by reading
    /// `self.tree_shape.node_field[node_id]`.
    /// Look up the synthesized `ForIterBody` boundary id for a given
    /// for-loop. Searches `tree_shape.boundaries` for the
    /// `ForIterBody { for_id }` entry; panics if absent (the
    /// synthesizer always emits one per for-loop).
    fn iter_body_id_for(&self, target: ForId) -> TreeBoundaryId {
        for b in &self.tree_shape.boundaries {
            if let TreeBoundaryKind::ForIterBody { for_id } = b.kind {
                if for_id == target {
                    return b.id;
                }
            }
        }
        panic!(
            "iter_body_id_for: no ForIterBody boundary in tree_shape for for_id {:?}",
            target
        );
    }

    /// Look up the synthesized `ForAnchor` boundary id for a given
    /// for-loop. Mirrors `iter_body_id_for` for the anchor side of the
    /// boundary pair.
    fn for_anchor_id_for(&self, target: ForId) -> TreeBoundaryId {
        for b in &self.tree_shape.boundaries {
            if let TreeBoundaryKind::ForAnchor { for_id, .. } = b.kind {
                if for_id == target {
                    return b.id;
                }
            }
        }
        panic!(
            "for_anchor_id_for: no ForAnchor boundary in tree_shape for for_id {:?}",
            target
        );
    }

    /// Resolve the SubBoundary `target_idx` for a NodeId — i.e. the
    /// boundary id allocated by the synthesizer for the if/for at `node_id`.
    /// Panics if the synthesizer and lowering disagree on the body shape.
    fn subboundary_target_for_node(&self, node_id: NodeId) -> TreeBoundaryId {
        let nfr = *self.tree_shape.node_field.get(&node_id).unwrap_or_else(|| {
            panic!(
                "subboundary_target_for_node: missing node_field entry for NodeId {:?}",
                node_id
            )
        });
        let parent = &self.tree_shape.boundaries[nfr.owning_boundary.index()];
        match parent.fields.get(nfr.field_idx as usize) {
            Some(TreeFieldDecl::SubBoundary { target_idx, .. }) => {
                TreeBoundaryId(*target_idx)
            }
            other => panic!(
                "subboundary_target_for_node: parent boundary field at idx {} for NodeId {:?} is not a SubBoundary: {:?}",
                nfr.field_idx, node_id, other
            ),
        }
    }

    fn alloc_boundary_field_slot_named(
        &mut self,
        boundary_id: TreeBoundaryId,
        field_idx: u32,
        name: impl Into<String>,
    ) -> LirSlotId {
        let id = LirSlotId(self.next_slot);
        self.next_slot += 1;
        self.slots.push(LirSlotInfo {
            id,
            kind: LirSlotKind::BoundaryField {
                boundary_id,
                field_idx,
            },
            val_ty: LirSlotValType::I32,
            name: Some(name.into()),
        });
        id
    }

    fn intern_string(&mut self, s: &str) -> StringId {
        if let Some(&id) = self.string_map.get(s) {
            return id;
        }
        let id = StringId(self.strings.len() as u32);
        self.strings.push(s.to_string());
        self.string_map.insert(s.to_string(), id);
        id
    }

    fn intern_expr(&mut self, expr: &LirExpr) -> ExprId {
        // For now, always add - could deduplicate later
        let id = ExprId(self.exprs.len() as u32);
        self.exprs.push(expr.clone());
        id
    }
}

// ============================================================================
// Block structural metadata pass
// ============================================================================
//
// Computes per-block metadata that codegen would otherwise recompute by
// re-walking each block's op tree on every function emit:
//   * `max_flat_scratch_counts` — per-valtype scratch local counts for
//     flat-slot signal stores (`InitSignal` / `SignalWriteExpr` of
//     composite types) and a single i32 base-pointer scratch for any
//     reachable composite `FieldAccess` load.
//   * `mount_component_count` — total `MountComponent` site count
//     reachable from the block (recursing into `If`/`Loop`).
//   * `mount_component_children` — distinct child component DefIds
//     mounted by the block, in first-occurrence order.
//
// This pass runs after all blocks are constructed so we can borrow the
// component's signals / exprs by reference while writing to each block.

use crate::lir::layout::{max_flat_counts, FlatValTypeCounts};

/// Walks every block in `component` and fills in the structural metadata
/// fields on each `LirBlock`.
pub(crate) fn populate_block_structural_metadata(
    ctx: &CompilerContext,
    component: &mut LirComponent,
) {
    let mut layout_ctx = LirLayoutContext::new(ctx);
    // Snapshot what we need to read from the component while iterating
    // mutably over its blocks. Cloning here is cheap relative to the
    // alternative of cloning the whole component.
    let signals: Vec<(Ty,)> = component.signals.iter().map(|s| (s.ty,)).collect();
    let exprs = component.exprs.clone();

    // Collect per-block metadata first to avoid borrow conflicts on
    // `component.blocks`.
    let mut metas: Vec<(FlatValTypeCounts, u32, Vec<DefId>)> =
        Vec::with_capacity(component.blocks.len());
    for block in &component.blocks {
        let scratch =
            compute_flat_scratch_counts(&block.ops, &signals, &exprs, ctx, &mut layout_ctx);
        let count = count_mount_sites(&block.ops);
        let mut children = Vec::new();
        collect_mount_children(&block.ops, &mut children);
        metas.push((scratch, count, children));
    }
    for (block, (scratch, count, children)) in component.blocks.iter_mut().zip(metas.into_iter()) {
        block.max_flat_scratch_counts = scratch;
        block.mount_component_count = count;
        block.mount_component_children = children;
    }
}

fn count_mount_sites(ops: &[LirOp]) -> u32 {
    let mut n = 0u32;
    for op in ops {
        match op {
            LirOp::MountComponent { .. } => n += 1,
            LirOp::If {
                then_ops, else_ops, ..
            } => {
                n += count_mount_sites(then_ops);
                n += count_mount_sites(else_ops);
            }
            LirOp::Loop { body_ops, .. } => {
                n += count_mount_sites(body_ops);
            }
            _ => {}
        }
    }
    n
}

fn collect_mount_children(ops: &[LirOp], out: &mut Vec<DefId>) {
    for op in ops {
        match op {
            LirOp::MountComponent { component_def, .. } => {
                if !out.contains(component_def) {
                    out.push(*component_def);
                }
            }
            LirOp::If {
                then_ops, else_ops, ..
            } => {
                collect_mount_children(then_ops, out);
                collect_mount_children(else_ops, out);
            }
            LirOp::Loop { body_ops, .. } => {
                collect_mount_children(body_ops, out);
            }
            _ => {}
        }
    }
}

fn compute_flat_scratch_counts(
    ops: &[LirOp],
    signals: &[(Ty,)],
    exprs: &[LirExpr],
    ctx: &CompilerContext,
    layout_ctx: &mut LirLayoutContext,
) -> FlatValTypeCounts {
    let (mut mi32, mut mi64, mut mf32, mut mf64) = (0u32, 0u32, 0u32, 0u32);
    let mut needs_load_scratch = false;
    let mut min_i32_for_extras: u32 = 0;
    let mut min_i64_for_extras: u32 = 0;
    let bump_extras =
        |e: &LirExpr, min_i32: &mut u32, min_i64: &mut u32, load_scratch: &mut bool| {
            if expr_contains_fat_ptr_load(e, ctx) {
                *load_scratch = true;
            }
            if expr_contains_tuple_construct(e) {
                *min_i32 = (*min_i32).max(1);
            }
            if expr_contains_min_max_call(e, ctx) {
                *min_i32 = (*min_i32).max(2);
            }
            let (need_i32, need_i64) = expr_contains_float_binop_scratch(e, ctx);
            if need_i32 {
                *min_i32 = (*min_i32).max(1);
            }
            if need_i64 {
                *min_i64 = (*min_i64).max(1);
            }
        };
    for op in ops {
        match op {
            LirOp::InitSignal { signal_idx, expr } => {
                if let Some((ty,)) = signals.get(*signal_idx as usize) {
                    let counts = layout_ctx.canonical_flat_valtype_counts(*ty);
                    let m = max_flat_counts((mi32, mi64, mf32, mf64), counts);
                    mi32 = m.0;
                    mi64 = m.1;
                    mf32 = m.2;
                    mf64 = m.3;
                }
                let e = &exprs[expr.0 as usize];
                if expr_contains_composite_field_load(e, ctx, layout_ctx) {
                    needs_load_scratch = true;
                }
                bump_extras(
                    e,
                    &mut min_i32_for_extras,
                    &mut min_i64_for_extras,
                    &mut needs_load_scratch,
                );
            }
            LirOp::SignalWriteExpr { expr, .. } => {
                let e = &exprs[expr.0 as usize];
                let counts = layout_ctx.canonical_flat_valtype_counts(e.ty);
                let m = max_flat_counts((mi32, mi64, mf32, mf64), counts);
                mi32 = m.0;
                mi64 = m.1;
                mf32 = m.2;
                mf64 = m.3;
                if expr_contains_composite_field_load(e, ctx, layout_ctx) {
                    needs_load_scratch = true;
                }
                bump_extras(
                    e,
                    &mut min_i32_for_extras,
                    &mut min_i64_for_extras,
                    &mut needs_load_scratch,
                );
            }
            LirOp::If {
                then_ops, else_ops, ..
            } => {
                let (a, b, c, d) =
                    compute_flat_scratch_counts(then_ops, signals, exprs, ctx, layout_ctx);
                mi32 = mi32.max(a);
                mi64 = mi64.max(b);
                mf32 = mf32.max(c);
                mf64 = mf64.max(d);
                let (a, b, c, d) =
                    compute_flat_scratch_counts(else_ops, signals, exprs, ctx, layout_ctx);
                mi32 = mi32.max(a);
                mi64 = mi64.max(b);
                mf32 = mf32.max(c);
                mf64 = mf64.max(d);
            }
            LirOp::Loop { body_ops, .. } => {
                let (a, b, c, d) =
                    compute_flat_scratch_counts(body_ops, signals, exprs, ctx, layout_ctx);
                mi32 = mi32.max(a);
                mi64 = mi64.max(b);
                mf32 = mf32.max(c);
                mf64 = mf64.max(d);
            }
            LirOp::CreateTextDynamic { expr, .. }
            | LirOp::SetTextContent { expr, .. }
            | LirOp::SetAttribute { expr, .. }
            | LirOp::EvalExpr { expr, .. }
            | LirOp::DropExpr { expr } => {
                let e = &exprs[expr.0 as usize];
                if expr_contains_composite_field_load(e, ctx, layout_ctx) {
                    needs_load_scratch = true;
                }
                bump_extras(
                    e,
                    &mut min_i32_for_extras,
                    &mut min_i64_for_extras,
                    &mut needs_load_scratch,
                );
            }
            _ => {}
        }
    }
    if needs_load_scratch && mi32 < 1 {
        mi32 = 1;
    }
    if mi32 < min_i32_for_extras {
        mi32 = min_i32_for_extras;
    }
    if mi64 < min_i64_for_extras {
        mi64 = min_i64_for_extras;
    }
    (mi32, mi64, mf32, mf64)
}

/// True if `expr` contains a sub-expression typed as a fat pointer
/// (string or list). The expression-emitter spills the second i32 of
/// such pairs to an i32 scratch local in patterns like `len()` (drop
/// ptr, keep len) and concat-arg prep. Any block emitting such an
/// expression must reserve at least one i32 flat-scratch local; using
/// the legacy hardcoded `local 2` is unsafe in blocks whose signature
/// places a typed boundary-ref param at that index.
fn expr_contains_fat_ptr_load(expr: &LirExpr, ctx: &CompilerContext) -> bool {
    if matches!(
        ctx.ty_kind(expr.ty),
        InternedTyKind::String | InternedTyKind::List(_)
    ) {
        return true;
    }

    match &expr.kind {
        LirExprKind::Field { base, .. } | LirExprKind::Unary { operand: base, .. } => {
            expr_contains_fat_ptr_load(base, ctx)
        }
        LirExprKind::Binary { lhs, rhs, .. } => {
            expr_contains_fat_ptr_load(lhs, ctx) || expr_contains_fat_ptr_load(rhs, ctx)
        }
        LirExprKind::Index { base, index } => {
            expr_contains_fat_ptr_load(base, ctx) || expr_contains_fat_ptr_load(index, ctx)
        }
        LirExprKind::Call { args, .. } | LirExprKind::GlobalCall { args, .. } => {
            args.iter().any(|a| expr_contains_fat_ptr_load(a, ctx))
        }
        LirExprKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            expr_contains_fat_ptr_load(condition, ctx)
                || expr_contains_fat_ptr_load(then_expr, ctx)
                || expr_contains_fat_ptr_load(else_expr, ctx)
        }
        LirExprKind::ListConstruct { elements, .. }
        | LirExprKind::TupleConstruct { elements, .. } => {
            elements.iter().any(|e| expr_contains_fat_ptr_load(e, ctx))
        }
        LirExprKind::RecordConstruct { fields, .. } => {
            fields.iter().any(|f| expr_contains_fat_ptr_load(f, ctx))
        }
        LirExprKind::Range { start, end, .. } => {
            expr_contains_fat_ptr_load(start, ctx) || expr_contains_fat_ptr_load(end, ctx)
        }
        LirExprKind::VariantCtor {
            payload: Some(p), ..
        } => expr_contains_fat_ptr_load(p, ctx),
        _ => false,
    }
}

/// True if `expr` contains a `TupleConstruct` sub-expression. The
/// tuple-construct codegen spills the alloc'd base pointer to an i32
/// scratch local while writing each element. Any block emitting a
/// tuple constructor must reserve at least one i32 flat-scratch local.
fn expr_contains_tuple_construct(expr: &LirExpr) -> bool {
    if matches!(expr.kind, LirExprKind::TupleConstruct { .. }) {
        return true;
    }
    match &expr.kind {
        LirExprKind::Field { base, .. } | LirExprKind::Unary { operand: base, .. } => {
            expr_contains_tuple_construct(base)
        }
        LirExprKind::Binary { lhs, rhs, .. } => {
            expr_contains_tuple_construct(lhs) || expr_contains_tuple_construct(rhs)
        }
        LirExprKind::Index { base, index } => {
            expr_contains_tuple_construct(base) || expr_contains_tuple_construct(index)
        }
        LirExprKind::Call { args, .. } | LirExprKind::GlobalCall { args, .. } => {
            args.iter().any(expr_contains_tuple_construct)
        }
        LirExprKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            expr_contains_tuple_construct(condition)
                || expr_contains_tuple_construct(then_expr)
                || expr_contains_tuple_construct(else_expr)
        }
        LirExprKind::ListConstruct { elements, .. }
        | LirExprKind::TupleConstruct { elements, .. } => {
            elements.iter().any(expr_contains_tuple_construct)
        }
        LirExprKind::RecordConstruct { fields, .. } => {
            fields.iter().any(expr_contains_tuple_construct)
        }
        LirExprKind::Range { start, end, .. } => {
            expr_contains_tuple_construct(start) || expr_contains_tuple_construct(end)
        }
        LirExprKind::VariantCtor {
            payload: Some(p), ..
        } => expr_contains_tuple_construct(p),
        _ => false,
    }
}

/// True if `expr` contains a `min`/`max` call. The codegen for these
/// stashes both args into a pair of i32 scratch locals; any block
/// containing such a call must reserve at least two i32 flat-scratch
/// locals.
fn expr_contains_min_max_call(expr: &LirExpr, ctx: &CompilerContext) -> bool {
    if let LirExprKind::Call { func, .. } = &expr.kind {
        let name = ctx.str(ctx.defs.name(*func));
        if name == "min" || name == "max" {
            return true;
        }
    }
    match &expr.kind {
        LirExprKind::Field { base, .. } | LirExprKind::Unary { operand: base, .. } => {
            expr_contains_min_max_call(base, ctx)
        }
        LirExprKind::Binary { lhs, rhs, .. } => {
            expr_contains_min_max_call(lhs, ctx) || expr_contains_min_max_call(rhs, ctx)
        }
        LirExprKind::Index { base, index } => {
            expr_contains_min_max_call(base, ctx) || expr_contains_min_max_call(index, ctx)
        }
        LirExprKind::Call { args, .. } | LirExprKind::GlobalCall { args, .. } => {
            args.iter().any(|a| expr_contains_min_max_call(a, ctx))
        }
        LirExprKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            expr_contains_min_max_call(condition, ctx)
                || expr_contains_min_max_call(then_expr, ctx)
                || expr_contains_min_max_call(else_expr, ctx)
        }
        LirExprKind::ListConstruct { elements, .. }
        | LirExprKind::TupleConstruct { elements, .. } => {
            elements.iter().any(|e| expr_contains_min_max_call(e, ctx))
        }
        LirExprKind::RecordConstruct { fields, .. } => {
            fields.iter().any(|f| expr_contains_min_max_call(f, ctx))
        }
        LirExprKind::Range { start, end, .. } => {
            expr_contains_min_max_call(start, ctx) || expr_contains_min_max_call(end, ctx)
        }
        LirExprKind::VariantCtor {
            payload: Some(p), ..
        } => expr_contains_min_max_call(p, ctx),
        _ => false,
    }
}

/// Check whether `expr` contains a F32 mod/and/or/xor or F64 mod
/// binary op whose codegen needs scratch locals. F32 arms need an i32
/// scratch; the F64 mod arm needs an i64 scratch. Returns
/// `(needs_i32, needs_i64)`.
fn expr_contains_float_binop_scratch(expr: &LirExpr, ctx: &CompilerContext) -> (bool, bool) {
    let mut needs_i32 = false;
    let mut needs_i64 = false;
    if let LirExprKind::Binary { lhs, op, .. } = &expr.kind {
        match (ctx.ty_kind(lhs.ty), op) {
            (
                InternedTyKind::F32,
                BinOp::Mod | BinOp::And | BinOp::BitAnd | BinOp::Or | BinOp::BitOr | BinOp::BitXor,
            ) => {
                needs_i32 = true;
            }
            (InternedTyKind::F64, BinOp::Mod) => {
                needs_i64 = true;
            }
            _ => {}
        }
    }
    let recurse = |e: &LirExpr, acc: &mut (bool, bool)| {
        let (a, b) = expr_contains_float_binop_scratch(e, ctx);
        acc.0 |= a;
        acc.1 |= b;
    };
    let mut acc = (needs_i32, needs_i64);
    match &expr.kind {
        LirExprKind::Field { base, .. } | LirExprKind::Unary { operand: base, .. } => {
            recurse(base, &mut acc);
        }
        LirExprKind::Binary { lhs, rhs, .. } => {
            recurse(lhs, &mut acc);
            recurse(rhs, &mut acc);
        }
        LirExprKind::Index { base, index } => {
            recurse(base, &mut acc);
            recurse(index, &mut acc);
        }
        LirExprKind::Call { args, .. } | LirExprKind::GlobalCall { args, .. } => {
            for a in args {
                recurse(a, &mut acc);
            }
        }
        LirExprKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            recurse(condition, &mut acc);
            recurse(then_expr, &mut acc);
            recurse(else_expr, &mut acc);
        }
        LirExprKind::ListConstruct { elements, .. }
        | LirExprKind::TupleConstruct { elements, .. } => {
            for e in elements {
                recurse(e, &mut acc);
            }
        }
        LirExprKind::RecordConstruct { fields, .. } => {
            for f in fields {
                recurse(f, &mut acc);
            }
        }
        LirExprKind::Range { start, end, .. } => {
            recurse(start, &mut acc);
            recurse(end, &mut acc);
        }
        LirExprKind::VariantCtor {
            payload: Some(p), ..
        } => {
            recurse(p, &mut acc);
        }
        _ => {}
    }
    acc
}

/// Recursively check whether `expr` contains a `FieldAccess` load of a
/// composite (option / result / variant-with-payload) field type, or an
/// `Index` whose element type is a non-fat-ptr multi-slot composite.
/// Mirrors the legacy codegen-side predicate; both routes need an i32
/// scratch local to stash the base pointer across per-slot loads.
fn expr_contains_composite_field_load(
    expr: &LirExpr,
    ctx: &CompilerContext,
    layout_ctx: &mut LirLayoutContext,
) -> bool {
    match &expr.kind {
        LirExprKind::Field { base, field_idx } => {
            if expr_contains_composite_field_load(base, ctx, layout_ctx) {
                return true;
            }
            if let InternedTyKind::Adt(record_def_id) = ctx.ty_kind(base.ty) {
                let record_def_id = *record_def_id;
                if let Some(record_layout) = layout_ctx.record_layout_by_id(record_def_id) {
                    if let Some((_, _, field_ty)) =
                        record_layout.field_offsets.get(field_idx.0 as usize)
                    {
                        let field_ty = *field_ty;
                        match ctx.ty_kind(field_ty) {
                            InternedTyKind::Option(_) | InternedTyKind::Result { .. } => {
                                return true;
                            }
                            InternedTyKind::Adt(def_id) => {
                                let def_id = *def_id;
                                if let Some(v) = ctx.defs.as_variant(def_id) {
                                    let cases = v.cases.clone();
                                    for c in cases {
                                        if let DefKind::VariantCase(case) = ctx.defs.kind(c) {
                                            if case.payload.is_some() {
                                                return true;
                                            }
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            false
        }
        LirExprKind::Binary { lhs, rhs, .. } => {
            expr_contains_composite_field_load(lhs, ctx, layout_ctx)
                || expr_contains_composite_field_load(rhs, ctx, layout_ctx)
        }
        LirExprKind::Unary { operand, .. } => {
            expr_contains_composite_field_load(operand, ctx, layout_ctx)
        }
        LirExprKind::Index { base, index } => {
            let elem_slot_count = layout_ctx.canonical_flat_valtypes(expr.ty).len();
            let is_fat_slot = matches!(
                ctx.ty_kind(expr.ty),
                InternedTyKind::String | InternedTyKind::List(_)
            );
            if elem_slot_count >= 2 && !is_fat_slot {
                return true;
            }
            expr_contains_composite_field_load(base, ctx, layout_ctx)
                || expr_contains_composite_field_load(index, ctx, layout_ctx)
        }
        LirExprKind::Call { args, .. } | LirExprKind::GlobalCall { args, .. } => args
            .iter()
            .any(|a| expr_contains_composite_field_load(a, ctx, layout_ctx)),
        LirExprKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            expr_contains_composite_field_load(condition, ctx, layout_ctx)
                || expr_contains_composite_field_load(then_expr, ctx, layout_ctx)
                || expr_contains_composite_field_load(else_expr, ctx, layout_ctx)
        }
        LirExprKind::VariantCtor {
            payload: Some(p), ..
        } => expr_contains_composite_field_load(p, ctx, layout_ctx),
        LirExprKind::ListConstruct { elements, .. }
        | LirExprKind::TupleConstruct { elements, .. } => elements
            .iter()
            .any(|e| expr_contains_composite_field_load(e, ctx, layout_ctx)),
        LirExprKind::RecordConstruct { fields, .. } => fields
            .iter()
            .any(|f| expr_contains_composite_field_load(f, ctx, layout_ctx)),
        LirExprKind::Range { start, end, .. } => {
            expr_contains_composite_field_load(start, ctx, layout_ctx)
                || expr_contains_composite_field_load(end, ctx, layout_ctx)
        }
        LirExprKind::Closure { .. } => false,
        LirExprKind::Local(_)
        | LirExprKind::Def(_)
        | LirExprKind::Literal(_)
        | LirExprKind::SignalRead(_)
        | LirExprKind::EnumCase { .. }
        | LirExprKind::VariantCtor { payload: None, .. }
        | LirExprKind::ListStatic { .. } => false,
    }
}
