//! Block-Based LIR Types
//!
//! This module defines a lower-level representation where:
//! - UI operations are explicit instructions (LirOp)
//! - Branches become separate blocks with mount/unmount operations
//! - Storage is pre-allocated (SlotId for temps and memory)
//! - Strings and expressions are interned (StringId, LirExprId)

use std::collections::BTreeSet;
use rustc_hash::FxHashMap as HashMap;

use serde::{Deserialize, Serialize};

use crate::ids::{BlockId, DefId, ForId, IfId, TreeBoundaryId};
use crate::types::Ty;
use crate::{LocalId, NodeId};

use super::struct_types::{LirArrayTypeIdx, LirStructTypeIdx};

/// Symbolic reference to a wasm GC type, deferring concrete
/// type-section index resolution to codegen.
///
/// LIR-emitting frontends (THIR→LIR lowering, the flow frontend, future
/// direct emitters) need to construct `StructNew` / `StructGet` /
/// `StructSet` ops *before* the wasm type-section layout has been
/// computed. The legacy variants carry a `ty_idx: u32` that codegen
/// fills in late; that works for codegen-synthesised emit (e.g. the
/// boundary-rewrite pass running after `gc_layouts` is populated) but
/// not for LIR built directly during lowering.
///
/// `LirTypeRef` names the *role* of the GC type symbolically. Codegen
/// resolves the variant to a concrete `u32` at emit time by consulting
/// its `gc_layouts` tables (component / boundary / for-anchor) or its
/// `LirResource::struct_types` registry.
///
/// Added in Phase 0.2 of the LIR-flattening refactor. Pure additive —
/// the legacy `u32`-ty_idx op variants coexist for back-compat.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LirTypeRef {
    /// The current component's own `$Comp_<i>` struct. Codegen
    /// resolves via `gc_layouts[comp_idx].component_struct_type_idx`.
    ComponentStruct,
    /// A child component's `$Comp_<j>` struct, identified by its
    /// `DefId`. Codegen looks up `j` via the per-resource component
    /// list and reads `gc_layouts[j].component_struct_type_idx`.
    OtherComponentStruct(DefId),
    /// A tree-boundary struct in the current component. Codegen
    /// resolves via `gc_layouts[comp_idx].tree_struct_type_idx[&id]`.
    TreeBoundary(TreeBoundaryId),
    /// A `ForAnchor`'s children-array type. Codegen resolves via
    /// `gc_layouts[comp_idx].tree_for_arr_type_idx[&anchor_id]`.
    ForChildrenArray(TreeBoundaryId),
    /// A `LirResource::struct_types` entry (the Stage 2 flat
    /// registry). Phase 0.2 reserves the variant; Phase 0.3 will
    /// wire codegen resolution. Until then this path traps.
    StructDecl(LirStructTypeIdx),
    /// A `LirResource::array_types` entry. Same status as
    /// [`LirTypeRef::StructDecl`].
    ArrayDecl(LirArrayTypeIdx),
    /// Module-shared `$handle` struct type — the registry-handle
    /// record `(struct (field $inst (mut anyref)) (field $next (mut
    /// i32)))`. Lives at module scope (not per-component): codegen
    /// resolves via `WasmPackageBuilder::shared_handle_type_idx`,
    /// which is set once by `emit_shared_handle_types` before any
    /// per-component GC types are emitted.
    ///
    /// Phase 0.3b: needed by the registry-allocation lifecycle ops
    /// the Phase 0.3e neutral LirOps will lower to.
    SharedHandleStruct,
    /// Module-shared `$handle-array` type — `(array (mut (ref null
    /// $handle)))`. Module-scope index stored on
    /// `WasmPackageBuilder::shared_handle_arr_type_idx`. Sibling of
    /// [`LirTypeRef::SharedHandleStruct`].
    SharedHandleArray,
    /// Task #99: a specific case subtype of a GcVariant supertype.
    /// First field is the parent type (option/result/variant); second
    /// is the case index (0 = None / Ok / first variant). Codegen
    /// resolves via `record_gc_types.gc_variant_case_idx[(ty, case_idx)]`.
    /// Used by the unified inline signal-init helper to emit
    /// `struct.new_default $<sup>_<case>` for default-init of
    /// GcVariant-backed signals.
    GcVariantCase(Ty, u32),
    /// Task #100: a tuple type's GC struct, keyed by the **tuple** `Ty`.
    /// Codegen resolves via `record_gc_types.tuple_struct_type_idx[ty]`.
    /// Used by the unified inline signal-write helper to target the
    /// tuple struct on `StructSetSym` / `StructNewSym` for tuple-typed
    /// signals.
    TupleStruct(Ty),
}

/// Symbolic reference to a wasm global. Lets LIR-emitting frontends
/// produce `LirOp::GlobalGet` / `LirOp::GlobalSet` without knowing
/// wasm global indices — codegen resolves at emit time via
/// `gc_layouts[comp_idx]`.
///
/// Phase 0.3c: needed by the registry-allocation lifecycle sequence
/// the Phase 0.3e neutral LirOps will lower to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LirGlobalRef {
    /// `(mut i32)` carrying the **current** host handle for the
    /// in-flight mount/constructor call. Per-component. Resolves via
    /// `gc_layouts[comp_idx_by_def_id(def)].current_handle_global`.
    CurrentHandle(DefId),
    /// `(mut (ref null $CompHandleArr))` — registry array global,
    /// per-component. Resolves via `gc_layouts[…].registry_global`.
    Registry(DefId),
    /// `(mut i32)` — current allocated length of the registry array,
    /// per-component. Resolves via `gc_layouts[…].registry_len_global`.
    RegistryLen(DefId),
    /// `(mut i32)` — head index of the free chain (next reusable
    /// handle), or `-1`, per-component. Resolves via
    /// `gc_layouts[…].registry_free_head_global`.
    RegistryFreeHead(DefId),
}

/// Slot ID for storage locations.
///
/// Slots can be either:
/// - `Block`: Per-block-owned WASM local (Temp / WasmParam). The
///   `block` field disambiguates ownership; `idx` indexes into that
///   block's `slots` vec.
/// - `Resource`: Component-wide addressing (Memory / BoundaryField).
///   Indexes into `LirResource.slots`.
///
/// Task #105 Phase B: this enum supersedes the legacy `pub struct
/// LirSlotId(pub u32)` flat-index form. `Block`-variant slots index the
/// owning block's `slots` vec; `Resource`-variant slots index
/// `LirResource.slots`.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum LirSlotId {
    /// Per-block local (Temp or WasmParam). Indexes into block.slots.
    Block { block: BlockId, idx: u16 },
    /// Component-wide addressing (Memory or BoundaryField). Indexes into LirResource.slots.
    Resource { idx: u32 },
}

impl std::fmt::Display for LirSlotId {
    /// Compact human-readable form for debug names: `r<idx>` for a
    /// Resource slot, `b<block>i<idx>` for a Block slot.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Block { block, idx } => write!(formatter, "b{}i{}", block.0, idx),
            Self::Resource { idx } => write!(formatter, "r{}", idx),
        }
    }
}

impl LirSlotId {
    /// SCAFFOLD (#105): construct a `Resource`-variant slot id from a
    /// flat index. Use only when the caller knows the index targets
    /// `LirResource.slots` (Memory / BoundaryField).
    pub fn resource(idx: u32) -> Self {
        Self::Resource { idx }
    }

    /// SCAFFOLD (#105): construct a `Block`-variant slot id. Use when
    /// the caller is allocating a Temp / WasmParam against a
    /// known-current block.
    pub fn block(block: BlockId, idx: u16) -> Self {
        Self::Block { block, idx }
    }

    /// Address the slot `offset` positions after this one, preserving the
    /// variant. Valid only for multi-slot values whose slots were allocated
    /// contiguously in the same space (the same block for `Block`-variant
    /// ids) — which the allocator guarantees for `EvalExprToSlots`
    /// destinations.
    pub fn offset_by(&self, offset: u32) -> Self {
        match self {
            Self::Block { block, idx } => Self::Block {
                block: *block,
                idx: idx
                    .checked_add(offset as u16)
                    .expect("offset_by: per-block slot index overflow"),
            },
            Self::Resource { idx } => Self::Resource { idx: idx + offset },
        }
    }
}

/// String ID for interned strings.
///
/// References a string stored in the component's string table.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct StringId(pub u32);

/// Expression ID for pre-lowered expressions.
///
/// References an expression stored in the component's expression table.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct LirExprId(pub u32);

// A block is a reusable sequence of DOM operations.
//
// Blocks can be:
// - Mount blocks: Create and attach DOM nodes
// - Unmount blocks: Remove DOM nodes
// - Update blocks: Called by effects to update reactive parts
// - Handler blocks: Event handler bodies

/// Compile-time metadata for a `for` loop. One entry per ForId on the
/// component. Used by fan-out update blocks: look up their enclosing
/// for's parent + range_item_buf when re-seeding loop state inside an
/// outer ancestor walk.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForContext {
    pub id: ForId,
    /// Parent for-loop, if this for is nested inside another.
    pub parent: Option<ForId>,
    /// Scratch memory slot for range iterables' current item value.
    /// `None` for non-range iterables. For ranges the loop body
    /// dereferences `item_ptr` (an address) to read the loop variable;
    /// this buf is that addressable storage, and fan-out update walks
    /// re-seed it from each iteration's iter-body `loop_var_value`
    /// field before re-running in-body expressions.
    pub range_item_buf: Option<LirSlotId>,
}

/// How a `Local` binding's slot value should be interpreted by the
/// expression emitter when reading the binding.
///
/// Today every for-iter item / captured-local entry holds a byte
/// address (a pointer into linear memory) and the codegen always
/// emits a typed load after the `local.get`. The GC list migration
/// (Phase 5b-v) introduces bindings whose slot already holds the
/// scalar value directly — for those we need to skip the load.
///
/// Phase 5b-v.2 adds the flag and threads it through; the
/// `Value` mode is not produced anywhere yet (5b-v.3 flips it on
/// for migrated-list iter bindings).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LirBindingMode {
    /// Slot holds a byte address; reading the binding requires a typed
    /// load from the address.
    Ptr,
    /// Slot holds the value directly; reading the binding emits a
    /// `LocalGet` only.
    Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirBlock {
    pub id: BlockId,
    pub ops: Vec<LirOp>,
    /// Mapping from LocalId to parameter slot index for captured locals.
    /// Used for for-loop items where the item is passed as a parameter to the body block.
    pub captured_locals: HashMap<LocalId, LirSlotId>,
    // NOTE: value is a `SlotId` identifying the block-param slot the captured
    // local lives in (typically the for-loop item_ptr slot). Codegen resolves
    // the slot's WASM local via `slot_local(...)` and the enclosing block's
    // `local_offset` to produce the absolute local index used for
    // `local.get`.
    /// Mapping from LocalId to SlotId for locals loaded from memory.
    /// Used for outer loop items in nested for-loops that are stored to memory
    /// and then loaded at the start of inner blocks.
    pub local_to_slot: HashMap<LocalId, LirSlotId>,
    /// Per-LocalId binding-mode override. When a `Local(id)` expression
    /// is emitted, the codegen looks up `id` here (after resolving the
    /// slot via `captured_locals` / `local_to_slot`) and gates the
    /// post-`local.get` typed load on the mode. Missing entries default
    /// to `BindingMode::Ptr` — today's behavior for every existing
    /// binding kind.
    ///
    /// Populated alongside `captured_locals` / `local_to_slot` in
    /// block_lower. Phase 5b-v.2 only emits `Ptr` defaults; 5b-v.3
    /// will set `Value` for migrated-list iter bindings.
    pub local_modes: HashMap<LocalId, LirBindingMode>,
    /// When `Some(slot)`, the block function returns i32 and emits a
    /// `local.get <slot>` as its final instruction. Used for for-item
    /// mount blocks: they need to return the root-node DOM handle so
    /// the caller can record it in the tracking array for later diff
    /// / unmount. `None` means the block returns `()` (the default).
    pub return_slot: Option<LirSlotId>,
    /// Slot ids holding the block's WASM function parameters, in
    /// argument order. The slot's `val_ty` determines the param type.
    /// At function entry each param is copied from the WASM param
    /// local (index `i`) into the slot's local so in-body code can
    /// reference the slot like any other slot.
    pub params: Vec<LirSlotId>,
    /// Per-valtype scratch local counts required by this block's
    /// flat-slot signal stores (covers `InitSignal` / `SignalWriteExpr`
    /// of composite signal types — option/result/variant-with-payload —
    /// plus an i32 base-pointer scratch for any composite FieldAccess
    /// load reachable from an op's expression tree). Computed once
    /// during block lowering; codegen reads instead of re-walking ops.
    /// Order: (i32, i64, f32, f64).
    pub max_flat_scratch_counts: (u32, u32, u32, u32),
    /// Number of `LirOp::MountComponent` sites reachable from this
    /// block (recursively through `If` / `Loop` bodies). Used by
    /// codegen to size parent/iter retention regions.
    pub mount_component_count: u32,
    /// Distinct child component DefIds mounted by this block
    /// (recursively). Order is deterministic (insertion order =
    /// first-occurrence order to mirror the legacy codegen helper).
    /// Codegen reserves one typed `(ref null $Comp_<child>)` local
    /// per entry for the mount-internal call/return.
    pub mount_component_children: Vec<DefId>,
    /// Distinct composite argument `Ty`s of `Call` exprs reachable from
    /// this block (recursively through `If` / `Loop` bodies). A callback
    /// invoked with a composite argument must push the argument's
    /// canonical-ABI flattening (what the host import declares), not the
    /// internal GC ref — codegen reserves one typed ref local per distinct
    /// GC struct/ref type here to hold the value while its bytes are
    /// materialized. Over-collection is fine; codegen filters to the types
    /// whose internal repr is a GC struct/ref (scalars / lists / strings are
    /// handled inline). Deduped; order is first-occurrence.
    pub callback_arg_composite_types: Vec<Ty>,

    /// Stage 4 of lir-resource-flatten: parallel slot ids for the
    /// trailing typed boundary-ref params. `boundary_param_slots[i]`
    /// is the slot that holds `boundary_params[i]`'s ref. The slot's
    /// `val_ty` is `RefNullForBoundary(boundary_params[i])`. Its WASM
    /// local is bound by `block_fn.rs`'s prologue from the wasm
    /// boundary-param at position `1 + lir_param_count + i`.
    ///
    /// The Stage 3 boundary-rewrite pass seeds its
    /// `current_boundary_locals` map from this so every
    /// `LoadHandle` / `StoreHandle` against a `BoundaryField` slot
    /// rewrites to an explicit `BoundaryStructGet` / `Set` op — no
    /// codegen-time chain walk required even when the boundary
    /// reference originated as a function parameter.
    ///
    /// Length matches `boundary_params`. Empty when `boundary_params`
    /// is empty.
    pub boundary_param_slots: Vec<LirSlotId>,

    /// Phase 0.3o: slot whose value ops should treat as the
    /// "implicit self ref" for `current_self_local` ambient state
    /// (SignalRead/Write, accessor `$self.tree` chains, handler-id
    /// encoding, etc.).
    ///
    /// For most blocks this points to `params[0]` (the wasm self-ref
    /// param). For blocks that allocate self in their body (e.g. the
    /// internal constructor's `StructNewDefaultSym` result slot),
    /// this points to a Temp slot. `None` for blocks with no concept
    /// of self (host export wrappers before registry lookup, flow
    /// free functions).
    pub implicit_self: Option<LirSlotId>,

    /// Task #105 Phase A: per-block slot storage. In Phase A this vec
    /// coexists with `LirResource.slots`; later phases migrate
    /// `LirSlotKind::Temp` / `LirSlotKind::WasmParam` allocations from
    /// the component-wide vec into here so block-local locals stop
    /// bleeding across blocks. `Memory` / `BoundaryField` slots stay
    /// on `LirResource` because they describe component-wide
    /// addressing schemes (linear-memory offsets, GC struct field
    /// paths), not wasm locals.
    pub slots: Vec<LirSlotInfo>,
}

impl LirBlock {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            ops: Vec::new(),
            captured_locals: HashMap::default(),
            local_to_slot: HashMap::default(),
            local_modes: HashMap::default(),
            return_slot: None,
            params: Vec::new(),
            max_flat_scratch_counts: (0, 0, 0, 0),
            mount_component_count: 0,
            mount_component_children: Vec::new(),
            callback_arg_composite_types: Vec::new(),
            boundary_param_slots: Vec::new(),
            implicit_self: None,
            slots: Vec::new(),
        }
    }

    /// Stage 5c: derive the ordered TreeBoundaryIds for this block's
    /// boundary params from `boundary_param_slots` instead of reading
    /// the `boundary_params` field directly. Each slot's `val_ty` is
    /// `RefNullForBoundary(b_id)` by Stage 4 invariant. Consumers
    /// migrated to this helper become independent of the
    /// `boundary_params` field, paving the way for Stage 5e to delete
    /// it.
    /// Allocate the typed boundary-ref mirror slots for `ids` on this
    /// block (Block-variant, per-block local_idx) and record them as
    /// `boundary_param_slots`. Producers call this once, right after the
    /// block is finished — there is no separate `boundary_params` id
    /// list; the ids live on the slots' `RefNullForBoundary` val_tys.
    pub fn set_boundary_params(&mut self, ids: Vec<TreeBoundaryId>) {
        let mut slot_ids: Vec<LirSlotId> = Vec::with_capacity(ids.len());
        for b_id in ids {
            let local_idx = self
                .slots
                .iter()
                .filter(|s| matches!(s.kind, LirSlotKind::Temp { .. }))
                .count() as u32;
            let slot_id = LirSlotId::Block {
                block: self.id,
                idx: self.slots.len() as u16,
            };
            self.slots.push(LirSlotInfo {
                id: slot_id,
                kind: LirSlotKind::Temp { local_idx },
                val_ty: LirSlotValType::RefNullForBoundary(b_id),
                name: Some(format!("bp_ref_{}", b_id.0)),
            });
            slot_ids.push(slot_id);
        }
        self.boundary_param_slots = slot_ids;
    }

    /// The ordered boundary ids of this block's boundary params, derived
    /// from the mirror slots' `RefNullForBoundary` val_tys. Panics on a
    /// Resource-variant slot — boundary-param mirrors are always
    /// allocated on the block itself.
    pub fn boundary_param_ids(&self) -> impl Iterator<Item = TreeBoundaryId> + '_ {
        self.boundary_param_slots.iter().map(move |slot_id| {
            let info = match slot_id {
                LirSlotId::Block { idx, .. } => &self.slots[*idx as usize],
                LirSlotId::Resource { .. } => panic!(
                    "boundary_param_ids: Resource-variant boundary-param slot {:?} — \
                     mirrors are allocated on the block itself",
                    slot_id
                ),
            };
            match info.val_ty {
                LirSlotValType::RefNullForBoundary(b_id) => b_id,
                other => panic!(
                    "boundary_param_ids: slot {:?} has non-boundary val_ty {:?}",
                    slot_id, other
                ),
            }
        })
    }

    pub fn boundary_param_ids_from_slots<'a>(
        &'a self,
        slots: &'a [LirSlotInfo],
    ) -> impl Iterator<Item = TreeBoundaryId> + 'a {
        self.boundary_param_slots.iter().map(move |slot_id| {
            // Task #105 B2: Block-variant ids index this block's own
            // slots; Resource-variant the component's.
            let info = match slot_id {
                LirSlotId::Block { idx, .. } => &self.slots[*idx as usize],
                LirSlotId::Resource { idx } => &slots[*idx as usize],
            };
            match info.val_ty {
                LirSlotValType::RefNullForBoundary(b_id) => b_id,
                ref other => panic!(
                    "boundary_param_slot {:?} has wrong val_ty {:?}; expected RefNullForBoundary(_)",
                    slot_id, other
                ),
            }
        })
    }
}

impl super::arena::LirFunctionLike for LirBlock {
    fn params(&self) -> &[LirSlotId] {
        &self.params
    }
    fn return_slot(&self) -> Option<LirSlotId> {
        self.return_slot
    }
    fn ops(&self) -> &[LirOp] {
        &self.ops
    }
    fn max_flat_scratch_counts(&self) -> (u32, u32, u32, u32) {
        self.max_flat_scratch_counts
    }
    fn captured_locals(&self) -> &HashMap<LocalId, LirSlotId> {
        &self.captured_locals
    }
    fn local_to_slot(&self) -> &HashMap<LocalId, LirSlotId> {
        &self.local_to_slot
    }
    fn local_modes(&self) -> &HashMap<LocalId, LirBindingMode> {
        &self.local_modes
    }
    fn boundary_param_slots(&self) -> &[LirSlotId] {
        &self.boundary_param_slots
    }
    fn mount_component_children(&self) -> &[crate::ids::DefId] {
        &self.mount_component_children
    }
    /// Phase 0.3o: derived from `implicit_self`. A block has a self
    /// ref iff its `implicit_self` slot is set. For wasm signature
    /// purposes, a block has a self ref param iff its `implicit_self`
    /// slot is a `WasmParam`-backed slot included in `params`.
    fn has_self_ref_param(&self) -> bool {
        self.implicit_self.is_some()
    }

    // No `calling_conv` override: building a full UI conv requires
    // the enclosing component's `DefId` plus its slot table, neither
    // of which a `LirBlock` owns. Callers (codegen's type-section
    // pass) reach for `lir::function::ui_block_calling_conv(block,
    // component.def_id, &component.slots)` instead, which has the
    // full context. The trait's default (`CallingConv::default()` —
    // empty) is what we'd return here regardless.
}

/// Low-level operations that map closely to WASM instructions.
///
/// Each operation either:
/// - Performs a DOM operation via host import
/// - Manipulates memory or locals
/// - Controls execution flow
/// Scalar value type for a linear-memory load/store ([`LirOp::LoadAddr`]
/// / [`LirOp::StoreAddr`]). Mirrors the four WASM numeric value types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MemoryValueType {
    I32,
    I64,
    F32,
    F64,
}

/// Store width for [`LirOp::StoreAddr`]: a full-width store or a
/// narrowing `i32.store8` / `i32.store16`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum StoreWidth {
    Full,
    Narrow8,
    Narrow16,
}

/// Integer comparison for [`LirOp::Compare`] — the wasm `i32` predicate
/// emitted over `(lhs, rhs)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CompareOp {
    /// Unsigned `>=` (`i32.ge_u`).
    GeU,
    /// Unsigned `<` (`i32.lt_u`).
    LtU,
    /// `!=` (`i32.ne`).
    Ne,
}

/// Integer arithmetic for [`LirOp::BinaryOp`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ArithOp {
    /// `i32.add`.
    Add,
    /// `i32.sub`.
    Sub,
    /// `i32.mul`.
    Mul,
}

/// Right-hand operand of [`LirOp::BinaryOp`]: either another slot's value
/// or an immediate constant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BinOperand {
    /// Value read from a slot (`local.get`).
    Slot(LirSlotId),
    /// Immediate constant (`i32.const`).
    Const(u32),
}

/// How an [`LirOp::ArrayGetItem`] element is delivered — the read
/// representation. Scalar elements land directly in a slot; string
/// elements are `(ref null $fat_value)` boxes unboxed to the canonical
/// `(ptr, len)` pair, either into two slots or written to a memory buffer.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ArrayItemRepr {
    /// Element read directly into `result` (`array.get` → `local.set`).
    Scalar { result: LirSlotId },
    /// String element unboxed to `(ptr, len)` in two i32 slots.
    Fat {
        ptr_result: LirSlotId,
        len_result: LirSlotId,
    },
    /// String element's `(ptr, len)` written to memory at `buf_addr`
    /// (`+0` = ptr, `+4` = len), preserving the legacy memory-backed
    /// `load_fat_ptr` item semantics for the for-iter body reader.
    FatToMem { buf_addr: LirSlotId },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LirOp {
    // === DOM Operations ===
    // Phase 2.3: the 11 legacy DOM `LirOp` variants (CreateElement,
    // CreateText, CreateComment, CreateTextDynamic, CreateFragment,
    // SetAttribute, SetTextContent, AppendChild, InsertAfter, Remove,
    // AddEventListener) were deleted after Phase 2.2b switched every
    // lowering site to `LirOp::CallFunction { func: ctx.dom_imports().<name>, … }`
    // prefixed by the Push* stack-prefix ops below. The byte-identical
    // call sequence remains; only the variant tags are gone.
    // === Stack-Push Primitives (Phase 2.2b) ===
    //
    // Direct-to-wasm-stack push ops, used as inline operand sources for a
    // *consuming* `LirOp::CallFunction` that immediately follows. Each
    // leaves one or more values on the wasm stack; the consumer's
    // `CallFunction { args: vec![], … }` then issues the `call`
    // instruction which pops them.
    //
    // **Invariant**: every Push* op MUST be followed (no intervening ops)
    // by a `LirOp::CallFunction` consuming the pushed value(s) in the
    // order they were pushed. Interleaving any other op breaks stack
    // discipline and is a lowering bug.
    //
    // The reason these aren't expressed through `args: Vec<LirSlotId>` is
    // that the legacy DOM emit arms pushed string-pool addresses,
    // expr-derived fat-pointer payloads, attribute-value variant
    // payloads, and handler-id encodings directly to the stack without
    // ever materializing them into wasm locals. Phase 2.2b switches the
    // DOM ops to `CallFunction` against `ctx.dom_imports.*` DefIds while
    // preserving that inline-push pattern byte-for-byte.
    /// Push a slot's wasm-local value (i32) onto the stack. Emits
    /// `local.get <slot_local + local_offset>`. Use to source the
    /// pre-existing `node` argument of the legacy DOM ops where the
    /// remaining operands are inline-pushed values; keeps everything in
    /// a single ordered stream that precedes a `CallFunction { args: vec![] }`.
    PushSlot { slot: LirSlotId },

    /// Push a string-pool entry's data pointer (i32) onto the stack.
    /// Emits `i32.const <data_ptr>`. The matching `PushStringLen` must
    /// follow for the (ptr, len) pair expected by DOM imports.
    PushStringPtr { string_id: StringId },

    /// Push a string-pool entry's byte length (i32) onto the stack.
    /// Emits `i32.const <len>`. Pairs with `PushStringPtr`.
    PushStringLen { string_id: StringId },

    /// Push `expr`'s canonical-ABI flat representation onto the stack
    /// (codegen `emit_expr`). Generic: the value's own type drives the
    /// flattening — a `VariantCtor` flattens to discriminant + joined
    /// payload slots, a scalar to its slot, etc. Used to pass a constructed
    /// value as a host-call argument without a bespoke per-shape push op
    /// (e.g. `set-attribute`'s `attribute-value`).
    PushExpr { expr: LirExprId },

    /// Push the encoded handler-id (i32) for the `handler` block onto
    /// the stack. Mirrors the legacy `AddEventListener` arm's
    /// `(handle << 16) | local_id` encoding, including the
    /// codegen-side mutation of `next_handler_local_id` and
    /// `global_handler_map`. Used by the AddEventListener
    /// `CallFunction` site.
    PushHandlerId { handler: BlockId },

    // === Memory Operations ===
    /// Store DOM handle from temp slot to memory slot.
    StoreHandle { slot: LirSlotId, from: LirSlotId },

    /// Load DOM handle from memory slot to temp slot.
    LoadHandle { slot: LirSlotId, to: LirSlotId },

    /// Store constant i32 to memory slot.
    StoreI32 { slot: LirSlotId, value: i32 },

    /// Store an i32 currently sitting in a Temp slot into a persistent
    /// slot (Memory or BoundaryField). Matches `StoreI32` but with a
    /// dynamic value source instead of a compile-time constant.
    StoreI32Slot { slot: LirSlotId, from: LirSlotId },

    /// Load i32 from memory slot to temp slot.
    LoadI32 { slot: LirSlotId, to: LirSlotId },

    /// Compare two i32 Temp slots with `op` and write the boolean result
    /// (1/0) to `result`. Generic over the predicate: `Ne` for if-update
    /// branch-change detection, `GeU`/`LtU` for for-loop bounds checks.
    Compare {
        op: CompareOp,
        lhs: LirSlotId,
        rhs: LirSlotId,
        result: LirSlotId,
    },

    /// Compare an i32 Temp slot to a compile-time constant; write 1 to
    /// `result` if equal, else 0. Used by if-update to dispatch on the
    /// active-branch tag.
    I32EqConst {
        lhs: LirSlotId,
        rhs: i32,
        result: LirSlotId,
    },

    /// Allocate a sub-boundary's GC struct, store it on its parent
    /// boundary's `SubBoundary` field, and bind the resulting ref
    /// into a Temp slot for the rest of the current emit scope.
    /// Codegen also registers `boundary_id → ref_slot.local_idx` in
    /// `current_boundary_locals` so subsequent `BoundaryField` slot
    /// accesses on this boundary resolve via the local. Used at mount
    /// time to populate the typed mount-tree's inner boundaries
    /// (IfAnchor / IfBranch / ForAnchor).
    AllocSubBoundary {
        /// The boundary being allocated. Its `parent_link` (recorded
        /// at synthesis time) supplies parent boundary id + field
        /// index of the SubBoundary slot to write.
        boundary_id: TreeBoundaryId,
        /// Temp slot that will hold the typed ref to the allocated
        /// boundary struct. Caller must allocate this as a Temp slot
        /// with the right ref type before emitting.
        ref_slot: LirSlotId,
    },

    /// Allocate a boundary's GC struct, store it in `ref_slot`, and
    /// register it in `current_boundary_locals`. Unlike
    /// `AllocSubBoundary`, this does NOT write the new ref onto a
    /// parent's SubBoundary field — used for boundaries whose
    /// `parent_link` is `None` (specifically `ForIterBody`, which is
    /// reachable only via the for-anchor's children-array). The
    /// caller is responsible for placing the ref into the children
    /// array.
    AllocBoundary {
        boundary_id: TreeBoundaryId,
        ref_slot: LirSlotId,
    },

    /// Compile-time scope-tracking op: register `slot`'s WASM local as
    /// the in-scope ref for `boundary_id` so subsequent
    /// `BoundaryField` slot accesses on this boundary resolve via
    /// `local.get <slot.local>`. Emits NO WASM instructions.
    ///
    /// Used by fan-out update walks: after `ArrayGet` has
    /// fetched an ancestor iter-body's typed ref into a Temp slot,
    /// `BindBoundaryLocal` advertises it so reads/writes against that
    /// boundary's fields routed through `BoundaryField` slots work.
    /// The bound slot's `val_ty` must be `RefNullForBoundary(boundary_id)`.
    BindBoundaryLocal {
        boundary_id: TreeBoundaryId,
        slot: LirSlotId,
    },

    // === Control Flow ===
    /// Evaluate expression and store result in slot.
    EvalExpr { expr: LirExprId, result: LirSlotId },

    /// Phase 1.1c-f: evaluate `expr` and store its flat-canonical-ABI
    /// result into `dest_first_slot..dest_first_slot + N` consecutive
    /// slots, where N is the type's flat arity (1 for primitive
    /// scalars / refs, 2 for fat-pointer string / list, etc.).
    ///
    /// Codegen delegates to the existing `emit_expr` helper that pushes
    /// the value onto the wasm stack, then issues `local.set` per stack
    /// value in reverse order to assign to the consecutive dest slots.
    ///
    /// Used to inline SignalWriteExpr / InitSignal at LIR-lowering
    /// source without expanding the LIR with type-driven codegen logic.
    EvalExprToSlots {
        expr: LirExprId,
        dest_first_slot: LirSlotId,
    },

    /// Evaluate expression purely for side effects, discarding any values it
    /// pushes on the stack. Used for expression statements in effect blocks
    /// (e.g. `on-click();` where the callback returns a value that isn't
    /// stored). Unlike `EvalExpr`, no slot is reserved and the exact number
    /// of stack values produced is drained at codegen time using the
    /// expression's flat core valtypes.
    DropExpr { expr: LirExprId },

    /// Conditional: if cond is non-zero, execute then_ops, else execute else_ops.
    /// Boxed: the two op vectors plus the label made this the largest `LirOp`
    /// variant, and every op in every block pays the max variant size.
    If(Box<LirIf>),

    /// Call a block with explicit args, in the callee's `params`
    /// order. Each arg slot is pushed onto the wasm stack via
    /// `local.get`. No implicit self push — callers that want to
    /// forward self must include it explicitly in `args`. `result`
    /// captures the callee's return value when set.
    CallBlock {
        block: BlockId,
        args: Vec<LirSlotId>,
        result: Option<LirSlotId>,
    },

    /// Call a top-level WASM function by `DefId`. Differs from
    /// `CallBlock*` in three ways:
    ///
    /// 1. Targets a `DefId`, not a `BlockId` — the callee is a regular
    ///    function in the module, not an internal block (no implicit
    ///    self-ref or DOM parent).
    /// 2. Arity is open: each arg is a slot, no DOM-parent param 0
    ///    convention. The callee's wasm function type is determined
    ///    by its registered `DefId`.
    /// 3. Optional single-slot return.
    ///
    /// Differs from `LirExprKind::Call` (which also takes a `DefId`) by
    /// being an *op* rather than an expression — usable on the exec
    /// spine, addressable by structured control flow, and not subject
    /// to expression-arena interning.
    ///
    /// Used by the flow frontend for `internal:call` nodes; the UI
    /// compiler does not emit this op today (its inter-block dispatch
    /// is `CallBlockN`).
    CallFunction {
        func: DefId,
        args: Vec<LirSlotId>,
        result: Option<LirSlotId>,
    },

    /// Return from current block/function with no value. The wasm
    /// `return` instruction; validates only when the function's
    /// declared return type is `()` (or when something else has
    /// already pushed the return value, the UI block convention).
    Return,

    /// Return from current function with `value` (a slot whose
    /// wasm-local value matches the function's declared return type).
    /// Lowers to `local.get <value>; return`. Used by top-level
    /// functions whose lowering emits early returns from inside
    /// structured control flow — see `crates/yel-wasm-codegen/src/wasm/functions.rs`
    /// for the calling convention.
    ///
    /// Distinct from `Return` (no-value) because UI blocks use the
    /// `block_fn.rs` trailing-`local.get` convention to satisfy their
    /// typed return, and would silently mis-emit if the bare `Return`
    /// arm also pushed a slot — flow / future free-function callers
    /// opt into the explicit-value form here instead.
    ReturnValue { value: LirSlotId },

    // === Signal Operations ===
    // Phase 1.1c (#62): `LirOp::SignalRead` deleted — never produced by
    // lowering (signal reads are folded into `LirExprKind::SignalRead`
    // inside expressions; see `op_emit.rs:893` historic stub arm).

    // §1.4: `SignalWrite` / `SignalWriteExpr` / `InitSignal` /
    // `InitSignalDefault` deleted — every signal write and init is
    // lowered inline by `inline_signal_write_or_init_from_expr` to
    // generic ops (`EvalExprToSlots` + `StructSetSym` /
    // `GlobalFieldSet`). `TriggerEffects` survives only as a
    // lowering-internal placeholder (below).
    /// Lowering-internal placeholder: "trigger every observer of this
    /// global signal". Emitted by global-signal write sites (the full
    /// observer set is unknowable mid-lowering — other components may
    /// not be lowered yet); the module-level `resolve_global_triggers`
    /// pass expands each one into direct `CallBlock`s to the observing
    /// components' synthesized fanout blocks. MUST NOT reach codegen —
    /// the codegen arm is a hard `InvalidIR` error. Component-local
    /// signals never use this: they dispatch via direct `CallBlock`s
    /// at emit time (`emit_trigger_for_signal`).
    TriggerEffects { signal: DefId },

    // === Constructor Operations ===
    /// Initialize memory slot to zero.
    /// Used to clear persistent storage during construction.
    InitMemorySlot { slot: LirSlotId },

    /// Phase 0.3m: convert a registry handle (i32 in `handle`) into a
    /// typed `(ref null $Comp_<comp>)` stored in `result`. Codegen
    /// delegates to the existing `emit_registry_lookup` helper.
    /// `comp` selects which component's registry to read; `result`'s
    /// val_ty must be `RefNullForComponent(comp)`.
    RegistryLookupToSelfRef {
        component: DefId,
        handle: LirSlotId,
        result: LirSlotId,
    },

    /// Phase 0.3m: allocate a registry entry for a typed component
    /// ref in `ref_slot`. Codegen delegates to `emit_registry_alloc`
    /// using the two scratch slots. `result_handle` receives the
    /// allocated registry index (i32).
    RegistryAlloc {
        component: DefId,
        ref_slot: LirSlotId,
        idx_scratch: LirSlotId,
        arr_scratch: LirSlotId,
        result_handle: LirSlotId,
    },

    /// Phase 0.3m: call the `[resource-new]X` import for the named
    /// exported component. Pushes `handle` and calls the resource-new
    /// wasm import; the returned host-handle is stored in `result`.
    CallResourceNew {
        component: DefId,
        handle: LirSlotId,
        result: LirSlotId,
    },

    // === Loop Operations ===
    /// Loop with break condition.
    /// Generates: block { loop { br_if 1 if cond; body_ops; br 0 } }
    Loop {
        /// Slot containing break condition (loop exits if truthy).
        break_cond: LirSlotId,
        /// Operations in the loop body.
        body_ops: Vec<LirOp>,
        /// Optional debug label surfaced in the WASM name section for both
        /// the outer `block` and inner `loop` structural ops this op emits.
        /// `None` means no label entries are added (debug-only hint).
        name: Option<String>,
    },

    /// Integer arithmetic: `result = lhs <op> rhs`, where `lhs` is a slot
    /// value and `rhs` is a slot value or an immediate. Covers slot+slot,
    /// slot-slot, slot*const, and in-place increment (`Add` with
    /// `rhs: Const(1)` and `result == lhs`).
    BinaryOp {
        op: ArithOp,
        lhs: LirSlotId,
        rhs: BinOperand,
        result: LirSlotId,
    },

    /// Allocate memory: alloc(size, align) -> ptr
    Alloc {
        size: LirSlotId,
        align: u32,
        result: LirSlotId,
    },

    /// Free memory: free(ptr, size)
    Free { ptr: LirSlotId, size: LirSlotId },

    /// Load a scalar of type `ty` from the linear-memory address in
    /// `addr` into `result`. Natural alignment per type (i32/f32 → 4,
    /// i64/f64 → 8).
    LoadAddr {
        addr: LirSlotId,
        result: LirSlotId,
        ty: MemoryValueType,
    },

    /// Store the scalar in `value` to the linear-memory address in `addr`.
    /// `width` selects a full store or a narrowing `i32.store8`/`store16`
    /// (narrow widths apply to `i32` values — narrow-typed signals
    /// `bool`/`u8`/`s8`/`char` and `u16`/`s16`, and `option<T>`
    /// discriminant bytes — so the store doesn't clobber adjacent bytes).
    StoreAddr {
        addr: LirSlotId,
        value: LirSlotId,
        ty: MemoryValueType,
        width: StoreWidth,
    },

    // === WASM GC ops (phase 2+ of the GC migration) ===
    //
    // Allocate a fresh GC struct: pops `fields.len()` values from the
    // stack in field order and executes `struct.new <ty_idx>`; the
    // reference lands in `result`. Each input value comes from its
    // own slot; codegen emits `local.get` for each in field order
    // before the `struct.new`.
    StructNew {
        ty_idx: u32,
        fields: Vec<LirSlotId>,
        result: LirSlotId,
    },

    // Stage 5e-4 (lir-resource-flatten): the dead `StructGet { ty_idx }` /
    // `StructSet { ty_idx }` ops (defined here, matched in dedupe + codegen,
    // but never constructed by any lowering path) were deleted. The names
    // `StructGet` / `StructSet` are now the generic struct-field ops further
    // down — they resolve the wasm struct-type index from `rec`'s `val_ty`
    // rather than carrying an explicit `ty_idx`.
    /// Phase 0.2: symbolic-ty companion of [`LirOp::StructNew`].
    /// `ty_ref` resolves to a wasm type-section index at codegen
    /// time via the component's `GcTypeLayout`. Field semantics are
    /// otherwise identical: pops `fields.len()` values from the
    /// stack in field order, executes `struct.new <resolved>`, and
    /// stores the resulting ref into `result`.
    ///
    /// Used by lowering paths that need to emit struct constructions
    /// before the wasm type section has been laid out (signal-struct
    /// migration, `lower_mount_component`).
    StructNewSym {
        ty_ref: LirTypeRef,
        fields: Vec<LirSlotId>,
        result: LirSlotId,
    },

    /// Phase 0.2: symbolic-ty companion of [`LirOp::StructGet`].
    /// `ty_ref` resolves at codegen time; field semantics are
    /// otherwise identical.
    StructGetSym {
        ty_ref: LirTypeRef,
        field: u32,
        rec: LirSlotId,
        result: LirSlotId,
    },

    /// Phase 0.2: symbolic-ty companion of [`LirOp::StructSet`].
    /// `ty_ref` resolves at codegen time; field semantics are
    /// otherwise identical.
    StructSetSym {
        ty_ref: LirTypeRef,
        field: u32,
        rec: LirSlotId,
        value: LirSlotId,
    },

    /// Allocate a GC struct with all fields default-initialised and
    /// store the result into `result`. Emits `struct.new_default
    /// <ty_idx>; local.set <result>`. `ty_ref` resolves at codegen
    /// time. Phase 0.3e — used by lifted internal-lifecycle blocks
    /// for `$Comp` and tree-root struct allocation.
    StructNewDefaultSym {
        ty_ref: LirTypeRef,
        result: LirSlotId,
    },

    /// Allocate a default-initialised GC struct of type `field_ty` and
    /// write it into field `field` of the struct ref held in `rec`,
    /// whose type is `struct_ty`. Emits the 3-instruction sequence
    /// `local.get rec; struct.new_default <field_ty_idx>; struct.set
    /// <struct_ty_idx> <field>`. Phase 0.3e — used by the internal
    /// constructor's tree-root field initialisation, where decomposing
    /// into separate `StructNewDefaultSym` + `StructSetSym` would
    /// introduce an extra `local.set`/`local.get` round-trip and
    /// break byte-identical wasm output.
    StructSetNewDefault {
        struct_ty: LirTypeRef,
        field: u32,
        rec: LirSlotId,
        field_ty: LirTypeRef,
    },

    /// Zero out a single i32-sized cell in component-relative linear
    /// memory. Emits the 3-instruction sequence `i32.const (base +
    /// addr); i32.const 0; i32.store (align 2)`. `addr` is the
    /// component-relative offset; codegen adds the per-component
    /// memory base, mirroring `LirOp::MemConst`. Phase 0.3e — used
    /// by the internal constructor's memory-slot zero-init loop.
    ZeroI32Mem { addr: u32 },

    /// Materialize a literal i32 constant into a slot. Emits
    /// `i32.const <value>; local.set <result>`. Differs from
    /// `MemConst` in that it does NOT add a per-component memory base
    /// — pure scalar constant. Phase 0.3h — used by the synthesized
    /// unmount block to push raw memory-slot offsets.
    I32Const { value: i32, result: LirSlotId },

    /// Read a wasm global into a slot. Emits `global.get <idx>;
    /// local.set <result>`. The `gref` resolves to a concrete wasm
    /// global index at codegen time. Phase 0.3c — pure additive.
    GlobalGet {
        gref: LirGlobalRef,
        result: LirSlotId,
    },

    /// Write a slot value into a wasm global. Emits `local.get <value>;
    /// global.set <idx>`. The `gref` resolves to a concrete wasm global
    /// index at codegen time.
    GlobalSet {
        gref: LirGlobalRef,
        value: LirSlotId,
    },

    /// Write slot `value` into storage slot `field` of named `global`
    /// block `block`. The single representation of a global-property
    /// write — it carries no self-ref and no struct concept (unlike the
    /// former `GlobalGet{GlobalBlockSelf} + StructSetSym{GlobalsStruct}`
    /// pair). Codegen resolves `(block, field)` to its backing storage
    /// (today the block's GC struct field; later a core wasm global) and
    /// emits the store. Global *reads* stay expression-position
    /// `SignalRead`s, resolved in codegen.
    GlobalFieldSet {
        block: DefId,
        field: u32,
        value: LirSlotId,
    },

    /// Generic `struct.get` on a typed GC struct: read field `field_idx`
    /// of the struct referenced by `rec` into `result`. Codegen emits
    /// `local.get rec; ref.as_non_null; struct.get <ty> <field>`, where
    /// `<ty>` is resolved **from `rec`'s `val_ty`** (a
    /// `RefNullForBoundary`/`RefNullForComponent`) — the op itself carries
    /// no frontend/tree concept. Produced by `boundary_rewrite` from the
    /// pre-rewrite [`StructFieldGet`].
    StructGet {
        rec: LirSlotId,
        field_idx: u32,
        result: LirSlotId,
    },

    /// Companion to [`StructGet`]: `struct.set` field `field_idx` of the
    /// struct in `rec` from slot `value`.
    StructSet {
        rec: LirSlotId,
        field_idx: u32,
        value: LirSlotId,
    },

    /// Companion to [`StructGet`]: `struct.set` a literal i32 (e.g. an
    /// active-tag flag) into field `field_idx` of the struct in `rec`.
    StructSetConst {
        rec: LirSlotId,
        field_idx: u32,
        value: i32,
    },

    /// Stage 5e-1: load a boundary's GC struct ref by walking the
    /// `$self.tree → ...` chain at function entry. Used by handler
    /// / dispatch / update blocks that access deep `$self`-rooted
    /// boundaries without receiving them as params and without
    /// emitting an `Alloc*Boundary` (the boundary already exists).
    /// Replaces the codegen-side fallback in `emit_boundary_ref`.
    /// Codegen translates to `local.get $self; struct.get <comp>
    /// $tree; struct.get <root> $sub; ... struct.get <parent>
    /// <field_idx>` — the same chain `emit_boundary_ref` walks
    /// today, just sequenced once at LIR layer.
    BoundaryRefFromSelf {
        boundary_id: TreeBoundaryId,
        result: LirSlotId,
    },

    /// Pre-rewrite (lir-resource-flatten Stage 5e-4): a symbolic read of
    /// field `field_idx` of the struct identified by `struct_ty`, into
    /// `result`. The lowerer emits this directly — `struct_ty` (a
    /// `TreeBoundaryId`, doubling as the `struct_types` registry index) tells
    /// the `boundary_rewrite` pass which struct instance to resolve a `rec`
    /// for via the tree. It lowers this to a generic [`StructGet`]; no
    /// `StructFieldGet` may survive to codegen.
    StructFieldGet {
        struct_ty: TreeBoundaryId,
        field_idx: u32,
        result: LirSlotId,
    },

    /// Pre-rewrite companion to [`StructFieldGet`]: a symbolic write of
    /// `value` into field `field_idx` of the `struct_ty` struct. Resolved to
    /// [`StructSet`] by `boundary_rewrite`.
    StructFieldSet {
        struct_ty: TreeBoundaryId,
        field_idx: u32,
        value: LirSlotId,
    },

    /// Pre-rewrite companion: a symbolic literal-i32 store into field
    /// `field_idx` of the `struct_ty` struct. Resolved to [`StructSetConst`]
    /// by `boundary_rewrite`.
    StructFieldSetConst {
        struct_ty: TreeBoundaryId,
        field_idx: u32,
        value: i32,
    },

    // The array-mutation ops (`ArrayNewDefault` / `ArrayGet` / `ArraySet`
    // / `ArrayCopy`) carry a `LirArrayTypeIdx` registry index — no UI
    // `TreeBoundaryId`. The lowering computes the index from the for-anchor
    // via `for_anchor_array_idx` (its ordinal among ForAnchor boundaries),
    // and codegen resolves the wasm type-section index as
    // `gc_layouts[comp].array_type_base + LirArrayTypeIdx`. Array *reads*
    // of typed-list signals still go through `ArrayGetItem` /
    // `ArrayGetItemFat` / `ArrayGetItemFatToMem`.
    /// Push the length of a GC array onto `result`.
    ArrayLen { arr: LirSlotId, result: LirSlotId },

    /// Convert a ref to non-null (trapping on null). Emits
    /// `ref.as_non_null`. Used when reading array elements declared
    /// nullable but known-non-null by construction.
    RefAsNonNull { slot: LirSlotId },

    /// Push a null ref of a concrete heap type. `ref.null <ty_idx>`.
    RefNull { ty_idx: u32, result: LirSlotId },

    /// `array.new_default` of a typed GC array, identified by its
    /// registry index into the resource's `array_types`. Codegen resolves
    /// the wasm type-section index from that registry index. (Today the
    /// only array types are for-anchor children arrays, whose element is
    /// the per-for `ForIterBody` struct.)
    ArrayNewDefault {
        array_type: LirArrayTypeIdx,
        len: LirSlotId,
        result: LirSlotId,
    },

    /// `array.get` of the registry array type `array_type`.
    ArrayGet {
        array_type: LirArrayTypeIdx,
        arr: LirSlotId,
        idx: LirSlotId,
        result: LirSlotId,
    },

    /// `array.set` of the registry array type `array_type`.
    ArraySet {
        array_type: LirArrayTypeIdx,
        arr: LirSlotId,
        idx: LirSlotId,
        value: LirSlotId,
    },

    /// `array.copy` of the registry array type `array_type`.
    ArrayCopy {
        array_type: LirArrayTypeIdx,
        dst: LirSlotId,
        dst_idx: LirSlotId,
        src: LirSlotId,
        src_idx: LirSlotId,
        count: LirSlotId,
    },

    /// Store constant i32 to slot (temp).
    SetSlot { slot: LirSlotId, value: i32 },

    /// Copy one slot's value into another. Pure register-to-register
    /// move with no arithmetic. Used by the for-loop diff to pick
    /// `min_len` and to seed a loop counter from an existing slot.
    CopySlot { from: LirSlotId, to: LirSlotId },

    /// Get the memory address of a memory slot into a temp slot.
    /// Used for range iteration where we need to pass the item buffer address.
    GetSlotAddress {
        mem_slot: LirSlotId,
        result: LirSlotId,
    },

    /// GC-array variant of `LoadList`: read a list-typed signal whose
    /// backing storage is a `(ref null $<elem>_list)` GC array on the
    /// component's struct (Phase 5b-iii target). Pushes the array ref
    /// into `ref_result` (a `RefNull(<list_array_type_idx>)` slot) and
    /// `array.len` of it into `len_result` (an i32 slot).
    ///
    /// Codegen sketch: `local.get $self; struct.get $comp_<name>
    /// $<sig>; local.tee <ref_result>; array.len; local.set <len_result>`.
    ///
    /// Added in Phase 5b-ii. No emitter produces it yet — the existing
    /// memory-backed `LoadList` is still used for all signals.
    LoadListGc {
        signal: DefId,
        ref_result: LirSlotId,
        len_result: LirSlotId,
    },

    /// GC-array variant of `EvalListExpr`: evaluate a list expression
    /// whose result is a typed GC array (e.g. `array.new_fixed
    /// $<elem>_list ...`) and store the resulting array ref into
    /// `ref_result` plus its `array.len` into `len_result`.
    ///
    /// Used for for-loops over literal lists once the codegen for
    /// `ListConstruct` is flipped to GC arrays (Phase 5b-iv). The
    /// expression's emit must leave a single array-ref value on the
    /// stack rather than the legacy `(ptr, len)` pair.
    ///
    /// Added in Phase 5b-ii. No emitter produces it yet.
    EvalListExprGc {
        expr: LirExprId,
        ref_result: LirSlotId,
        len_result: LirSlotId,
    },

    /// GC-array variant of `ComputeItemPtr`: read element `idx` of a
    /// typed GC array `arr` (declared as `(array (mut <elem>))` whose
    /// list type is `list_ty`) directly into `result`. The result is
    /// the element value (or ref) — not a memory address.
    ///
    /// Codegen resolves `list_ty` to its `array.get` type index via
    /// `record_gc_types.list_array_type_idx[list_ty]` and emits
    /// `local.get <arr>; local.get <idx>; array.get $<list_ty>;
    /// local.set <result>`.
    ///
    /// Added in Phase 5b-ii alongside `LoadListGc` / `EvalListExprGc`
    /// to replace `ComputeItemPtr` for migrated lists in Phase 5b-iv /
    /// 5b-v. No emitter produces it yet.
    /// Read element `idx` from the GC array backing list-typed signal
    /// `list_ty` (`record_gc_types.list_array_type_idx[list_ty]`). The
    /// `repr` selects how the element is delivered — directly into a slot,
    /// unboxed to a fat-pointer pair, or stored to memory. See
    /// [`ArrayItemRepr`].
    ArrayGetItem {
        arr: LirSlotId,
        idx: LirSlotId,
        list_ty: Ty,
        repr: ArrayItemRepr,
    },

    /// Phase 1.1c-l: `ref.cast (ref null <ty>)` on a value source. Emits
    /// `local.get from; ref.cast <heap_ty>; local.set result`. `ty_ref`
    /// resolves at codegen time. Needed by the synthesized global-signal
    /// fanout block to narrow an `anyref` from the shared-handle struct
    /// to a concrete `(ref null $Comp_<observer>)`.
    RefCast {
        from: LirSlotId,
        ty_ref: LirTypeRef,
        result: LirSlotId,
    },

    /// Phase 1.1c-l: `ref.is_null`. Emits `local.get from; ref.is_null;
    /// local.set result`. Used by the fanout block for null-guarding the
    /// registry array, individual handle entries, and the anyref inst.
    RefIsNull { from: LirSlotId, result: LirSlotId },

    /// Phase 1.1c-l: typed-ref-keyed `array.get`. Differs from
    /// `ArrayGetItem` (keyed by `Ty`) in that it takes a `LirTypeRef`,
    /// so it works for array types named symbolically (e.g.
    /// `SharedHandleArray`). Codegen resolves `ty_ref` to a wasm
    /// type-section index and emits
    /// `local.get arr; local.get idx; array.get <ty_idx>; local.set result`.
    ArrayGetTyped {
        ty_ref: LirTypeRef,
        arr: LirSlotId,
        idx: LirSlotId,
        result: LirSlotId,
    },
}

/// Payload of [`LirOp::If`], boxed to keep `LirOp` small (it is stored in a
/// `Vec<LirOp>` per block, so the enum's footprint is paid per op).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirIf {
    /// Condition slot; the branch is taken when its value is non-zero.
    pub cond: LirSlotId,
    /// Ops executed when the condition is non-zero.
    pub then_ops: Vec<LirOp>,
    /// Ops executed when the condition is zero.
    pub else_ops: Vec<LirOp>,
    /// Optional debug label surfaced in the WASM name section as a
    /// `label` subsection entry on the emitted `if` instruction.
    /// `None` means no label entry is emitted for this if (the name
    /// section is a debug hint, so omitting is valid).
    pub name: Option<String>,
}

// `LirOp` is stored in a `Vec<LirOp>` per block, so its size is paid per op.
// Guard against a variant accidentally re-bloating the enum (see the boxed
// `If`/`LirIf` split). Bump deliberately if a real new field needs the space.
const _: () = assert!(std::mem::size_of::<LirOp>() <= 56);

/// Collected dynamic-binding entry — one per dynamic site (attr/text
/// /structural) emitted during body lowering.
///
/// `binding_collector` drives both `build_boundary_dep_index` and the
/// per-(boundary, signal) update-fn emitter; this struct carries the
/// metadata each consumer needs to identify and route the binding.
#[derive(Debug, Clone)]
pub struct PendingBinding {
    /// The boundary that owns the target field (where the bound DOM
    /// node's handle lives in the typed tree). For attr/text bindings
    /// this is `tree_shape.node_field[node_id].owning_boundary`. For
    /// derived-signal / if-cond / for-list bindings it's the boundary
    /// that lexically owns the if/for node — derived from the
    /// `for_iter_body_stack` snapshot or the enclosing if/for node's
    /// parent boundary.
    pub owning_boundary: TreeBoundaryId,
    pub dependencies: Vec<DefId>,
    pub kind: PendingBindingKind,
    /// Stable opaque id for this binding. Used as the key into the
    /// per-kind `*_binding_data` maps and as the dedupe-set member in
    /// `emit_per_boundary_signal_updates`. For structural bindings
    /// it's the corresponding `LirBlockEffect.id`; for inline bindings
    /// (AttrSet, DynamicText) it's a counter-minted id from
    /// `next_binding_id` that starts at 1_000_000 to stay disjoint
    /// from real effect ids.
    pub binding_id: u32,
}

#[derive(Debug, Clone)]
pub enum PendingBindingKind {
    AttrSet,
    DynamicText,
    DerivedSignal,
    IfCondReroute,
    ForListReroute,
}

/// Per-component dependency index over tree boundaries.
///
/// Built once after all `PendingBinding`s have been collected by
/// `BlockLowering::build_boundary_dep_index`. The per-(boundary,
/// signal) update emitter consumes this
/// to emit one `update_b<boundary>_s<signal>` fn per
/// `(boundary, signal)` pair where `boundary ∈ signal_to_path[signal]`.
///
/// Keys/values use `BTreeSet` so iteration order is deterministic
/// across runs (DefId / TreeBoundaryId have stable Ord).
#[derive(Debug, Default, Clone)]
pub struct BoundaryDepIndex {
    /// Direct binding deps on this boundary (union of dependencies of
    /// every `PendingBinding` whose `owning_boundary == this`).
    pub boundary_deps: HashMap<TreeBoundaryId, BTreeSet<DefId>>,
    /// `boundary_deps[b] ∪ subtree_deps[children of b]`, computed
    /// bottom-up via `parent_link`.
    pub subtree_deps: HashMap<TreeBoundaryId, BTreeSet<DefId>>,
    /// For each signal `s`, the set of boundaries `b` where
    /// `s ∈ subtree_deps[b]`.
    pub signal_to_path: HashMap<DefId, BTreeSet<TreeBoundaryId>>,
}

/// Effect definition - connects signal dependencies to update blocks.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirBlockEffect {
    pub id: u32,
    /// Signals this effect depends on.
    pub dependencies: Vec<DefId>,
    /// Block to call when any dependency changes.
    pub update_block: BlockId,
}

/// WASM value type for temp slots.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum LirSlotValType {
    #[default]
    I32,
    I64,
    F32,
    F64,
    /// Nullable reference to a concrete GC type (struct or array),
    /// keyed by the emitted type index. Used for per-iteration record
    /// refs and tracking-array refs. The codegen side materialises
    /// this into a `(ref null <ty_idx>)` local.
    RefNull(u32),
    /// Symbolic: nullable ref to the GC struct of a tree-boundary
    /// (root / IfAnchor / IfBranch / ForAnchor / ForIterBody).
    /// Codegen resolves the concrete WASM type index via
    /// `gc_layouts[comp].tree_struct_type_idx[boundary]`.
    RefNullForBoundary(TreeBoundaryId),
    /// Symbolic: nullable ref to a `ForAnchor` boundary's children
    /// array type — `(array (mut (ref null <iter_body_struct>)))`.
    /// Codegen resolves via `gc_layouts[comp].tree_for_arr_type_idx[anchor_boundary_id]`.
    RefNullForChildrenArray(TreeBoundaryId),
    /// Symbolic: nullable ref to a component's `$Comp_<j>` GC struct,
    /// keyed by the component's `DefId`. Codegen resolves via
    /// `comp_idx_by_def_id(def_id)` followed by
    /// `gc_layouts[j].component_struct_type_idx`. Used by typed
    /// scratch locals in lifted internal-lifecycle blocks (Phase 0.3e)
    /// that hold a child component instance ref during mount sequencing.
    RefNullForComponent(DefId),
    /// Phase 5b-v.3: nullable ref to a list's typed GC array type
    /// `(array (mut <scalar>))`. The contained `Ty` is the **list**
    /// type (e.g. `list<s32>`), keyed in
    /// `RecordGcTypes::list_array_type_idx`. Codegen resolves via that
    /// map at local-declaration time.
    RefNullForListGc(Ty),
    /// Phase 5e.1: nullable ref to a DTR record's GC struct type
    /// `(struct ...)`. The contained `Ty` is the record type, keyed
    /// in `RecordGcTypes::record_type_idx` (via the record's DefId).
    /// Codegen resolves via that map at local-declaration time. Used
    /// for iter-binding locals when iterating a list<record>.
    RefNullForRecord(Ty),
    /// Phase 5e.5: nullable ref to a `option<T>` / `result<T,E>` /
    /// user-`variant` parent supertype. The contained `Ty` is the
    /// parent type itself, keyed in
    /// `RecordGcTypes::gc_variant_super_idx`. Codegen resolves the heap
    /// type index via that map at local-declaration time. Replaces
    /// the multi-i32 flat-slot allocation when the parent is migrated
    /// to the W3C subtype-hierarchy GC representation.
    RefNullForGcVariant(Ty),
    /// Task #100: nullable ref to a tuple's GC struct type
    /// `(struct <field-per-elem>)`. The contained `Ty` is the **tuple**
    /// type itself, keyed in `RecordGcTypes::tuple_struct_type_idx`.
    /// Codegen resolves the heap-type index via that map at
    /// local-declaration time. Mirrors `RefNullForRecord` for tuples.
    RefNullForTuple(Ty),
    /// strings-to-GC (`plans/strings-to-gc.md`): nullable ref to the
    /// module-shared `$str_bytes = (array (mut i8))` byte-array type that
    /// backs a GC-native `String`. Codegen resolves the heap-type index via
    /// `RecordGcTypes::str_bytes_array_idx`.
    RefNullForStringBytes,
    /// Phase 0.3m: nullable ref to the module-shared `$handle-array`
    /// type. Used by the synthesized export-constructor block to type
    /// the array-scratch slot fed to `RegistryAlloc`. Codegen resolves
    /// via `WasmPackageBuilder::shared_handle_arr_type_idx`.
    RefNullForSharedHandleArray,
    /// Task #98: nullable ref to the module-shared `$handle` struct
    /// type (the element type of `$handle-array`). Used by the
    /// synthesized global-fanout block to type the scratch slot that
    /// holds each iteration's array-entry handle before its `$inst`
    /// field is loaded and cast to a typed component ref. Codegen
    /// resolves via `WasmPackageBuilder::shared_handle_type_idx`.
    RefNullForSharedHandle,
    /// Task #98: nullable `anyref` — the wasm GC top reference type.
    /// Used by the synthesized global-fanout block to hold the
    /// intermediate value from `struct.get $handle.$inst` before
    /// `ref.cast` narrows it to a typed component ref.
    AnyRef,
}

/// Information about a slot's allocation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirSlotInfo {
    pub id: LirSlotId,
    pub kind: LirSlotKind,
    /// WASM value type for temp slots. Defaults to I32.
    pub val_ty: LirSlotValType,
    /// Optional debug name, surfaced in the WASM name section so WAT
    /// dumps show e.g. `$for0_anchor_ref` instead of `local 97`. Every
    /// call site with a semantic purpose should set this — makes
    /// debugging layout / lifetime bugs dramatically easier.
    pub name: Option<String>,
}

// ============================================================================
// Concrete-typed mount-tree shape
// ============================================================================

/// Kind of a tree boundary node. One boundary per emitted GC struct type
/// in the concrete-typed mount-tree.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TreeBoundaryKind {
    /// The component root struct (one per component).
    Root,
    /// Anchor struct for an `if` op: holds the anchor handle, the active
    /// branch tag, and one ref-field per branch (then / else_if_n / else).
    IfAnchor {
        if_id: IfId,
        /// Boundary indices of the branch boundaries this anchor owns,
        /// in declaration order (then, else_if_0, else_if_1, ..., else).
        branches: Vec<u32>,
    },
    /// One branch body of an `if`. Holds the branch's element handles
    /// and any nested boundaries.
    IfBranch {
        if_id: IfId,
        /// 0 = then, 1+ = else_if_n, last = else (when present).
        branch_idx: u32,
    },
    /// Anchor struct for a `for` op: holds the parent + anchor + the
    /// children-array ref. The array's element type is the iter-body
    /// boundary.
    ForAnchor {
        for_id: ForId,
        /// Boundary index of the per-iteration body struct.
        iter_body_idx: u32,
    },
    /// Per-iteration body struct for a `for`. Holds the loop variable
    /// (first field) and any in-body element handles + nested boundaries.
    ForIterBody { for_id: ForId },
}

/// Declaration of one field on a tree-boundary struct. Order in
/// `TreeBoundary::fields` is the field order in the emitted GC struct.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TreeFieldDecl {
    /// `(mut i32)` field holding a DOM handle (an element, text node,
    /// or comment-anchor's id in the host-side handle registry).
    DomHandle { name: String },
    /// Loop variable in an iter-body. Typed per the iterable's item type.
    /// For pointer-passed types (records / tuples) the val_ty is `I32`
    /// holding the address.
    LoopVar {
        name: String,
        val_ty: LirSlotValType,
    },
    /// Reference to a nested sub-boundary's struct type.
    SubBoundary {
        name: String,
        /// Index into `ComponentTreeShape.boundaries`.
        target_idx: u32,
    },
    /// `(mut (ref null <arr_ty>))` field holding the children array of
    /// a `for` anchor. The array's element type is `<iter_body>`.
    ChildrenArray {
        name: String,
        /// Index into `ComponentTreeShape.boundaries` of the iter-body
        /// boundary whose struct is the array's element type.
        arr_target_idx: u32,
    },
    /// `(mut i32)` flag tracking which branch of an `if` is currently
    /// active. Encoding: 0 = none, 1 = then, 2.. = else_if_n, last = else.
    ActiveTag { name: String },
}

/// One tree-boundary node in a component's concrete-typed mount tree.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TreeBoundary {
    pub id: TreeBoundaryId,
    pub kind: TreeBoundaryKind,
    pub fields: Vec<TreeFieldDecl>,
    /// Parent boundary (if any) and the field index of the
    /// `SubBoundary` slot on the parent that points at *this*
    /// boundary. `None` for the root boundary. For `ForIterBody`
    /// the link is also `None` because the iter-body is reachable
    /// only via the for-anchor's children-array (runtime index
    /// required) — not via a static `SubBoundary` field.
    pub parent_link: Option<(TreeBoundaryId, u32)>,
}

/// Per-component shape of the concrete-typed mount tree. Synthesized
/// once during block lowering by walking the body tree. Consumed by
/// gc_types emission to declare the per-position struct + array types,
/// and by op emission to compute typed walks for state slots.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ComponentTreeShape {
    /// All boundaries, indexed by their position in this Vec. The id
    /// of the boundary at index `i` is `TreeBoundaryId(i)`.
    pub boundaries: Vec<TreeBoundary>,
    /// Index into `boundaries` of the component's root boundary
    /// (always 0 in the canonical layout, but stored explicitly for
    /// flexibility).
    pub root_idx: u32,
    /// Map from each `LirNode.id` that has a persistent state field
    /// in the typed mount tree to the `(boundary, field_idx)` where
    /// that field lives. Populated by `tree_shape::synthesize` for
    /// every Element / DynamicText / If / For node in the body tree
    /// (StaticText / ChildrenSlot have no persistent state and are
    /// absent). Consumers look this up to compute typed-walk slot
    /// paths without re-walking the tree.
    pub node_field: HashMap<NodeId, NodeFieldRef>,
}

/// Reference to where a `LirNode`'s primary persistent-state field
/// lives in the mount tree.
///
/// For an `Element` / `DynamicText`: a `DomHandle` field on the node's
/// owning boundary.
///
/// For an `If`: a `SubBoundary` field on the parent boundary that
/// points at the `IfAnchor` boundary owning the if's anchor handle +
/// active-tag + branch refs. The `IfAnchor`'s id is reachable via
/// `boundary.fields[field_idx]` (a `TreeFieldDecl::SubBoundary`
/// whose `target_idx` is the anchor's `TreeBoundaryId`).
///
/// For a `For`: same shape as `If`, with the `SubBoundary` pointing
/// at the `ForAnchor` boundary.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct NodeFieldRef {
    /// Boundary that owns the field — i.e. the boundary the node was
    /// lowered into during synthesis.
    pub owning_boundary: TreeBoundaryId,
    /// Field index within `owning_boundary.fields`.
    pub field_idx: u32,
}

/// Whether a slot is temporary (WASM local) or persistent (memory).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LirSlotKind {
    /// Temporary slot - becomes a WASM local.
    /// Only valid within a single function call.
    Temp {
        /// Index of the WASM local variable.
        local_idx: u32,
    },
    /// Memory slot - pre-computed address in linear memory.
    /// Persists across function calls (for DOM handles, state).
    Memory {
        /// Byte offset in component's memory region.
        offset: u32,
        /// Size in bytes.
        size: u32,
    },
    // Stage 5e-4 (lir-resource-flatten): the `BoundaryField { boundary_id,
    // field_idx }` slot kind was deleted. Persistent GC-struct fields are now
    // accessed via the symbolic `LirOp::StructField{Get,Set,SetConst}` ops
    // (carrying `struct_ty` + `field_idx`), which `boundary_rewrite` resolves
    // to `StructGet`/`StructSet` against an in-scope ref slot. The slot kind
    // existed only as the lowerer→rewriter handshake; the ops carry that data
    // directly now.
    /// Slot lives directly in a wasm function parameter — no local
    /// declaration, no copy from param. `idx` is the wasm-level
    /// parameter index (0..param_count-1). Reads/writes via this slot
    /// resolve to `local.get/set <idx>` regardless of the enclosing
    /// function's `local_offset`. Phase 0.3i — lets the synthesized
    /// mount block declare its parent-DOM-id input as `WasmParam(1)`
    /// instead of receiving it through a prologue param-copy.
    WasmParam { idx: u32 },
}
