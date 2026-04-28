//! Block-Based LIR Types
//!
//! This module defines a lower-level representation where:
//! - UI operations are explicit instructions (LirOp)
//! - Branches become separate blocks with mount/unmount operations
//! - Storage is pre-allocated (SlotId for temps and memory)
//! - Strings and expressions are interned (StringId, ExprId)

use std::collections::BTreeSet;
use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::ids::{BlockId, DefId, ForId, IfId, TreeBoundaryId};
use crate::types::Ty;
use crate::{LocalId, NodeId};

/// Slot ID for storage locations.
///
/// Slots can be either:
/// - Temporary: WASM locals within a function (short-lived)
/// - Memory: Pre-computed addresses in linear memory (persist across calls)
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct LirSlotId(pub u32);

/// String ID for interned strings.
///
/// References a string stored in the component's string table.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct StringId(pub u32);

/// Expression ID for pre-lowered expressions.
///
/// References an expression stored in the component's expression table.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct ExprId(pub u32);

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
    /// Tree-boundary refs this block expects to receive as additional
    /// WASM function parameters, AFTER the legacy i32 params declared in
    /// `params`. When non-empty, the block opts into a dynamic per-block
    /// function type whose signature is
    /// `(ref $Comp, <i32 args from `params`...>, (ref null <boundary_0_struct>), ...) -> <return>`.
    ///
    /// Each entry is registered in `current_boundary_locals` at function
    /// entry so subsequent `BoundaryField` slot accesses on those
    /// boundaries resolve to a `local.get` on the param.
    ///
    /// Empty for blocks whose only inputs are i32 args (these stick to
    /// the fixed `block_1param_type_idx` / `block_2param_*` shapes).
    pub boundary_params: Vec<TreeBoundaryId>,
}

impl LirBlock {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            ops: Vec::new(),
            captured_locals: HashMap::new(),
            local_to_slot: HashMap::new(),
            local_modes: HashMap::new(),
            return_slot: None,
            params: Vec::new(),
            max_flat_scratch_counts: (0, 0, 0, 0),
            mount_component_count: 0,
            mount_component_children: Vec::new(),
            boundary_params: Vec::new(),
        }
    }

    pub fn with_ops(id: BlockId, ops: Vec<LirOp>) -> Self {
        Self {
            id,
            ops,
            captured_locals: HashMap::new(),
            local_to_slot: HashMap::new(),
            local_modes: HashMap::new(),
            return_slot: None,
            params: Vec::new(),
            max_flat_scratch_counts: (0, 0, 0, 0),
            mount_component_count: 0,
            mount_component_children: Vec::new(),
            boundary_params: Vec::new(),
        }
    }

    pub fn with_ops_and_captures(
        id: BlockId,
        ops: Vec<LirOp>,
        captured_locals: HashMap<LocalId, LirSlotId>,
    ) -> Self {
        Self {
            id,
            ops,
            captured_locals,
            local_to_slot: HashMap::new(),
            local_modes: HashMap::new(),
            return_slot: None,
            params: Vec::new(),
            max_flat_scratch_counts: (0, 0, 0, 0),
            mount_component_count: 0,
            mount_component_children: Vec::new(),
            boundary_params: Vec::new(),
        }
    }

    pub fn set_return_slot(&mut self, slot: LirSlotId) {
        self.return_slot = Some(slot);
    }
}

/// Low-level operations that map closely to WASM instructions.
///
/// Each operation either:
/// - Performs a DOM operation via host import
/// - Manipulates memory or locals
/// - Controls execution flow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LirOp {
    // === DOM Operations ===
    /// Create an element node.
    /// Result: DOM handle in `result` slot.
    CreateElement { tag: StringId, result: LirSlotId },

    /// Create a layout-neutral fragment wrapper element. Used by
    /// `for` iter-mount and `if` branch-mount blocks to group their
    /// content under a single DOM root so a single host `remove`
    /// cascades on teardown. Lowers to a call to the
    /// `yel:ui/dom@0.1.0/create-fragment` host import.
    /// Result: DOM handle in `result` slot.
    CreateFragment { result: LirSlotId },

    /// Create a static text node.
    /// Result: DOM handle in `result` slot.
    CreateText {
        content: StringId,
        result: LirSlotId,
    },

    /// Create a dynamic text node by evaluating expression.
    /// Used for initial mount of reactive text.
    /// Result: DOM handle in `result` slot.
    CreateTextDynamic { expr: ExprId, result: LirSlotId },

    /// Create a comment node (used as anchor for conditionals).
    /// Result: DOM handle in `result` slot.
    CreateComment {
        content: StringId,
        result: LirSlotId,
    },

    /// Append child to parent.
    AppendChild { parent: LirSlotId, child: LirSlotId },

    /// Insert node after anchor (for conditional/loop rendering).
    InsertAfter {
        parent: LirSlotId,
        node: LirSlotId,
        anchor: LirSlotId,
    },

    /// Remove node from DOM.
    Remove { node: LirSlotId },

    /// Set text content by evaluating expression.
    SetTextContent { node: LirSlotId, expr: ExprId },

    /// Set attribute by evaluating expression.
    SetAttribute {
        node: LirSlotId,
        name: StringId,
        expr: ExprId,
    },

    /// Add event listener.
    AddEventListener {
        node: LirSlotId,
        event: StringId,
        handler: BlockId,
    },

    /// Mount a child component instance.
    /// Creates the component, mounts it to the parent, and returns instance handle.
    /// The child component's root element is appended to `parent`.
    MountComponent {
        /// DefId of the component to instantiate.
        component_def: DefId,
        /// Parent DOM element to mount into.
        parent: LirSlotId,
        /// When the target component is a container (has `@children`),
        /// its `mount` returns a children-root DOM node id. Codegen
        /// captures that value into this slot so caller-supplied child
        /// nodes can be appended under it. `None` for non-container
        /// targets (mount returns no value).
        children_root: Option<LirSlotId>,
    },

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

    /// Compare two i32 Temp slots; write 1 to `result` if `lhs != rhs`,
    /// else 0. Used by if-update routing to detect a branch change.
    I32Ne {
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
    /// Used by fan-out update walks: after `ChildrenArrayGet` has
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
    EvalExpr { expr: ExprId, result: LirSlotId },

    /// Evaluate expression purely for side effects, discarding any values it
    /// pushes on the stack. Used for expression statements in effect blocks
    /// (e.g. `on-click();` where the callback returns a value that isn't
    /// stored). Unlike `EvalExpr`, no slot is reserved and the exact number
    /// of stack values produced is drained at codegen time using the
    /// expression's flat core valtypes.
    DropExpr { expr: ExprId },

    /// Conditional: if cond is non-zero, execute then_ops, else execute else_ops.
    If {
        cond: LirSlotId,
        then_ops: Vec<LirOp>,
        else_ops: Vec<LirOp>,
        /// Optional debug label surfaced in the WASM name section as a
        /// `label` subsection entry on the emitted `if` instruction.
        /// `None` means no label entry is emitted for this if (the name
        /// section is a debug hint, so omitting is valid).
        name: Option<String>,
    },

    /// Call a block (used for branch mount/unmount).
    /// Parent slot is passed to allow insertion.
    CallBlock { block: BlockId, parent: LirSlotId },

    /// Return from current block/function.
    Return,

    // === Signal Operations ===
    /// Read signal value into slot.
    SignalRead { signal: DefId, result: LirSlotId },

    /// Write value from slot to signal (component-local or global property).
    SignalWrite { signal: DefId, value: LirSlotId },

    /// Write an expression's value to a signal (component-local or global
    /// property) without first funneling through a single-slot temp. Used for
    /// composite signal types (option, result, variant-with-payload, record,
    /// tuple) whose flat canonical-ABI shape is multi-slot and therefore
    /// cannot fit the single-SlotId `SignalWrite` form. Codegen emits the
    /// expression and stores each flat slot to its offset.
    SignalWriteExpr { signal: DefId, expr: ExprId },

    /// Trigger all effects that depend on signal.
    TriggerEffects { signal: DefId },

    // === Constructor Operations ===
    /// Initialize signal with expression value.
    /// Used during component construction to set initial values.
    InitSignal { signal_idx: u32, expr: ExprId },

    /// Initialize signal with zero/empty default.
    /// Used when no default value is provided.
    InitSignalDefault { signal_idx: u32 },

    /// Initialize memory slot to zero.
    /// Used to clear persistent storage during construction.
    InitMemorySlot { slot: LirSlotId },

    /// Call resource.new to create the resource handle.
    /// Returns the component's base address wrapped in a resource.
    ResourceNew { base_addr: i32 },

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

    /// Call a block function with two parameters.
    /// Used for for-loop body blocks: (parent, item_ptr) -> () or -> i32.
    /// When `result` is `Some(slot)`, the callee is expected to return an
    /// i32 (see `LirBlock::return_slot`) and the returned value is stored
    /// into `slot`. Used for for-item mount blocks so the caller can
    /// record the root node handle for later diff / unmount.
    CallBlock2 {
        block: BlockId,
        param0: LirSlotId,
        param1: LirSlotId,
        result: Option<LirSlotId>,
    },

    /// Evaluate condition (index >= len) and store in slot.
    /// Generates: local.get index; local.get len; i32.ge_u
    GeU {
        index: LirSlotId,
        len: LirSlotId,
        result: LirSlotId,
    },

    /// Evaluate `a < b` (unsigned) and store in slot.
    /// Generates: local.get a; local.get b; i32.lt_u
    LtU {
        a: LirSlotId,
        b: LirSlotId,
        result: LirSlotId,
    },

    /// Compute item pointer: base + index * element_size
    ComputeItemPtr {
        base: LirSlotId,
        index: LirSlotId,
        element_size: u32,
        result: LirSlotId,
    },

    /// Increment slot value by 1.
    IncrSlot { slot: LirSlotId },

    /// Allocate memory: alloc(size, align) -> ptr
    Alloc {
        size: LirSlotId,
        align: u32,
        result: LirSlotId,
    },

    /// Free memory: free(ptr, size)
    Free { ptr: LirSlotId, size: LirSlotId },

    /// Multiply slot by constant: slot * constant -> result
    MulConst {
        slot: LirSlotId,
        constant: u32,
        result: LirSlotId,
    },

    /// Add slots: a + b -> result
    AddSlots {
        a: LirSlotId,
        b: LirSlotId,
        result: LirSlotId,
    },

    /// Subtract slots: a - b -> result
    SubSlots {
        a: LirSlotId,
        b: LirSlotId,
        result: LirSlotId,
    },

    /// Load i32 from address (not memory slot).
    LoadI32Addr { addr: LirSlotId, result: LirSlotId },

    /// Store i32 to address (not memory slot).
    StoreI32Addr { addr: LirSlotId, value: LirSlotId },

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

    /// Read field `field` of a struct-typed ref in `rec`, store to
    /// `result`. Emits `local.get rec; struct.get <ty_idx> <field>;
    /// local.set result`.
    StructGet {
        ty_idx: u32,
        field: u32,
        rec: LirSlotId,
        result: LirSlotId,
    },

    /// Write `value` to field `field` of struct-typed ref in `rec`.
    /// Emits `local.get rec; local.get value; struct.set <ty_idx>
    /// <field>`.
    StructSet {
        ty_idx: u32,
        field: u32,
        rec: LirSlotId,
        value: LirSlotId,
    },

    /// Allocate a default-initialised GC array of length `len` and
    /// element type declared by `ty_idx`. Equivalent to
    /// `array.new_default <ty_idx> len`.
    ArrayNewDefault {
        ty_idx: u32,
        len: LirSlotId,
        result: LirSlotId,
    },

    /// Read the `idx`-th element of a GC array into `result`.
    /// `array.get <ty_idx>`.
    ArrayGet {
        ty_idx: u32,
        arr: LirSlotId,
        idx: LirSlotId,
        result: LirSlotId,
    },

    /// Write `value` into the `idx`-th slot of a GC array.
    /// `array.set <ty_idx>`.
    ArraySet {
        ty_idx: u32,
        arr: LirSlotId,
        idx: LirSlotId,
        value: LirSlotId,
    },

    /// Copy a range from `src[src_idx..src_idx+count]` into
    /// `dst[dst_idx..dst_idx+count]`. `array.copy <dst_ty> <src_ty>`.
    /// Typically dst_ty == src_ty for the survivor-preservation copy.
    ArrayCopy {
        dst_ty_idx: u32,
        src_ty_idx: u32,
        dst: LirSlotId,
        dst_idx: LirSlotId,
        src: LirSlotId,
        src_idx: LirSlotId,
        count: LirSlotId,
    },

    /// Push the length of a GC array onto `result`.
    ArrayLen { arr: LirSlotId, result: LirSlotId },

    /// Convert a ref to non-null (trapping on null). Emits
    /// `ref.as_non_null`. Used when reading array elements declared
    /// nullable but known-non-null by construction.
    RefAsNonNull { slot: LirSlotId },

    /// Push a null ref of a concrete heap type. `ref.null <ty_idx>`.
    RefNull { ty_idx: u32, result: LirSlotId },

    /// Symbolic `array.new_default` of a `ForAnchor`'s children-array
    /// type. Element type is the per-for `ForIterBody` struct.
    /// Codegen resolves via `gc_layouts[comp].tree_for_arr_type_idx[anchor_boundary_id]`.
    ChildrenArrayNewDefault {
        anchor_boundary: TreeBoundaryId,
        len: LirSlotId,
        result: LirSlotId,
    },

    /// Symbolic `array.get` of a `ForAnchor`'s children-array.
    ChildrenArrayGet {
        anchor_boundary: TreeBoundaryId,
        arr: LirSlotId,
        idx: LirSlotId,
        result: LirSlotId,
    },

    /// Symbolic `array.set` of a `ForAnchor`'s children-array.
    ChildrenArraySet {
        anchor_boundary: TreeBoundaryId,
        arr: LirSlotId,
        idx: LirSlotId,
        value: LirSlotId,
    },

    /// Symbolic `array.copy` of a `ForAnchor`'s children-array.
    ChildrenArrayCopy {
        anchor_boundary: TreeBoundaryId,
        dst: LirSlotId,
        dst_idx: LirSlotId,
        src: LirSlotId,
        src_idx: LirSlotId,
        count: LirSlotId,
    },

    /// Load list (ptr, len) from signal memory address.
    /// Stores ptr to ptr_result and len to len_result.
    LoadList {
        signal: DefId,
        ptr_result: LirSlotId,
        len_result: LirSlotId,
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

    /// Evaluate a list expression and store (ptr, len) results.
    /// Used for for-loops over literal lists like `[0, 1]`.
    EvalListExpr {
        expr: ExprId,
        ptr_result: LirSlotId,
        len_result: LirSlotId,
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
        expr: ExprId,
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
    ArrayGetItem {
        arr: LirSlotId,
        idx: LirSlotId,
        list_ty: Ty,
        result: LirSlotId,
    },
}

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
    /// Persistent state lives at `field_idx` of `boundary_id`'s GC
    /// struct in the component's concrete-typed mount tree. Read /
    /// written via `local.get <boundary>; struct.get <ty> <field>` or
    /// `local.get <boundary>; <value>; struct.set <ty> <field>` —
    /// where `<boundary>` is the WASM local holding the boundary's
    /// struct ref, supplied by the emission scope (function param,
    /// for-iter callback param, if-branch mount param, or the root
    /// materialized from `$self.tree` at function entry).
    ///
    /// No path / walk is stored on the slot — each emission scope is
    /// responsible for arranging the appropriate boundary ref to be
    /// in scope (mount/update functions take their boundary as a
    /// parameter; fan-out wraps the call with the per-iteration
    /// boundary loaded from a `for`-anchor's children array).
    BoundaryField {
        boundary_id: TreeBoundaryId,
        field_idx: u32,
    },
}
