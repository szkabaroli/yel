//! Block-Based LIR Types
//!
//! This module defines a lower-level representation where:
//! - UI operations are explicit instructions (LirOp)
//! - Branches become separate blocks with mount/unmount operations
//! - Storage is pre-allocated (SlotId for temps and memory)
//! - Strings and expressions are interned (StringId, ExprId)

use crate::ids::DefId;

// Re-export BlockId from ids module
pub use crate::ids::BlockId;

/// Slot ID for storage locations.
///
/// Slots can be either:
/// - Temporary: WASM locals within a function (short-lived)
/// - Memory: Pre-computed addresses in linear memory (persist across calls)
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SlotId(pub u32);

/// String ID for interned strings.
///
/// References a string stored in the component's string table.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct StringId(pub u32);

/// Expression ID for pre-lowered expressions.
///
/// References an expression stored in the component's expression table.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ExprId(pub u32);

/// A block is a reusable sequence of DOM operations.
///
/// Blocks can be:
/// - Mount blocks: Create and attach DOM nodes
/// - Unmount blocks: Remove DOM nodes
/// - Update blocks: Called by effects to update reactive parts
/// - Handler blocks: Event handler bodies
#[derive(Debug, Clone)]
pub struct LirBlock {
    pub id: BlockId,
    pub ops: Vec<LirOp>,
    /// Mapping from LocalId to parameter slot index for captured locals.
    /// Used for for-loop items where the item is passed as a parameter to the body block.
    pub captured_locals: std::collections::HashMap<crate::ids::LocalId, u32>,
    /// Mapping from LocalId to SlotId for locals loaded from memory.
    /// Used for outer loop items in nested for-loops that are stored to memory
    /// and then loaded at the start of inner blocks.
    pub local_to_slot: std::collections::HashMap<crate::ids::LocalId, SlotId>,
}

impl LirBlock {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            ops: Vec::new(),
            captured_locals: std::collections::HashMap::new(),
            local_to_slot: std::collections::HashMap::new(),
        }
    }

    pub fn with_ops(id: BlockId, ops: Vec<LirOp>) -> Self {
        Self {
            id,
            ops,
            captured_locals: std::collections::HashMap::new(),
            local_to_slot: std::collections::HashMap::new(),
        }
    }

    pub fn with_ops_and_captures(id: BlockId, ops: Vec<LirOp>, captured_locals: std::collections::HashMap<crate::ids::LocalId, u32>) -> Self {
        Self {
            id,
            ops,
            captured_locals,
            local_to_slot: std::collections::HashMap::new(),
        }
    }
}

/// Low-level operations that map closely to WASM instructions.
///
/// Each operation either:
/// - Performs a DOM operation via host import
/// - Manipulates memory or locals
/// - Controls execution flow
#[derive(Debug, Clone)]
pub enum LirOp {
    // === DOM Operations ===
    /// Create an element node.
    /// Result: DOM handle in `result` slot.
    CreateElement { tag: StringId, result: SlotId },

    /// Create a static text node.
    /// Result: DOM handle in `result` slot.
    CreateText { content: StringId, result: SlotId },

    /// Create a dynamic text node by evaluating expression.
    /// Used for initial mount of reactive text.
    /// Result: DOM handle in `result` slot.
    CreateTextDynamic { expr: ExprId, result: SlotId },

    /// Create a comment node (used as anchor for conditionals).
    /// Result: DOM handle in `result` slot.
    CreateComment { content: StringId, result: SlotId },

    /// Append child to parent.
    AppendChild { parent: SlotId, child: SlotId },

    /// Insert node after anchor (for conditional/loop rendering).
    InsertAfter { parent: SlotId, node: SlotId, anchor: SlotId },

    /// Remove node from DOM.
    Remove { node: SlotId },

    /// Set text content by evaluating expression.
    SetTextContent { node: SlotId, expr: ExprId },

    /// Set attribute by evaluating expression.
    SetAttribute { node: SlotId, name: StringId, expr: ExprId },

    /// Add event listener.
    AddEventListener { node: SlotId, event: StringId, handler: BlockId },

    // === Memory Operations ===
    /// Store DOM handle from temp slot to memory slot.
    StoreHandle { slot: SlotId, from: SlotId },

    /// Load DOM handle from memory slot to temp slot.
    LoadHandle { slot: SlotId, to: SlotId },

    /// Store constant i32 to memory slot.
    StoreI32 { slot: SlotId, value: i32 },

    /// Load i32 from memory slot to temp slot.
    LoadI32 { slot: SlotId, to: SlotId },

    // === Control Flow ===
    /// Evaluate expression and store result in slot.
    EvalExpr { expr: ExprId, result: SlotId },

    /// Conditional: if cond is non-zero, execute then_ops, else execute else_ops.
    If {
        cond: SlotId,
        then_ops: Vec<LirOp>,
        else_ops: Vec<LirOp>,
    },

    /// Call a block (used for branch mount/unmount).
    /// Parent slot is passed to allow insertion.
    CallBlock { block: BlockId, parent: SlotId },

    /// Return from current block/function.
    Return,

    // === Signal Operations ===
    /// Read signal value into slot.
    SignalRead { signal: DefId, result: SlotId },

    /// Write value from slot to signal.
    SignalWrite { signal: DefId, value: SlotId },

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
    InitMemorySlot { slot: SlotId },

    /// Call resource.new to create the resource handle.
    /// Returns the component's base address wrapped in a resource.
    ResourceNew { base_addr: i32 },

    // === Loop Operations ===
    /// Loop with break condition.
    /// Generates: block { loop { br_if 1 if cond; body_ops; br 0 } }
    Loop {
        /// Slot containing break condition (loop exits if truthy).
        break_cond: SlotId,
        /// Operations in the loop body.
        body_ops: Vec<LirOp>,
    },

    /// Call a block function with two parameters.
    /// Used for for-loop body blocks: (parent, item_ptr) -> ()
    CallBlock2 {
        block: BlockId,
        param0: SlotId,
        param1: SlotId,
    },

    /// Evaluate condition (index >= len) and store in slot.
    /// Generates: local.get index; local.get len; i32.ge_u
    GeU {
        index: SlotId,
        len: SlotId,
        result: SlotId,
    },

    /// Compute item pointer: base + index * element_size
    ComputeItemPtr {
        base: SlotId,
        index: SlotId,
        element_size: u32,
        result: SlotId,
    },

    /// Increment slot value by 1.
    IncrSlot { slot: SlotId },

    /// Allocate memory: alloc(size, align) -> ptr
    Alloc {
        size: SlotId,
        align: u32,
        result: SlotId,
    },

    /// Free memory: free(ptr, size)
    Free { ptr: SlotId, size: SlotId },

    /// Multiply slot by constant: slot * constant -> result
    MulConst {
        slot: SlotId,
        constant: u32,
        result: SlotId,
    },

    /// Add slots: a + b -> result
    AddSlots {
        a: SlotId,
        b: SlotId,
        result: SlotId,
    },

    /// Load i32 from address (not memory slot).
    LoadI32Addr { addr: SlotId, result: SlotId },

    /// Store i32 to address (not memory slot).
    StoreI32Addr { addr: SlotId, value: SlotId },

    /// Load list (ptr, len) from signal memory address.
    /// Stores ptr to ptr_result and len to len_result.
    LoadList {
        signal: DefId,
        ptr_result: SlotId,
        len_result: SlotId,
    },

    /// Load list (ptr, len) from a field access on a local.
    /// Used for nested for-loops like `for sub in item.subitems`.
    LoadListFromLocal {
        /// The local variable holding the base record pointer.
        base_local: crate::ids::LocalId,
        /// The field index within the record.
        field_idx: crate::ids::FieldIdx,
        /// The record type DefId (for computing field offset).
        record_def: DefId,
        /// Slot to store the list pointer.
        ptr_result: SlotId,
        /// Slot to store the list length.
        len_result: SlotId,
    },

    /// Store constant i32 to slot (temp).
    SetSlot { slot: SlotId, value: i32 },
}

/// Effect definition - connects signal dependencies to update blocks.
#[derive(Debug, Clone)]
pub struct LirBlockEffect {
    pub id: u32,
    /// Signals this effect depends on.
    pub dependencies: Vec<DefId>,
    /// Block to call when any dependency changes.
    pub update_block: BlockId,
}

/// WASM value type for temp slots.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SlotValType {
    #[default]
    I32,
    I64,
    F32,
    F64,
}

/// Information about a slot's allocation.
#[derive(Debug, Clone)]
pub struct SlotInfo {
    pub id: SlotId,
    pub kind: SlotKind,
    /// WASM value type for temp slots. Defaults to I32.
    pub val_ty: SlotValType,
}

/// Whether a slot is temporary (WASM local) or persistent (memory).
#[derive(Debug, Clone)]
pub enum SlotKind {
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
}

