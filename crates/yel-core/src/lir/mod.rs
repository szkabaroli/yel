//! Low-Level Intermediate Representation (LIR).
//!
//! LIR is the wasm-codegen-facing IR: explicit instructions
//! (`LirOp`), pre-allocated storage (`LirSlot*`), interned strings
//! and expressions. The block-based representation maps directly to
//! WASM instructions.
//!
//! ## Layering
//!
//! LIR is *frontend-neutral*. Yel-lang components reach it via
//! `crate::lower_to_lir` (THIR→LIR); the flow-graph frontend (in
//! `yel-flow-core`) targets LIR directly without going through HIR
//! or THIR; a future direct emitter would do the same.
//!
//! Modules in this directory therefore depend **only** on the
//! frontend-neutral foundations:
//!
//! * `crate::ops`      — operator semantics (`BinOp`, `UnaryOp`)
//! * `crate::ids`      — IR identifiers (`DefId`, `BlockId`, …)
//! * `crate::types`    — interned `Ty` and kind enum
//! * `crate::interner` — name interning
//! * `crate::source`   — `Span`
//! * `crate::definitions` and `crate::context` — type registry
//!
//! Anything frontend-specific (THIR shapes, `ThirComponent`,
//! `HirLiteral`, …) belongs in `crate::lower_to_lir` instead, never
//! here.

pub mod arena;
pub mod block;
pub mod boundary_rewrite;
pub mod dedupe;
pub mod diff;
pub mod expr;
pub mod function;
pub mod layout;
pub mod module;
pub mod node;
pub mod signal;
pub mod signal_layout;
pub mod struct_types;
pub mod tree_shape;

// Block-based types
pub use block::{
    ArithOp, ArrayItemRepr, BinOperand, CompareOp, LirBindingMode, LirBlock, LirBlockEffect,
    LirExprId, LirGlobalRef, LirOp, LirSlotId, LirSlotInfo, LirSlotKind, LirSlotValType,
    LirTypeRef, MemoryValueType, StoreWidth, StringId,
};

// Arena traits — abstract over expr/string/slot ownership and the
// function-shape so non-component callers (flow functions) can plug
// into the same code-gen helpers.
pub use arena::{LirExprArena, LirFunctionLike, LirResourceArena, LirSlotArena, LirStringArena};

// Function metadata: identity (DefId vs BlockId) + calling convention
// (implicit params + return types). Pure data; the body lives in the
// backing `LirBlock` / `FlowFunc` and is accessed through the arena
// traits.
pub use function::{
    CallingConv, ExportShape, FunctionRole, ImplicitParam, InternalPurpose, LirFunction,
};

// Expression types
pub use expr::{LirExpr, LirExprKind, LirLiteral};

// Layout utilities
pub use layout::{
    FlatValTypeCounts, LirCoreValType, LirLayoutContext, LirTypeLayout, RecordLayout,
    VariantLayout, align_to, discriminant_size, per_valtype_counts,
};

// Main lowering entry point — re-exported for back-compat. The
// actual code lives outside `lir/` because it depends on
// HIR/THIR, which would violate this module's frontend-neutral
// layering rule.
pub use crate::lower_to_lir::{lower_component, lower_globals};

// Module-scoped compilation unit
pub use module::{
    InterfaceDirection, LirGlobal, LirGlobalProperty, LirIfaceFn, LirImport, LirInterface,
    LirModule, LirReceiver, ModuleScope,
};

// Component and node types
pub use node::{LirBinding, LirHandler, LirNode, LirNodeKind, LirResource};

// Signal types (LirEffect is internal but used by tree-lowering)
pub use signal::{LirEffect, LirSignal};

// Per-signal storage layout (Phase 1.1a: where each signal lives —
// in the component's $Comp_<i> GC struct or in linear memory).
pub use signal_layout::{GcSlot, SignalLayout, SignalStorage, compute_signal_layout};
