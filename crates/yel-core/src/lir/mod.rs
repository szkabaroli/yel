//! Low-Level Intermediate Representation (LIR)
//!
//! LIR is optimized for code generation. It:
//! - Extracts reactive signals and effects
//! - Converts UI tree to block-based operations
//! - Pre-computes memory layouts for types
//! - Interns strings and expressions
//!
//! The block-based representation maps directly to WASM instructions.

pub mod block;
pub(crate) mod block_lower;
pub mod dedupe;
pub mod diff;
pub mod expr;
pub mod layout;
pub mod lower;
pub mod module;
pub mod node;
pub mod signal;
pub mod tree_shape;

// Block-based types
pub use block::{
    ExprId, LirBindingMode, LirBlock, LirBlockEffect, LirOp, LirSlotId, LirSlotInfo, LirSlotKind,
    LirSlotValType, StringId,
};

// Expression types
pub use expr::{LirExpr, LirExprKind, LirLiteral};

// Layout utilities
pub use layout::{
    align_to, discriminant_size, max_flat_counts, per_valtype_counts, FlatValTypeCounts,
    LirCoreValType, LirLayoutContext, LirTypeLayout, RecordLayout, VariantLayout,
};

// Main lowering entry point
pub use lower::{lower_component, lower_globals};

// Module-scoped compilation unit
pub use module::LirModule;

// Component and node types
pub use node::{LirBinding, LirComponent, LirHandler, LirNode, LirNodeKind};

// Signal types (LirEffect is internal but used by tree-lowering)
pub use signal::{LirEffect, LirSignal};
