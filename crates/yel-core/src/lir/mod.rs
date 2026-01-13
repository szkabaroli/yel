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
pub mod expr;
pub mod layout;
pub mod lower;
pub mod node;
pub mod signal;

// Block-based types
pub use block::{
    BlockId, ExprId, LirBlock, LirBlockEffect, LirOp, SlotId, SlotInfo, SlotKind, SlotValType,
    StringId,
};

// Expression types
pub use expr::{LirExpr, LirExprKind, LirLiteral};

// Layout utilities
pub use layout::{align_to, discriminant_size, LayoutContext, RecordLayout, TypeLayout, VariantLayout};

// Main lowering entry point
pub use lower::lower_component;

// Component and node types
pub use node::{LirBinding, LirComponent, LirHandler, LirNode, LirNodeKind};

// Signal types (LirEffect is internal but used by tree-lowering)
pub use signal::{LirEffect, LirSignal};
