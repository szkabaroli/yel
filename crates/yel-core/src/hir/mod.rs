//! High-Level Intermediate Representation (HIR)
//!
//! HIR is the first IR after parsing. It:
//! - Resolves names to DefId/LocalId
//! - Interns types to Ty handles
//! - Maintains source spans for error reporting

pub mod expr;
pub mod local_scope;
pub mod lower;
pub mod node;

pub use expr::{HirExpr, HirExprKind, HirLiteral};
pub use local_scope::{LocalInfo, LocalScope};
pub use lower::lower_file;
pub use node::{HirBinding, HirComponent, HirHandler, HirNode, HirNodeKind};
