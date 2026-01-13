//! Typed High-Level Intermediate Representation (THIR)
//!
//! THIR is the result of type checking HIR. All expressions have types,
//! field accesses are resolved to indices, and patterns are fully typed.

pub mod expr;
pub mod node;
pub mod typeck;

pub use expr::{ThirExpr, ThirExprKind, ThirInterpolationPart, ThirPattern, ThirStatement};
pub use node::{ThirBinding, ThirComponent, ThirHandler, ThirNode, ThirNodeKind};
pub use typeck::{type_check, type_check_with_map, Mode, TypeCheckResult, TypeMap};
