//! Yel Compiler
//!
//! This crate implements a compiler for the Yel DSL with:
//! - Centralized type definitions with interning
//! - Multi-stage IR lowering (AST → HIR → THIR → LIR)
//! - Unified definition storage with DefId references
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────────┐
//! │                          CompilerContext                                │
//! │  ┌───────────┐  ┌───────────┐  ┌───────────┐  ┌───────────┐            │
//! │  │ Interner  │  │  Types    │  │   Defs    │  │ SourceMap │            │
//! │  │ (strings) │  │ (Ty→Kind) │  │(DefId→Def)│  │ (files)   │            │
//! │  └───────────┘  └───────────┘  └───────────┘  └───────────┘            │
//! └─────────────────────────────────────────────────────────────────────────┘
//!         ↑              ↑              ↑
//!         │              │              │
//!    ┌────┴────┐    ┌────┴────┐    ┌────┴────┐    ┌─────────┐
//!    │  Parse  │ →  │   HIR   │ →  │  THIR   │ →  │   LIR   │ → Codegen
//!    │  (AST)  │    │ Lower   │    │ TypeCk  │    │ Lower   │
//!    └─────────┘    └─────────┘    └─────────┘    └─────────┘
//! ```

// Lowering routines legitimately thread many positional parameters; bundling
// them into immediately-destructured structs hurt readability more than it
// helped, so the lint is allowed crate-wide.
#![allow(clippy::too_many_arguments)]

// Core modules
pub mod compiler;
pub mod context;
pub mod definitions;
pub mod diagnostic;
pub mod dom_imports;
pub mod ids;
pub mod index_vec;
pub mod interner;
pub mod known;
pub mod naming;
pub mod ops;
pub mod source;
pub mod stdlib_lookup;
pub mod syntax;
pub mod types;

// IR modules
pub mod hir;
pub mod lir;
pub mod thir;

/// THIR → LIR lowering. Frontend-bridge code, kept outside
/// `crate::lir` so `lir/` stays free of HIR/THIR dependencies.
pub mod lower_to_lir;

// Re-exports
pub use compiler::{CompileError, CompileResult, Compiler};
pub use context::{BlockDebugName, CompilerContext};
pub use definitions::{DefKind, Definitions, Namespace};
pub use diagnostic::{Diagnostic, Diagnostics, Severity};
pub use ids::{DefId, ExprId, FieldIdx, LocalId, NodeId, VariantIdx};
pub use index_vec::{Idx, IndexVec};
pub use interner::{ArcStr, Interner, Name};
pub use source::{Source, SourceId, SourceMap, Span};
pub use syntax::{ParseError, parse};
pub use types::{InternedTyKind, Ty, TypeInterner};
