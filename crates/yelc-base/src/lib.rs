//! `yelc-base` — shared compiler infrastructure: diagnostics, source files,
//! string interning, typed ids.
//!
//! # What lives here
//!
//! | Module | What |
//! |---|---|
//! | [`diagnostic`] | `Diagnostic` builder, `ErrorCode`, the accumulating `Diagnostics` sink, span-aware rendering |
//! | [`source`] | `SourceMap`, `Span`, byte offsets, snippet rendering |
//! | [`interner`] | `NameInterner`/`Name` — no `String` survives past lowering |
//! | [`ids`] | Typed `u32` newtype per index space |
//! | [`index_vec`] | `IndexVec<I, T>` — never a raw `usize` index |
//!
//! # What deliberately does NOT live here
//!
//! Frontend vocabulary. This crate is general infrastructure that back-end
//! crates depend on too, so stage-specific index spaces (AST node ids, HIR
//! ids, UI-tree ids) are defined by the stage that owns them. A stage that
//! needs an index space defines its own newtype; it does not reuse an existing
//! one because the integer happens to fit.
//!
//! Note `yelc-syntax` defines its own `NodeId` for AST nodes — a different
//! index space from `HirId`.
//!
//! # Policy this crate carries
//!
//! - **Accumulate and continue.** Push to [`diagnostic::Diagnostics`] and keep
//!   going; recover and lower the rest of the program. Never early-return on the
//!   first user error.
//! - **No silent fallbacks.** Unimplemented paths are `todo!("…")` or a typed
//!   error — never placeholder values.
//! - **Determinism.** `FxHashMap`/`FxHashSet` only; std `HashMap`/`HashSet` are
//!   denied by the workspace `clippy.toml`. Sort anything map-derived before it
//!   reaches output.

pub mod diagnostic;
pub mod ids;
pub mod index_vec;
pub mod interner;
pub mod source;

pub use diagnostic::{Diagnostic, Diagnostics, ErrorCode, Severity};
pub use ids::{DefId, ExprId, FieldIdx, LocalId, ParamIdx, VariantIdx};
pub use index_vec::{Idx, IndexVec};
pub use interner::{ArcStr, Name, NameInterner};
pub use source::{Source, SourceId, SourceMap, Span};
