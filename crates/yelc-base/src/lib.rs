//! `yelc-base` — shared compiler infrastructure for the rewritten pipeline.
//!
//! This crate is the **keep-list** made concrete: the parts of the frozen
//! compiler that were already better than what a rewrite would produce, carried
//! over intact. See `plans/rewrite/keep-list.md`.
//!
//! # These files are copies, not a dependency
//!
//! `yelc-base` deliberately does **not** depend on `yel-core`. The frozen tree
//! and the new tree share nothing mutable: a shared dependency is an edge along
//! which the freeze eventually breaks, and the frozen tree is the differential
//! baseline that must behave identically in week 1 and week 20.
//!
//! # What lives here
//!
//! | Module | Carried over from | Why it stays |
//! |---|---|---|
//! | [`diagnostic`] | `yel-core/src/diagnostic.rs` | Builder API, real `ErrorCode` enum, accumulating sink, span-aware rendering |
//! | [`source`] | `yel-core/src/source.rs` | `SourceMap`, `Span`, byte offsets, snippet rendering |
//! | [`interner`] | `yel-core/src/interner.rs` | `Interner`/`Name` — no `String` survives past lowering |
//! | [`ids`] | `yel-core/src/ids.rs` | Typed `u32` newtype per index space |
//! | [`index_vec`] | `yel-core/src/index_vec.rs` | `IndexVec<I, T>` — never a raw `usize` index |
//!
//! # What deliberately did NOT come over
//!
//! The UI-specific ids — `NodeId` (UI tree nodes), `BlockId`, `ForId`, `IfId`,
//! `TreeBoundaryId` — are frontend vocabulary, not general infrastructure. They
//! must not be visible to `yelc-lir` or `yelc-codegen`, which depend on this
//! crate. A stage that needs an index space defines its own newtype; it does not
//! reuse an existing one because the integer happens to fit.
//!
//! Note `yelc-syntax` defines its own `NodeId` for AST nodes. It is a different
//! index space from the frozen tree's UI-node `NodeId`, and from `HirId`.
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
pub use ids::{DefId, ExprId, FieldIdx, InterfaceId, LocalId, ParamIdx, VariantIdx};
pub use index_vec::{Idx, IndexVec};
pub use interner::{ArcStr, Interner, Name};
pub use source::{Source, SourceId, SourceMap, Span};
