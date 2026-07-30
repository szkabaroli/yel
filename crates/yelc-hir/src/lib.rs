//! `yelc-hir` — one IR, built in stage 3 and checked in stage 4.
//!
//! Replaces the frozen `yel-core/src/hir/` (1,995 lines) and, in stage 4,
//! `yel-core/src/thir/`. THIR is not a second IR: it merged into this one on
//! 2026-07-28, as a second phase over the same nodes
//! (`plans/rewrite/seam-changes.md`).
//!
//! | phase | does | produces |
//! |---|---|---|
//! | **3** | AST → HIR; register items; resolve names; collect declared types; desugar the UI tree to functions and calls | HIR, [`Definitions`](yelc_sema::Definitions) typed |
//! | **4** | bidirectional type checking over the same nodes | [`HirModule::types`] total |
//!
//! # SEAM. This file is the contract stage 3's lowering is written against.
//!
//! It landed **before the lowering body**, deliberately: a seam discovered while
//! writing the body gets shaped by the body's convenience. A change is a request
//! in `plans/rewrite/seam-changes.md`, not an edit.
//!
//! # What this crate does not contain yet
//!
//! - **The lowering.** [`lower_files`] is a signature with a `todo!()` body.
//! - **The node vocabulary.** [`HirItem`] and [`HirBody`] are uninhabited, which
//!   is the honest spelling of *"phase 3 declares these"* — an empty enum cannot
//!   be constructed, so there is no placeholder variant to become permanent.
//! - **`type_of`.** ⚠️ **The one seam type that could not be landed**, and now the
//!   gate on phase 3. Three things about it are unresolved and all three are
//!   contract:
//!
//!   1. The brief writes `pub fn type_of(&mut self, ty: TypeId) -> Ty` — `&mut
//!      self` names no receiver, and no type in the brief owns it.
//!   2. Its memo is specified as a [`NodeMap<Ty>`], which keys [`HirId`]; a
//!      [`TypeId`] is not one. The two declarations sit twenty lines apart in the
//!      same contract block and contradict each other.
//!   3. The definition of done requires it *"structurally unreachable from H1
//!      phase 1 (the collector does not exist yet)"* — a statement about a type
//!      that is never named.
//!
//!   Landing it under a guess would have made the guess the contract. Naming its
//!   owner closes all three at once: the receiver is that type, and the memo is a
//!   field on it keyed by `TypeId`.
//!
//! # Two decisions this crate's shape already encodes
//!
//! | | | where |
//! |---|---|---|
//! | analysis results live **beside** nodes, never on them | [B3](../../../plans/rewrite/anti-spec.md) | [`NodeMap`] |
//! | a HIR node points back at **which file's** AST node | [D8](../../../plans/rewrite/stage-3-hir-build.md) + stage 1's per-file ids | [`SourceNodeId`] |
//!
//! The second is a correction to the brief, found by landing it — ark's
//! `hir_map.rs` keys the reverse map by a bare `NodeId` and is right to, because
//! ark allocates them from a process-global counter. `yelc-syntax` allocates per
//! file from zero, on purpose. See [`SourceNodeId`].

pub mod ids;
pub mod map;
pub mod module;

pub use ids::{BodyId, HirId, HirItemId, SourceNodeId, TypeId};
pub use map::HirMap;
pub use module::{HirBody, HirItem, HirModule};
pub use node_map::NodeMap;

mod node_map;

use yelc_sema::CompilerContext;
use yelc_syntax::ParsedFile;

/// Build the HIR for one package.
///
/// # Three phases, each sweeping every file before the next begins
///
/// Invariant H1 (`plans/rewrite/stage-3-hir-build.md`). This is why the whole
/// file set arrives at once rather than one file at a time:
///
/// | phase | does, across all files | may not |
/// |---|---|---|
/// | 1 · register | a `DefId` + name for every item | resolve a type — no name is guaranteed to exist yet |
/// | 2 · collect | resolve every **declared** type into the definition tables | look at any body |
/// | 3 · lower | lower bodies, desugar the UI tree | register new items |
///
/// A body may reference any item regardless of source order, **and so may a
/// declared type, and so may either across file boundaries.** The frozen driver
/// merges fully-lowered files inside a loop, so cross-file references resolve in
/// one direction only; phase-major sweeping is what fixes that.
///
/// # Postcondition
///
/// [`HirModule::types`] is **empty**. Declared types are in
/// [`CompilerContext::defs`]; expression types are stage 4's.
///
/// Errors accumulate in [`CompilerContext::diagnostics`] and lowering continues —
/// there is no `Result` here on purpose
/// (`plans/rewrite/keep-list.md`, accumulate-and-continue).
pub fn lower_files(parsed: &[ParsedFile], ctx: &mut CompilerContext) -> HirModule {
    let _ = (parsed, ctx);
    todo!("phase 3: AST → HIR (register, collect, lower) — see plans/rewrite/stage-3-hir-build.md")
}
