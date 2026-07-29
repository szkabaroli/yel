//! `yelc-sema` — shared semantic infrastructure for the rewritten pipeline.
//!
//! Phase 1 of [stage 3](../../../plans/rewrite/stage-3-hir-build.md). It
//! transforms no IR, which is why it has no stage number of its own; it holds
//! the state every later phase threads.
//!
//! Replaces, in part: `yel-core/src/{context.rs, definitions.rs, known.rs,
//! stdlib_lookup.rs, types/}` — ~3,536 lines, read as specification and written
//! fresh ([README § Read the frozen tree; do not port it](../../../plans/rewrite/README.md)).
//!
//! # SEAM. Frozen for stages 3 and 4.
//!
//! Everything public here is contract. A change is a request in
//! `plans/rewrite/seam-changes.md`, not an edit.
//!
//! # The decisions this crate encodes
//!
//! Recorded in `plans/rewrite/open-decisions.md` and
//! `stage-3-hir-build.md` § Decisions. Listed here because the *code* is where
//! they get quietly reversed.
//!
//! | | decision | where |
//! |---|---|---|
//! | A1 | generics are monomorphized **by type** | builtin table (pending) |
//! | A3 | `Ty` gains [`TyKind::Param`] | [`types`] |
//! | A4 | `Ty` gains [`TyKind::Infer`] | [`types`] |
//! | B1 | `Ty` **must not** derive `Serialize` | [`types`] |
//! | B2 | [`DefId`] is module-qualified from day one | [`ids`] |
//! | B3 | one [`OverloadKey`], two consumers | [`ids`] |
//! | C1 | builtins are **one table**, two accessors | pending |
//! | C1c | arity has a **variadic** form | pending |
//! | C2 | builtin elements/enums/variants get a separate home, holding `DefId` not `Option<DefId>` | pending |
//! | D0 | the context holds **six** fields | pending |
//!
//! # What is deliberately absent
//!
//! `block_id_counter`, `block_names`, `component_lifecycle_blocks` and the
//! fanout table live on the frozen `CompilerContext`. They are `yelc-lir` types,
//! `sema → lir` is forbidden by the crate graph, and so they **cannot compile
//! here** — the boundary is enforced rather than remembered.
//!
//! Reactivity is also absent, and stays absent:
//! [anti-spec C1](../../../plans/rewrite/anti-spec.md) forbids `signal` and
//! `effect` below the frontend seam, and `signal_deps` belongs to `yelc-hir`
//! (decision D0a) because it is analysis about a program, not infrastructure.

pub mod ids;
pub mod types;

pub use ids::{DefId, DefPath, ModuleId, OverloadKey};
pub use types::{Ty, TyKind, TypeInterner};
