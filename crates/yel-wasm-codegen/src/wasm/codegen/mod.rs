//! Core WASM module generation, organized by responsibility.
//!
//! Originally a single ~7k line file (`core_module.rs`). Split here into
//! focused submodules along section / responsibility lines. Every
//! submodule operates on `WasmPackageBuilder<'a>` via additional `impl`
//! blocks.
//!
//! Submodule overview:
//! - `constants` — module-level numeric constants (handler-id encoding).
//! - `scratch`   — small numeric helpers + free functions (mem_arg,
//!   slot_local, per-valtype slot bookkeeping).
//! - `accessors` — getter/setter generation + value-coercion helpers.
//! - `name_section` — name-section emission (debug names).
//! - `op_emit`   — the giant `LirOp` match.
//! - `record_list` — record/list/filter constructor function emission +
//!   `extract_signal_reads`.
//! - `build`     — the rest: top-level orchestrator (`build_core_module`),
//!   constructor / mount / unmount / dispatch / block_fn,
//!   signal-store/read helpers, registry helpers, effect
//!   fan-out, globals_init.

pub(super) mod accessors;
pub(super) mod block_fn;
pub(super) mod build;
pub(super) mod constants;
pub(super) mod dispatch;
pub(super) mod lifecycle;
pub(super) mod name_section;
pub(super) mod op_emit;
pub(super) mod record_list;
pub(super) mod scratch;
pub(super) mod signal_emit;

// Re-export helpers used by sibling modules outside `codegen` (e.g.
// `super::expr` references `mem_arg` and `slot_local`).
pub(super) use scratch::{mem_arg, slot_local};
