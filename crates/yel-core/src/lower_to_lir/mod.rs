//! THIR → LIR lowering.
//!
//! This is the yel-lang frontend's bridge into LIR: it consumes typed
//! HIR (`ThirComponent`, `ThirExpr`) and produces ready-for-codegen
//! `LirResource`s. Lives outside `crate::lir` because LIR itself is a
//! frontend-neutral target — flow-core targets LIR directly without
//! going through THIR, and a future direct emitter would too.
//!
//! Public entry points stay re-exported from `crate::lir` for
//! backwards compatibility (`yel_core::lir::lower_component`, etc.).
//! The split is purely about *layering hygiene*: anything inside
//! `crate::lir/` may only reach into neutral foundations (`ops`,
//! `ids`, `types`, `interner`, `source`, `definitions`, `context`).
//! Anything inside `crate::lower_to_lir/` may consume HIR/THIR
//! freely — that's its job.

pub(crate) mod blocks;
mod component;
pub(crate) mod lifecycle_inline;

pub use blocks::{
    is_scalar_list_ty_struct, resolve_global_triggers, synth_globals_init_block,
    ty_to_slot_val_type,
};
pub use component::{lower_component, lower_globals};
