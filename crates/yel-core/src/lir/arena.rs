//! Arena traits — abstract over *who owns* the interned LIR expression
//! and string tables.
//!
//! Background. LIR was born inside the UI compiler, where every block
//! lives inside a [`crate::lir::node::LirResource`] (for the UI frontend,
//! one component) and every `LirExprId` / `StringId` indexes into that
//! resource's tables. Code-gen reaches for `&LirResource` whenever it has
//! to dereference an id — directly into `resource.exprs[id]` and
//! `resource.strings[id]`.
//!
//! That coupling is fine for the UI frontend but blocks new callers
//! (notably the flow-graph frontend) from re-using the same body emitter
//! when their interning lives elsewhere — e.g. per-function on a
//! `FlowFunc`.
//!
//! This trait is the smallest abstraction that lets such a caller plug
//! in: implement `expr` (and optionally `strings`) however you store
//! them, and the same code-gen helpers can read through the trait.
//!
//! Two traits because not every arena owns strings — flow functions
//! don't carry any, and forcing them to stub a panicking impl would be
//! a silent fallback at runtime. Keeping them split makes the
//! "this caller has no strings" case representable in the type system.

use rustc_hash::FxHashMap as HashMap;

use crate::ids::{BlockId, DefId, LocalId};
use crate::interner::Name;

use super::block::{
    LirExprId, LirBindingMode, LirBlock, LirOp, LirSlotId, LirSlotInfo, StringId,
};
use super::expr::LirExpr;
use super::function::{CallingConv, FunctionRole};
use super::signal::LirSignal;
use super::struct_types::{LirArrayTypeDecl, LirStructTypeDecl};

/// Read-only access to a LIR expression table.
///
/// `id` is interpreted *with respect to the implementor* — two
/// different arenas may both hand out the integer `5` and they refer
/// to different expressions. The arena is the scope.
pub trait LirExprArena {
    /// The whole expression arena. `LirExprId`s index into it.
    fn exprs(&self) -> &[LirExpr];

    /// Fetch one expression by id. Defaults to indexing [`Self::exprs`].
    fn expr(&self, id: LirExprId) -> &LirExpr {
        &self.exprs()[id.0 as usize]
    }
}

/// Read-only access to an interned-string table. Implementors are
/// arenas that carry static-ish text (tag names, attribute keys, etc.);
/// flow-style functions that don't need any text won't implement this.
pub trait LirStringArena {
    fn string(&self, id: StringId) -> &str;
}

/// Read-only access to a slot-info table. `LirSlotId`s are interpreted
/// against the implementor — the UI frontend owns Resource-variant slots
/// on `LirResource` (Block-variant slots live per-`LirBlock`); flow
/// functions own them per-`FlowFunc`. The previous code-gen path read
/// `&LirResource` everywhere it needed a slot; routing through this
/// trait lets non-UI callers (flow) reuse the same helpers.
pub trait LirSlotArena {
    fn slots(&self) -> &[LirSlotInfo];
}

/// Function-shaped object that the wasm body emitter can consume.
///
/// Two implementors today:
/// * [`crate::lir::block::LirBlock`] — UI's "block" abstraction
///   (carries the implicit `(ref $Comp)` self-ref by convention).
/// * `yel_flow_core::flow_ir::FlowFunc` — a top-level flow function
///   (no self-ref, arbitrary return type).
///
/// Most fields default to empty / `false` so flow's impl only has to
/// fill in what it actually has.
pub trait LirFunctionLike {
    /// Slot ids holding the function's WASM-level value parameters,
    /// in declared order. For UI blocks this excludes the implicit
    /// self-ref param 0 (declared via `has_self_ref_param`).
    fn params(&self) -> &[LirSlotId];

    /// Slot whose value is the function's single-value return, if any.
    fn return_slot(&self) -> Option<LirSlotId>;

    /// Top-level op stream emitted into the function body.
    fn ops(&self) -> &[LirOp];

    /// Pre-validated per-valtype scratch local counts required by the
    /// body's flat-slot signal stores. `(i32, i64, f32, f64)`. Zero
    /// for flow functions (no signals).
    fn max_flat_scratch_counts(&self) -> (u32, u32, u32, u32) {
        (0, 0, 0, 0)
    }

    /// Captured-local bindings used by for-loop body blocks. Empty
    /// outside that UI lowering.
    fn captured_locals(&self) -> &HashMap<LocalId, LirSlotId>;

    /// Locals loaded from memory at block entry. Empty outside UI.
    fn local_to_slot(&self) -> &HashMap<LocalId, LirSlotId>;

    /// Per-LocalId binding-mode override (Ptr vs Value). Empty
    /// outside UI.
    fn local_modes(&self) -> &HashMap<LocalId, LirBindingMode>;

    /// Trailing typed boundary-ref params (UI only). Empty for flow.
    fn boundary_param_slots(&self) -> &[LirSlotId];

    /// Distinct child components reachable via `MountComponent`.
    /// Empty for flow.
    fn mount_component_children(&self) -> &[DefId];

    /// Whether this function's calling convention prepends an
    /// implicit `(ref null $Comp)` as wasm param 0. `true` for UI
    /// blocks (their bodies need `local.get 0` to reach the
    /// component instance); `false` for flow functions, which take
    /// only their declared params.
    ///
    /// Kept on the trait as a fast-path so callers that *only* need
    /// "does this prepend a self-ref?" don't have to materialise a
    /// full `CallingConv`. Implementations should keep this
    /// consistent with `calling_conv()`'s first implicit param.
    fn has_self_ref_param(&self) -> bool {
        true
    }

    /// The wasm-level invocation shape for this function. Drives
    /// type-section emission + prologue param-copy. Defaults to a
    /// UI-style legacy convention `(ref $Comp) -> ()` so the existing
    /// `LirBlock` impl doesn't have to override unless it has typed
    /// params, boundary refs, or a return.
    ///
    /// Concrete impls should construct this once and return a
    /// reference; recomputing on every call is fine for now (the
    /// struct is small) but the codegen pipeline will only call this
    /// twice per function (type registration + prologue) so even
    /// owned-return semantics would be acceptable.
    fn calling_conv(&self) -> CallingConv {
        CallingConv::default()
    }

    /// Who this function is — its identity space and contextual role.
    /// Codegen uses this to drive index-map population (DefId-keyed
    /// vs BlockId-keyed) and WIT export decisions. The default is
    /// `None` so existing `LirBlock` callers (which don't yet supply
    /// a role) keep compiling; codegen interprets `None` as "treat
    /// like a legacy UI block, no WIT export."
    fn role(&self) -> Option<FunctionRole> {
        None
    }
}

/// Module-level view of a LIR resource — every neutral accessor the
/// wasm code generator needs to consume a multi-block compilation unit
/// that owns slots/exprs/strings/GC-type tables and has an identity +
/// export flag. For the UI frontend a resource is a component; for the
/// flow frontend it is a function package.
///
/// `LirResource` implements this directly. The flow frontend's
/// per-function adapter (one-block resource packaging) also
/// implements it. Codegen reads through the trait so the
/// `Vec<LirResource>` shape isn't baked in.
///
/// UI-only metadata (signals / effects / mount block / DOM bookkeeping)
/// is **not** part of this trait. Those fields are still read by UI
/// codegen during the transitional phases; they disappear as
/// THIR→LIR lowers them inline to plain LirOps. Once the UI fields
/// are gone, this trait is the full read API and nothing reaches for
/// concrete `LirResource` outside the lowering pass.
pub trait LirResourceArena: LirExprArena + LirStringArena + LirSlotArena {
    /// Stable identity of this resource. Used by codegen for
    /// export-name resolution and for `LirOp::CallFunction` target
    /// lookups across the module.
    fn def_id(&self) -> DefId;

    /// Interned name (resolves to the user-facing identifier via the
    /// compiler context's string table).
    fn name(&self) -> Name;

    /// Whether this resource is part of the WIT export surface.
    /// World-level free functions / exported UI components → `true`.
    fn is_export(&self) -> bool;

    /// Every basic block belonging to this resource. `BlockId(n)`
    /// indexes into the returned slice — codegen relies on this
    /// ordering for `LirOp::CallBlock` resolution.
    fn blocks(&self) -> &[LirBlock];

    /// Convenience: fetch a block by id. After structural dedupe a block's
    /// `BlockId.0` is no longer guaranteed to equal its index in `blocks()`
    /// (duplicate blocks are spliced out and their `CallBlock`s rewritten to
    /// a canonical survivor), so this checks the fast-path index and falls
    /// back to a linear scan — mirroring `LirResource::get_block`.
    fn block(&self, id: BlockId) -> &LirBlock {
        let blocks = self.blocks();
        let idx = id.0 as usize;
        if let Some(b) = blocks.get(idx)
            && b.id == id
        {
            return b;
        }
        blocks
            .iter()
            .find(|b| b.id == id)
            .unwrap_or_else(|| panic!("block {:?} not found in resource", id))
    }

    /// GC struct types declared by this resource (record / variant /
    /// component-instance layouts that the wasm GC type section
    /// emits up-front).
    fn struct_types(&self) -> &[LirStructTypeDecl];

    /// GC array types declared (typed lists, children arrays, etc.).
    fn array_types(&self) -> &[LirArrayTypeDecl];

    /// Reactive signals owned by this resource (UI component properties).
    /// **Transitional:** signals are UI metadata that codegen still reads by
    /// index during the migration (see this trait's header) — the emit path
    /// resolves a `SignalRead`/`Def` of a component-local signal through this.
    /// Defaults to empty so non-UI arenas (flow functions, module-scope
    /// expression scopes) that own no signals don't have to stub it; those
    /// scopes only ever reference globals, which resolve without this table.
    fn signals(&self) -> &[LirSignal] {
        &[]
    }
}
