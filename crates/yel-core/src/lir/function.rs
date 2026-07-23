//! Functions in the LIR: identity, calling convention, and the export
//! shape that flows out of them.
//!
//! ## What's a function, in this LIR?
//!
//! Any unit that compiles to one wasm function: yel-lang component
//! mount/unmount/handler/update blocks, yel-lang free functions and
//! component methods (when added), and flow-frontend top-level
//! functions. They differ along three orthogonal axes:
//!
//! 1. **Identity** — addressed by [`DefId`] (cross-module callable)
//!    or by [`BlockId`] (component-internal).
//! 2. **Calling convention** — what implicit params (self-ref,
//!    boundary refs, legacy DOM parent) sit *before* the user-declared
//!    params in the wasm signature, and what's returned.
//! 3. **Body shape** — the op stream + slot/expression arenas. Read
//!    through the [`crate::lir::arena::LirFunctionLike`] /
//!    [`crate::lir::arena::LirSlotArena`] /
//!    [`crate::lir::arena::LirExprArena`] traits — those vary per
//!    backing type (`LirBlock` reuses its component's arenas;
//!    `FlowFunc` owns its own).
//!
//! This module covers axes 1 and 2. The traits handle axis 3.
//!
//! Codegen consults this data twice:
//!
//! * **Type-section emission**: `CallingConv` → wasm function type.
//! * **WIT export**: `(is_export, role, conv)` → `ExportShape`.
//!
//! Body emission stays in the traits — same machinery for every kind.

use serde::{Deserialize, Serialize};

use crate::ids::{BlockId, DefId, TreeBoundaryId};
use crate::lir::block::{LirBlock, LirSlotInfo, LirSlotValType, StringId};

/// One implicit wasm parameter prepended before the user-declared
/// params of a function. Order matters — it's the wasm-param-index
/// order in which they sit at positions `0..implicit.len()`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ImplicitParam {
    /// `(ref null $Comp_<def>)`. Present on every UI block (the
    /// component instance the block runs against) and on yel-lang
    /// component methods. Absent on free functions (yel-lang globals,
    /// flow functions).
    SelfRef(DefId),

    /// `(ref null <boundary-struct>)`. UI update blocks declare these
    /// when they take a tree-boundary param (e.g. an `if`-branch
    /// boundary's typed struct). Order is significant — matches the
    /// existing `LirBlock::boundary_param_slots` order.
    Boundary(TreeBoundaryId),

    /// Untyped `i32`. The legacy DOM-parent-handle convention used by
    /// blocks predating the typed-boundary migration. Distinct from
    /// `Boundary` so codegen knows whether to emit a typed `ref` or
    /// a raw `i32`.
    LegacyI32,

    /// `i32` canonical-ABI resource handle. Present on WIT resource
    /// instance methods as the implicit leading `self` param.
    /// Codegen materialises this as a raw `i32` (resource handles
    /// cross the component-model boundary as opaque integers),
    /// distinct from `SelfRef(DefId)` which materialises as a typed
    /// `(ref null $Comp_<def>)` for yel-UI components.
    ///
    /// The `DefId` tags which resource the method belongs to so
    /// `LirFunction::export_shape` can return
    /// `ExportShape::ResourceMethod { resource: d }`.
    ResourceSelf(DefId),
}

/// How the wasm function is invoked. Drives type-section emission +
/// prologue handling. Two functions with identical `LirFunctionLike`
/// bodies but different `CallingConv` are different wasm functions.
///
/// The wasm parameter order is:
///
/// ```text
///     [implicit_pre...] [user params...] [implicit_post...]
/// ```
///
/// Two separate lists (rather than a single ordered one) because the
/// UI block convention sandwiches the user-declared params between
/// the leading `(ref $Comp)` self-ref and the trailing typed boundary
/// refs. Free functions and flow functions leave both lists empty.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CallingConv {
    /// Implicit params prepended before the user params. Typically
    /// just `[SelfRef(comp)]` for UI / method calling conventions;
    /// empty for free functions.
    pub implicit_pre: Vec<ImplicitParam>,

    /// Implicit params appended after the user params. Used by UI
    /// blocks for boundary-ref params and the legacy single-i32
    /// parent-DOM-handle fallback. Empty for free functions.
    pub implicit_post: Vec<ImplicitParam>,

    /// Wasm-level returns. Empty for void functions; a single entry
    /// is the common case. Held explicitly (rather than derived from
    /// the function's `return_slot`) because UI block conventions
    /// sometimes diverge — for-iter-mount blocks return `i32` (a DOM
    /// handle) even when their LIR-level `return_slot` carries a
    /// different yel-level type.
    pub returns: Vec<LirSlotValType>,
}

impl CallingConv {
    /// Builder: append to `implicit_pre`.
    pub fn with_pre(mut self, p: ImplicitParam) -> Self {
        self.implicit_pre.push(p);
        self
    }

    /// Builder: append to `implicit_post`.
    pub fn with_post(mut self, p: ImplicitParam) -> Self {
        self.implicit_post.push(p);
        self
    }
}

/// Who this function is — its identity space and contextual role.
/// Codegen uses this to populate the two function-index maps
/// (`DefId → wasm idx` for free functions / methods, `(comp, BlockId)
/// → wasm idx` for internals) and to drive WIT export decisions.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FunctionRole {
    /// Cross-module callable. `LirOp::CallFunction { func: DefId }`
    /// resolves through these. Yel-lang free functions, yel-lang
    /// component methods, and flow functions all fall here — the
    /// only difference between them is whether `CallingConv.implicit`
    /// carries a `SelfRef`.
    FreeFunction {
        def_id: DefId,
        /// WIT-exported? Drives whether this appears in the produced
        /// world / interface.
        is_export: bool,
    },

    /// Component-internal. Addressed by `BlockId` within a component
    /// scope. Mount, update, handler, iter-body, if-branch and the
    /// like all live here. Never WIT-exported on their own; the
    /// owning component is exported as a resource and its lifecycle
    /// methods are emitted by the component-model encoder.
    Internal {
        block_id: BlockId,
        component: DefId,
        purpose: InternalPurpose,
    },
}

/// What an internal block is for. Mostly a diagnostic / debugging aid
/// — codegen treats most variants identically (build a function with
/// the declared `CallingConv`, emit the body) — but the variant carries
/// the *reason*, which the WIT pass and the name section both want.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InternalPurpose {
    Constructor,
    Mount,
    Unmount,
    /// Update fan-out for a specific signal.
    Update {
        signal: DefId,
    },
    /// Event-handler body.
    Handler {
        event: StringId,
    },
    /// Body of a `for` iteration.
    IterBody,
    /// Mount body of an `if` branch (or `else-if`).
    IfBranch,
    /// Catch-all for less-classified blocks (legacy / migration).
    Other,
}

/// How a function projects into the WIT package. Derived from
/// `is_export + role + conv.implicit` — see
/// [`LirFunction::export_shape`].
#[derive(Debug, Clone)]
pub enum ExportShape {
    /// Not exported. Internal to the wasm module; no WIT entry.
    Internal,
    /// World/interface-level `func`. Yel free functions, flow
    /// functions.
    FreeFunction,
    /// `func` inside a `resource` block. The implicit `SelfRef(D)`
    /// is the resource's `self`.
    ResourceMethod {
        resource: DefId,
    },
}

/// Convenience aggregate: identity + signature shape + export flag.
/// Doesn't own the body — body access goes through
/// [`crate::lir::arena::LirFunctionLike`] on the backing
/// `LirBlock` / `FlowFunc`. This keeps `LirFunction` cheap to
/// construct (small fixed-size fields only) and lets two different
/// LIR shapes project into the same `LirFunction` view.
#[derive(Debug, Clone)]
pub struct LirFunction {
    pub role: FunctionRole,
    pub conv: CallingConv,
}

/// Build the full UI-block calling convention from a `LirBlock`'s
/// fields plus its enclosing component context. The block alone
/// can't know the component's `DefId` (that lives on `LirResource`)
/// so callers thread it in.
///
/// Mirrors the conventions today's `block_fn.rs` prologue + the
/// type-section pass enforce:
///
/// 1. Implicit `SelfRef(component)` at param 0.
/// 2. Each typed boundary param slot, in declared order.
/// 3. If neither typed params nor boundary refs are present, a
///    `LegacyI32` fallback (the parent DOM handle).
/// 4. Returns `[I32]` when the block has a `return_slot` (UI's DOM-
///    handle convention), `[]` otherwise.
///
/// This is the canonical place to encode UI conventions — codegen's
/// type-section pass + prologue consume it instead of re-deriving.
/// `slots` is the component's slot table, used to read each
/// boundary param's typed identity (`val_ty: RefNullForBoundary(b)`).
pub fn ui_block_calling_conv(
    block: &LirBlock,
    component: DefId,
    slots: &[LirSlotInfo],
) -> CallingConv {
    // Phase 0.3o: a block carries a leading wasm self-ref iff its
    // `implicit_self` field is set. Host export wrappers and flow
    // free functions leave `implicit_self: None` and so produce a
    // conv with no leading `SelfRef`.
    let implicit_pre = if block.implicit_self.is_some() {
        vec![ImplicitParam::SelfRef(component)]
    } else {
        Vec::new()
    };

    // Post-user implicit: typed boundary refs (in declared order),
    // then the legacy single-i32 parent-DOM-handle fallback when
    // nothing else is present at all.
    let mut implicit_post = Vec::new();
    for slot_id in &block.boundary_param_slots {
        // Task #105 B2: dispatch on the slot-id variant — Block-variant
        // ids index the block's own slots vec.
        let info = match slot_id {
            super::block::LirSlotId::Block { idx, .. } => block.slots.get(*idx as usize),
            super::block::LirSlotId::Resource { idx } => slots.get(*idx as usize),
        };
        if let Some(info) = info
            && let LirSlotValType::RefNullForBoundary(b_id) = info.val_ty {
                implicit_post.push(ImplicitParam::Boundary(b_id));
            }
    }
    if block.params.is_empty() && block.boundary_param_slots.is_empty() {
        implicit_post.push(ImplicitParam::LegacyI32);
    }

    // UI blocks return i32 (DOM handle) when they declare a
    // return_slot. The return slot's actual yel-level type is
    // available on `slots` but ignored — codegen's calling
    // convention here is the i32-handle convention regardless.
    let returns = if block.return_slot.is_some() {
        vec![LirSlotValType::I32]
    } else {
        Vec::new()
    };
    CallingConv {
        implicit_pre,
        implicit_post,
        returns,
    }
}

impl LirFunction {
    /// Resolve the WIT export shape from the function's role +
    /// convention. The mapping is:
    ///
    /// | role / conv                               | shape           |
    /// |-------------------------------------------|-----------------|
    /// | `Internal { .. }`                         | `Internal`      |
    /// | `FreeFunction { is_export: false }`       | `Internal`      |
    /// | `FreeFunction`, no implicit               | `FreeFunction`  |
    /// | `FreeFunction`, implicit `[SelfRef(D)]`   | `ResourceMethod`|
    /// | anything else                             | `Internal`      |
    ///   (e.g. `[Boundary, ..]` — not exportable)
    pub fn export_shape(&self) -> ExportShape {
        match &self.role {
            FunctionRole::Internal { .. } => ExportShape::Internal,
            FunctionRole::FreeFunction { is_export: false, .. } => ExportShape::Internal,
            FunctionRole::FreeFunction { .. } => {
                // WIT-exportable shapes look only at `implicit_pre`:
                // anything in `implicit_post` (boundary refs, legacy
                // i32) is UI-machinery that can't be encoded as a WIT
                // surface.
                if !self.conv.implicit_post.is_empty() {
                    return ExportShape::Internal;
                }
                match self.conv.implicit_pre.as_slice() {
                    [] => ExportShape::FreeFunction,
                    [ImplicitParam::SelfRef(d)] => {
                        ExportShape::ResourceMethod { resource: *d }
                    }
                    [ImplicitParam::ResourceSelf(d)] => {
                        // Canonical-ABI variant — `i32` handle rather than
                        // typed component ref. Same export shape from the
                        // WIT pass's POV.
                        ExportShape::ResourceMethod { resource: *d }
                    }
                    _ => ExportShape::Internal,
                }
            }
        }
    }

    /// `true` when the function has a wasm-level self-ref at param 0.
    /// Convenience for callers that don't need to inspect `implicit_pre`
    /// further.
    pub fn has_self_ref(&self) -> bool {
        matches!(
            self.conv.implicit_pre.first(),
            Some(ImplicitParam::SelfRef(_))
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ids::{BlockId, DefId};

    fn def(n: u32) -> DefId {
        DefId(n)
    }

    /// Free function with no implicit params → world-level WIT `func`.
    #[test]
    fn free_function_exports_at_world_level() {
        let f = LirFunction {
            role: FunctionRole::FreeFunction {
                def_id: def(7),
                is_export: true,
            },
            conv: CallingConv::default(),
        };
        assert!(matches!(f.export_shape(), ExportShape::FreeFunction));
        assert!(!f.has_self_ref());
    }

    /// Same function but `is_export: false` → internal.
    #[test]
    fn non_exported_free_function_stays_internal() {
        let f = LirFunction {
            role: FunctionRole::FreeFunction {
                def_id: def(7),
                is_export: false,
            },
            conv: CallingConv::default(),
        };
        assert!(matches!(f.export_shape(), ExportShape::Internal));
    }

    /// Free function with leading `SelfRef(C)` → method on resource C.
    #[test]
    fn self_ref_makes_resource_method() {
        let comp = def(3);
        let f = LirFunction {
            role: FunctionRole::FreeFunction {
                def_id: def(7),
                is_export: true,
            },
            conv: CallingConv::default().with_pre(ImplicitParam::SelfRef(comp)),
        };
        match f.export_shape() {
            ExportShape::ResourceMethod { resource } => assert_eq!(resource, comp),
            other => panic!("expected ResourceMethod, got {other:?}"),
        }
        assert!(f.has_self_ref());
    }

    /// Post-user implicit refs (boundary refs, legacy i32) block WIT
    /// export even when the pre-user side is a clean SelfRef. They're
    /// UI internal-machinery params with no WIT representation.
    #[test]
    fn post_user_refs_block_wit_export() {
        let f = LirFunction {
            role: FunctionRole::FreeFunction {
                def_id: def(7),
                is_export: true,
            },
            conv: CallingConv::default()
                .with_pre(ImplicitParam::SelfRef(def(3)))
                .with_post(ImplicitParam::Boundary(crate::ids::TreeBoundaryId(1))),
        };
        assert!(matches!(f.export_shape(), ExportShape::Internal));
    }

    /// Internal blocks are never exported regardless of conv.
    #[test]
    fn internal_blocks_are_never_exported() {
        let f = LirFunction {
            role: FunctionRole::Internal {
                block_id: BlockId(5),
                component: def(2),
                purpose: InternalPurpose::Mount,
            },
            conv: CallingConv::default().with_pre(ImplicitParam::SelfRef(def(2))),
        };
        assert!(matches!(f.export_shape(), ExportShape::Internal));
    }
}
