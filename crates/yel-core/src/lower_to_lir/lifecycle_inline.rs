//! Inline expansion of UI lifecycle LirOps (`MountComponent`,
//! `ResourceNew`) to neutral op sequences. Phase 3.3 of the
//! LIR-flattening refactor.
//!
//! # Status: neutralized
//!
//! `lower_mount_component` now emits the full neutral-op sequence the
//! old `LirOp::MountComponent` codegen arm produced. The legacy
//! `LirOp::MountComponent` enum variant and its codegen arm have been
//! deleted alongside the dead `LirOp::ResourceNew` variant (never
//! emitted from yel-core lowering — see the Phase 3.1 report on
//! `blocks.rs:2010` for the codegen-internal `i32.const <base_addr>`
//! path inside `emit_internal_ctor`, which is now inlined into that
//! single emit site rather than going through a `LirOp`).
//!
//! # The expansion
//!
//! ```text
//! 1. CallBlock { block: <child's internal_constructor_block>,
//!                 args: [], result: Some(child_ref) }
//!    → pushes (ref null $Comp_<child>) into child_ref
//! 2. RegistryAlloc { component: child, ref_slot: child_ref,
//!                    idx_scratch, arr_scratch,
//!                    result_handle: handle }
//!    → allocates a registry slot for the child and yields an i32 handle
//! 3. GlobalSet { gref: CurrentHandle(child), value: handle }
//!    → stashes the handle in the child's `current_handle_global` so the
//!      child's mount-internal emissions read the right value
//! 4. StructSetSym { ty_ref: ComponentStruct, field: parent_retention_base + cursor,
//!                   rec: parent_self_ref, value: child_ref }
//!    → parent retention: writes the child ref into the surrounding
//!      component's `$Comp_<i>` retention region so GC tracing keeps
//!      the child alive. Emitted unconditionally — every mount site has
//!      a parent-retention field reserved by
//!      `compute_mount_retention_counts`. (Skipped only when the parent
//!      has no retention base, which the old emit arm also tolerated.)
//! 5. CallBlock { block: <child's mount_block>,
//!                 args: [child_ref, parent_dom_id],
//!                 result: children_root }
//!    → invokes the child's internal mount block; container children
//!      return their children-root i32 into `children_root`.
//! ```
//!
//! The codegen-time `next_mount_retention_target` cursor is gone — the
//! retention cursor is now driven at lowering time (per-block scope, in
//! emission order), which matches the legacy codegen ordering 1:1.

use crate::ids::DefId;
use crate::lir::block::{
    LirGlobalRef, LirOp, LirSlotId, LirSlotValType, LirTypeRef,
};

use super::blocks::BlockLowering;

impl<'a> BlockLowering<'a> {
    /// Lower a `MountComponent` site to the neutral op sequence the
    /// codegen `LirOp::MountComponent` emit arm used to produce.
    ///
    /// Parameters:
    /// - `child`: DefId of the component being mounted.
    /// - `parent_dom_id`: slot holding the parent DOM node id (i32).
    /// - `children_root`: optional slot to receive the
    ///   children-root DOM id returned by the child's mount-internal
    ///   (Some iff the target component has a `@children` slot).
    pub(crate) fn lower_mount_component(
        &mut self,
        child: DefId,
        parent_dom_id: LirSlotId,
        children_root: Option<LirSlotId>,
    ) {
        // 1. Allocate the typed child-ref scratch slot. This holds
        //    `(ref null $Comp_<child>)` between the ctor call and the
        //    matching mount-internal call / retention store.
        let child_ref = self.alloc_temp_slot_typed_named(
            LirSlotValType::RefNullForComponent(child),
            "mount_child_ref",
        );
        // Registry-alloc scratches: i32 index, (ref null
        // $handle-array). These mirror the locals the old
        // mount-component codegen arm reserved per-(child) site.
        let alloc_idx = self.alloc_temp_slot_named("mount_alloc_idx");
        let alloc_arr = self.alloc_temp_slot_typed_named(
            LirSlotValType::RefNullForSharedHandleArray,
            "mount_alloc_arr",
        );
        let handle = self.alloc_temp_slot_named("mount_child_handle");

        // 2. Call child's internal constructor. BlockIds are
        //    module-wide unique (Phase 0.3q), so we resolve the
        //    child's `internal_constructor_block` from the ctx-side
        //    lifecycle-block registry and emit a plain `CallBlock`
        //    that targets it directly.
        let child_blocks = self
            .ctx
            .lookup_component_lifecycle_blocks(child)
            .unwrap_or_else(|| {
                panic!(
                    "lower_mount_component: child component {:?} has no \
                     registered lifecycle blocks (must be lowered before parent)",
                    child
                )
            });
        let child_ctor_block = child_blocks
            .internal_constructor_block
            .unwrap_or_else(|| {
                panic!(
                    "lower_mount_component: child {:?} has no internal_constructor_block",
                    child
                )
            });
        self.emit(LirOp::CallBlock {
            block: child_ctor_block,
            args: Vec::new(),
            result: Some(child_ref),
        });

        // 3. Allocate registry handle for the child ref.
        self.emit(LirOp::RegistryAlloc {
            component: child,
            ref_slot: child_ref,
            idx_scratch: alloc_idx,
            arr_scratch: alloc_arr,
            result_handle: handle,
        });

        // 4. Stash the freshly-allocated handle in the child's
        //    `current_handle_global` so the child's mount-internal
        //    AddEventListener / handler-id emissions read the right
        //    value.
        self.emit(LirOp::GlobalSet {
            gref: LirGlobalRef::CurrentHandle(child),
            value: handle,
        });

        // 5. Parent-retention: write `child_ref` into the surrounding
        //    component's `$Comp_<i>` retention region. Field idx is
        //    `parent_retention_field_base + per-component cursor`. The
        //    cursor is per-component, monotonically incremented across
        //    all mount sites lowered within the component (matches the
        //    codegen-time `parent_retention_cursor` it replaces).
        //
        //    `rec` is the parent's self ref — every block this code is
        //    reachable from has `implicit_self` set at finalize time;
        //    until then we route through the component-wide
        //    `resource_self_ref_slot` allocated at the top of
        //    `lower_component`.
        if let Some(rec) = self.resource_self_ref_slot {
            let signal_field_count: u32 = self
                .signal_layout_early
                .signals
                .iter()
                .filter_map(|s| s.gc.map(|g| g.field_count))
                .sum();
            let cursor = self.parent_retention_cursor;
            self.parent_retention_cursor += 1;
            let field = signal_field_count + cursor;
            self.emit(LirOp::StructSetSym {
                ty_ref: LirTypeRef::ComponentStruct,
                field,
                rec,
                value: child_ref,
            });
        }

        // 6. Call child's internal mount block with explicit args.
        self.emit(LirOp::CallBlock {
            block: child_blocks.mount_block,
            args: vec![child_ref, parent_dom_id],
            result: children_root,
        });
    }
}
