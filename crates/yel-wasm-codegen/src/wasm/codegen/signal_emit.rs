//! Signal storage helpers (struct.get/set, flat-slot stores), registry
//! allocate/lookup, effect-trigger fan-out, and the per-global-signal
//! fanout helper. All methods live on `WasmPackageBuilder<'a>` via an
//! additional impl block.

use wasm_encoder::{Function, Instruction, ValType};
use yel_core::DefId;
use yel_core::ids::BlockId;
use yel_core::lir::{LirExpr, LirResource};

use super::super::CodegenError;
use super::super::WasmPackageBuilder;

impl<'a> WasmPackageBuilder<'a> {
    /// Push the current function's `(ref $Comp_<i>)` self ref onto
    /// the WASM stack. Sources from `current_self_local` — the
    /// per-instance, ref-typed entry-point convention.
    ///
    /// Strict: there is no singleton fallback. Every emit site must
    /// have entered with `current_self_local` set to a local of the
    /// matching component; mismatches and missing locals are hard
    /// errors so callers can't accidentally route to the wrong
    /// instance.
    pub(super) fn emit_self_ref(
        &self,
        func: &mut Function,
        comp_idx: usize,
    ) -> Result<(), CodegenError> {
        let local_idx = self.current_self_local.ok_or_else(|| {
            CodegenError::InvalidIR(format!(
                "emit_self_ref: no current_self_local in scope for component {} — \
                 every emit site must establish the per-instance self ref before \
                 calling helpers that need it (no singleton fallback exists)",
                comp_idx
            ))
        })?;
        let self_ci = self.current_self_comp_idx.ok_or_else(|| {
            CodegenError::InvalidIR(format!(
                "emit_self_ref: current_self_local set but no current_self_comp_idx — \
                 emitter must record both consistently for component {}",
                comp_idx
            ))
        })?;
        if self_ci != comp_idx {
            return Err(CodegenError::InvalidIR(format!(
                "emit_self_ref: comp_idx mismatch — current self is for component {}, \
                 requested {}. Cross-component access must go through the registry \
                 (e.g. global-signal fanout helpers), not via emit_self_ref.",
                self_ci, comp_idx
            )));
        }
        func.instruction(&Instruction::LocalGet(local_idx));
        Ok(())
    }

    /// Push the boundary struct ref for `boundary_id` onto the stack.
    /// Strict resolution — no runtime tree walk. Order:
    ///
    /// 1. **In-scope local fast path**: if the function received the
    ///    boundary as a typed parameter (recorded in
    ///    `current_boundary_locals` at function entry), emit
    ///    `local.get <local>`. This is the dominant path: every inner
    ///    function (branch mount, iter mount, update block, fan-out
    ///    callback) takes its operative boundary as a function param,
    ///    so reads/writes inside that function are O(1).
    /// 2. **Root boundary**: read `$self.tree` directly. Component
    ///    constructor pre-populates the root and never replaces it.
    ///
    /// Inner boundaries (if-anchor, if-branch, for-anchor) NOT in
    /// scope are a hard error — the model is "callers compute and
    /// pass the boundary as a param", not "callees fetch it". Callers
    /// chain `struct.get`s once at the call site to produce the typed
    /// ref, then thread it through.
    ///
    /// `ForIterBody` is the same — only ever reachable via fan-out
    /// callback param.
    pub(crate) fn emit_boundary_ref(
        &self,
        func: &mut Function,
        comp_idx: usize,
        boundary_id: yel_core::ids::TreeBoundaryId,
    ) -> Result<(), CodegenError> {
        // Fast path: boundary in scope as a function parameter or
        // mount-scope alloc.
        if let Some(&local) = self.current_boundary_locals.get(&boundary_id) {
            func.instruction(&Instruction::LocalGet(local));
            return Ok(());
        }

        let gc = &self.gc_layouts[comp_idx];
        let component = &self.components[comp_idx];

        // Root: load $self.tree.
        // Stage 5d: read the kind from the resource registry instead
        // of comparing against `tree_shape.root_idx`. The root struct
        // is the unique entry whose `kind == TreeBoundaryKind::Root`.
        let is_root = component
            .struct_types
            .get(boundary_id.index())
            .map(|s| matches!(s.kind, yel_core::lir::block::TreeBoundaryKind::Root))
            .unwrap_or(false);
        if is_root {
            let comp_struct_ty = gc.component_struct_type_idx.ok_or_else(|| {
                CodegenError::InvalidIR(
                    "emit_boundary_ref: missing component_struct_type_idx".into(),
                )
            })?;
            let tree_field = gc.tree_root_field_idx.ok_or_else(|| {
                CodegenError::InvalidIR(
                    "emit_boundary_ref (root): component has no tree-root \
                     field — body_tree was empty when types were emitted"
                        .into(),
                )
            })?;
            self.emit_self_ref(func, comp_idx)?;
            func.instruction(&Instruction::StructGet {
                struct_type_index: comp_struct_ty,
                field_index: tree_field,
            });
            return Ok(());
        }

        // Inner boundary not in scope. The CALLER (this is most often
        // a trigger fan-out helper or a mount-block CallBlock site)
        // must compute the ref by chaining `struct.get`s through
        // `parent_link`. This is bounded compile-time emission — we
        // walk the parent chain once at the call site, NOT per slot
        // access. The callee receives the ref as a function param and
        // accesses fields via `local.get` thereafter (O(1)).
        // Stage 5d: parent link from struct_types registry.
        let component = &self.components[comp_idx];
        let parent_link = component
            .struct_types
            .get(boundary_id.index())
            .and_then(|s| s.parent)
            .map(|p| (yel_core::ids::TreeBoundaryId(p.parent.0), p.field_idx));
        let (parent_id, field_idx) = parent_link.ok_or_else(|| {
            let in_scope: Vec<_> = self.current_boundary_locals.keys().collect();
            CodegenError::InvalidIR(format!(
                "emit_boundary_ref: boundary {} has no parent_link and is not in \
                 scope. ForIterBody can only be reached via a fan-out callback \
                 that supplies the iter-body ref via `current_boundary_locals`. \
                 In-scope locals: {:?}",
                boundary_id, in_scope
            ))
        })?;
        // Recurse to push parent ref, then read this boundary's
        // SubBoundary field on the parent.
        self.emit_boundary_ref(func, comp_idx, parent_id)?;
        func.instruction(&Instruction::RefAsNonNull);
        let parent_struct_ty = *gc.tree_struct_type_idx.get(&parent_id).ok_or_else(|| {
            CodegenError::InvalidIR(format!(
                "emit_boundary_ref: missing tree struct type for parent {}",
                parent_id
            ))
        })?;
        func.instruction(&Instruction::StructGet {
            struct_type_index: parent_struct_ty,
            field_index: field_idx,
        });
        Ok(())
    }

    /// Push the host's WIT resource handle (the i32 returned by
    /// `[resource-new]X`) for `component`'s current instance onto the
    /// stack. Sources it from the trailing `$self_handle` field on
    /// `$Comp_<Name>` via the in-scope self ref. Used by callback
    /// emit sites to pass `borrow<Self>` back to the host.
    pub(crate) fn emit_self_handle_load(
        &self,
        func: &mut Function,
        component: &yel_core::lir::LirResource,
    ) -> Result<(), CodegenError> {
        let comp_idx = self.comp_idx_of(component).ok_or_else(|| {
            CodegenError::InvalidIR(
                "emit_self_handle_load: component is not in self.components — \
                 callback emit sites must run inside a real component context"
                    .into(),
            )
        })?;
        let gc = &self.gc_layouts[comp_idx];
        let struct_ty = gc.component_struct_type_idx.ok_or_else(|| {
            CodegenError::InvalidIR(
                "emit_self_handle_load: missing component_struct_type_idx".into(),
            )
        })?;
        let field_idx = gc.self_handle_field_idx.ok_or_else(|| {
            CodegenError::InvalidIR("emit_self_handle_load: missing self_handle_field_idx".into())
        })?;
        self.emit_self_ref(func, comp_idx)?;
        func.instruction(&Instruction::StructGet {
            struct_type_index: struct_ty,
            field_index: field_idx,
        });
        Ok(())
    }

    /// Inline the registry-allocation sequence for component `comp_idx`.
    /// Reads from `instance_local` (must hold a `(ref $Comp_<i>)`),
    /// pops nothing, and **pushes** the freshly allocated handle index
    /// (i32) onto the stack.
    ///
    /// Algorithm:
    ///   1. If `free_head ≥ 0`: pop the free chain, reuse that handle.
    ///   2. Else: ensure the registry array exists and has capacity
    ///      (lazily allocate at len 8, double to grow). Append a new
    ///      `$CompHandle` at index `len`, bump `len`.
    ///
    /// `scratch_idx_local` and `scratch_arr_local` must be reserved
    /// by the caller and have types `i32` and `(ref null
    /// $CompHandleArr_<i>)` respectively. They're scratch — content
    /// before this call is irrelevant.
    ///
    /// TODO: wire `[resource-drop]` so freed handles return to the
    /// free chain (the reuse path above already supports it). Today
    /// the registry only grows — slot reuse becomes important when
    /// long-lived hosts unmount components without recycling. The
    /// drop sequence is small (null `arr[h].inst`, link onto
    /// free_head); add it back when needed.
    pub(super) fn emit_registry_alloc(
        &self,
        func: &mut Function,
        comp_idx: usize,
        instance_local: u32,
        scratch_idx_local: u32,
        scratch_arr_local: u32,
    ) -> Result<(), CodegenError> {
        let gc = &self.gc_layouts[comp_idx];
        let handle_struct_ty = self.shared_handle_type_idx.ok_or_else(|| {
            CodegenError::InvalidIR("registry alloc: shared $handle type not emitted".into())
        })?;
        let handle_arr_ty = self.shared_handle_arr_type_idx.ok_or_else(|| {
            CodegenError::InvalidIR("registry alloc: shared $handle-array type not emitted".into())
        })?;
        let registry_g = gc
            .registry_global
            .ok_or_else(|| CodegenError::InvalidIR("registry alloc: no registry global".into()))?;
        let len_g = gc
            .registry_len_global
            .ok_or_else(|| CodegenError::InvalidIR("registry alloc: no len global".into()))?;
        let free_head_g = gc
            .registry_free_head_global
            .ok_or_else(|| CodegenError::InvalidIR("registry alloc: no free_head global".into()))?;

        // `if (free_head != -1) { reuse } else { grow-or-append }`
        // Both arms leave the new handle index on the stack.
        func.instruction(&Instruction::GlobalGet(free_head_g));
        func.instruction(&Instruction::I32Const(-1));
        func.instruction(&Instruction::I32Ne);
        func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
            wasm_encoder::ValType::I32,
        )));

        // ---- Reuse path ----
        // idx = free_head
        func.instruction(&Instruction::GlobalGet(free_head_g));
        func.instruction(&Instruction::LocalSet(scratch_idx_local));
        // free_head = arr[idx].next
        func.instruction(&Instruction::GlobalGet(registry_g));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::LocalGet(scratch_idx_local));
        func.instruction(&Instruction::ArrayGet(handle_arr_ty));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::StructGet {
            struct_type_index: handle_struct_ty,
            field_index: 1,
        });
        func.instruction(&Instruction::GlobalSet(free_head_g));
        // arr[idx].inst = instance
        func.instruction(&Instruction::GlobalGet(registry_g));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::LocalGet(scratch_idx_local));
        func.instruction(&Instruction::ArrayGet(handle_arr_ty));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::LocalGet(instance_local));
        func.instruction(&Instruction::StructSet {
            struct_type_index: handle_struct_ty,
            field_index: 0,
        });
        // arr[idx].next = -1 (alive marker; tidies the chain)
        func.instruction(&Instruction::GlobalGet(registry_g));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::LocalGet(scratch_idx_local));
        func.instruction(&Instruction::ArrayGet(handle_arr_ty));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::I32Const(-1));
        func.instruction(&Instruction::StructSet {
            struct_type_index: handle_struct_ty,
            field_index: 1,
        });
        // push idx
        func.instruction(&Instruction::LocalGet(scratch_idx_local));

        func.instruction(&Instruction::Else);

        // ---- Grow-or-append path ----
        // If registry is null, allocate fresh array of len 8.
        func.instruction(&Instruction::GlobalGet(registry_g));
        func.instruction(&Instruction::RefIsNull);
        func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
            handle_struct_ty,
        )));
        func.instruction(&Instruction::I32Const(8));
        func.instruction(&Instruction::ArrayNew(handle_arr_ty));
        func.instruction(&Instruction::GlobalSet(registry_g));
        func.instruction(&Instruction::End);

        // If len >= cap, grow: alloc new (cap*2 or +8) array and copy.
        func.instruction(&Instruction::GlobalGet(registry_g));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::LocalSet(scratch_idx_local)); // reuse as $cap
        func.instruction(&Instruction::GlobalGet(len_g));
        func.instruction(&Instruction::LocalGet(scratch_idx_local));
        func.instruction(&Instruction::I32GeU);
        func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        // new_arr = array.new (ref.null, cap*2)
        func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
            handle_struct_ty,
        )));
        func.instruction(&Instruction::LocalGet(scratch_idx_local)); // $cap
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Shl);
        func.instruction(&Instruction::ArrayNew(handle_arr_ty));
        func.instruction(&Instruction::LocalSet(scratch_arr_local));
        // array.copy(new_arr, 0, old_arr, 0, cap)
        func.instruction(&Instruction::LocalGet(scratch_arr_local));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::GlobalGet(registry_g));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalGet(scratch_idx_local));
        func.instruction(&Instruction::ArrayCopy {
            array_type_index_dst: handle_arr_ty,
            array_type_index_src: handle_arr_ty,
        });
        // registry = new_arr
        func.instruction(&Instruction::LocalGet(scratch_arr_local));
        func.instruction(&Instruction::GlobalSet(registry_g));
        func.instruction(&Instruction::End);

        // idx = len; len += 1
        func.instruction(&Instruction::GlobalGet(len_g));
        func.instruction(&Instruction::LocalSet(scratch_idx_local));
        func.instruction(&Instruction::GlobalGet(len_g));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::GlobalSet(len_g));

        // arr[idx] = struct.new $CompHandle (instance, -1)
        func.instruction(&Instruction::GlobalGet(registry_g));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::LocalGet(scratch_idx_local));
        func.instruction(&Instruction::LocalGet(instance_local));
        func.instruction(&Instruction::I32Const(-1));
        func.instruction(&Instruction::StructNew(handle_struct_ty));
        func.instruction(&Instruction::ArraySet(handle_arr_ty));

        // push idx
        func.instruction(&Instruction::LocalGet(scratch_idx_local));

        func.instruction(&Instruction::End); // end if/else

        Ok(())
    }

    /// Inline the registry-lookup sequence: read `arr[handle_local]`
    /// and store the resolved `(ref $Comp_<i>)` into `result_local`.
    /// Traps if the handle is out of range or has been freed (via
    /// `ref.as_non_null` on a null `$inst`).
    pub(super) fn emit_registry_lookup(
        &self,
        func: &mut Function,
        comp_idx: usize,
        handle_local: u32,
        result_local: u32,
    ) -> Result<(), CodegenError> {
        let gc = &self.gc_layouts[comp_idx];
        let handle_struct_ty = self.shared_handle_type_idx.ok_or_else(|| {
            CodegenError::InvalidIR("registry lookup: shared $handle type not emitted".into())
        })?;
        let handle_arr_ty = self.shared_handle_arr_type_idx.ok_or_else(|| {
            CodegenError::InvalidIR("registry lookup: shared $handle-array type not emitted".into())
        })?;
        let comp_struct_ty = gc.component_struct_type_idx.ok_or_else(|| {
            CodegenError::InvalidIR("registry lookup: missing component struct type idx".into())
        })?;
        let registry_g = gc
            .registry_global
            .ok_or_else(|| CodegenError::InvalidIR("registry lookup: no registry global".into()))?;

        func.instruction(&Instruction::GlobalGet(registry_g));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::LocalGet(handle_local));
        func.instruction(&Instruction::ArrayGet(handle_arr_ty));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::StructGet {
            struct_type_index: handle_struct_ty,
            field_index: 0,
        });
        func.instruction(&Instruction::RefAsNonNull);
        // Shared `$handle.$inst` is anyref; recover the typed component
        // ref via ref.cast. Traps on mismatch (same correctness as the
        // pre-unification null-trap from a per-component-typed handle).
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(comp_struct_ty),
        ));
        func.instruction(&Instruction::LocalSet(result_local));
        Ok(())
    }

    /// Emit code that loads a component-local signal's value onto the
    /// stack via `<self_ref>; struct.get $T $f` per ABI slot, where
    /// `<self_ref>` is sourced from the current self-local (see
    /// `emit_self_ref`). Pushes `field_path.len()` values in order
    /// (slot 0 first, last on top).
    pub(crate) fn emit_signal_struct_read(
        &self,
        func: &mut Function,
        comp_idx: usize,
        signal_idx: usize,
    ) -> Result<(), CodegenError> {
        let gc_layout = &self.gc_layouts[comp_idx];
        let struct_ty = gc_layout.component_struct_type_idx.ok_or_else(|| {
            CodegenError::InvalidIR(
                "emit_signal_struct_read: missing component struct type idx".into(),
            )
        })?;
        let component = &self.components[comp_idx];
        if component.signal_layout.signals.get(signal_idx).is_none() {
            return Err(CodegenError::InvalidIR(format!(
                "emit_signal_struct_read: no field path for signal {}",
                signal_idx
            )));
        }
        let field_path: Vec<u32> = component.signal_layout.signal_field_path(signal_idx);
        for &field_idx in &field_path {
            self.emit_self_ref(func, comp_idx)?;
            func.instruction(&Instruction::StructGet {
                struct_type_index: struct_ty,
                field_index: field_idx,
            });
        }
        Ok(())
    }


    /// Push every ABI slot of a migrated global property onto the
    /// WASM stack via `global.get $globals_<block>_self; struct.get
    /// $globals_<block> $field` per slot. Pushes one stack value per
    /// slot in canonical order (slot 0 first, last on top).
    /// Resolve a migrated global property to the core wasm global index
    /// backing each of its storage slots, in canonical (slot 0 first)
    /// order. The live replacement for `resolve_global_struct_target`'s
    /// `(struct_ty, self_global, field_path)`.
    pub(crate) fn resolve_global_core_globals(
        &self,
        prop_def_id: DefId,
    ) -> Result<Vec<u32>, CodegenError> {
        let block_id = self
            .ctx
            .defs
            .owning_global_block(prop_def_id)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "global core-global resolve: DefId {:?} is not a global-block property",
                    prop_def_id
                ))
            })?;
        let &layout_idx = self.global_block_def_to_idx.get(&block_id).ok_or_else(|| {
            CodegenError::InvalidIR(format!(
                "global core-global resolve: no globals layout for block {:?}",
                block_id
            ))
        })?;
        let layout = &self.globals_layouts[layout_idx];
        let block = self.ctx.defs.as_global(block_id).ok_or_else(|| {
            CodegenError::InvalidIR(format!(
                "global core-global resolve: block {:?} is not a GlobalDef",
                block_id
            ))
        })?;
        let prop_pos = block
            .properties
            .iter()
            .position(|&p| p == prop_def_id)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "global core-global resolve: property {:?} not in block {:?}",
                    prop_def_id, block_id
                ))
            })?;
        let field_path = layout.property_field_paths.get(prop_pos).ok_or_else(|| {
            CodegenError::InvalidIR(format!(
                "global core-global resolve: no field path for property {:?} (pos {})",
                prop_def_id, prop_pos
            ))
        })?;
        if field_path.is_empty() {
            return Err(CodegenError::InvalidIR(format!(
                "global core-global resolve: property {:?} is pointer-typed (legacy memory path)",
                prop_def_id
            )));
        }
        Ok(field_path
            .iter()
            .map(|&f| layout.field_core_globals[f as usize])
            .collect())
    }

    pub(crate) fn emit_global_struct_read(
        &self,
        func: &mut Function,
        prop_def_id: DefId,
    ) -> Result<(), CodegenError> {
        let core_globals = self.resolve_global_core_globals(prop_def_id)?;
        for &g in &core_globals {
            func.instruction(&Instruction::GlobalGet(g));
        }
        Ok(())
    }

    /// Evaluate `expr` and store its value into the migrated global
    /// property's struct fields. Mirrors
    /// `emit_signal_struct_store_from_expr` but sources the self ref
    /// from the per-block `(mut (ref null $globals_<i>))` global
    /// instead of `current_self_local`.
    pub(crate) fn emit_global_struct_store_from_expr(
        &mut self,
        func: &mut Function,
        prop_def_id: DefId,
        expr: &LirExpr,
        component: &LirResource,
        scratch: crate::wasm::FlatScratchBases,
    ) -> Result<(), CodegenError> {
        let _ = scratch;
        let core_globals = self.resolve_global_core_globals(prop_def_id)?;
        let slot_valtypes = self.signal_storage_valtypes(expr.ty);
        if slot_valtypes.len() != core_globals.len() {
            return Err(CodegenError::InvalidIR(format!(
                "emit_global_struct_store_from_expr: storage valtypes ({}) disagree with core globals ({}) for property {:?}",
                slot_valtypes.len(),
                core_globals.len(),
                prop_def_id,
            )));
        }
        if slot_valtypes.is_empty() {
            // Defensive: nothing to write. emit expr for side effects.
            self.emit_expr(func, expr, component)?;
            return Ok(());
        }
        // Emit the value expr (pushes N slots, slot 0 deepest / slot N-1
        // on top), then `global.set` each slot in reverse so the top of
        // stack lands in the last field — no scratch spill needed.
        self.emit_expr(func, expr, component)?;
        for i in (0..core_globals.len()).rev() {
            func.instruction(&Instruction::GlobalSet(core_globals[i]));
        }
        Ok(())
    }

    /// Emit calls to every effect-update block whose dependency set
    /// includes `signal`. Handles both:
    ///   - global signals: scan *all* components' effects
    ///   - local signals:  scan only `local_comp_idx`'s effects
    ///
    /// Each update block takes one i32 param (unused for effects; a
    /// dummy `0` is passed as the parent slot).
    pub(super) fn emit_trigger_effects(
        &self,
        func: &mut Function,
        signal: DefId,
        local_comp_idx: usize,
    ) -> Result<(), CodegenError> {
        let is_global = self.ctx.defs.owning_global_block(signal).is_some();
        if is_global {
            // Cross-component fan-out runs in a dedicated helper so the
            // call site needs no scratch locals. The helper walks every
            // observing component's registry array and calls each live
            // instance's effect block with its typed self ref. If no
            // helper was registered, the global has no observers and
            // the trigger is a no-op.
            if let Some(&fanout_idx) = self.global_fanout_func_idx.get(&signal) {
                func.instruction(&Instruction::Call(fanout_idx));
            }
        } else {
            let component = &self.components[local_comp_idx];
            if let Some(effect_ids) = component.effects_by_signal.get(&signal) {
                for &eid in effect_ids {
                    if let Some(effect) = component.effects.iter().find(|e| e.id == eid)
                        && let Some(&fi) = self.block_func_indices.get(&effect.update_block)
                    {
                        // Caller-side: assemble the call's args to
                        // match the callee's actual signature.
                        //
                        // - `self_ref` always (every block takes it).
                        // - One i32 per `block.params` slot. Update
                        //   blocks today have an empty `params` Vec
                        //   (they read parent from memory inside);
                        //   for those, no i32 is pushed.
                        // - One ref per `block.boundary_params`,
                        //   computed via `emit_boundary_ref` (chain
                        //   of `struct.get`s from `$self.tree`).
                        self.emit_self_ref(func, local_comp_idx)?;
                        let update_block = component
                            .blocks
                            .iter()
                            .find(|b| b.id == effect.update_block);
                        // Match block_fn.rs's lir_param_count
                        // logic: empty params + empty
                        // boundary_params → fixed `block_1param`
                        // signature with 1 implicit i32 parent;
                        // otherwise the dynamic shape uses
                        // exactly `params.len()` i32 args.
                        // Stage 5c: derive boundary-id list from the
                        // callee's `boundary_param_slots` (each
                        // slot's val_ty carries the id). Same data
                        // as `boundary_params`, independent of that
                        // field.
                        let component_slots: &[_] = &component.slots;
                        let (n_i32_args, boundary_ids): (u32, Vec<_>) = match update_block {
                            Some(b) if !b.params.is_empty() => (
                                b.params.len() as u32,
                                b.boundary_param_ids_from_slots(component_slots).collect(),
                            ),
                            Some(b) if !b.boundary_param_slots.is_empty() => (
                                0,
                                b.boundary_param_ids_from_slots(component_slots).collect(),
                            ),
                            _ => (1, Vec::new()),
                        };
                        for _ in 0..n_i32_args {
                            func.instruction(&Instruction::I32Const(0));
                        }
                        for b_id in boundary_ids {
                            self.emit_boundary_ref(func, local_comp_idx, b_id)?;
                        }
                        func.instruction(&Instruction::Call(fi));
                    }
                }
            }
        }
        Ok(())
    }

    /// Emit the body of `$global_fanout_<signal>` — the per-global-signal
    /// helper that walks each observing component's registry array and
    /// calls every live instance's effect block. Signature is `() -> ()`.
    ///
    /// For each (component, effect) where `effect.dependencies` contains
    /// `signal`, emit:
    ///
    /// ```wat
    /// i32.const 0
    /// local.set $idx
    /// block $break
    ///   loop $L
    ///     ;; if $idx >= len, break
    ///     local.get $idx
    ///     global.get $registry_len_<comp>
    ///     i32.ge_u
    ///     br_if $break
    ///     ;; if registry array is null, break (no instances ever allocated)
    ///     global.get $registry_<comp>
    ///     ref.is_null
    ///     br_if $break
    ///     ;; entry = arr[idx]
    ///     global.get $registry_<comp>
    ///     ref.as_non_null
    ///     local.get $idx
    ///     array.get $CompHandleArr_<comp>
    ///     ;; if entry is null (shouldn't happen), skip
    ///     ref.is_null
    ///     if
    ///       ;; nothing
    ///     else
    ///       ;; inst = entry.inst (load the ref)
    ///       global.get $registry_<comp>
    ///       ref.as_non_null
    ///       local.get $idx
    ///       array.get $CompHandleArr_<comp>
    ///       ref.as_non_null
    ///       struct.get $CompHandle_<comp> $inst
    ///       ;; if inst is null (freed slot), skip
    ///       ref.is_null
    ///       if
    ///       else
    ///         ;; call effect(inst, 0)
    ///         <re-load inst>
    ///         i32.const 0
    ///         call $effect_<i>
    ///       end
    ///     end
    ///     ;; idx += 1; continue
    ///     local.get $idx
    ///     i32.const 1
    ///     i32.add
    ///     local.set $idx
    ///     br $L
    ///   end
    /// end
    /// ```
    /// Inline emission of the parent-link chain to fetch a boundary
    /// ref inside the global-fanout helper, where neither
    /// `current_self_local` nor `current_boundary_locals` is
    /// established. Re-loads the typed self ref from the registry
    /// array entry on each call (matching the rest of the helper's
    /// per-effect re-load pattern), then chains `struct.get`s through
    /// `parent_link` to reach `boundary_id`.
    #[allow(clippy::too_many_arguments)]
    fn emit_boundary_chain_from_self_inline(
        &self,
        func: &mut Function,
        comp_idx: usize,
        boundary_id: yel_core::ids::TreeBoundaryId,
        registry_g: u32,
        idx_local: u32,
        handle_arr_ty: u32,
        handle_struct_ty: u32,
        comp_struct_ty: u32,
    ) -> Result<(), CodegenError> {
        let component = &self.components[comp_idx];
        let gc = &self.gc_layouts[comp_idx];

        // Build the chain of (boundary, parent_field_idx) walks from
        // boundary_id back up to root.
        // Stage 5d: walk parent chain via the resource registry.
        let mut chain: Vec<(yel_core::ids::TreeBoundaryId, u32)> = Vec::new();
        let mut cur = boundary_id;
        while let Some(p) = component
            .struct_types
            .get(cur.index())
            .and_then(|s| s.parent)
        {
            chain.push((cur, p.field_idx));
            cur = yel_core::ids::TreeBoundaryId(p.parent.0);
        }
        // `cur` is now the root.

        // Push typed self ref by re-loading from registry[idx].
        func.instruction(&Instruction::GlobalGet(registry_g));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::ArrayGet(handle_arr_ty));
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::StructGet {
            struct_type_index: handle_struct_ty,
            field_index: 0,
        });
        func.instruction(&Instruction::RefAsNonNull);
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(comp_struct_ty),
        ));

        // self.tree → root struct ref.
        let tree_field = gc.tree_root_field_idx.ok_or_else(|| {
            CodegenError::InvalidIR(
                "global fanout boundary chain: comp has no tree-root field".into(),
            )
        })?;
        func.instruction(&Instruction::StructGet {
            struct_type_index: comp_struct_ty,
            field_index: tree_field,
        });

        // Walk down: chain is [innermost, ..., outermost]. Reverse to
        // step from root toward boundary_id.
        for (b, fidx) in chain.iter().rev() {
            // We just pushed the parent-of-`b` ref. Fetch `b`'s ref
            // from that parent's SubBoundary field.
            // Stage 5d: parent link from registry.
            let parent_link = component
                .struct_types
                .get(b.index())
                .and_then(|s| s.parent)
                .map(|p| (yel_core::ids::TreeBoundaryId(p.parent.0), p.field_idx));
            let (parent_id, _) = parent_link.ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "global fanout boundary chain: missing parent_link for {}",
                    b
                ))
            })?;
            let parent_struct = *gc.tree_struct_type_idx.get(&parent_id).ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "global fanout: missing tree struct type for {}",
                    parent_id
                ))
            })?;
            func.instruction(&Instruction::RefAsNonNull);
            func.instruction(&Instruction::StructGet {
                struct_type_index: parent_struct,
                field_index: *fidx,
            });
        }
        Ok(())
    }

    pub(super) fn generate_global_fanout_for(
        &self,
        signal: DefId,
    ) -> Result<Function, CodegenError> {
        // One i32 scratch local for the loop counter.
        let mut func = Function::new([(1, ValType::I32)]);
        let idx_local: u32 = 0;

        for (ci, comp) in self.components.iter().enumerate() {
            // Collect this component's effects that depend on the signal,
            // via the precomputed inverted dep index. Each entry pairs
            // the effect's WASM function index with the LIR `BlockId`
            // so we can look up its `boundary_params` and emit the
            // right call shape.
            let observing_effects: Vec<(u32, BlockId)> = comp
                .effects_by_signal
                .get(&signal)
                .map(|ids| ids.as_slice())
                .unwrap_or(&[])
                .iter()
                .filter_map(|eid| {
                    let effect = comp.effects.iter().find(|e| e.id == *eid)?;
                    let fi = self.block_func_indices.get(&effect.update_block)?;
                    Some((*fi, effect.update_block))
                })
                .collect();
            if observing_effects.is_empty() {
                continue;
            }
            let gc = &self.gc_layouts[ci];
            let registry_g = gc.registry_global.ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "global fanout: component {} has no registry global",
                    ci
                ))
            })?;
            let len_g = gc.registry_len_global.ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "global fanout: component {} has no registry_len global",
                    ci
                ))
            })?;
            let handle_struct_ty = self.shared_handle_type_idx.ok_or_else(|| {
                CodegenError::InvalidIR("global fanout: shared $handle type not emitted".into())
            })?;
            let handle_arr_ty = self.shared_handle_arr_type_idx.ok_or_else(|| {
                CodegenError::InvalidIR(
                    "global fanout: shared $handle-array type not emitted".into(),
                )
            })?;
            let comp_struct_ty = gc.component_struct_type_idx.ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "global fanout: component {} has no component struct type",
                    ci
                ))
            })?;

            // Reset idx = 0
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::LocalSet(idx_local));

            // Outer block as break target, inner loop for iteration.
            func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
            func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));

            // if registry_<ci> is null → break (no array allocated yet)
            func.instruction(&Instruction::GlobalGet(registry_g));
            func.instruction(&Instruction::RefIsNull);
            func.instruction(&Instruction::BrIf(1));

            // if idx >= len → break
            func.instruction(&Instruction::LocalGet(idx_local));
            func.instruction(&Instruction::GlobalGet(len_g));
            func.instruction(&Instruction::I32GeU);
            func.instruction(&Instruction::BrIf(1));

            // entry = arr[idx]; if entry is null → skip
            func.instruction(&Instruction::GlobalGet(registry_g));
            func.instruction(&Instruction::RefAsNonNull);
            func.instruction(&Instruction::LocalGet(idx_local));
            func.instruction(&Instruction::ArrayGet(handle_arr_ty));
            func.instruction(&Instruction::RefIsNull);
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
            // null entry — fall through to increment
            func.instruction(&Instruction::Else);

            // inst = entry.inst (re-fetch for the load); if null → skip
            func.instruction(&Instruction::GlobalGet(registry_g));
            func.instruction(&Instruction::RefAsNonNull);
            func.instruction(&Instruction::LocalGet(idx_local));
            func.instruction(&Instruction::ArrayGet(handle_arr_ty));
            func.instruction(&Instruction::RefAsNonNull);
            func.instruction(&Instruction::StructGet {
                struct_type_index: handle_struct_ty,
                field_index: 0,
            });
            func.instruction(&Instruction::RefIsNull);
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
            // freed slot — skip
            func.instruction(&Instruction::Else);

            // For each observing effect, push the typed self-ref then
            // call args matching the block's signature: legacy blocks
            // get `(self, 0_i32)`; dynamic-typed blocks get `(self,
            // <0 per LIR i32 param>, <boundary_ref per
            // boundary_params>)`. Reload inst per effect (cheaper
            // than reserving a typed per-component scratch local in
            // this multi-comp fanout). To resolve boundary refs we
            // temporarily set the self-local context so
            // `emit_boundary_ref` can chain `struct.get`s from the
            // freshly cast typed self ref.
            //
            // The typed self ref must already be in a WASM local
            // before `emit_boundary_ref` runs — we reserve a
            // function-scratch local lazily on first use.
            for (effect_func_idx, block_id) in &observing_effects {
                // Stage 5c: derive boundary-id list from slots.
                let block = comp.blocks.iter().find(|b| b.id == *block_id);
                let (n_i32_args, boundary_ids): (u32, Vec<_>) = match block {
                    Some(b) if !b.params.is_empty() => (
                        b.params.len() as u32,
                        b.boundary_param_ids_from_slots(&comp.slots).collect(),
                    ),
                    Some(b) if !b.boundary_param_slots.is_empty() => {
                        (0, b.boundary_param_ids_from_slots(&comp.slots).collect())
                    }
                    _ => (1, Vec::new()),
                };

                // Push self ref.
                func.instruction(&Instruction::GlobalGet(registry_g));
                func.instruction(&Instruction::RefAsNonNull);
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::ArrayGet(handle_arr_ty));
                func.instruction(&Instruction::RefAsNonNull);
                func.instruction(&Instruction::StructGet {
                    struct_type_index: handle_struct_ty,
                    field_index: 0,
                });
                func.instruction(&Instruction::RefAsNonNull);
                func.instruction(&Instruction::RefCastNonNull(
                    wasm_encoder::HeapType::Concrete(comp_struct_ty),
                ));

                // i32 LIR args (parent placeholders).
                for _ in 0..n_i32_args {
                    func.instruction(&Instruction::I32Const(0));
                }

                // Boundary refs: re-fetch from $self via the chain.
                // We don't have `current_self_local` set here because
                // this helper runs outside any per-component emit
                // scope, so we inline the chain manually.
                for &b_id in &boundary_ids {
                    self.emit_boundary_chain_from_self_inline(
                        &mut func,
                        ci,
                        b_id,
                        registry_g,
                        idx_local,
                        handle_arr_ty,
                        handle_struct_ty,
                        comp_struct_ty,
                    )?;
                }

                func.instruction(&Instruction::Call(*effect_func_idx));
            }

            // end inner-if (inst non-null arm)
            func.instruction(&Instruction::End);
            // end outer-if (entry non-null arm)
            func.instruction(&Instruction::End);

            // idx += 1; continue loop.
            func.instruction(&Instruction::LocalGet(idx_local));
            func.instruction(&Instruction::I32Const(1));
            func.instruction(&Instruction::I32Add);
            func.instruction(&Instruction::LocalSet(idx_local));
            func.instruction(&Instruction::Br(0)); // continue loop $L

            // end loop
            func.instruction(&Instruction::End);
            // end block (break target)
            func.instruction(&Instruction::End);

            // Suppress the unused warning on `comp_struct_ty` —
            // referenced in the WAT comment but not used by the
            // hand-emitted bytecode (struct_type_index is enough).
            let _ = comp_struct_ty;
        }

        func.instruction(&Instruction::End);
        Ok(func)
    }
}
