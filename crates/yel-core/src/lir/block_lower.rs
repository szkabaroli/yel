//! Block Lowering Pass
//!
//! Converts tree-based LIR (`TreeLirComponent`) to block-based LIR (`LirComponent`).
//!
//! The lowering process:
//! 1. Walks the UI tree and emits LirOp instructions
//! 2. Interns strings (tag names, text content, attributes)
//! 3. Interns expressions (conditions, values)
//! 4. Allocates slots (temps for DOM handles, memory for persistent state)
//! 5. Creates blocks for branches (if/else) and handlers

use std::collections::HashMap;

use crate::context::CompilerContext;
use crate::ids::{DefId, LocalId, FieldIdx};
use crate::types::Ty;

use super::block::{
    BlockId, ExprId, LirBlock, LirBlockEffect, LirOp, SlotId, SlotInfo, SlotKind, SlotValType,
    StringId,
};
use super::expr::{LirExpr ,LirExprKind, LirStatement, LirLiteral};
use super::lower::TreeLirComponent;
use super::node::{LirComponent, LirNode, LirNodeKind, LirHandler};
use super::signal::{LirEffect, LirSignal, UpdateKind};

/// Classification of a for-loop iterable expression.
enum IterableKind {
    /// Iterable is a signal (state variable).
    Signal(DefId),
    /// Iterable is a field access on a local variable (e.g., `item.subitems`).
    FieldAccess {
        base_local: LocalId,
        field_idx: FieldIdx,
        record_def: DefId,
    },
    /// Unsupported iterable expression.
    Unsupported,
}

/// State for the block lowering pass.
pub(crate)struct BlockLowering<'a> {
    ctx: &'a CompilerContext,
    /// DefId of the component being lowered (for block naming).
    component_id: DefId,

    // Input - tree-based effects and signals for lookup
    tree_effects: &'a [LirEffect],
    tree_signals: &'a [LirSignal],

    // Output
    blocks: Vec<LirBlock>,
    effects: Vec<LirBlockEffect>,
    slots: Vec<SlotInfo>,
    strings: Vec<String>,
    string_map: HashMap<String, StringId>,
    exprs: Vec<LirExpr>,

    // Allocation counters
    next_slot: u32,
    next_block: u32,
    next_memory_offset: u32,

    // Current block being built
    current_ops: Vec<LirOp>,
    // Stack for nested block creation
    ops_stack: Vec<Vec<LirOp>>,

    // For-loop item bindings: LocalId -> (SlotId containing item ptr, item type)
    local_bindings: HashMap<LocalId, (SlotId, Ty)>,

    // Active outer item memory slots for nested blocks.
    // When inside a for-loop, outer items are stored to memory slots so that
    // nested blocks (if-branches, etc.) can load them.
    // Maps LocalId -> (Type, memory SlotId).
    outer_item_memory: HashMap<LocalId, (Ty, SlotId)>,
}

impl<'a> BlockLowering<'a> {
    pub(crate) fn new(ctx: &'a CompilerContext, tree: &'a TreeLirComponent) -> Self {
        Self {
            ctx,
            component_id: tree.def_id,
            tree_effects: &tree.effects,
            tree_signals: &tree.signals,
            blocks: Vec::new(),
            effects: Vec::new(),
            slots: Vec::new(),
            strings: Vec::new(),
            string_map: HashMap::new(),
            exprs: Vec::new(),
            next_slot: 0,
            next_block: 0,
            // Start at a high enough offset to avoid collisions with:
            // - Runtime scratch buffer (0-31)
            // - String data section (256+, varies by component)
            // - Signal storage (after strings)
            // Using 1024 (0x400) as a safe starting point for component memory slots.
            // TODO: Compute this dynamically in codegen based on actual string/signal sizes.
            next_memory_offset: 1024,
            current_ops: Vec::new(),
            ops_stack: Vec::new(),
            local_bindings: HashMap::new(),
            outer_item_memory: HashMap::new(),
        }
    }

    /// Find a tree-based effect by its ID.
    fn find_tree_effect(&self, effect_id: u32) -> Option<&LirEffect> {
        self.tree_effects.iter().find(|e| e.id == effect_id)
    }

    /// Find a signal's type by its DefId.
    fn find_signal_type(&self, signal_def_id: DefId) -> Option<Ty> {
        self.tree_signals
            .iter()
            .find(|s| s.def_id == signal_def_id)
            .map(|s| s.ty)
    }

    /// Convert a Ty to SlotValType for WASM local declaration.
    fn ty_to_slot_val_type(&self, ty: Ty) -> SlotValType {
        use crate::types::InternedTyKind;
        match self.ctx.ty_kind(ty) {
            InternedTyKind::F32 => SlotValType::F32,
            InternedTyKind::F64 => SlotValType::F64,
            InternedTyKind::S64 | InternedTyKind::U64 => SlotValType::I64,
            _ => SlotValType::I32,
        }
    }

    pub (crate) fn lower_component(&mut self, tree: &TreeLirComponent) -> LirComponent {
        // Generate constructor block first
        let constructor_block = self.generate_constructor_block(tree);

        // Start mount block
        self.start_block();

        // Lower the body - we need a parent slot
        // The mount function receives parent as parameter (slot 0)
        let parent_slot = self.alloc_temp_slot();

        for node in &tree.body {
            self.lower_node(node, parent_slot);
        }

        // Finish mount block
        let mount_block = self.finish_block_named("mount");

        LirComponent {
            def_id: tree.def_id,
            name: tree.name,
            span: tree.span,
            is_export: tree.is_export,
            blocks: std::mem::take(&mut self.blocks),
            constructor_block,
            mount_block,
            effects: std::mem::take(&mut self.effects),
            slots: std::mem::take(&mut self.slots),
            strings: std::mem::take(&mut self.strings),
            exprs: std::mem::take(&mut self.exprs),
            signals: tree.signals.clone(),
        }
    }

    /// Generate the constructor block that initializes signals and memory slots.
    fn generate_constructor_block(&mut self, tree: &TreeLirComponent) -> BlockId {
        self.start_block();

        // Initialize each signal
        for (i, signal) in tree.signals.iter().enumerate() {
            if let Some(default_expr) = &signal.default {
                // Signal has a default value - intern the expression and emit InitSignal
                let expr_id = self.intern_expr(default_expr);
                self.emit(LirOp::InitSignal {
                    signal_idx: i as u32,
                    expr: expr_id,
                });
            } else {
                // No default - emit InitSignalDefault to set zero/empty
                self.emit(LirOp::InitSignalDefault {
                    signal_idx: i as u32,
                });
            }
        }

        // Note: Memory slots will be initialized during codegen since they're
        // allocated during mount block lowering which happens after this.
        // The codegen will iterate through slots and initialize Memory slots.

        // ResourceNew is emitted during codegen based on whether component is exported

        self.finish_block_named("constructor")
    }

    /// Lower a single node, emitting ops to the current block.
    fn lower_node(&mut self, node: &LirNode, parent_slot: SlotId) {
        match &node.kind {
            LirNodeKind::Element {
                tag,
                static_bindings,
                dynamic_binding_ids,
                handlers,
                children,
                ..
            } => {
                // Create element
                let elem_slot = self.alloc_temp_slot();
                let tag_id = self.intern_string(tag);
                self.emit(LirOp::CreateElement {
                    tag: tag_id,
                    result: elem_slot,
                });

                // Append to parent
                self.emit(LirOp::AppendChild {
                    parent: parent_slot,
                    child: elem_slot,
                });

                // Static bindings (attributes set at creation time)
                for binding in static_bindings {
                    let name_id = self.intern_string(&binding.name);
                    let expr_id = self.intern_expr(&binding.value);
                    self.emit(LirOp::SetAttribute {
                        node: elem_slot,
                        name: name_id,
                        expr: expr_id,
                    });
                }

                // Dynamic bindings: set initial value AND create effects for updates
                if !dynamic_binding_ids.is_empty() {
                    // Store element handle in memory so effects can access it
                    let elem_mem_slot = self.alloc_memory_slot(4); // i32 for DOM handle
                    self.emit(LirOp::StoreHandle {
                        slot: elem_mem_slot,
                        from: elem_slot,
                    });

                    // Create effects for each dynamic binding
                    for &effect_id in dynamic_binding_ids {
                        if let Some(effect) = self.find_tree_effect(effect_id) {
                            let deps = effect.dependencies.clone();
                            let expr = effect.expr.clone();
                            let update_kind = effect.update_kind.clone();

                            // Get property name from update_kind
                            if let UpdateKind::Property(prop_name) = &update_kind {
                                let expr_id = self.intern_expr(&expr);
                                let name_id = self.intern_string(prop_name);

                                // Set initial attribute value during mount
                                self.emit(LirOp::SetAttribute {
                                    node: elem_slot,
                                    name: name_id,
                                    expr: expr_id,
                                });

                                // Create effect for reactive updates (only if has dependencies)
                                if !deps.is_empty() {
                                    let update_block = self.create_attribute_update_block(
                                        elem_mem_slot,
                                        name_id,
                                        expr_id,
                                    );
                                    self.effects.push(LirBlockEffect {
                                        id: self.effects.len() as u32,
                                        dependencies: deps,
                                        update_block,
                                    });
                                }
                            }
                        }
                    }
                }

                // Event handlers
                for handler in handlers {
                    let handler_block = self.lower_handler(handler);
                    let event_id = self.intern_string(&handler.event);
                    self.emit(LirOp::AddEventListener {
                        node: elem_slot,
                        event: event_id,
                        handler: handler_block,
                    });
                }

                // Children
                for child in children {
                    self.lower_node(child, elem_slot);
                }
            }

            LirNodeKind::StaticText(text) => {
                let text_slot = self.alloc_temp_slot();
                let content_id = self.intern_string(text);
                self.emit(LirOp::CreateText {
                    content: content_id,
                    result: text_slot,
                });
                self.emit(LirOp::AppendChild {
                    parent: parent_slot,
                    child: text_slot,
                });
            }

            LirNodeKind::DynamicText { effect_id } => {
                // Dynamic text: create with initial content, store handle for effects
                let text_slot = self.alloc_temp_slot();
                let text_mem_slot = self.alloc_memory_slot(4);

                // Find the effect that owns this dynamic text to get the expression
                // Clone what we need before mutably borrowing self
                let (expr, dependencies) = self
                    .find_tree_effect(*effect_id)
                    .map(|e| (e.expr.clone(), Some(e.dependencies.clone())))
                    .unwrap_or_else(|| {
                        // Fallback: empty string if effect not found
                        (
                            LirExpr::new(
                                LirExprKind::Literal(
                                    LirLiteral::String(String::new()),
                                ),
                                crate::types::Ty::STRING,
                            ),
                            None,
                        )
                    });
                let expr_id = self.intern_expr(&expr);

                // Create text node with initial dynamic content
                self.emit(LirOp::CreateTextDynamic {
                    expr: expr_id,
                    result: text_slot,
                });

                // Store handle in memory for effect updates
                self.emit(LirOp::StoreHandle {
                    slot: text_mem_slot,
                    from: text_slot,
                });

                // Append to parent
                self.emit(LirOp::AppendChild {
                    parent: parent_slot,
                    child: text_slot,
                });

                // Create update block only if there are actual signal dependencies
                // (not for text that only references for-loop items)
                if let Some(deps) = dependencies {
                    if !deps.is_empty() {
                        let update_block = self.create_text_update_block(text_mem_slot, expr_id);
                        self.effects.push(LirBlockEffect {
                            id: self.effects.len() as u32,
                            dependencies: deps,
                            update_block,
                        });
                    }
                }
            }

            LirNodeKind::If {
                condition,
                then_branch,
                else_if_branches,
                else_branch,
            } => {
                self.lower_if(
                    condition,
                    then_branch,
                    else_if_branches,
                    else_branch.as_deref(),
                    parent_slot,
                );
            }

            LirNodeKind::For {
                item,
                item_name: _,
                item_span: _,
                item_ty,
                iterable,
                key: _,
                body,
            } => {
                self.lower_for(*item, *item_ty, iterable, body, parent_slot);
            }
        }
    }

    /// Lower an if node with potential else-if branches.
    fn lower_if(
        &mut self,
        condition: &LirExpr,
        then_branch: &[LirNode],
        else_if_branches: &[(LirExpr, Vec<LirNode>)],
        else_branch: Option<&[LirNode]>,
        parent_slot: SlotId,
    ) {
        // Create anchor comment - used as insertion point for branch content
        let anchor_slot = self.alloc_temp_slot();
        let anchor_mem = self.alloc_memory_slot(4); // Store anchor handle
        let active_flag = self.alloc_memory_slot(4); // 0 = else/nothing, 1 = then
        let parent_mem = self.alloc_memory_slot(4); // Store parent handle for effects

        let anchor_text = self.intern_string("if");
        self.emit(LirOp::CreateComment {
            content: anchor_text,
            result: anchor_slot,
        });
        self.emit(LirOp::AppendChild {
            parent: parent_slot,
            child: anchor_slot,
        });
        self.emit(LirOp::StoreHandle {
            slot: anchor_mem,
            from: anchor_slot,
        });
        // Store parent for use by effects
        self.emit(LirOp::StoreHandle {
            slot: parent_mem,
            from: parent_slot,
        });
        // Allocate memory slots to track content nodes for each branch
        let then_content_mem = self.alloc_memory_slot(4); // Handle of then branch's root node
        let else_content_mem = self.alloc_memory_slot(4); // Handle of else branch's root node

        // Create then branch (mount + unmount blocks)
        let (then_mount, then_unmount) = self.create_branch_with_tracking(
            then_branch,
            parent_slot,
            anchor_mem,
            then_content_mem,
        );

        // Create else branch (handles else-if recursively) - None if no else
        let (else_mount, else_unmount): (Option<BlockId>, Option<BlockId>) = if !else_if_branches.is_empty() {
            // Recursive: else-if becomes nested if in a wrapper block
            let (first_cond, first_then) = &else_if_branches[0];
            let remaining = &else_if_branches[1..];

            // Mount block for else-if
            // In block functions, parent is passed as first parameter (SlotId(0))
            self.start_block();
            let block_parent = SlotId(0); // Parent is block function's first parameter
            self.lower_if(first_cond, first_then, remaining, else_branch, block_parent);
            let mount = self.finish_block_named("if-else-if-mount");

            // Unmount: the nested if handles its own cleanup, so no unmount needed here
            (Some(mount), None)
        } else if let Some(else_nodes) = else_branch {
            let (mount, unmount) = self.create_branch_with_tracking(
                else_nodes,
                parent_slot,
                anchor_mem,
                else_content_mem,
            );
            (Some(mount), Some(unmount))
        } else {
            // No else branch - no blocks needed
            (None, None)
        };

        // Register effect for this if only if there are signal dependencies
        // (for conditions that only reference for-loop items, no effect is needed)
        let deps = self.collect_dependencies(condition);
        if !deps.is_empty() {
            // Create update block with proper state transition logic
            // State: 0 = else mounted (or nothing), 1 = then mounted
            let update_block = self.create_if_update_block_with_unmount(
                condition,
                anchor_mem,
                active_flag,
                parent_mem, // Pass parent_mem so update block can load parent
                then_mount,
                then_unmount,
                else_mount,
                else_unmount,
            );
            self.effects.push(LirBlockEffect {
                id: self.effects.len() as u32,
                dependencies: deps,
                update_block,
            });
        }

        // Initial mount: evaluate condition and mount appropriate branch directly
        // This avoids the "nothing mounted" state complexity
        let initial_cond = self.alloc_temp_slot();
        let cond_expr = self.intern_expr(condition);
        self.emit(LirOp::EvalExpr {
            expr: cond_expr,
            result: initial_cond,
        });

        // If condition true: mount then, store 1
        // If condition false: mount else (if exists), store 0
        let initial_then_ops = vec![
            LirOp::CallBlock { block: then_mount, parent: parent_slot },
            LirOp::StoreI32 { slot: active_flag, value: 1 },
        ];

        let mut initial_else_ops = Vec::new();
        if let Some(block) = else_mount {
            initial_else_ops.push(LirOp::CallBlock { block, parent: parent_slot });
        }
        initial_else_ops.push(LirOp::StoreI32 { slot: active_flag, value: 0 });

        self.emit(LirOp::If {
            cond: initial_cond,
            then_ops: initial_then_ops,
            else_ops: initial_else_ops,
        });
    }

    /// Lower a for-loop node.
    /// Creates three blocks:
    /// - for-item-mount: renders body for each item (parent, item_ptr) -> ()
    /// - for-item-unmount: removes rendered node (node) -> ()
    /// - for-update: handles list updates (parent) -> ()
    ///
    /// Then emits inline loop ops in the current block to do initial mount.
    fn lower_for(
        &mut self,
        item: LocalId,
        item_ty: Ty,
        iterable: &LirExpr,
        body: &[LirNode],
        parent_slot: SlotId,
    ) {
        // Create anchor comment for this for-loop
        let anchor_slot = self.alloc_temp_slot();
        let anchor_text = self.intern_string("for");
        self.emit(LirOp::CreateComment {
            content: anchor_text,
            result: anchor_slot,
        });
        self.emit(LirOp::AppendChild {
            parent: parent_slot,
            child: anchor_slot,
        });

        // Allocate memory slots for tracking
        let parent_mem = self.alloc_memory_slot(4);       // Store parent handle
        let anchor_mem = self.alloc_memory_slot(4);       // Store anchor handle
        let tracking_ptr_mem = self.alloc_memory_slot(4); // Store tracking array pointer
        let tracking_len_mem = self.alloc_memory_slot(4); // Store current length

        // Store parent and anchor for effects/updates
        self.emit(LirOp::StoreHandle {
            slot: parent_mem,
            from: parent_slot,
        });
        self.emit(LirOp::StoreHandle {
            slot: anchor_mem,
            from: anchor_slot,
        });

        // Calculate element size based on item type
        let element_size = self.compute_element_size(item_ty);

        // Classify the iterable expression
        let iterable_kind = self.classify_iterable(iterable);

        // Collect outer items from local_bindings (for nested for-loops)
        // These need to be stored to memory so inner blocks can access them
        let outer_items: Vec<(LocalId, SlotId, Ty)> = self
            .local_bindings
            .iter()
            .map(|(id, (slot, ty))| (*id, *slot, *ty))
            .collect();

        // For each outer item, allocate a memory slot and emit store
        let mut outer_item_mem_slots: HashMap<crate::ids::LocalId, (crate::types::Ty, SlotId)> = HashMap::new();
        for (outer_id, outer_slot, outer_ty) in &outer_items {
            let outer_element_size = self.compute_element_size(*outer_ty);
            let mem_slot = self.alloc_memory_slot(outer_element_size);
            // Emit store in current block (the outer for-item-mount block context)
            self.emit(LirOp::StoreHandle {
                slot: mem_slot,
                from: *outer_slot,
            });
            outer_item_mem_slots.insert(*outer_id, (*outer_ty, mem_slot));
        }

        // Create for-item-mount block: (parent, item_ptr) -> ()
        let mount_block = self.create_for_item_mount_block(item, item_ty, body, &outer_item_mem_slots);

        // Create for-item-unmount block: (node) -> ()
        let unmount_block = self.create_for_item_unmount_block();

        // For signal iterables, create update block and register effect
        // For field access iterables, no effect needed (parent loop handles reactivity)
        let _maybe_signal_def_id = match &iterable_kind {
            IterableKind::Signal(def_id) => {
                let update_block = self.create_for_update_block(
                    *def_id,
                    element_size,
                    mount_block,
                    unmount_block,
                    parent_mem,
                    anchor_mem,
                    tracking_ptr_mem,
                    tracking_len_mem,
                );
                // Register effect for list signal updates
                self.effects.push(LirBlockEffect {
                    id: self.effects.len() as u32,
                    dependencies: vec![*def_id],
                    update_block,
                });
                Some(*def_id)
            }
            IterableKind::FieldAccess { .. } | IterableKind::Unsupported => None,
        };

        // === Emit inline loop ops for initial mount ===
        // Temp slots for loop variables
        let list_ptr = self.alloc_temp_slot();
        let list_len = self.alloc_temp_slot();
        let tracking_arr = self.alloc_temp_slot();
        let index = self.alloc_temp_slot();
        let item_ptr = self.alloc_temp_slot();
        let break_cond = self.alloc_temp_slot();
        let arr_size = self.alloc_temp_slot();

        // Load list (ptr, len) based on iterable kind
        match &iterable_kind {
            IterableKind::Signal(signal_def_id) => {
                self.emit(LirOp::LoadList {
                    signal: *signal_def_id,
                    ptr_result: list_ptr,
                    len_result: list_len,
                });
            }
            IterableKind::FieldAccess { base_local, field_idx, record_def } => {
                self.emit(LirOp::LoadListFromLocal {
                    base_local: *base_local,
                    field_idx: *field_idx,
                    record_def: *record_def,
                    ptr_result: list_ptr,
                    len_result: list_len,
                });
            }
            IterableKind::Unsupported => {
                panic!("Unsupported for-loop iterable expression");
            }
        }

        // Store len in tracking_len_mem
        self.emit(LirOp::StoreHandle {
            slot: tracking_len_mem,
            from: list_len,
        });

        // Allocate tracking array: len * 16 bytes (each entry: node_handle, key, ...)
        self.emit(LirOp::MulConst {
            slot: list_len,
            constant: 16,
            result: arr_size,
        });
        self.emit(LirOp::Alloc {
            size: arr_size,
            align: 4,
            result: tracking_arr,
        });

        // Store tracking ptr in tracking_ptr_mem
        self.emit(LirOp::StoreHandle {
            slot: tracking_ptr_mem,
            from: tracking_arr,
        });

        // Init index to 0 and compute initial break_cond (exit immediately if list is empty)
        self.emit(LirOp::SetSlot { slot: index, value: 0 });
        self.emit(LirOp::GeU {
            index,
            len: list_len,
            result: break_cond,
        });

        // Build loop body ops
        // Order: compute item ptr, do work, increment, then compute break condition for NEXT iteration
        let mut loop_body = Vec::new();

        // Compute item_ptr = list_ptr + index * element_size
        loop_body.push(LirOp::ComputeItemPtr {
            base: list_ptr,
            index,
            element_size,
            result: item_ptr,
        });

        // Call mount_block(parent, item_ptr)
        loop_body.push(LirOp::CallBlock2 {
            block: mount_block,
            param0: parent_slot,
            param1: item_ptr,
        });

        // TODO: Store node handle and key in tracking array

        // Increment index
        loop_body.push(LirOp::IncrSlot { slot: index });

        // Check break condition for NEXT iteration: index >= list_len
        loop_body.push(LirOp::GeU {
            index,
            len: list_len,
            result: break_cond,
        });

        // Emit the loop
        self.emit(LirOp::Loop {
            break_cond,
            body_ops: loop_body,
        });
    }

    /// Classify an iterable expression for a for-loop.
    fn classify_iterable(&self, expr: &LirExpr) -> IterableKind {
        match &expr.kind {
            LirExprKind::SignalRead(def_id) => {
                IterableKind::Signal(*def_id)
            }
            LirExprKind::Field { base, field_idx } => {
                // Check if base is a Local (loop variable)
                if let LirExprKind::Local(local_id) = &base.kind {
                    // Get the record type from the base expression
                    if let crate::types::InternedTyKind::Adt(record_def) = self.ctx.ty_kind(base.ty) {
                        return IterableKind::FieldAccess {
                            base_local: *local_id,
                            field_idx: *field_idx,
                            record_def: *record_def,
                        };
                    }
                }
                IterableKind::Unsupported
            }
            _ => IterableKind::Unsupported,
        }
    }

    /// Compute the size of an element type in bytes.
    fn compute_element_size(&self, ty: crate::types::Ty) -> u32 {
        use crate::types::InternedTyKind;
        match self.ctx.ty_kind(ty) {
            InternedTyKind::S8 | InternedTyKind::U8 | InternedTyKind::Bool => 1,
            InternedTyKind::S16 | InternedTyKind::U16 => 2,
            InternedTyKind::S32 | InternedTyKind::U32 | InternedTyKind::F32 | InternedTyKind::Char => 4,
            InternedTyKind::S64 | InternedTyKind::U64 | InternedTyKind::F64 => 8,
            InternedTyKind::String | InternedTyKind::List(_) => 8, // ptr + len
            InternedTyKind::Adt(def_id) => {
                // For records, compute total size from fields
                if let Some(record) = self.ctx.defs.as_record(*def_id) {
                    let mut size = 0u32;
                    for &field_def_id in &record.fields {
                        // Look up field type from its DefId
                        if let crate::definitions::DefKind::Field(f) = self.ctx.defs.kind(field_def_id) {
                            size += self.compute_element_size(f.ty);
                        }
                    }
                    // Align to 4 bytes
                    (size + 3) & !3
                } else {
                    4 // Enum discriminant
                }
            }
            InternedTyKind::Tuple(elems) => {
                let mut size = 0u32;
                for elem_ty in elems {
                    size += self.compute_element_size(*elem_ty);
                }
                (size + 3) & !3
            }
            _ => 4, // Default
        }
    }

    /// Create the for-item-mount block.
    /// Called for each item with: (parent: i32, item_ptr: i32) -> ()
    fn create_for_item_mount_block(
        &mut self,
        item: crate::ids::LocalId,
        item_ty: crate::types::Ty,
        body: &[LirNode],
        outer_item_mem_slots: &HashMap<crate::ids::LocalId, (crate::types::Ty, SlotId)>,
    ) -> BlockId {
        self.start_block();

        // In the body block:
        // - SlotId(0) is parent
        // - SlotId(1) is item pointer (the captured local)
        let block_parent = SlotId(0);
        let item_ptr_slot = SlotId(1);

        // Load outer items from memory and update local_bindings
        // Track the loaded slots so we can set local_to_slot on the block
        let mut outer_loaded_slots: HashMap<crate::ids::LocalId, SlotId> = HashMap::new();
        for (outer_id, (outer_ty, mem_slot)) in outer_item_mem_slots {
            let temp_slot = self.alloc_temp_slot();
            self.emit(LirOp::LoadHandle {
                slot: *mem_slot,
                to: temp_slot,
            });
            // Update local_bindings to use the loaded temp slot
            self.local_bindings.insert(*outer_id, (temp_slot, *outer_ty));
            outer_loaded_slots.insert(*outer_id, temp_slot);
        }

        // Store the current item binding info for expression lowering
        self.local_bindings.insert(item, (item_ptr_slot, item_ty));

        // Update outer_item_memory so nested blocks (if-branches, etc.) can access for-loop items
        // Save old outer_item_memory and restore after lowering body
        let old_outer_item_memory = std::mem::take(&mut self.outer_item_memory);

        // Copy inherited outer items (they're already stored to memory)
        for (outer_id, (outer_ty, mem_slot)) in outer_item_mem_slots {
            self.outer_item_memory.insert(*outer_id, (*outer_ty, *mem_slot));
        }

        // Store current item to memory so nested if-branches can access it
        let item_element_size = self.compute_element_size(item_ty);
        let item_mem_slot = self.alloc_memory_slot(item_element_size);
        self.emit(LirOp::StoreHandle {
            slot: item_mem_slot,
            from: item_ptr_slot,
        });
        self.outer_item_memory.insert(item, (item_ty, item_mem_slot));

        // Lower each body node
        for node in body {
            self.lower_node(node, block_parent);
        }

        // Restore outer_item_memory
        self.outer_item_memory = old_outer_item_memory;

        // Clean up bindings
        self.local_bindings.remove(&item);
        for outer_id in outer_item_mem_slots.keys() {
            self.local_bindings.remove(outer_id);
        }

        let block_id = self.finish_block_named("for-item-mount");

        // Set captured_locals for this block only - maps the item LocalId to slot 1 (item_ptr param)
        // Also set local_to_slot for outer items that were loaded from memory
        if let Some(block) = self.blocks.iter_mut().find(|b| b.id == block_id) {
            block.captured_locals.insert(item, 1); // slot 1 = item_ptr parameter
            block.local_to_slot = outer_loaded_slots;
        }

        block_id
    }

    /// Create the for-item-unmount block.
    /// Called to remove a rendered item: (node: i32) -> ()
    fn create_for_item_unmount_block(&mut self) -> BlockId {
        self.start_block();

        // SlotId(0) is the node to remove
        let node_slot = SlotId(0);
        self.emit(LirOp::Remove { node: node_slot });

        self.finish_block_named("for-item-unmount")
    }

    /// Create the for-update block.
    /// Handles list updates: unmounts old items, mounts new items.
    fn create_for_update_block(
        &mut self,
        signal: DefId,
        element_size: u32,
        mount_block: BlockId,
        unmount_block: BlockId,
        parent_mem: SlotId,
        _anchor_mem: SlotId,
        tracking_ptr_mem: SlotId,
        tracking_len_mem: SlotId,
    ) -> BlockId {
        self.start_block();

        // Temp slots
        let parent_slot = self.alloc_temp_slot();
        let old_tracking_ptr = self.alloc_temp_slot();
        let old_len = self.alloc_temp_slot();
        let new_list_ptr = self.alloc_temp_slot();
        let new_len = self.alloc_temp_slot();
        let index = self.alloc_temp_slot();
        let break_cond = self.alloc_temp_slot();
        let tracked_entry_ptr = self.alloc_temp_slot();
        let node_handle = self.alloc_temp_slot();
        let item_ptr = self.alloc_temp_slot();
        let old_size = self.alloc_temp_slot();
        let new_tracking_ptr = self.alloc_temp_slot();
        let arr_size = self.alloc_temp_slot();

        // Load parent from memory
        self.emit(LirOp::LoadHandle {
            slot: parent_mem,
            to: parent_slot,
        });

        // Load old tracking info
        self.emit(LirOp::LoadHandle {
            slot: tracking_ptr_mem,
            to: old_tracking_ptr,
        });
        self.emit(LirOp::LoadHandle {
            slot: tracking_len_mem,
            to: old_len,
        });

        // Load new list from signal
        self.emit(LirOp::LoadList {
            signal,
            ptr_result: new_list_ptr,
            len_result: new_len,
        });

        // === Unmount loop: for each old item, call unmount ===
        self.emit(LirOp::SetSlot { slot: index, value: 0 });

        let mut unmount_body = Vec::new();
        // Check break: index >= old_len
        unmount_body.push(LirOp::GeU {
            index,
            len: old_len,
            result: break_cond,
        });
        // tracked_entry_ptr = old_tracking_ptr + index * 16
        unmount_body.push(LirOp::ComputeItemPtr {
            base: old_tracking_ptr,
            index,
            element_size: 16, // tracking entry size
            result: tracked_entry_ptr,
        });
        // node_handle = load from tracked_entry_ptr
        unmount_body.push(LirOp::LoadI32Addr {
            addr: tracked_entry_ptr,
            result: node_handle,
        });
        // Call unmount_block(node_handle)
        unmount_body.push(LirOp::CallBlock {
            block: unmount_block,
            parent: node_handle,
        });
        // index++
        unmount_body.push(LirOp::IncrSlot { slot: index });

        self.emit(LirOp::Loop {
            break_cond,
            body_ops: unmount_body,
        });

        // Free old tracking array
        self.emit(LirOp::MulConst {
            slot: old_len,
            constant: 16,
            result: old_size,
        });
        self.emit(LirOp::Free {
            ptr: old_tracking_ptr,
            size: old_size,
        });

        // Allocate new tracking array
        self.emit(LirOp::MulConst {
            slot: new_len,
            constant: 16,
            result: arr_size,
        });
        self.emit(LirOp::Alloc {
            size: arr_size,
            align: 4,
            result: new_tracking_ptr,
        });

        // Store new tracking info
        self.emit(LirOp::StoreHandle {
            slot: tracking_ptr_mem,
            from: new_tracking_ptr,
        });
        self.emit(LirOp::StoreHandle {
            slot: tracking_len_mem,
            from: new_len,
        });

        // === Mount loop: for each new item, call mount ===
        self.emit(LirOp::SetSlot { slot: index, value: 0 });

        let mut mount_body = Vec::new();
        // Check break: index >= new_len
        mount_body.push(LirOp::GeU {
            index,
            len: new_len,
            result: break_cond,
        });
        // item_ptr = new_list_ptr + index * element_size
        mount_body.push(LirOp::ComputeItemPtr {
            base: new_list_ptr,
            index,
            element_size,
            result: item_ptr,
        });
        // Call mount_block(parent, item_ptr)
        mount_body.push(LirOp::CallBlock2 {
            block: mount_block,
            param0: parent_slot,
            param1: item_ptr,
        });
        // TODO: Store node handle in tracking array
        // index++
        mount_body.push(LirOp::IncrSlot { slot: index });

        self.emit(LirOp::Loop {
            break_cond,
            body_ops: mount_body,
        });

        self.finish_block_named("for-update")
    }

    /// Create an update block for dynamic text.
    fn create_text_update_block(&mut self, text_mem_slot: SlotId, expr_id: ExprId) -> BlockId {
        self.start_block();

        // Load the text node handle from memory
        let text_slot = self.alloc_temp_slot();
        self.emit(LirOp::LoadHandle {
            slot: text_mem_slot,
            to: text_slot,
        });

        // Update text content with new value
        self.emit(LirOp::SetTextContent {
            node: text_slot,
            expr: expr_id,
        });

        self.finish_block_named("text-update")
    }

    /// Create an update block for dynamic attribute binding.
    fn create_attribute_update_block(
        &mut self,
        elem_mem_slot: SlotId,
        name_id: StringId,
        expr_id: ExprId,
    ) -> BlockId {
        self.start_block();

        // Load the element handle from memory
        let elem_slot = self.alloc_temp_slot();
        self.emit(LirOp::LoadHandle {
            slot: elem_mem_slot,
            to: elem_slot,
        });

        // Update attribute with new value
        self.emit(LirOp::SetAttribute {
            node: elem_slot,
            name: name_id,
            expr: expr_id,
        });

        self.finish_block_named("attr-update")
    }

    /// Create mount and unmount blocks for a branch with node tracking.
    /// Returns (mount_block, unmount_block).
    ///
    /// The mount block:
    /// - Creates the branch's nodes
    /// - Stores the first node's handle in content_mem for later removal
    /// - Inserts nodes after the anchor
    ///
    /// The unmount block:
    /// - Loads the stored handle from content_mem
    /// - Removes the node from DOM
    fn create_branch_with_tracking(
        &mut self,
        nodes: &[LirNode],
        _parent_slot: SlotId, // Outer parent - not used in block functions
        anchor_mem: SlotId,
        content_mem: SlotId,
    ) -> (BlockId, BlockId) {
        // Capture outer_item_memory before starting the block
        // (These are for-loop items that need to be accessible in this block)
        let outer_items_snapshot: Vec<(crate::ids::LocalId, crate::types::Ty, SlotId)> = self
            .outer_item_memory
            .iter()
            .map(|(id, (ty, slot))| (*id, *ty, *slot))
            .collect();

        // === Mount block ===
        // In block functions, parent is passed as first parameter (SlotId(0))
        self.start_block();
        let block_parent = SlotId(0); // Parent is block function's first parameter

        // Load for-loop items from memory at start of block
        let mut loaded_items: HashMap<crate::ids::LocalId, SlotId> = HashMap::new();
        for (local_id, _ty, mem_slot) in &outer_items_snapshot {
            let temp_slot = self.alloc_temp_slot();
            self.emit(LirOp::LoadHandle {
                slot: *mem_slot,
                to: temp_slot,
            });
            loaded_items.insert(*local_id, temp_slot);
        }

        if !nodes.is_empty() {
            // Load anchor for InsertAfter
            let anchor_temp = self.alloc_temp_slot();
            self.emit(LirOp::LoadHandle {
                slot: anchor_mem,
                to: anchor_temp,
            });

            // For each root node, create it and insert after anchor
            // We track the first node for unmount (simplified - assumes single root)
            let mut first_node_slot: Option<SlotId> = None;

            for node in nodes {
                // Create a temp slot for this node
                let node_slot = self.alloc_temp_slot();

                // Lower the node - this emits CreateElement/CreateText etc.
                // We need to intercept the result slot
                match &node.kind {
                    LirNodeKind::Element { tag, children, static_bindings, .. } => {
                        let tag_id = self.intern_string(tag);
                        self.emit(LirOp::CreateElement {
                            tag: tag_id,
                            result: node_slot,
                        });

                        // Static attributes
                        for binding in static_bindings {
                            let name_id = self.intern_string(&binding.name);
                            let expr_id = self.intern_expr(&binding.value);
                            self.emit(LirOp::SetAttribute {
                                node: node_slot,
                                name: name_id,
                                expr: expr_id,
                            });
                        }

                        // Insert after anchor (use block_parent, not outer parent_slot)
                        self.emit(LirOp::InsertAfter {
                            parent: block_parent,
                            node: node_slot,
                            anchor: anchor_temp,
                        });

                        // Recurse for children
                        for child in children {
                            self.lower_node(child, node_slot);
                        }
                    }
                    LirNodeKind::StaticText(text) => {
                        let content_id = self.intern_string(text);
                        self.emit(LirOp::CreateText {
                            content: content_id,
                            result: node_slot,
                        });
                        self.emit(LirOp::InsertAfter {
                            parent: block_parent,
                            node: node_slot,
                            anchor: anchor_temp,
                        });
                    }
                    LirNodeKind::DynamicText { effect_id } => {
                        // Get expression from effect
                        let expr = self
                            .find_tree_effect(*effect_id)
                            .map(|e| e.expr.clone())
                            .unwrap_or_else(|| {
                                LirExpr::new(
                                    LirExprKind::Literal(
                                        LirLiteral::String(String::new()),
                                    ),
                                    crate::types::Ty::STRING,
                                )
                            });
                        let expr_id = self.intern_expr(&expr);
                        self.emit(LirOp::CreateTextDynamic {
                            expr: expr_id,
                            result: node_slot,
                        });
                        self.emit(LirOp::InsertAfter {
                            parent: block_parent,
                            node: node_slot,
                            anchor: anchor_temp,
                        });
                    }
                    _ => {
                        // For If/For nodes, use regular lowering with block's parent
                        self.lower_node(node, block_parent);
                    }
                }

                // Track first node for unmount
                if first_node_slot.is_none() {
                    first_node_slot = Some(node_slot);
                }
            }

            // Store first node handle for later unmount
            if let Some(first_slot) = first_node_slot {
                self.emit(LirOp::StoreHandle {
                    slot: content_mem,
                    from: first_slot,
                });
            }
        }

        let mount_block = self.finish_block_named("if-branch-mount");

        // Set local_to_slot for loaded for-loop items
        if !loaded_items.is_empty() {
            if let Some(block) = self.blocks.iter_mut().find(|b| b.id == mount_block) {
                block.local_to_slot = loaded_items.clone();
            }
        }

        // === Unmount block ===
        self.start_block();

        // Load for-loop items from memory (unmount might need them for cleanup)
        let mut unmount_loaded_items: HashMap<crate::ids::LocalId, SlotId> = HashMap::new();
        for (local_id, _ty, mem_slot) in &outer_items_snapshot {
            let temp_slot = self.alloc_temp_slot();
            self.emit(LirOp::LoadHandle {
                slot: *mem_slot,
                to: temp_slot,
            });
            unmount_loaded_items.insert(*local_id, temp_slot);
        }

        if !nodes.is_empty() {
            // Load the stored node handle
            let node_temp = self.alloc_temp_slot();
            self.emit(LirOp::LoadHandle {
                slot: content_mem,
                to: node_temp,
            });

            // Remove it from DOM
            self.emit(LirOp::Remove { node: node_temp });
        }

        let unmount_block = self.finish_block_named("if-branch-unmount");

        // Set local_to_slot for unmount block too
        if !unmount_loaded_items.is_empty() {
            if let Some(block) = self.blocks.iter_mut().find(|b| b.id == unmount_block) {
                block.local_to_slot = unmount_loaded_items;
            }
        }

        (mount_block, unmount_block)
    }

    /// Create the update block for an if statement with proper unmount handling.
    ///
    /// State machine:
    /// - active_flag: 0 = else/nothing mounted, 1 = then mounted
    ///
    /// Logic (called only after initial mount, so something is always mounted):
    /// - If cond true && old_state == 1: do nothing (already then)
    /// - If cond true && old_state == 0: unmount else (if exists), mount then
    /// - If cond false && old_state == 0: do nothing (already else)
    /// - If cond false && old_state == 1: unmount then, mount else
    fn create_if_update_block_with_unmount(
        &mut self,
        condition: &LirExpr,
        _anchor_mem: SlotId,
        active_flag: SlotId,
        parent_mem: SlotId, // Memory slot containing parent handle
        then_mount: BlockId,
        then_unmount: BlockId,
        else_mount: Option<BlockId>,
        else_unmount: Option<BlockId>,
    ) -> BlockId {
        self.start_block();

        // Load parent from memory (stored during initial mount)
        // This is needed because effects call this block without knowing the parent
        let parent_slot = self.alloc_temp_slot();
        self.emit(LirOp::LoadHandle {
            slot: parent_mem,
            to: parent_slot,
        });

        // Evaluate condition to get new_state (1 = true, 0 = false)
        let cond_slot = self.alloc_temp_slot();
        let cond_expr = self.intern_expr(condition);
        self.emit(LirOp::EvalExpr {
            expr: cond_expr,
            result: cond_slot,
        });

        // Load old_state
        let old_state = self.alloc_temp_slot();
        self.emit(LirOp::LoadI32 {
            slot: active_flag,
            to: old_state,
        });

        // Build ops for switching to then branch
        let mut switch_to_then_ops = Vec::new();
        // Unmount else if it exists
        if let Some(block) = else_unmount {
            switch_to_then_ops.push(LirOp::CallBlock { block, parent: parent_slot });
        }
        // Mount then
        switch_to_then_ops.push(LirOp::CallBlock { block: then_mount, parent: parent_slot });
        // Store new state
        switch_to_then_ops.push(LirOp::StoreI32 { slot: active_flag, value: 1 });

        // Build ops for switching to else branch
        let mut switch_to_else_ops = Vec::new();
        // Unmount then
        switch_to_else_ops.push(LirOp::CallBlock { block: then_unmount, parent: parent_slot });
        // Mount else if it exists
        if let Some(block) = else_mount {
            switch_to_else_ops.push(LirOp::CallBlock { block, parent: parent_slot });
        }
        // Store new state
        switch_to_else_ops.push(LirOp::StoreI32 { slot: active_flag, value: 0 });

        // Compare: if cond (new wants then branch)
        // State: 0 = else/nothing, 1 = then (both are valid since initial mount handled separately)
        self.emit(LirOp::If {
            cond: cond_slot,
            then_ops: vec![
                // Want then branch
                // If old_state == 1 (truthy): already then, do nothing
                // If old_state == 0 (falsy): switch from else to then
                LirOp::If {
                    cond: old_state,
                    then_ops: vec![], // already then
                    else_ops: switch_to_then_ops, // was else, switch to then
                },
            ],
            else_ops: vec![
                // Want else branch
                // If old_state == 1 (truthy): switch from then to else
                // If old_state == 0 (falsy): already else, do nothing
                LirOp::If {
                    cond: old_state,
                    then_ops: switch_to_else_ops, // was then, switch to else
                    else_ops: vec![], // already else
                },
            ],
        });

        self.finish_block_named("if-update")
    }

    /// Lower an event handler to a block.
    fn lower_handler(&mut self, handler: &LirHandler) -> BlockId {
        self.start_block();

        // Lower handler body statements
        for stmt in &handler.body {
            self.lower_statement(stmt);
        }

        self.finish_block_named(format!("handler-{}", handler.event))
    }

    /// Lower a statement.
    fn lower_statement(&mut self, stmt: &LirStatement) {
        match stmt {
            LirStatement::Expr(expr) => {
                let result_slot = self.alloc_temp_slot();
                let expr_id = self.intern_expr(expr);
                self.emit(LirOp::EvalExpr {
                    expr: expr_id,
                    result: result_slot,
                });
            }
            LirStatement::SignalWrite { signal, value } => {
                // Allocate typed slot based on signal type
                let val_ty = self
                    .find_signal_type(*signal)
                    .map(|ty| self.ty_to_slot_val_type(ty))
                    .unwrap_or(SlotValType::I32);
                let value_slot = self.alloc_temp_slot_typed(val_ty);
                let value_expr = self.intern_expr(value);
                self.emit(LirOp::EvalExpr {
                    expr: value_expr,
                    result: value_slot,
                });
                self.emit(LirOp::SignalWrite {
                    signal: *signal,
                    value: value_slot,
                });
                self.emit(LirOp::TriggerEffects { signal: *signal });
            }
            LirStatement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_slot = self.alloc_temp_slot();
                let cond_expr = self.intern_expr(condition);
                self.emit(LirOp::EvalExpr {
                    expr: cond_expr,
                    result: cond_slot,
                });

                let mut then_ops = Vec::new();
                let mut else_ops = Vec::new();

                // Build then ops
                std::mem::swap(&mut self.current_ops, &mut then_ops);
                for s in then_branch {
                    self.lower_statement(s);
                }
                std::mem::swap(&mut self.current_ops, &mut then_ops);

                // Build else ops
                if let Some(else_stmts) = else_branch {
                    std::mem::swap(&mut self.current_ops, &mut else_ops);
                    for s in else_stmts {
                        self.lower_statement(s);
                    }
                    std::mem::swap(&mut self.current_ops, &mut else_ops);
                }

                self.emit(LirOp::If {
                    cond: cond_slot,
                    then_ops,
                    else_ops,
                });
            }
        }
    }

    /// Collect signal dependencies from an expression.
    fn collect_dependencies(&self, expr: &LirExpr) -> Vec<DefId> {
        let mut deps = Vec::new();
        self.collect_deps_recursive(expr, &mut deps);
        deps
    }

    fn collect_deps_recursive(&self, expr: &LirExpr, deps: &mut Vec<DefId>) {
        match &expr.kind {
            LirExprKind::SignalRead(def_id) => {
                if !deps.contains(def_id) {
                    deps.push(*def_id);
                }
            }
            LirExprKind::Binary { lhs, rhs, .. } => {
                self.collect_deps_recursive(lhs, deps);
                self.collect_deps_recursive(rhs, deps);
            }
            LirExprKind::Unary { operand, .. } => {
                self.collect_deps_recursive(operand, deps);
            }
            LirExprKind::Call { args, .. } => {
                for arg in args {
                    self.collect_deps_recursive(arg, deps);
                }
            }
            LirExprKind::Field { base, .. } => {
                self.collect_deps_recursive(base, deps);
            }
            LirExprKind::Index { base, index } => {
                self.collect_deps_recursive(base, deps);
                self.collect_deps_recursive(index, deps);
            }
            LirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.collect_deps_recursive(condition, deps);
                self.collect_deps_recursive(then_expr, deps);
                self.collect_deps_recursive(else_expr, deps);
            }
            LirExprKind::VariantCtor { payload, .. } => {
                if let Some(p) = payload {
                    self.collect_deps_recursive(p, deps);
                }
            }
            // List/Record constructs - collect deps from elements/fields
            LirExprKind::ListConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_deps_recursive(elem, deps);
                }
            }
            LirExprKind::RecordConstruct { fields, .. } => {
                for field in fields {
                    self.collect_deps_recursive(field, deps);
                }
            }
            LirExprKind::TupleConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_deps_recursive(elem, deps);
                }
            }
            // Leaf nodes - no dependencies
            LirExprKind::Local(_)
            | LirExprKind::Def(_)
            | LirExprKind::Literal(_)
            | LirExprKind::EnumCase { .. }
            | LirExprKind::ListStatic { .. } => {}
        }
    }

    // === Helper methods ===

    fn start_block(&mut self) {
        // Save any in-progress ops to the stack
        if !self.current_ops.is_empty() {
            self.ops_stack.push(std::mem::take(&mut self.current_ops));
        }
        self.current_ops = Vec::new();
    }

    fn finish_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;

        let ops = std::mem::take(&mut self.current_ops);
        // Don't apply captured_locals automatically - only for-item-mount blocks
        // get captured_locals set manually after creation
        self.blocks.push(LirBlock {
            id,
            ops,
            captured_locals: HashMap::new(),
            local_to_slot: HashMap::new(),
        });

        // Restore previous ops from stack
        if let Some(prev_ops) = self.ops_stack.pop() {
            self.current_ops = prev_ops;
        }

        id
    }

    /// Finish the current block with a debug name.
    fn finish_block_named(&mut self, name: impl Into<String>) -> BlockId {
        let id = self.finish_block();
        let name_str = name.into();
        self.ctx.set_block_name(self.component_id, id, name_str);
        id
    }

    fn emit(&mut self, op: LirOp) {
        self.current_ops.push(op);
    }

    fn alloc_temp_slot(&mut self) -> SlotId {
        self.alloc_temp_slot_typed(SlotValType::I32)
    }

    fn alloc_temp_slot_typed(&mut self, val_ty: SlotValType) -> SlotId {
        let id = SlotId(self.next_slot);
        self.next_slot += 1;
        self.slots.push(SlotInfo {
            id,
            kind: SlotKind::Temp {
                local_idx: id.0, // Will be reassigned during codegen
            },
            val_ty,
        });
        id
    }

    fn alloc_memory_slot(&mut self, size: u32) -> SlotId {
        let id = SlotId(self.next_slot);
        self.next_slot += 1;

        // Align to 4 bytes
        let align = 4;
        let offset = (self.next_memory_offset + align - 1) & !(align - 1);
        self.next_memory_offset = offset + size;

        self.slots.push(SlotInfo {
            id,
            kind: SlotKind::Memory { offset, size },
            val_ty: SlotValType::I32, // Memory slots are accessed as i32
        });
        id
    }

    fn intern_string(&mut self, s: &str) -> StringId {
        if let Some(&id) = self.string_map.get(s) {
            return id;
        }
        let id = StringId(self.strings.len() as u32);
        self.strings.push(s.to_string());
        self.string_map.insert(s.to_string(), id);
        id
    }

    fn intern_expr(&mut self, expr: &LirExpr) -> ExprId {
        // For now, always add - could deduplicate later
        let id = ExprId(self.exprs.len() as u32);
        self.exprs.push(expr.clone());
        id
    }
}
