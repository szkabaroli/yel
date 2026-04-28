//! WASM GC type synthesis for reactive-runtime state.
//!
//! Emits the per-component concrete-typed mount-tree GC types: one
//! struct per `TreeBoundary` (Root / IfAnchor / IfBranch / ForAnchor /
//! ForIterBody) plus a companion `(array (mut (ref null <iter_body>)))`
//! per `ForAnchor`. Plus the per-component `$Comp_<Name>` struct and
//! the module-shared registry-handle types. All types live in rec
//! groups so cyclic struct ↔ array references resolve.

use std::collections::HashMap;
use wasm_encoder::{
    ArrayType, CompositeInnerType, CompositeType, FieldType, HeapType, RefType, StorageType,
    StructType, SubType, TypeSection, ValType,
};
use yel_core::ids::TreeBoundaryId;
use yel_core::lir::block::{ComponentTreeShape, LirSlotValType, TreeBoundaryKind, TreeFieldDecl};

/// Type-index assignments for one component's GC types.
///
/// Populated during type-section emission, read by every emission path
/// that needs to `struct.new` / `array.get` / etc.
#[derive(Debug, Default, Clone)]
pub struct GcTypeLayout {
    /// Type index of `$Comp_<Name>` — the struct holding this
    /// component's signals as fields. Set by `emit_component_struct_type`.
    pub component_struct_type_idx: Option<u32>,
    /// For each signal (index matches `LirComponent.signals`), the list
    /// of struct-field indices that hold its canonical-ABI slots.
    /// Single-slot signals get a one-element vec; multi-slot ones
    /// (string/list = (ptr, len), variant = (discr, payload…)) get one
    /// entry per ABI slot in order.
    pub signal_field_paths: Vec<Vec<u32>>,
    /// WASM global index of `(mut (ref null $CompHandleArr_<Name>))` —
    /// the registry array. Lazily allocated on first constructor call.
    pub registry_global: Option<u32>,
    /// WASM global index of `(mut i32)` `len` — current allocated
    /// length of the registry array (handles 0..len exist).
    pub registry_len_global: Option<u32>,
    /// WASM global index of `(mut i32)` `free_head` — head index of
    /// the free chain (next reusable handle), or `-1` if empty.
    pub registry_free_head_global: Option<u32>,
    /// WASM global index of `(mut i32)` carrying the **current** host
    /// handle for the in-flight mount/constructor call. Set on entry to
    /// the export wrapper (param 0) and read by `AddEventListener` op
    /// emission to encode `(handle << 16) | local_id`. Transient: only
    /// meaningful during mount/constructor execution. Not a singleton
    /// component-ref global — just an i32 carrying the registry index
    /// across the mount-internal call. WASM is single-threaded, so no
    /// concurrent-mount aliasing concern.
    pub current_handle_global: Option<u32>,
    /// Function-type index for `() -> (ref null $Comp_<i>)` — internal
    /// constructor signature. Internal callers use this to obtain a
    /// typed self ref directly without going through the host's
    /// `[resource-new]` round-trip.
    pub constructor_internal_type_idx: Option<u32>,
    /// Function-type index for the internal mount entry point. Either
    /// `(ref null $Comp_<i>, i32) -> ()` (non-container) or
    /// `(ref null $Comp_<i>, i32) -> i32` (container — returns the
    /// children-root DOM node id).
    pub mount_internal_type_idx: Option<u32>,
    /// Function-type index for `(ref null $Comp_<i>) -> ()` — internal
    /// unmount entry.
    pub unmount_internal_type_idx: Option<u32>,
    /// Number of static `MountComponent` sites in this component
    /// **outside** any for-loop body. Each gets a `(mut (ref null
    /// any))` retention field appended to `$Comp_<i>` so the parent
    /// instance keeps its child instances alive through the GC.
    /// `parent_retention_field_base` records the field index of the
    /// first such field; subsequent sites use consecutive field
    /// indices.
    pub parent_retention_count: u32,
    /// First struct-field index of the parent-retention region in
    /// `$Comp_<i>`. `None` when `parent_retention_count == 0`.
    pub parent_retention_field_base: Option<u32>,
    /// Index of the trailing `(mut i32)` field on `$Comp_<Name>` that
    /// caches the host's resource handle returned by `[resource-new]X`.
    /// The constructor writes it once after `[resource-new]`; every
    /// callback emit site reads it via `struct.get` to pass
    /// `borrow<Self>` back to the host.
    pub self_handle_field_idx: Option<u32>,

    /// Per-tree-boundary struct type index — the GC struct emitted for
    /// each boundary in the component's `ComponentTreeShape`. Keyed by
    /// `TreeBoundaryId`. Phase B.3: populated, not yet consumed by
    /// emission paths.
    pub tree_struct_type_idx: HashMap<TreeBoundaryId, u32>,
    /// Per-`ForAnchor`-boundary array type index — the GC array type
    /// `(array (mut (ref null <iter_body_struct>)))` used as the
    /// children-array element. Keyed by the *anchor* boundary id.
    pub tree_for_arr_type_idx: HashMap<TreeBoundaryId, u32>,
    /// Convenience: type index of the component's root tree boundary
    /// (also discoverable via `tree_struct_type_idx[shape.root_idx]`).
    pub tree_root_type_idx: Option<u32>,
    /// Index of the trailing `(mut (ref null $<comp>_tree_root))`
    /// field on `$Comp_<Name>`. `None` when the component has no
    /// body tree (e.g. `empty_module_carrier`). The constructor
    /// populates this field with a freshly-allocated root struct so
    /// every instance starts with a non-null typed root.
    ///
    pub tree_root_field_idx: Option<u32>,
    /// Per-block function-type indices, keyed by `BlockId`. Every
    /// emitted block (i.e. every block except the inlined mount
    /// block) has its own unique entry here — there are no longer
    /// shared `block_1p` / `block_2p_*` shape pools.
    /// Signature is
    /// `(ref $Comp, <i32 args from `params`...>, (ref null <bp_0>), ...) -> <ret>`,
    /// computed at type-section emission from the block's `params` slot
    /// valtypes plus its `boundary_params`. Blocks with neither
    /// `params` nor `boundary_params` use the legacy single-i32-parent
    /// shape `(ref $Comp, i32) -> ()`.
    pub block_dynamic_type_idx: HashMap<yel_core::ids::BlockId, u32>,
}

/// Emit, in a single rec group:
///  - `$Comp_<Name>`: signals-as-fields struct.
///  - `$CompHandle_<Name>`: registry-handle struct
///    `(struct (field $inst (mut (ref null $Comp))) (field $next (mut i32)))`.
///  - `$CompHandleArr_<Name>`: `(array (mut (ref null $CompHandle)))`.
///
/// The three types are co-defined so the handle struct can reference
/// the component struct, and the handle array can reference the handle
/// struct, without out-of-order resolution issues. Mutates `layout` in
/// place: writes `component_struct_type_idx`, `signal_field_paths`,
/// `handle_struct_type_idx`, `handle_array_type_idx`. Type indices are
/// assigned sequentially starting from `base_type_idx` — caller must
/// reserve **3** slots.
pub fn emit_component_struct_type(
    signal_slot_valtypes: &[Vec<ValType>],
    parent_retention_count: u32,
    types: &mut TypeSection,
    base_type_idx: u32,
    layout: &mut GcTypeLayout,
) -> u32 {
    // Pre-allocate indices so the rec group's forward refs resolve.
    let comp_idx = base_type_idx;

    // 1. $Comp_<Name>: one mutable field per signal ABI slot.
    let mut fields: Vec<FieldType> = Vec::new();
    let mut field_paths: Vec<Vec<u32>> = Vec::with_capacity(signal_slot_valtypes.len());
    for slots in signal_slot_valtypes {
        let mut path: Vec<u32> = Vec::with_capacity(slots.len());
        for vt in slots {
            let idx = fields.len() as u32;
            fields.push(FieldType {
                element_type: StorageType::Val(*vt),
                mutable: true,
            });
            path.push(idx);
        }
        field_paths.push(path);
    }
    // Per-instance retention region: one `(mut (ref null any))` field per
    // static `MountComponent` site outside any for-loop body. We use
    // `anyref` so the retention type does not need to forward-reference
    // the child component's `$Comp_<j>` struct (which may not yet be
    // declared when this rec group is emitted).
    let parent_retention_field_base = if parent_retention_count > 0 {
        let base = fields.len() as u32;
        for _ in 0..parent_retention_count {
            fields.push(FieldType {
                element_type: StorageType::Val(ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Abstract {
                        shared: false,
                        ty: wasm_encoder::AbstractHeapType::Any,
                    },
                })),
                mutable: true,
            });
        }
        Some(base)
    } else {
        None
    };
    // Trailing slot for the WIT resource handle returned by
    // `[resource-new]X`. Lives on the component struct (instead of in
    // a per-component linear-memory cell) so the host-handle stash and
    // every callback's `borrow<Self>` lift can route through the same
    // typed self ref everything else uses. `(mut i32)` because the
    // handle is the i32 the host gave us back from `[resource-new]`.
    let self_handle_field_idx = fields.len() as u32;
    fields.push(FieldType {
        element_type: StorageType::Val(ValType::I32),
        mutable: true,
    });

    // Trailing field: typed root of the mount-state tree. Holds a
    // nullable ref to the component's `<comp>_tree_root` boundary
    // struct. Inner boundaries (if-anchors, if-branches, for-anchors,
    // for-iter-bodies) are reached via the nested `SubBoundary` and
    // `ChildrenArray` fields on the root and on each inner boundary
    // struct — NOT via flat fields on `$Comp`. Functions that need
    // an inner boundary in scope receive it as a typed parameter
    // computed once at the call site (caller chains `struct.get`s
    // from `$self.tree`); the function then accesses fields via
    // `local.get $boundary_param` for O(1) reads/writes.
    let tree_root_field_idx = layout.tree_root_type_idx.map(|root_ty_idx| {
        let idx = fields.len() as u32;
        fields.push(FieldType {
            element_type: StorageType::Val(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(root_ty_idx),
            })),
            mutable: true,
        });
        idx
    });

    let comp_struct = StructType {
        fields: fields.into_boxed_slice(),
    };

    // Single-element rec group: `$Comp_<Name>` is the only type
    // declared here. The previously per-component handle struct and
    // handle array were unified into module-shared `$handle` /
    // `$handle-array` (see `emit_shared_handle_types`); registry
    // lookup recovers the typed `(ref $Comp_<Name>)` via `ref.cast`.
    let sub_types = vec![SubType {
        is_final: true,
        supertype_idx: None,
        composite_type: CompositeType {
            shared: false,
            inner: CompositeInnerType::Struct(comp_struct),
        },
    }];
    types.ty().rec(sub_types);

    layout.component_struct_type_idx = Some(comp_idx);
    layout.signal_field_paths = field_paths;
    layout.parent_retention_count = parent_retention_count;
    layout.parent_retention_field_base = parent_retention_field_base;
    layout.self_handle_field_idx = Some(self_handle_field_idx);
    layout.tree_root_field_idx = tree_root_field_idx;
    comp_idx
}

/// Emit the module-shared `$handle` / `$handle-array` types. Called
/// once before the per-component rec groups so registry helpers can
/// reference these indices freely from any component context.
///
/// Layout:
/// ```text
/// (type $handle (struct (field $inst (mut (ref null any)))
///                       (field $next (mut i32))))
/// (type $handle-array (array (mut (ref null $handle))))
/// ```
///
/// The `$inst` field uses `anyref` (instead of a typed component
/// struct ref) so a single handle/array type-pair can serve every
/// component's registry. Lookup sites recover the typed component
/// ref via `ref.cast (ref $Comp_<i>)`.
///
/// Reserves and returns the two type indices `(handle_idx, handle_arr_idx)`
/// starting at `base_type_idx`. Caller advances the cursor by 2.
pub fn emit_shared_handle_types(types: &mut TypeSection, base_type_idx: u32) -> (u32, u32) {
    let handle_idx = base_type_idx;
    let handle_arr_idx = base_type_idx + 1;

    let handle_struct = StructType {
        fields: Box::from([
            FieldType {
                element_type: StorageType::Val(ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Abstract {
                        shared: false,
                        ty: wasm_encoder::AbstractHeapType::Any,
                    },
                })),
                mutable: true,
            },
            FieldType {
                element_type: StorageType::Val(ValType::I32),
                mutable: true,
            },
        ]),
    };
    let handle_array = ArrayType(FieldType {
        element_type: StorageType::Val(ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Concrete(handle_idx),
        })),
        mutable: true,
    });

    types.ty().rec(vec![
        SubType {
            is_final: true,
            supertype_idx: None,
            composite_type: CompositeType {
                shared: false,
                inner: CompositeInnerType::Struct(handle_struct),
            },
        },
        SubType {
            is_final: true,
            supertype_idx: None,
            composite_type: CompositeType {
                shared: false,
                inner: CompositeInnerType::Array(handle_array),
            },
        },
    ]);

    (handle_idx, handle_arr_idx)
}

/// Number of type indices reserved by `emit_component_struct_type`
/// per component. Callers advance their type-index cursor by this
/// constant after the call.
pub const COMPONENT_TYPE_COUNT: u32 = 1;

/// Emit the per-component concrete-typed mount-tree GC types from a
/// `ComponentTreeShape`. Produces, in a single rec group:
/// - One struct type per `TreeBoundary` (Root / IfAnchor / IfBranch /
///   ForAnchor / ForIterBody), in `boundaries` order.
/// - One companion array type per `ForAnchor`, with element type
///   `(mut (ref null <iter_body_struct>))`.
///
/// Type-index assignment within the rec group:
/// - `base + i` — struct for boundary `i` (for `i` in `0..N` where
///   `N = boundaries.len()`).
/// - `base + N + j` — array companion for the `j`-th `ForAnchor` (in
///   anchor declaration order).
///
/// All structs/arrays in the rec group can forward-reference each
/// other so cyclic walks (anchor → array → iter-body → nested anchor
/// → ...) resolve.
///
/// Returns the number of types reserved (the caller advances its
/// cursor by this amount). Mutates `layout` in place: writes
/// `tree_struct_type_idx`, `tree_for_arr_type_idx`, and
/// `tree_root_type_idx`.
///
/// **Phase B.3**: types are declared but no emission path consumes the
/// indices yet. Mount/effect/fan-out emit migrate to typed walks in
/// later sub-phases (B.6–B.8).
pub fn emit_component_tree_types(
    shape: &ComponentTreeShape,
    types: &mut TypeSection,
    base_type_idx: u32,
    layout: &mut GcTypeLayout,
    ctx: &yel_core::context::CompilerContext,
    record_gc_types: &RecordGcTypes,
) -> u32 {
    if shape.boundaries.is_empty() {
        return 0;
    }

    let n = shape.boundaries.len() as u32;

    // Pass 1: assign struct type indices for every boundary, and array
    // type indices for every ForAnchor. Pre-assigning indices lets the
    // body of pass 2 reference targets that appear later in the rec
    // group without out-of-order resolution issues.
    for (i, b) in shape.boundaries.iter().enumerate() {
        layout
            .tree_struct_type_idx
            .insert(b.id, base_type_idx + i as u32);
    }
    let mut arr_count: u32 = 0;
    for b in &shape.boundaries {
        if matches!(b.kind, TreeBoundaryKind::ForAnchor { .. }) {
            layout
                .tree_for_arr_type_idx
                .insert(b.id, base_type_idx + n + arr_count);
            arr_count += 1;
        }
    }
    layout.tree_root_type_idx = Some(base_type_idx + shape.root_idx);

    // Pass 2: build the SubTypes in declaration order.
    let mut sub_types: Vec<SubType> = Vec::with_capacity((n + arr_count) as usize);

    // Boundary structs.
    for b in &shape.boundaries {
        let struct_ty = build_tree_boundary_struct(&b.fields, layout, ctx, record_gc_types);
        sub_types.push(SubType {
            is_final: true,
            supertype_idx: None,
            composite_type: CompositeType {
                shared: false,
                inner: CompositeInnerType::Struct(struct_ty),
            },
        });
    }

    // Per-ForAnchor companion array types.
    for b in &shape.boundaries {
        if let TreeBoundaryKind::ForAnchor { iter_body_idx, .. } = b.kind {
            let iter_body_id = TreeBoundaryId(iter_body_idx);
            let elem_struct_idx = layout.tree_struct_type_idx[&iter_body_id];
            let arr_ty = ArrayType(FieldType {
                element_type: StorageType::Val(ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(elem_struct_idx),
                })),
                mutable: true,
            });
            sub_types.push(SubType {
                is_final: true,
                supertype_idx: None,
                composite_type: CompositeType {
                    shared: false,
                    inner: CompositeInnerType::Array(arr_ty),
                },
            });
        }
    }

    types.ty().rec(sub_types);
    n + arr_count
}

/// Build the struct type for one tree-boundary by translating each
/// `TreeFieldDecl` into a `FieldType`. `layout` must already contain
/// the struct/array indices for every boundary referenced from the
/// fields (populated in pass 1 above).
fn build_tree_boundary_struct(
    fields: &[TreeFieldDecl],
    layout: &GcTypeLayout,
    ctx: &yel_core::context::CompilerContext,
    record_gc_types: &RecordGcTypes,
) -> StructType {
    let wasm_fields: Vec<FieldType> = fields
        .iter()
        .map(|f| match f {
            TreeFieldDecl::DomHandle { .. } => FieldType {
                element_type: StorageType::Val(ValType::I32),
                mutable: true,
            },
            TreeFieldDecl::ActiveTag { .. } => FieldType {
                element_type: StorageType::Val(ValType::I32),
                mutable: true,
            },
            TreeFieldDecl::LoopVar { val_ty, .. } => FieldType {
                element_type: StorageType::Val(slot_val_ty_to_val_ty(val_ty, ctx, record_gc_types)),
                mutable: true,
            },
            TreeFieldDecl::SubBoundary { target_idx, .. } => {
                let target_struct_idx = layout.tree_struct_type_idx[&TreeBoundaryId(*target_idx)];
                FieldType {
                    element_type: StorageType::Val(ValType::Ref(RefType {
                        nullable: true,
                        heap_type: HeapType::Concrete(target_struct_idx),
                    })),
                    mutable: true,
                }
            }
            TreeFieldDecl::ChildrenArray { arr_target_idx, .. } => {
                // ChildrenArray fields only appear on ForAnchor
                // boundaries. `arr_target_idx` is the iter-body
                // boundary id; the array's type idx is the for-anchor's
                // companion-array index. The synthesizer's invariant
                // (iter-body allocated immediately before anchor) lets
                // us recover the anchor id from the iter-body id.
                let arr_idx =
                    find_anchor_array_idx_for_iter_body(layout, TreeBoundaryId(*arr_target_idx));
                FieldType {
                    element_type: StorageType::Val(ValType::Ref(RefType {
                        nullable: true,
                        heap_type: HeapType::Concrete(arr_idx),
                    })),
                    mutable: true,
                }
            }
        })
        .collect();
    StructType {
        fields: wasm_fields.into_boxed_slice(),
    }
}

/// Look up the array type index for the for-anchor whose iter-body is
/// `iter_body_id`. Slow path used by `build_tree_boundary_struct` when
/// translating a `ChildrenArray` field. Relies on a side channel: the
/// synthesizer pairs each anchor with its iter-body, and the array
/// index is keyed in `layout.tree_for_arr_type_idx` by the anchor's
/// id. Since the anchor↔iter-body mapping isn't directly stored on
/// `GcTypeLayout`, we encode it implicitly via the anchor ordering:
/// anchor is always declared immediately *before* its iter-body in
/// `shape.boundaries`. This function is only correct when called from
/// `build_tree_boundary_struct`, where the same `shape` was used to
/// pre-compute indices.
///
/// To make this robust without threading `shape` through, we store
/// the inverse mapping during pass 1 of `emit_component_tree_types`.
/// See the call site for the population.
fn find_anchor_array_idx_for_iter_body(layout: &GcTypeLayout, iter_body_id: TreeBoundaryId) -> u32 {
    // Layout invariant: the iter-body's struct idx is one less than
    // the for-anchor's struct idx (synthesizer allocates iter-body
    // first, anchor second — see `tree_shape::synthesize`). So the
    // anchor's id is `iter_body_id - 1` in struct-index space, but
    // boundary ids are small ints stored in `tree_struct_type_idx`
    // — we can scan the for-arr map for whose key has struct idx ==
    // iter_body_id's idx + 1.
    let iter_body_struct_idx = layout.tree_struct_type_idx[&iter_body_id];
    layout
        .tree_for_arr_type_idx
        .iter()
        .find(|(anchor_id, _)| layout.tree_struct_type_idx[anchor_id] == iter_body_struct_idx + 1)
        .map(|(_, &arr_idx)| arr_idx)
        .expect("anchor↔iter-body invariant: anchor declared right after iter-body")
}

/// Map a `SlotValType` to the corresponding `wasm_encoder::ValType`
/// for boundary struct field emission.
fn slot_val_ty_to_val_ty(
    svt: &LirSlotValType,
    ctx: &yel_core::context::CompilerContext,
    record_gc_types: &RecordGcTypes,
) -> ValType {
    match svt {
        LirSlotValType::I32 => ValType::I32,
        LirSlotValType::I64 => ValType::I64,
        LirSlotValType::F32 => ValType::F32,
        LirSlotValType::F64 => ValType::F64,
        LirSlotValType::RefNull(idx) => ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Concrete(*idx),
        }),
        LirSlotValType::RefNullForBoundary(_) | LirSlotValType::RefNullForChildrenArray(_) => {
            unreachable!("tree-boundary loop-var field cannot use boundary ref types")
        }
        LirSlotValType::RefNullForListGc(_) => {
            unreachable!("GC list array ref not expected as a tree loop-var field type")
        }
        LirSlotValType::RefNullForRecord(record_ty) => {
            use yel_core::types::InternedTyKind;
            let def_id = match ctx.ty_kind(*record_ty) {
                InternedTyKind::Adt(d) => *d,
                _ => panic!("RefNullForRecord on non-Adt type"),
            };
            let &type_idx = record_gc_types
                .record_type_idx
                .get(&def_id)
                .expect("RefNullForRecord: missing record_type_idx");
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(type_idx),
            })
        }
    }
}

/// Layout record describing one named `global Foo { ... }` block's
/// per-block GC struct.
///
/// Globals are always singletons — no registry / handle / array
/// scaffolding. We only need the struct type plus the WASM self-global
/// holding the lazily-allocated singleton ref.
///
/// `property_field_paths[i]` holds the WASM struct-field indices for
/// the `i`-th property of the owning `GlobalDef.properties`. Empty vec
/// means the property is pointer-typed (record/tuple) and stays on the
/// legacy linear-memory path — same dispatch convention as
/// `GcTypeLayout.signal_field_paths`.
#[derive(Debug, Clone)]
pub struct GlobalsBlockLayout {
    /// DefId of the owning `global Foo { ... }` block. Read by Step 2
    /// helpers that resolve blocks by their owning global property.
    #[allow(dead_code)]
    pub block_def_id: yel_core::DefId,
    pub struct_type_idx: u32,
    pub self_global_idx: u32,
    /// Per-property field-index path inside the block's GC struct,
    /// indexed by property position in `GlobalDef.properties`. Empty
    /// vec marks pointer-typed properties that stay on the legacy
    /// memory path. Read by Step 2 read/write helpers.
    #[allow(dead_code)]
    pub property_field_paths: Vec<Vec<u32>>,
}

/// Emit one `(struct $globals_<block> ...)` GC type for a named global
/// block. Layout mirrors the per-component struct: one mutable field
/// per ABI slot of each property, in property-declaration order.
/// Pointer-typed properties (records/tuples) contribute zero fields
/// and get an empty `property_field_paths` entry.
///
/// Reserves exactly **1** type index. The caller is responsible for
/// bumping its cursor by 1 after the call.
pub fn emit_globals_struct_type(
    block_def_id: yel_core::DefId,
    prop_slot_valtypes: &[Vec<ValType>],
    types: &mut TypeSection,
    base_type_idx: u32,
) -> GlobalsBlockLayout {
    let mut fields: Vec<FieldType> = Vec::new();
    let mut property_field_paths: Vec<Vec<u32>> = Vec::with_capacity(prop_slot_valtypes.len());
    for slots in prop_slot_valtypes {
        let mut path: Vec<u32> = Vec::with_capacity(slots.len());
        for vt in slots {
            let idx = fields.len() as u32;
            fields.push(FieldType {
                element_type: StorageType::Val(*vt),
                mutable: true,
            });
            path.push(idx);
        }
        property_field_paths.push(path);
    }
    let struct_ty = StructType {
        fields: fields.into_boxed_slice(),
    };
    types.ty().struct_(struct_ty.fields.iter().copied());

    GlobalsBlockLayout {
        block_def_id,
        struct_type_idx: base_type_idx,
        self_global_idx: 0, // populated when the global section emits the self-global
        property_field_paths,
    }
}

// ============================================================================
// Phase 1 of records-to-GC migration: per-program record / tuple GC types.
// ============================================================================

/// Program-scope registry of GC struct types emitted for user-defined
/// records and (Phase 1: simplified-naming) tuples.
///
/// Populated once during type-section emission by
/// `emit_program_record_types`. Phase 1 only emits the types — no
/// consumer reads from this map yet (signal storage, field access,
/// constructors all stay on the legacy memory path). Phase 2+ migration
/// reads `record_type_idx[def_id]` at every `struct.new` / `struct.get`
/// / `struct.set` site.
#[derive(Debug, Default, Clone)]
pub struct RecordGcTypes {
    /// Record `DefId` → emitted GC struct type index in the module's
    /// type section. One entry per `Definitions::records()`.
    pub record_type_idx: std::collections::HashMap<yel_core::DefId, u32>,
    /// Record `DefId` → list of GC struct field indices, parallel to
    /// the record's `field_offsets`. `field_gc_indices[i]` is the GC
    /// field index for the `i`-th declared field. Today field indices
    /// are simply `0..N`, but we still keep the indirection so future
    /// changes (multi-slot variant payloads, padding fields) can shift
    /// the GC index without bleeding into call sites.
    pub field_gc_indices: std::collections::HashMap<yel_core::DefId, Vec<u32>>,
    /// Debug names emitted into the WASM name section's type subsection
    /// for each record's GC struct. Populated alongside the type indices
    /// so the name-section pass can emit them without a second pass over
    /// `Definitions`.
    pub type_names: Vec<(u32, String)>,
    /// Phase 3: shared `$fat_value` GC struct type
    /// `(struct (field $ptr (mut i32)) (field $len (mut i32)))`.
    /// Used to box string and `list<scalar>` values when they appear as
    /// fields of a single-level record (SLR) — a record whose every
    /// field is primitive, string, or `list<scalar>`. The record's GC
    /// struct stores `(ref null $fat_value)` for string/list fields;
    /// readers `struct.get` the box ref, then `struct.get` `ptr` / `len`
    /// to recover the (ptr, len) fat-pointer pair on stack.
    /// `None` if no records were emitted (no rec group built).
    pub fat_value_type_idx: Option<u32>,
    /// Phase 5a: per-list-element-type GC array type indices.
    /// Keyed by the **list `Ty`** itself (i.e. the `Ty` whose
    /// `InternedTyKind` is `List(elem)`), NOT by the element `Ty`.
    /// Keying on the list `Ty` matches the call sites that have a
    /// `list<T>` value's `Ty` in hand at `Index` / `ListConstruct`.
    /// Phase 5a: types are emitted but no consumer reads from this
    /// map yet — `Index` / `ListConstruct` still go through the
    /// inline-byte runtime helpers. Phase 5b+ flips consumers over.
    pub list_array_type_idx: std::collections::HashMap<yel_core::Ty, u32>,
    /// Phase 5a: per-tuple-type GC struct type indices, keyed by the
    /// tuple `Ty` (whose `InternedTyKind` is `Tuple(elements)`).
    /// One emitted struct per **distinct** tuple type, sharing types
    /// across components / records. Phase 5a only emits; Phase 5d
    /// switches `TupleConstruct` / tuple-`Field` to `struct.new` /
    /// `struct.get`.
    pub tuple_struct_type_idx: std::collections::HashMap<yel_core::Ty, u32>,
}

/// Emit, in a single program-scope rec group, one `(struct ...)` GC
/// type per user-defined record. All record types co-exist in one rec
/// group so a record field whose type is another record can reference
/// the inner record's type index via forward reference within the same
/// rec group — wasm rec groups resolve recursive references.
///
/// Phase 1 field-type rules (from the migration plan, §1):
/// - `bool` / `s8` / `s16` / `s32` / `u8` / `u16` / `u32` / `char` /
///   enum / `option<scalar>` / `result<scalar, scalar>` → single `i32`
///   field (the `signal_storage_valtypes` rules collapse all of these
///   to one i32 internally).
/// - `s64` / `u64` → `i64`.
/// - `f32` → `f32`, `f64` → `f64`.
/// - `string` → `anyref` (cast to `(ref $fat_string)` at access time;
///   open question 1 resolution).
/// - `list<T>` → `anyref` (Phase 5 makes these concrete `(array ...)`).
/// - Record / tuple field → `(ref null $<inner>_record)` for nested
///   records (forward reference resolved by the rec group), or
///   `anyref` for tuples (Phase 1 simplification: tuple GC types are
///   not emitted yet — owner-context plumbing deferred to Phase 4 per
///   the plan's allowance).
/// - Variant / option-with-payload / result-with-payload of an
///   aggregate → `anyref` (Phase 1 simplification — the canonical-ABI
///   join shape is multi-slot which doesn't fit in one struct field;
///   Phase 4 revisits per the plan's variant-payload open question).
///
/// Reserves exactly `defs.records().count()` type indices when
/// non-empty; advances the cursor by the same amount. Returns the
/// number of types reserved (caller advances its cursor by this
/// amount). When there are no user records, returns 0 and emits
/// nothing — the rec group itself is not emitted.
///
/// Naming: each emitted struct is named `$<lowercased_name>_record`
/// (e.g. record `Point` → `$point_record`). Names accumulate in
/// `RecordGcTypes::type_names` for the name-section pass.
pub fn emit_program_record_types(
    ctx: &yel_core::context::CompilerContext,
    types: &mut TypeSection,
    base_type_idx: u32,
    extra_seed_tys: &[yel_core::Ty],
) -> (u32, RecordGcTypes) {
    use std::collections::HashMap;
    use yel_core::definitions::DefKind;
    use yel_core::types::InternedTyKind;

    let record_def_ids: Vec<yel_core::DefId> = ctx.defs.records().collect();

    // Phase 5a: collect all list and tuple types referenced anywhere
    // in the program (signal types, record-field types, transitively
    // through nested compounds). The walk is conservative — it traverses
    // every ADT field and every type subterm — and dedupes by `Ty` so
    // each unique list element-type and tuple shape gets exactly one
    // emitted GC type.
    let (list_tys, tuple_tys) = collect_list_and_tuple_tys(ctx, extra_seed_tys);

    if record_def_ids.is_empty() && list_tys.is_empty() && tuple_tys.is_empty() {
        return (0, RecordGcTypes::default());
    }

    let mut registry = RecordGcTypes::default();

    // Phase 3: emit a shared `$fat_value` struct as the FIRST type in
    // the rec group. Records that contain string / `list<scalar>` fields
    // store those fields as `(ref null $fat_value)`; the box carries the
    // (ptr, len) fat-pointer pair as two i32 fields. Co-locating in the
    // rec group lets record types forward-reference its index.
    let fat_value_idx = base_type_idx;
    registry.fat_value_type_idx = Some(fat_value_idx);

    // Pre-assign type indices in a fixed order so every emission step
    // can forward-reference any other type within the rec group:
    //   [base+0]               fat_value
    //   [records_base..]       record structs
    //   [list_arrays_base..]   list element arrays
    //   [tuple_structs_base..] tuple structs
    let records_base = base_type_idx + 1;
    for (i, &def_id) in record_def_ids.iter().enumerate() {
        registry
            .record_type_idx
            .insert(def_id, records_base + i as u32);
    }
    let list_arrays_base = records_base + record_def_ids.len() as u32;
    for (i, &list_ty) in list_tys.iter().enumerate() {
        registry
            .list_array_type_idx
            .insert(list_ty, list_arrays_base + i as u32);
    }
    let tuple_structs_base = list_arrays_base + list_tys.len() as u32;
    for (i, &tuple_ty) in tuple_tys.iter().enumerate() {
        registry
            .tuple_struct_type_idx
            .insert(tuple_ty, tuple_structs_base + i as u32);
    }

    // Build each record's struct type. Field types follow the rules
    // documented above. We also accumulate `field_gc_indices` and the
    // name-section debug entries.
    let mut sub_types: Vec<SubType> = Vec::with_capacity(record_def_ids.len() + 1);

    // Phase 3: emit the shared `$fat_value` struct first.
    let fat_value_struct = StructType {
        fields: Box::from([
            FieldType {
                element_type: StorageType::Val(ValType::I32),
                mutable: true,
            },
            FieldType {
                element_type: StorageType::Val(ValType::I32),
                mutable: true,
            },
        ]),
    };
    sub_types.push(SubType {
        is_final: true,
        supertype_idx: None,
        composite_type: CompositeType {
            shared: false,
            inner: CompositeInnerType::Struct(fat_value_struct),
        },
    });
    registry
        .type_names
        .push((fat_value_idx, "fat_value".to_string()));
    for &def_id in &record_def_ids {
        let record_def = ctx
            .defs
            .as_record(def_id)
            .expect("DefId from defs.records() must resolve to RecordDef");

        let mut wasm_fields: Vec<FieldType> = Vec::with_capacity(record_def.fields.len());
        let mut gc_indices: Vec<u32> = Vec::with_capacity(record_def.fields.len());
        for &field_def_id in &record_def.fields {
            let field_def = match ctx.defs.kind(field_def_id) {
                yel_core::definitions::DefKind::Field(f) => f,
                _ => continue,
            };
            let field_idx = wasm_fields.len() as u32;
            let storage = record_field_storage_type(ctx, field_def.ty, &registry);
            wasm_fields.push(FieldType {
                element_type: StorageType::Val(storage),
                mutable: true,
            });
            gc_indices.push(field_idx);
        }

        let type_idx = registry.record_type_idx[&def_id];
        registry.field_gc_indices.insert(def_id, gc_indices);
        let raw_name = ctx.str(record_def.name);
        let lowered = raw_name.to_ascii_lowercase();
        registry
            .type_names
            .push((type_idx, format!("{}_record", lowered)));

        sub_types.push(SubType {
            is_final: true,
            supertype_idx: None,
            composite_type: CompositeType {
                shared: false,
                inner: CompositeInnerType::Struct(StructType {
                    fields: wasm_fields.into_boxed_slice(),
                }),
            },
        });
    }

    // Phase 5a: emit one `(array (mut <elem>))` per unique list type.
    // Element ValType chosen via `list_element_storage_type`:
    //   - scalars unboxed (i32 / i64 / f32 / f64)
    //   - string / list<...> → `(ref null $fat_value)` for now (Phase
    //     5e will switch list<list<T>> to a concrete inner array ref)
    //   - records (DTR) → `(ref null $<record>_record)`
    //   - tuples → `(ref null $tuple_<n>)`
    //   - option / result / variant → `anyref` (Phase 5e refines)
    for (idx, &list_ty) in list_tys.iter().enumerate() {
        let arr_idx = list_arrays_base + idx as u32;
        let elem_ty = match ctx.ty_kind(list_ty) {
            InternedTyKind::List(t) => *t,
            _ => unreachable!("collect_list_and_tuple_tys returns List Tys"),
        };
        let elem_storage = list_element_storage_type(ctx, elem_ty, &registry);
        let arr_ty = ArrayType(FieldType {
            element_type: StorageType::Val(elem_storage),
            mutable: true,
        });
        sub_types.push(SubType {
            is_final: true,
            supertype_idx: None,
            composite_type: CompositeType {
                shared: false,
                inner: CompositeInnerType::Array(arr_ty),
            },
        });
        let elem_name = list_elem_short_name(ctx, elem_ty);
        registry
            .type_names
            .push((arr_idx, format!("{}_list", elem_name)));
    }

    // Phase 5a: emit one `(struct ...)` per unique tuple type.
    for (i, &tuple_ty) in tuple_tys.iter().enumerate() {
        let struct_idx = tuple_structs_base + i as u32;
        let elem_tys: Vec<yel_core::Ty> = match ctx.ty_kind(tuple_ty) {
            InternedTyKind::Tuple(els) => els.clone(),
            _ => unreachable!("collect_list_and_tuple_tys returns Tuple Tys"),
        };
        let mut wasm_fields: Vec<FieldType> = Vec::with_capacity(elem_tys.len());
        for elem_ty in &elem_tys {
            // Tuple elements use the same per-field storage rules as
            // record fields. `record_field_storage_type` already handles
            // primitives → unboxed, string/list → `(ref null $fat_value)`,
            // records → concrete ref, nested tuples → concrete ref via
            // `registry.tuple_struct_type_idx`.
            let storage = record_field_storage_type(ctx, *elem_ty, &registry);
            wasm_fields.push(FieldType {
                element_type: StorageType::Val(storage),
                mutable: true,
            });
        }
        sub_types.push(SubType {
            is_final: true,
            supertype_idx: None,
            composite_type: CompositeType {
                shared: false,
                inner: CompositeInnerType::Struct(StructType {
                    fields: wasm_fields.into_boxed_slice(),
                }),
            },
        });
        registry
            .type_names
            .push((struct_idx, format!("tuple_{}", i)));
        let _ = DefKind::Record; // silence unused import lint
    }

    types.ty().rec(sub_types);
    let _ = HashMap::<yel_core::DefId, u32>::new(); // silence import lint when empty
    // Total reserved indices: 1 (fat_value) + N records + L lists + T tuples.
    let total = 1 + record_def_ids.len() + list_tys.len() + tuple_tys.len();
    (total as u32, registry)
}

/// Walk every signal type and every record-field type, recursively
/// descending into list / tuple / option / result subterms, and return
/// (1) every distinct `list<T>` `Ty` referenced and (2) every distinct
/// tuple `Ty` referenced. Output ordering is deterministic: insertion
/// order of first encounter so the assigned type indices are stable
/// across runs (`HashSet` would not be).
fn collect_list_and_tuple_tys(
    ctx: &yel_core::context::CompilerContext,
    extra_seed_tys: &[yel_core::Ty],
) -> (Vec<yel_core::Ty>, Vec<yel_core::Ty>) {
    use yel_core::definitions::DefKind;
    use yel_core::types::InternedTyKind;

    let mut list_seen: std::collections::HashSet<yel_core::Ty> = std::collections::HashSet::new();
    let mut tuple_seen: std::collections::HashSet<yel_core::Ty> = std::collections::HashSet::new();
    let mut list_order: Vec<yel_core::Ty> = Vec::new();
    let mut tuple_order: Vec<yel_core::Ty> = Vec::new();

    fn walk(
        ctx: &yel_core::context::CompilerContext,
        ty: yel_core::Ty,
        list_seen: &mut std::collections::HashSet<yel_core::Ty>,
        tuple_seen: &mut std::collections::HashSet<yel_core::Ty>,
        list_order: &mut Vec<yel_core::Ty>,
        tuple_order: &mut Vec<yel_core::Ty>,
    ) {
        match ctx.ty_kind(ty) {
            InternedTyKind::List(inner) => {
                if list_seen.insert(ty) {
                    list_order.push(ty);
                }
                walk(ctx, *inner, list_seen, tuple_seen, list_order, tuple_order);
            }
            InternedTyKind::Tuple(els) => {
                if tuple_seen.insert(ty) {
                    tuple_order.push(ty);
                }
                let els = els.clone();
                for e in els {
                    walk(ctx, e, list_seen, tuple_seen, list_order, tuple_order);
                }
            }
            InternedTyKind::Option(inner) => {
                walk(ctx, *inner, list_seen, tuple_seen, list_order, tuple_order);
            }
            InternedTyKind::Result { ok, err } => {
                if let Some(t) = ok {
                    walk(ctx, *t, list_seen, tuple_seen, list_order, tuple_order);
                }
                if let Some(t) = err {
                    walk(ctx, *t, list_seen, tuple_seen, list_order, tuple_order);
                }
            }
            InternedTyKind::Adt(_) => {
                // Records are walked separately below; don't recurse
                // into their fields here to avoid revisiting the same
                // record N times. Variants/enums carry no list/tuple
                // payloads we need to discover (variant payloads are
                // their own Tys handled when those Tys are referenced).
            }
            _ => {}
        }
    }

    // Walk every signal type.
    for (def_id, item) in ctx.defs.iter() {
        if let DefKind::Signal(s) = &item.kind {
            walk(
                ctx,
                s.ty,
                &mut list_seen,
                &mut tuple_seen,
                &mut list_order,
                &mut tuple_order,
            );
        }
        let _ = def_id;
    }

    // Walk every record field type.
    for def_id in ctx.defs.records() {
        let record_def = match ctx.defs.as_record(def_id) {
            Some(r) => r,
            None => continue,
        };
        for &field_def_id in &record_def.fields {
            if let DefKind::Field(f) = ctx.defs.kind(field_def_id) {
                walk(
                    ctx,
                    f.ty,
                    &mut list_seen,
                    &mut tuple_seen,
                    &mut list_order,
                    &mut tuple_order,
                );
            }
        }
    }

    // Walk every variant payload type.
    for def_id in ctx.defs.variants() {
        if let DefKind::Variant(v) = ctx.defs.kind(def_id) {
            let case_ids = v.cases.clone();
            for case_def_id in case_ids {
                if let DefKind::VariantCase(c) = ctx.defs.kind(case_def_id) {
                    if let Some(payload_ty) = c.payload {
                        walk(
                            ctx,
                            payload_ty,
                            &mut list_seen,
                            &mut tuple_seen,
                            &mut list_order,
                            &mut tuple_order,
                        );
                    }
                }
            }
        }
    }

    // Phase 5e.6: also walk caller-supplied extra seed types — used to
    // catch list types that appear only in LIR expressions (list literals
    // iterated by `for`, etc.) and have no Def-level reference.
    for &ty in extra_seed_tys {
        walk(
            ctx,
            ty,
            &mut list_seen,
            &mut tuple_seen,
            &mut list_order,
            &mut tuple_order,
        );
    }

    (list_order, tuple_order)
}

/// Map a list element type to its Phase 5a GC array element ValType.
/// Mirrors `record_field_storage_type` but for the *element* slot of
/// `(array (mut <elem>))`.
fn list_element_storage_type(
    ctx: &yel_core::context::CompilerContext,
    elem_ty: yel_core::Ty,
    registry: &RecordGcTypes,
) -> ValType {
    use yel_core::types::InternedTyKind;
    // Phase 5e.2: nested lists — when the element is itself a typed
    // GC array (list<scalar>, list<DTR-record>, list<list<...>>),
    // store a concrete `(ref null $<inner_arr>)` so callers can
    // `array.get` directly without going through $fat_value.
    if let InternedTyKind::List(_) = ctx.ty_kind(elem_ty) {
        if let Some(&inner_arr_idx) = registry.list_array_type_idx.get(&elem_ty) {
            return ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(inner_arr_idx),
            });
        }
    }
    // Phase 5e.3: tuples — store a concrete `(ref null $tuple_<n>)`
    // typed struct ref. The tuple struct type was emitted alongside
    // record types in this rec group.
    if let InternedTyKind::Tuple(_) = ctx.ty_kind(elem_ty) {
        if let Some(&tup_idx) = registry.tuple_struct_type_idx.get(&elem_ty) {
            return ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(tup_idx),
            });
        }
    }
    // Phase 5e.5: option<scalar-i32-fits> — reuse $fat_value as the
    // 2-i32 box (disc + payload). Only applies when the option's
    // canonical-ABI is exactly 2 i32 slots.
    if let InternedTyKind::Option(inner) = ctx.ty_kind(elem_ty) {
        let inner_fits = matches!(
            ctx.ty_kind(*inner),
            InternedTyKind::Bool
                | InternedTyKind::S8
                | InternedTyKind::S16
                | InternedTyKind::S32
                | InternedTyKind::U8
                | InternedTyKind::U16
                | InternedTyKind::U32
                | InternedTyKind::F32
                | InternedTyKind::Char
        ) || matches!(
            ctx.ty_kind(*inner),
            InternedTyKind::Adt(d) if matches!(ctx.defs.kind(*d), yel_core::definitions::DefKind::Enum(_))
        );
        if inner_fits {
            if let Some(fv) = registry.fat_value_type_idx {
                return ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(fv),
                });
            }
        }
    }
    // Otherwise reuse record-field rules: scalars unboxed, strings as
    // $fat_value, records as concrete refs, tuples as concrete refs,
    // option/result/variant as anyref.
    record_field_storage_type(ctx, elem_ty, registry)
}

/// Short, lowercased name fragment used in the emitted `<elem>_list`
/// debug name for a list array type.
fn list_elem_short_name(ctx: &yel_core::context::CompilerContext, elem_ty: yel_core::Ty) -> String {
    use yel_core::definitions::DefKind;
    use yel_core::types::InternedTyKind;
    match ctx.ty_kind(elem_ty) {
        InternedTyKind::S8 => "s8".into(),
        InternedTyKind::S16 => "s16".into(),
        InternedTyKind::S32 => "s32".into(),
        InternedTyKind::S64 => "s64".into(),
        InternedTyKind::U8 => "u8".into(),
        InternedTyKind::U16 => "u16".into(),
        InternedTyKind::U32 => "u32".into(),
        InternedTyKind::U64 => "u64".into(),
        InternedTyKind::F32 => "f32".into(),
        InternedTyKind::F64 => "f64".into(),
        InternedTyKind::Bool => "bool".into(),
        InternedTyKind::Char => "char".into(),
        InternedTyKind::String => "string".into(),
        InternedTyKind::List(_) => "list".into(),
        InternedTyKind::Tuple(_) => "tuple".into(),
        InternedTyKind::Option(_) => "option".into(),
        InternedTyKind::Result { .. } => "result".into(),
        InternedTyKind::Adt(def_id) => match ctx.defs.kind(*def_id) {
            DefKind::Record(r) => ctx.str(r.name).to_ascii_lowercase(),
            DefKind::Enum(e) => ctx.str(e.name).to_ascii_lowercase(),
            DefKind::Variant(v) => ctx.str(v.name).to_ascii_lowercase(),
            _ => "elem".into(),
        },
        _ => "elem".into(),
    }
}

/// Map a record-field type to its Phase 1 GC storage `ValType`.
///
/// The mapping intentionally collapses every "complicated" Yel type to
/// `anyref` for Phase 1 — Phase 1 only needs the types to *exist* in
/// the module so the WAT-inspection test can find them; no consumer
/// reads from these fields yet, so a coarse-but-future-proof shape
/// keeps the emission deterministic and side-effect-free.
/// Phase 5e.6: gate for whether a list type can be stored as a typed
/// `(ref null $<elem>_list)` GC array on a record/tuple field. Mirrors
/// `repr.rs::is_scalar_list_ty` — kept here to avoid the cyclic
/// dependency between `gc_types` and `WasmPackageBuilder` methods.
fn is_gc_eligible_list_ty(
    ctx: &yel_core::context::CompilerContext,
    ty: yel_core::Ty,
) -> bool {
    use yel_core::definitions::DefKind;
    use yel_core::types::InternedTyKind;
    let elem = match ctx.ty_kind(ty) {
        InternedTyKind::List(e) => *e,
        _ => return false,
    };
    if matches!(
        ctx.ty_kind(elem),
        InternedTyKind::Bool
            | InternedTyKind::S8
            | InternedTyKind::S16
            | InternedTyKind::S32
            | InternedTyKind::U8
            | InternedTyKind::U16
            | InternedTyKind::U32
            | InternedTyKind::S64
            | InternedTyKind::U64
            | InternedTyKind::F32
            | InternedTyKind::F64
            | InternedTyKind::Char
    ) || matches!(
        ctx.ty_kind(elem),
        InternedTyKind::Adt(d) if matches!(ctx.defs.kind(*d), DefKind::Enum(_))
    ) {
        return true;
    }
    if matches!(ctx.ty_kind(elem), InternedTyKind::List(_))
        && is_gc_eligible_list_ty(ctx, elem)
    {
        return true;
    }
    if matches!(ctx.ty_kind(elem), InternedTyKind::String) {
        return true;
    }
    if let InternedTyKind::Option(inner) = ctx.ty_kind(elem) {
        let inner_fits = matches!(
            ctx.ty_kind(*inner),
            InternedTyKind::Bool
                | InternedTyKind::S8
                | InternedTyKind::S16
                | InternedTyKind::S32
                | InternedTyKind::U8
                | InternedTyKind::U16
                | InternedTyKind::U32
                | InternedTyKind::F32
                | InternedTyKind::Char
        ) || matches!(
            ctx.ty_kind(*inner),
            InternedTyKind::Adt(d) if matches!(ctx.defs.kind(*d), DefKind::Enum(_))
        );
        if inner_fits {
            return true;
        }
    }
    if let InternedTyKind::Adt(d) = ctx.ty_kind(elem) {
        if matches!(ctx.defs.kind(*d), DefKind::Record(_)) {
            // Records: assume eligible if all DTR fields are. The full
            // DTR check would require recursive seen-tracking; mirror
            // the simple case (single-level record with primitive
            // fields) — the codegen path falls back to fat_value for
            // non-eligible records, which is safe.
            return true;
        }
    }
    false
}

fn record_field_storage_type(
    ctx: &yel_core::context::CompilerContext,
    ty: yel_core::Ty,
    registry: &RecordGcTypes,
) -> ValType {
    use yel_core::definitions::DefKind;
    use yel_core::types::InternedTyKind;
    match ctx.ty_kind(ty) {
        InternedTyKind::S64 | InternedTyKind::U64 => ValType::I64,
        InternedTyKind::F32 => ValType::F32,
        InternedTyKind::F64 => ValType::F64,
        // Single-i32 scalars: bool / narrow ints / char / s32 / u32 /
        // unit-typed UI scalars. Match the `signal_storage_valtypes`
        // collapse rules so a `bool`-field record stores 1 byte's worth
        // of i32, not a fat-pointer.
        InternedTyKind::Bool
        | InternedTyKind::S8
        | InternedTyKind::S16
        | InternedTyKind::S32
        | InternedTyKind::U8
        | InternedTyKind::U16
        | InternedTyKind::U32
        | InternedTyKind::Char
        | InternedTyKind::Length
        | InternedTyKind::PhysicalLength
        | InternedTyKind::Angle
        | InternedTyKind::Duration
        | InternedTyKind::Percent
        | InternedTyKind::RelativeFontSize
        | InternedTyKind::Color
        | InternedTyKind::Brush
        | InternedTyKind::Image
        | InternedTyKind::Easing => ValType::I32,
        // Phase 3: strings and `list<scalar>` are stored as a concrete
        // `(ref null $fat_value)` — a 2-i32 box (ptr, len). Resolution
        // of the original "anyref + cast" plan: a concrete typed ref
        // is uniformly better since both strings and scalar-element
        // lists share the same fat-pointer shape.
        InternedTyKind::List(_) => {
            // Phase 5e.6: if the list type has a typed GC array registered
            // (any GC-eligible list — scalar, string, nested list, DTR
            // record, tuple, …), use the concrete `(ref null $<elem>_list)`
            // type instead of the legacy `$fat_value` (ptr, len) box.
            if is_gc_eligible_list_ty(ctx, ty) {
                if let Some(&arr_idx) = registry.list_array_type_idx.get(&ty) {
                    return ValType::Ref(RefType {
                        nullable: true,
                        heap_type: HeapType::Concrete(arr_idx),
                    });
                }
            }
            let fat_value_idx = registry
                .fat_value_type_idx
                .expect("fat_value type idx must be assigned before record fields are typed");
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(fat_value_idx),
            })
        }
        InternedTyKind::String => {
            let fat_value_idx = registry
                .fat_value_type_idx
                .expect("fat_value type idx must be assigned before record fields are typed");
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(fat_value_idx),
            })
        }
        // Options/results, variants, tuples → anyref.
        // Phase 4+ revisits these (variant payloads and nested tuples).
        InternedTyKind::Option(_)
        | InternedTyKind::Result { .. }
        | InternedTyKind::Tuple(_) => ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Abstract {
                shared: false,
                ty: wasm_encoder::AbstractHeapType::Any,
            },
        }),
        InternedTyKind::Adt(def_id) => match ctx.defs.kind(*def_id) {
            DefKind::Record(_) => {
                // Nested record: reference the sibling record's type
                // via the registry's pre-assigned index (forward refs
                // within a rec group resolve naturally).
                let inner_idx = registry.record_type_idx[def_id];
                ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(inner_idx),
                })
            }
            // Enums collapse to a single i32 discriminant, like the
            // `signal_storage_valtypes` flat path.
            DefKind::Enum(_) => ValType::I32,
            // Variants with payload have a multi-slot canonical-ABI
            // shape that doesn't fit in one struct field — store them
            // as anyref for Phase 1 (Phase 4 revisits).
            _ => ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Abstract {
                    shared: false,
                    ty: wasm_encoder::AbstractHeapType::Any,
                },
            }),
        },
        // Function refs / unit / error / unknown: i32 fallback (matches
        // the `signal_storage_valtypes` default).
        _ => ValType::I32,
    }
}
