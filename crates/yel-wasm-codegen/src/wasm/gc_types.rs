//! WASM GC type synthesis for reactive-runtime state.
//!
//! Emits the per-component concrete-typed mount-tree GC types: one
//! struct per `TreeBoundary` (Root / IfAnchor / IfBranch / ForAnchor /
//! ForIterBody) plus a companion `(array (mut (ref null <iter_body>)))`
//! per `ForAnchor`. Plus the per-component `$Comp_<Name>` struct and
//! the module-shared registry-handle types. All types live in rec
//! groups so cyclic struct ↔ array references resolve.

use std::collections::{HashMap, HashSet, VecDeque};
use wasm_encoder::{
    AbstractHeapType, ArrayType, CompositeInnerType, CompositeType, FieldType, HeapType, RefType,
    StorageType, StructType, SubType, TypeSection, ValType,
};
use yel_core::ids::TreeBoundaryId;
use yel_core::lir::LirResource;
use yel_core::lir::block::{LirSlotValType, TreeBoundaryKind};
use yel_core::{CompilerContext, DefId, DefKind, InternedTyKind, Ty};

/// Type-index assignments for one component's GC types.
///
/// Populated during type-section emission, read by every emission path
/// that needs to `struct.new` / `array.get` / etc.
#[derive(Debug, Default, Clone)]
pub struct GcTypeLayout {
    /// Type index of `$Comp_<Name>` — the struct holding this
    /// component's signals as fields. Set by `emit_component_struct_type`.
    pub component_struct_type_idx: Option<u32>,
    /// For each signal (index matches `LirResource.signals`), the list
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
    /// `TreeBoundaryId`. Populated, not yet consumed by emission paths.
    pub tree_struct_type_idx: HashMap<TreeBoundaryId, u32>,
    /// Per-`ForAnchor`-boundary array type index — the GC array type
    /// `(array (mut (ref null <iter_body_struct>)))` used as the
    /// children-array element. Keyed by the *anchor* boundary id.
    pub tree_for_arr_type_idx: HashMap<TreeBoundaryId, u32>,
    /// Wasm type-section index of `array_types[0]` — the base the
    /// registry-indexed `Array*` ops resolve against
    /// (`wasm_idx = array_type_base + LirArrayTypeIdx`).
    pub array_type_base: u32,
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
    component: &LirResource,
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
            descriptor: None,
            describes: None,
        },
    }];
    types.ty().rec(sub_types);

    // The LIR-side `SignalLayout` must agree with the codegen-side
    // field-paths exactly; divergence means `compute_signal_layout` and
    // `signal_storage_valtypes` have drifted apart.
    #[cfg(debug_assertions)]
    for (sig_idx, path) in field_paths.iter().enumerate() {
        let lir_path = component.signal_layout.signal_field_path(sig_idx);
        debug_assert_eq!(
            path, &lir_path,
            "signal {} field-path mismatch: codegen={:?} lir={:?}",
            sig_idx, path, lir_path
        );
    }
    // LIR-side `comp_struct_layout` mirrors the field allocations
    // above; LIR pass and codegen must agree slot-for-slot.
    debug_assert_eq!(
        parent_retention_field_base, component.comp_struct_layout.parent_retention_field_base,
        "comp_struct_layout: parent_retention_field_base drift",
    );
    debug_assert_eq!(
        self_handle_field_idx, component.comp_struct_layout.self_handle_field_idx,
        "comp_struct_layout: self_handle_field_idx drift",
    );
    debug_assert_eq!(
        tree_root_field_idx, component.comp_struct_layout.tree_root_field_idx,
        "comp_struct_layout: tree_root_field_idx drift",
    );

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
                descriptor: None,
                describes: None,
            },
        },
        SubType {
            is_final: true,
            supertype_idx: None,
            composite_type: CompositeType {
                shared: false,
                inner: CompositeInnerType::Array(handle_array),
                descriptor: None,
                describes: None,
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
/// Types are declared but no emission path consumes the indices yet.
pub fn emit_component_tree_types(
    component: &yel_core::lir::node::LirResource,
    types: &mut TypeSection,
    base_type_idx: u32,
    layout: &mut GcTypeLayout,
    ctx: &yel_core::context::CompilerContext,
    record_gc_types: &RecordGcTypes,
) -> u32 {
    // Stage 5d: walk the resource's struct_types / array_types
    // registry instead of the parallel `tree_shape.boundaries`.
    // Registry index `i` corresponds to TreeBoundaryId(i) by the
    // synthesizer's projection invariant, so the per-boundary
    // `tree_struct_type_idx` / `tree_for_arr_type_idx` maps still
    // populate using TreeBoundaryId keys for compatibility with
    // codegen sites that haven't migrated yet.
    let struct_types = &component.struct_types;
    let array_types = &component.array_types;
    if struct_types.is_empty() {
        return 0;
    }
    let n = struct_types.len() as u32;
    let n_arrays = array_types.len() as u32;
    // Arrays are emitted right after the structs, so registry array index
    // `k` lands at `base_type_idx + n + k`.
    layout.array_type_base = base_type_idx + n;

    // Pass 1: assign struct + array type indices.
    for i in 0..n {
        layout
            .tree_struct_type_idx
            .insert(TreeBoundaryId(i), base_type_idx + i);
    }
    // Map each ForAnchor's struct → its array's wasm type idx. The
    // array's element points at the iter-body struct, and the
    // ChildrenArray field on the ForAnchor carries the array index
    // — but we walk struct_types looking for kind == ForAnchor and
    // find the matching array by ChildrenArray field's ref_target.
    let mut arr_assigned: u32 = 0;
    for (i, struct_decl) in struct_types.iter().enumerate() {
        if !matches!(struct_decl.kind, TreeBoundaryKind::ForAnchor { .. }) {
            continue;
        }
        // The for-anchor has a ChildrenArray field; its
        // LirRefTarget::Array gives the registry array idx. Sanity
        // check; otherwise fall back to sequential allocation.
        let child_arr_field = struct_decl
            .fields
            .iter()
            .find_map(|f| match f.ref_target {
                Some(yel_core::lir::struct_types::LirRefTarget::Array(idx)) => Some(idx.0),
                _ => None,
            })
            .unwrap_or(arr_assigned);
        let _ = child_arr_field;
        layout
            .tree_for_arr_type_idx
            .insert(TreeBoundaryId(i as u32), base_type_idx + n + arr_assigned);
        arr_assigned += 1;
    }
    debug_assert_eq!(
        arr_assigned, n_arrays,
        "registry array_types count must match ForAnchor count in struct_types"
    );
    // Root type idx: the unique struct with kind == Root.
    layout.tree_root_type_idx = struct_types
        .iter()
        .position(|s| matches!(s.kind, TreeBoundaryKind::Root))
        .map(|i| base_type_idx + i as u32);

    // Pass 2: build SubTypes in declaration order from the registry.
    let mut sub_types: Vec<SubType> = Vec::with_capacity((n + n_arrays) as usize);
    for struct_decl in struct_types {
        let struct_ty = build_struct_from_decls(&struct_decl.fields, layout, ctx, record_gc_types);
        sub_types.push(SubType {
            is_final: true,
            supertype_idx: None,
            composite_type: CompositeType {
                shared: false,
                inner: CompositeInnerType::Struct(struct_ty),
                descriptor: None,
                describes: None,
            },
        });
    }
    for array_decl in array_types {
        let elem_struct_idx = match array_decl.element {
            yel_core::lir::struct_types::LirArrayElement::StructRef(idx) => base_type_idx + idx.0,
        };
        let arr_ty = ArrayType(FieldType {
            element_type: StorageType::Val(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(elem_struct_idx),
            })),
            mutable: array_decl.mutable,
        });
        sub_types.push(SubType {
            is_final: true,
            supertype_idx: None,
            composite_type: CompositeType {
                shared: false,
                inner: CompositeInnerType::Array(arr_ty),
                descriptor: None,
                describes: None,
            },
        });
    }

    types.ty().rec(sub_types);
    n + n_arrays
}

/// Stage 5d sibling to `build_tree_boundary_struct` — translates
/// `LirStructFieldDecl`s from the resource registry to wasm
/// `FieldType`s. The `ref_target` field carries cross-reference
/// resolution info; layout's `tree_struct_type_idx` (populated in
/// pass 1) maps `LirStructTypeIdx` → wasm struct type index.
fn build_struct_from_decls(
    fields: &[yel_core::lir::struct_types::LirStructFieldDecl],
    layout: &GcTypeLayout,
    ctx: &yel_core::context::CompilerContext,
    record_gc_types: &RecordGcTypes,
) -> StructType {
    use yel_core::lir::struct_types::{LirFieldRole, LirRefTarget};
    let wasm_fields: Vec<FieldType> = fields
        .iter()
        .map(|f| match (f.role, f.ref_target) {
            (LirFieldRole::DomHandle | LirFieldRole::ActiveTag, _) => FieldType {
                element_type: StorageType::Val(ValType::I32),
                mutable: f.mutable,
            },
            (LirFieldRole::LoopVar, _) => FieldType {
                element_type: StorageType::Val(slot_val_ty_to_val_ty(
                    &f.val_ty,
                    ctx,
                    record_gc_types,
                )),
                mutable: f.mutable,
            },
            (LirFieldRole::SubBoundary, Some(LirRefTarget::Struct(target))) => {
                let target_struct_idx = layout.tree_struct_type_idx[&TreeBoundaryId(target.0)];
                FieldType {
                    element_type: StorageType::Val(ValType::Ref(RefType {
                        nullable: true,
                        heap_type: HeapType::Concrete(target_struct_idx),
                    })),
                    mutable: f.mutable,
                }
            }
            (LirFieldRole::ChildrenArray, Some(LirRefTarget::Array(_))) => {
                // The ChildrenArray field points at the for-anchor's
                // companion array. layout.tree_for_arr_type_idx is
                // keyed by the OWNING for-anchor's TreeBoundaryId,
                // not by array index — but the synthesizer's
                // ordering invariant means the iter-body's struct
                // comes right before the for-anchor, and we encoded
                // that in the registry projection. Recover the
                // anchor by walking layout.tree_for_arr_type_idx for
                // the matching idx (same approach the legacy
                // builder used).
                //
                // Cleaner long-term: the LirRefTarget::Array carries
                // the idx directly; layout could store
                // `array_ty_idx` keyed by `LirArrayTypeIdx` too.
                // For Stage 5d we keep the existing layout shape
                // and look up via the legacy helper.
                let arr_idx = match f.ref_target {
                    Some(LirRefTarget::Array(idx)) => {
                        // The registry array idx is `idx.0`.
                        // Compute its wasm-type-section idx as
                        // `tree_root_or_first_struct + n + idx.0`.
                        // We don't have base/n directly here, so
                        // recover from the layout's first array
                        // entry.
                        let any_arr =
                            *layout.tree_for_arr_type_idx.values().min().expect(
                                "ChildrenArray field requires at least one ForAnchor array",
                            );
                        any_arr + idx.0
                    }
                    _ => unreachable!(),
                };
                FieldType {
                    element_type: StorageType::Val(ValType::Ref(RefType {
                        nullable: true,
                        heap_type: HeapType::Concrete(arr_idx),
                    })),
                    mutable: f.mutable,
                }
            }
            other => unreachable!("unexpected (role, ref_target) combination: {:?}", other),
        })
        .collect();
    StructType {
        fields: wasm_fields.into_boxed_slice(),
    }
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
        LirSlotValType::RefNullForComponent(_) => {
            unreachable!("tree-boundary loop-var field cannot hold a component instance ref")
        }
        LirSlotValType::RefNullForListGc(list_ty) => {
            // A `list<scalar-list>` / `list<option<scalar-list>>` iter-body
            // loop-var field holds the inner list's typed array ref.
            let &idx = record_gc_types
                .list_array_type_idx
                .get(list_ty)
                .expect("RefNullForListGc: missing list_array_type_idx");
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })
        }
        LirSlotValType::RefNullForStringBytes => {
            // strings-to-GC: a `list<string>` iter-body loop-var field holds
            // the element's `$str_bytes` ref directly.
            let idx = record_gc_types
                .str_bytes_array_idx
                .expect("RefNullForStringBytes: $str_bytes array type not registered");
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })
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
        LirSlotValType::RefNullForTuple(tuple_ty) => {
            // A `list<tuple>` / `list<option<tuple>>` iter-body loop-var
            // field holds the tuple's GC struct ref.
            let &idx = record_gc_types
                .tuple_struct_type_idx
                .get(tuple_ty)
                .expect("RefNullForTuple: missing tuple_struct_type_idx");
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })
        }
        LirSlotValType::RefNullForFlatGc(ty) => {
            // list<FlatGcStruct> iter-body LoopVar field stores the
            // supertype ref directly.
            let &super_idx = record_gc_types
                .flat_gc_super_idx
                .get(ty)
                .expect("RefNullForFlatGc: missing flat_gc_super_idx");
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(super_idx),
            })
        }
        LirSlotValType::RefNullForSharedHandleArray => {
            unreachable!("shared handle-array ref not expected as a tree loop-var field type")
        }
        LirSlotValType::RefNullForSharedHandle => {
            unreachable!("shared handle ref not expected as a tree loop-var field type")
        }
        LirSlotValType::AnyRef => {
            unreachable!("anyref not expected as a tree loop-var field type")
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
    /// DefId of the owning `global Foo { ... }` block. Read by helpers
    /// that resolve blocks by their owning global property.
    pub block_def_id: DefId,
    /// Per-property field-index path into the block's storage slots,
    /// indexed by property position in `GlobalDef.properties`. Empty
    /// vec marks pointer-typed properties that stay on the linear-memory
    /// path. `field_core_globals[property_field_paths[p][s]]` is the core
    /// global backing property `p`'s storage slot `s`.
    pub property_field_paths: Vec<Vec<u32>>,
    /// Wasm valtype of each storage field, in field-index order
    /// (`field_valtypes[f]` is the valtype of slot `f`). Used to declare
    /// the per-field core wasm globals that back the block's state.
    pub field_valtypes: Vec<ValType>,
    /// Per-field core wasm global index (`field_core_globals[f]` is the
    /// mutable global holding field `f`'s value). Populated by the global
    /// section. This is the singleton's live storage.
    pub field_core_globals: Vec<u32>,
}

/// Compute a named global block's storage layout: one slot per ABI slot
/// of each property, in property-declaration order. Pointer-typed
/// properties (records/tuples) contribute zero slots and get an empty
/// `property_field_paths` entry (they stay on the linear-memory path).
/// Each slot is backed by a core wasm global, assigned by the global
/// section into `field_core_globals`.
pub fn compute_globals_block_layout(
    block_def_id: DefId,
    prop_slot_valtypes: &[Vec<ValType>],
) -> GlobalsBlockLayout {
    let mut field_valtypes: Vec<ValType> = Vec::new();
    let mut property_field_paths: Vec<Vec<u32>> = Vec::with_capacity(prop_slot_valtypes.len());
    for slots in prop_slot_valtypes {
        let mut path: Vec<u32> = Vec::with_capacity(slots.len());
        for vt in slots {
            path.push(field_valtypes.len() as u32);
            field_valtypes.push(*vt);
        }
        property_field_paths.push(path);
    }

    GlobalsBlockLayout {
        block_def_id,
        property_field_paths,
        field_valtypes,
        field_core_globals: Vec::new(), // populated by the global section
    }
}

// ============================================================================
// Per-program record / tuple GC types.
// ============================================================================

/// Program-scope registry of GC struct types emitted for user-defined
/// records and tuples. Populated once during type-section emission by
/// `emit_program_record_types`; read at every `struct.new` /
/// `struct.get` / `struct.set` site.
#[derive(Debug, Default, Clone)]
pub struct RecordGcTypes {
    /// Record `DefId` → emitted GC struct type index in the module's
    /// type section. One entry per `Definitions::records()`.
    pub record_type_idx: HashMap<DefId, u32>,
    /// Record `DefId` → list of GC struct field indices, parallel to
    /// the record's `field_offsets`. `field_gc_indices[i]` is the GC
    /// field index for the `i`-th declared field. Today field indices
    /// are simply `0..N`, but we still keep the indirection so future
    /// changes (multi-slot variant payloads, padding fields) can shift
    /// the GC index without bleeding into call sites.
    pub field_gc_indices: HashMap<DefId, Vec<u32>>,
    /// Debug names emitted into the WASM name section's type subsection
    /// for each record's GC struct. Populated alongside the type indices
    /// so the name-section pass can emit them without a second pass over
    /// `Definitions`.
    pub type_names: Vec<(u32, String)>,
    /// Shared `$str_bytes = (array (mut i8))` GC type backing every
    /// `String` (strings-to-GC migration, `plans/strings-to-gc.md`). One
    /// packed-byte-array type per program, always emitted; the
    /// `internal_repr` `String → GcArrayRef` hook reads it. `None` only
    /// before the rec group is built.
    pub str_bytes_array_idx: Option<u32>,
    /// Per-list-element-type GC array type indices, keyed by the
    /// **list `Ty`** (the `Ty` whose `InternedTyKind` is `List(elem)`),
    /// NOT by the element `Ty` — matches call sites that have a
    /// `list<T>` value's `Ty` in hand at `Index` / `ListConstruct`.
    pub list_array_type_idx: std::collections::HashMap<yel_core::Ty, u32>,
    /// Per-tuple-type GC struct type indices, keyed by the tuple `Ty`
    /// (whose `InternedTyKind` is `Tuple(elements)`). One emitted
    /// struct per **distinct** tuple type, shared across components.
    pub tuple_struct_type_idx: std::collections::HashMap<yel_core::Ty, u32>,
    /// Per-`option<T>` / `result<T,E>` / user-`variant` parent `Ty` →
    /// emitted **supertype** struct index (an empty `(sub (struct))`
    /// non-final type). Storage of a migrated value is a single
    /// `(ref null $supertype)` slot per the W3C component-model GC ABI
    /// proposal (issue #525).
    ///
    /// Cycle / topo invariant: emitted in payload-dependency order —
    /// if `option<variant<…>>` is a parent, the inner variant's
    /// supertype must already be in this map when the outer rec group
    /// is built. YEL has no recursive variants today; if it ever adds
    /// them, fold the SCC into one rec group.
    pub flat_gc_super_idx: std::collections::HashMap<yel_core::Ty, u32>,
    /// Per-`(parent Ty, case_idx)` → emitted **case-subtype** struct
    /// index (a `(sub final $supertype …)`).
    /// Cases with payload have one struct field; cases without have
    /// an empty struct.
    pub flat_gc_case_idx: std::collections::HashMap<(yel_core::Ty, u32), u32>,
    /// Per-parent `Ty` → number of cases. Lets consumers iterate cases
    /// without re-querying defs / type kinds.
    pub flat_gc_case_count: std::collections::HashMap<yel_core::Ty, u32>,
}

/// Emit, in a single program-scope rec group, one `(struct ...)` GC
/// type per user-defined record. All record types co-exist in one rec
/// group so a record field whose type is another record can reference
/// the inner record's type index via forward reference within the same
/// rec group — wasm rec groups resolve recursive references.
///
/// Field-type rules:
/// - `bool` / `s8` / `s16` / `s32` / `u8` / `u16` / `u32` / `char` /
///   enum / `option<scalar>` / `result<scalar, scalar>` → single `i32`
///   field.
/// - `s64` / `u64` → `i64`. `f32` → `f32`, `f64` → `f64`.
/// - `string` → `anyref` (cast to `(ref $fat_string)` at access time).
/// - `list<T>` → typed `(array ...)` ref when registered, else `anyref`.
/// - Record / tuple field → `(ref null $<inner>_record)` for nested
///   records (forward reference resolved by the rec group), or
///   `anyref` for tuples when not registered.
/// - Variant / option-with-payload / result-with-payload of an
///   aggregate → `anyref` when the canonical-ABI join shape doesn't
///   fit in one struct field.
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

    let record_def_ids: Vec<DefId> = ctx.defs.records().collect();

    // Collect all list and tuple types referenced anywhere in the
    // program; dedupe by `Ty` so each unique element-type / tuple
    // shape gets exactly one emitted GC type.
    let (list_tys, tuple_tys, flat_gc_tys_unsorted) =
        collect_list_and_tuple_tys(ctx, extra_seed_tys);
    // Emit flat-gc parents in payload-dependency order so any nested
    // option<variant<…>>'s inner supertype index is already registered
    // when the outer rec group's payload-field storage is resolved.
    let flat_gc_tys = topo_sort_flat_gc_tys(ctx, &flat_gc_tys_unsorted);

    // strings-to-GC: the shared `$str_bytes` byte-array type is always
    // emitted (a `String` is a GC byte array everywhere), so there is never
    // a zero-GC-type program — this rec group is always non-empty.

    let mut registry = RecordGcTypes::default();

    // Emission order — each block is its own rec group so cross-block
    // references resolve via indices already declared:
    //
    //   [base+0]                           $str_bytes (singleton rec group)
    //   [base+1 .. base+1+Σ(1+cases)]      flat-gc rec groups, topo-sorted
    //   [records_base .. tuple_end]        records / lists / tuples
    //                                      (single rec group)
    //
    // Records / lists / tuples need flat-gc supertype indices for nested
    // option / variant fields, so they come last.

    // strings-to-GC: emit the shared `$str_bytes = (array (mut i8))` as its
    // own singleton rec group first. Packed i8 = one byte per element
    // (UTF-8). Always emitted — a `String` is a GC byte array.
    let after_singletons = {
        let sb_idx = base_type_idx;
        types.ty().rec(vec![SubType {
            is_final: true,
            supertype_idx: None,
            composite_type: CompositeType {
                shared: false,
                inner: CompositeInnerType::Array(ArrayType(FieldType {
                    element_type: StorageType::I8,
                    mutable: true,
                })),
                descriptor: None,
                describes: None,
            },
        }]);
        registry.str_bytes_array_idx = Some(sb_idx);
        registry
            .type_names
            .push((sb_idx, "str_bytes".to_string()));
        sb_idx + 1
    };

    // Merge flat-gc parents, records, list arrays, and tuple structs
    // into ONE big rec group. Forward refs within a rec group
    // resolve naturally — so `option<Person>`'s Some-case can reference
    // `$person_record` as a typed `(ref null $person_record)` even
    // though Person's struct body comes later in the rec group, and
    // `record User { x: option<Address> }` can likewise reference its
    // option supertype regardless of emission order. Earlier we tried
    // separate rec groups + topo-sort; that breaks down for the
    // bidirectional cycle (records → flat-gc → records) and forced an
    // anyref fallback in `record_field_storage_type`. The merged group
    // sidesteps the cycle entirely.
    //
    // Index layout within the merged rec group:
    //   [cursor .. cursor + flat_gc_total)        flat-gc supertypes + cases
    //   [records_base .. list_arrays_base)        records
    //   [list_arrays_base .. tuple_structs_base)  list arrays
    //   [tuple_structs_base .. tuple_end)         tuple structs
    let mut cursor = after_singletons;
    for &parent_ty in &flat_gc_tys {
        let consumed = assign_flat_gc_indices(ctx, parent_ty, cursor, &mut registry);
        cursor += consumed;
    }

    let records_base = cursor;
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

    // Build the merged rec group's SubType vector. Order MUST match
    // the index reservations above so each SubType lands at the
    // index assigned in the registry.
    let flat_gc_total: u32 = flat_gc_tys
        .iter()
        .map(|ty| 1 + flat_gc_case_count(ctx, *ty).unwrap_or(0))
        .sum();
    let total_subtypes = (flat_gc_total
        + record_def_ids.len() as u32
        + list_tys.len() as u32
        + tuple_tys.len() as u32) as usize;
    let mut sub_types: Vec<SubType> = Vec::with_capacity(total_subtypes);

    for &parent_ty in &flat_gc_tys {
        build_flat_gc_subtypes(ctx, parent_ty, &registry, &mut sub_types);
    }

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
            // Stage 8: record fields are immutable. YEL records are
            // values — assigning a record-typed signal replaces the
            // whole ref via struct.set on the *component* struct, not
            // on the record's own fields.
            wasm_fields.push(FieldType {
                element_type: StorageType::Val(storage),
                mutable: false,
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
                descriptor: None,
                describes: None,
            },
        });
    }

    // Emit one `(array (mut <elem>))` per unique list type. Element
    // ValType chosen via `list_element_storage_type`:
    //   - scalars unboxed (i32 / i64 / f32 / f64)
    //   - string / list<...> → `(ref null $fat_value)`
    //   - records (DTR) → `(ref null $<record>_record)`
    //   - tuples → `(ref null $tuple_<n>)`
    //   - option / result / variant → `anyref`
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
                descriptor: None,
                describes: None,
            },
        });
        let elem_name = list_elem_short_name(ctx, elem_ty);
        registry
            .type_names
            .push((arr_idx, format!("{}_list", elem_name)));
    }

    // Emit one `(struct ...)` per unique tuple type.
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
            // Stage 8: tuple fields are immutable. YEL tuples are values;
            // rebinding a tuple-typed slot replaces the whole ref via
            // struct.set on the *parent* struct, not on the tuple's
            // own fields.
            wasm_fields.push(FieldType {
                element_type: StorageType::Val(storage),
                mutable: false,
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
                descriptor: None,
                describes: None,
            },
        });
        registry
            .type_names
            .push((struct_idx, format!("tuple_{}", i)));
        let _ = DefKind::Record; // silence unused import lint
    }

    if !sub_types.is_empty() {
        types.ty().rec(sub_types);
    }
    let _ = HashMap::<DefId, u32>::new(); // silence import lint when empty
    // Total reserved indices:
    //   1 ($str_bytes, always emitted)
    //   + Σ (1 + case_count) over flat-gc parents
    //   + N (records) + L (lists) + T (tuples).
    // (`flat_gc_total` is computed above for the SubType capacity hint.)
    let total = 1 // $str_bytes (always emitted)
        + flat_gc_total
        + record_def_ids.len() as u32
        + list_tys.len() as u32
        + tuple_tys.len() as u32;
    (total, registry)
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
) -> (Vec<yel_core::Ty>, Vec<yel_core::Ty>, Vec<yel_core::Ty>) {
    use yel_core::definitions::DefKind;
    use yel_core::types::InternedTyKind;

    let mut list_seen: std::collections::HashSet<yel_core::Ty> = std::collections::HashSet::new();
    let mut tuple_seen: std::collections::HashSet<yel_core::Ty> = std::collections::HashSet::new();
    let mut flat_gc_seen: std::collections::HashSet<yel_core::Ty> =
        std::collections::HashSet::new();
    let mut list_order: Vec<yel_core::Ty> = Vec::new();
    let mut tuple_order: Vec<yel_core::Ty> = Vec::new();
    let mut flat_gc_order: Vec<yel_core::Ty> = Vec::new();

    fn walk(
        ctx: &yel_core::context::CompilerContext,
        ty: yel_core::Ty,
        list_seen: &mut std::collections::HashSet<yel_core::Ty>,
        tuple_seen: &mut std::collections::HashSet<yel_core::Ty>,
        flat_gc_seen: &mut std::collections::HashSet<yel_core::Ty>,
        list_order: &mut Vec<yel_core::Ty>,
        tuple_order: &mut Vec<yel_core::Ty>,
        flat_gc_order: &mut Vec<yel_core::Ty>,
    ) {
        match ctx.ty_kind(ty) {
            InternedTyKind::List(inner) => {
                if list_seen.insert(ty) {
                    list_order.push(ty);
                }
                walk(
                    ctx, *inner, list_seen, tuple_seen, flat_gc_seen, list_order, tuple_order,
                    flat_gc_order,
                );
            }
            InternedTyKind::Tuple(els) => {
                if tuple_seen.insert(ty) {
                    tuple_order.push(ty);
                }
                let els = els.clone();
                for e in els {
                    walk(
                        ctx, e, list_seen, tuple_seen, flat_gc_seen, list_order, tuple_order,
                        flat_gc_order,
                    );
                }
            }
            InternedTyKind::Option(inner) => {
                // Register the option Ty itself as a FlatGcStruct
                // candidate, then recurse into its inner.
                if flat_gc_seen.insert(ty) {
                    flat_gc_order.push(ty);
                }
                walk(
                    ctx, *inner, list_seen, tuple_seen, flat_gc_seen, list_order, tuple_order,
                    flat_gc_order,
                );
            }
            InternedTyKind::Result { ok, err } => {
                if flat_gc_seen.insert(ty) {
                    flat_gc_order.push(ty);
                }
                if let Some(t) = ok {
                    walk(
                        ctx, *t, list_seen, tuple_seen, flat_gc_seen, list_order, tuple_order,
                        flat_gc_order,
                    );
                }
                if let Some(t) = err {
                    walk(
                        ctx, *t, list_seen, tuple_seen, flat_gc_seen, list_order, tuple_order,
                        flat_gc_order,
                    );
                }
            }
            InternedTyKind::Adt(d)
                // User variants register as FlatGcStruct parents.
                // Records and enums do NOT — records have their own GC
                // struct path; enums lower to plain i32.
                if matches!(ctx.defs.kind(*d), DefKind::Variant(_))
                    && flat_gc_seen.insert(ty) =>
            {
                flat_gc_order.push(ty);
            }
            // Records' fields and variants' payloads are walked
            // separately (over `defs.records()` / `defs.variants()`),
            // so we don't recurse into them here — that would
            // revisit each parent's inner Tys N times.
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
                &mut flat_gc_seen,
                &mut list_order,
                &mut tuple_order,
                &mut flat_gc_order,
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
                    &mut flat_gc_seen,
                    &mut list_order,
                    &mut tuple_order,
                    &mut flat_gc_order,
                );
            }
        }
    }

    // Walk every variant payload type.
    for def_id in ctx.defs.variants() {
        if let DefKind::Variant(v) = ctx.defs.kind(def_id) {
            let case_ids = v.cases.clone();
            for case_def_id in case_ids {
                if let DefKind::VariantCase(c) = ctx.defs.kind(case_def_id)
                    && let Some(payload_ty) = c.payload {
                        walk(
                            ctx,
                            payload_ty,
                            &mut list_seen,
                            &mut tuple_seen,
                            &mut flat_gc_seen,
                            &mut list_order,
                            &mut tuple_order,
                            &mut flat_gc_order,
                        );
                    }
            }
        }
    }

    // Also walk caller-supplied extra seed types — catches list types
    // that appear only in LIR expressions (list literals iterated by
    // `for`, etc.) and have no Def-level reference.
    for &ty in extra_seed_tys {
        walk(
            ctx,
            ty,
            &mut list_seen,
            &mut tuple_seen,
            &mut flat_gc_seen,
            &mut list_order,
            &mut tuple_order,
            &mut flat_gc_order,
        );
    }

    (list_order, tuple_order, flat_gc_order)
}

/// Number of cases for an option/result/variant `Ty`.
/// Returns `None` when `parent_ty` is not a flat-gc-eligible parent.
pub(crate) fn flat_gc_case_count(
    ctx: &yel_core::context::CompilerContext,
    parent_ty: yel_core::Ty,
) -> Option<u32> {
    use yel_core::definitions::DefKind;
    use yel_core::types::InternedTyKind;
    match ctx.ty_kind(parent_ty) {
        InternedTyKind::Option(_) => Some(2),
        InternedTyKind::Result { .. } => Some(2),
        InternedTyKind::Adt(d) => match ctx.defs.kind(*d) {
            DefKind::Variant(v) => Some(v.cases.len() as u32),
            _ => None,
        },
        _ => None,
    }
}

/// Payload type for a given `(parent_ty, case_idx)`. Per the W3C
/// component-model GC ABI proposal, each case becomes one
/// final subtype struct with at most one payload field. Returns `None`
/// when the case has no payload (e.g. `option`'s `none`, `variant`'s
/// unit case, `result`'s unit ok/err arm).
///
/// **Case-index conventions** — these match YEL's THIR/LIR lowering
/// (see `thir::typeck::build_option_*` / `build_result_*`):
/// - `option<T>`: **0 = Some(T)**, 1 = None.
/// - `result<T,E>`: **0 = Ok(T)**, 1 = Err(E).
/// - User variant: matches `VariantDef::cases` declaration order.
///
/// Case 0 is the "default" (zero-inited) case for legacy memory
/// init parity: `struct.new_default $<sup>_<case0>` fills payload
/// fields with their type defaults (matching zero-byte memory).
pub(crate) fn case_payload_ty(
    ctx: &yel_core::context::CompilerContext,
    parent_ty: yel_core::Ty,
    case_idx: u32,
) -> Option<yel_core::Ty> {
    use yel_core::definitions::DefKind;
    use yel_core::types::InternedTyKind;
    match ctx.ty_kind(parent_ty) {
        InternedTyKind::Option(inner) => {
            if case_idx == 0 {
                Some(*inner)
            } else {
                None
            }
        }
        InternedTyKind::Result { ok, err } => match case_idx {
            0 => *ok,
            1 => *err,
            _ => None,
        },
        InternedTyKind::Adt(d) => match ctx.defs.kind(*d) {
            DefKind::Variant(v) => {
                let case_def_id = *v.cases.get(case_idx as usize)?;
                match ctx.defs.kind(case_def_id) {
                    DefKind::VariantCase(c) => c.payload,
                    _ => None,
                }
            }
            _ => None,
        },
        _ => None,
    }
}

/// Human-readable short name for a case, used in the name section.
/// `option` cases are `none`/`some`; `result` cases are `ok`/`err`;
/// user-variant cases come from `VariantCaseDef.name`.
fn case_short_name(
    ctx: &yel_core::context::CompilerContext,
    parent_ty: yel_core::Ty,
    case_idx: u32,
) -> String {
    use yel_core::definitions::DefKind;
    use yel_core::types::InternedTyKind;
    match ctx.ty_kind(parent_ty) {
        InternedTyKind::Option(_) => {
            if case_idx == 0 {
                "some".to_string()
            } else {
                "none".to_string()
            }
        }
        InternedTyKind::Result { .. } => {
            if case_idx == 0 {
                "ok".to_string()
            } else {
                "err".to_string()
            }
        }
        InternedTyKind::Adt(d) => match ctx.defs.kind(*d) {
            DefKind::Variant(v) => v
                .cases
                .get(case_idx as usize)
                .and_then(|cid| match ctx.defs.kind(*cid) {
                    DefKind::VariantCase(c) => Some(ctx.str(c.name).to_ascii_lowercase()),
                    _ => None,
                })
                .unwrap_or_else(|| format!("case_{}", case_idx)),
            _ => format!("case_{}", case_idx),
        },
        _ => format!("case_{}", case_idx),
    }
}

/// Short prefix for the parent (`opt`, `res`, or the variant name
/// lower-cased).
fn flat_gc_parent_short_name(
    ctx: &yel_core::context::CompilerContext,
    parent_ty: yel_core::Ty,
) -> String {
    use yel_core::definitions::DefKind;
    use yel_core::types::InternedTyKind;
    match ctx.ty_kind(parent_ty) {
        InternedTyKind::Option(inner) => {
            format!("opt_{}", list_elem_short_name(ctx, *inner))
        }
        InternedTyKind::Result { ok, err } => {
            let ok_n = ok
                .map(|t| list_elem_short_name(ctx, t))
                .unwrap_or_else(|| "unit".to_string());
            let err_n = err
                .map(|t| list_elem_short_name(ctx, t))
                .unwrap_or_else(|| "unit".to_string());
            format!("res_{}_{}", ok_n, err_n)
        }
        InternedTyKind::Adt(d) => match ctx.defs.kind(*d) {
            DefKind::Variant(v) => format!("var_{}", ctx.str(v.name).to_ascii_lowercase()),
            _ => "flat_gc".to_string(),
        },
        _ => "flat_gc".to_string(),
    }
}

/// Emit one rec group for an option/result/variant parent `Ty`:
///   - empty supertype `(sub (struct))` (NOT final — has subtypes)
///   - one case-subtype `(sub final $super (struct …))` per case,
///     with at most one mutable field whose storage type is the
///     case's payload (per W3C #525 packed-storage rules).
///
/// Pre-assigns indices: `super = base_idx; cases[i] = base_idx + 1 + i`.
/// Records the per-case payload's heap-type indirection through the
/// existing `record_field_storage_type` helper, which already routes
/// record/list/tuple payloads to their concrete refs and falls back
/// to `$fat_value` for strings / non-typed-array lists.
///
/// Side effects: pushes name-section entries for the supertype and
/// every case subtype; populates `registry.flat_gc_super_idx`,
/// `flat_gc_case_idx`, and `flat_gc_case_count`.
///
/// Returns the count of types reserved (`1 + case_count`).
/// Reserve type-section indices for `parent_ty`'s flat-gc supertype and
/// per-case subtypes, and emit name-section entries. Returns the number
/// of indices consumed (1 + case_count). Does NOT build SubType bodies
/// — that happens in `build_flat_gc_subtypes` after every record / list
/// / tuple has its own index reserved, so case-payload field types can
/// reference them as forward refs within the merged rec group.
fn assign_flat_gc_indices(
    ctx: &yel_core::context::CompilerContext,
    parent_ty: yel_core::Ty,
    base_idx: u32,
    registry: &mut RecordGcTypes,
) -> u32 {
    let case_count = flat_gc_case_count(ctx, parent_ty)
        .expect("assign_flat_gc_indices: parent_ty is not flat-gc-eligible");
    let super_idx = base_idx;
    registry.flat_gc_super_idx.insert(parent_ty, super_idx);
    registry.flat_gc_case_count.insert(parent_ty, case_count);
    for i in 0..case_count {
        registry
            .flat_gc_case_idx
            .insert((parent_ty, i), super_idx + 1 + i);
    }
    let parent_short = flat_gc_parent_short_name(ctx, parent_ty);
    registry.type_names.push((super_idx, parent_short.clone()));
    for i in 0..case_count {
        let case_name = case_short_name(ctx, parent_ty, i);
        registry
            .type_names
            .push((super_idx + 1 + i, format!("{}_{}", parent_short, case_name)));
    }
    1 + case_count
}

/// Append the SubType bodies for `parent_ty`'s supertype and per-case
/// subtypes to `out`. Indices must already be reserved via
/// `assign_flat_gc_indices`.
fn build_flat_gc_subtypes(
    ctx: &yel_core::context::CompilerContext,
    parent_ty: yel_core::Ty,
    registry: &RecordGcTypes,
    out: &mut Vec<SubType>,
) {
    let super_idx = registry.flat_gc_super_idx[&parent_ty];
    let case_count = registry.flat_gc_case_count[&parent_ty];

    // Supertype: `(sub (struct))` — non-final, no parent.
    out.push(SubType {
        is_final: false,
        supertype_idx: None,
        composite_type: CompositeType {
            shared: false,
            inner: CompositeInnerType::Struct(StructType {
                fields: Box::from([]),
            }),
            descriptor: None,
            describes: None,
        },
    });

    for i in 0..case_count {
        let payload_ty = case_payload_ty(ctx, parent_ty, i);
        let fields: Box<[FieldType]> = match payload_ty {
            Some(p_ty) => Box::from([field_storage_for_case_payload(ctx, p_ty, registry)]),
            None => Box::from([]),
        };
        out.push(SubType {
            is_final: true,
            supertype_idx: Some(super_idx),
            composite_type: CompositeType {
                shared: false,
                inner: CompositeInnerType::Struct(StructType { fields }),
                descriptor: None,
                describes: None,
            },
        });
    }
}

/// Structural mirror of
/// `WasmPackageBuilder::flat_gc_migrated` (in `wasm/repr.rs`) and
/// `yel_core::lir::block_lower::is_flat_gc_migrated_ty`. ALL THREE
/// must agree per `Ty` — they decide whether option/result/variant
/// uses the W3C subtype-hierarchy GC repr (1 nullable supertype ref
/// slot) or the canonical-flat repr (multi-slot).
///
/// The free function form lives here because `gc_types` is the type-
/// emission module, called *before* `WasmPackageBuilder` exists. Both
/// `record_field_storage_type` and `list_element_storage_type`
/// consult this gate when deciding whether to emit a typed supertype
/// ref or fall back to anyref / canonical-flat shape.
pub(crate) fn is_flat_gc_migrated(
    ctx: &yel_core::context::CompilerContext,
    ty: yel_core::Ty,
    registry: &RecordGcTypes,
) -> bool {
    let mut visiting = HashSet::new();
    is_flat_gc_migrated_recursive(ctx, ty, registry, &mut visiting)
}

fn is_flat_gc_migrated_recursive(
    ctx: &CompilerContext,
    ty: Ty,
    registry: &RecordGcTypes,
    visiting: &mut HashSet<DefId>,
) -> bool {
    let admitted = match ctx.ty_kind(ty) {
        InternedTyKind::Option(inner) => {
            let inner = *inner;
            if is_gc_eligible_list_ty(ctx, inner) {
                return false;
            }
            if let InternedTyKind::Adt(d) = ctx.ty_kind(inner)
                && matches!(ctx.defs.kind(*d), DefKind::Record(_))
                    && is_dtr_record_for_collapse(ctx, *d)
                {
                    return false;
                }
            is_flat_gc_payload_admissible(ctx, inner, registry, visiting)
        }
        InternedTyKind::Result { ok, err } => {
            let ok_ok = match ok {
                Some(t) => is_flat_gc_payload_admissible(ctx, *t, registry, visiting),
                None => true,
            };
            let err_ok = match err {
                Some(t) => is_flat_gc_payload_admissible(ctx, *t, registry, visiting),
                None => true,
            };
            ok_ok && err_ok
        }
        InternedTyKind::Adt(def_id) => {
            let def_id = *def_id;
            let cases = match ctx.defs.as_variant(def_id) {
                Some(v) => v.cases.clone(),
                None => return false,
            };
            if !visiting.insert(def_id) {
                return true;
            }
            let result = cases.iter().all(|&c| {
                if let DefKind::VariantCase(case) = ctx.defs.kind(c) {
                    match case.payload {
                        None => true,
                        Some(p) => is_flat_gc_payload_admissible(ctx, p, registry, visiting),
                    }
                } else {
                    false
                }
            });
            visiting.remove(&def_id);
            result
        }
        _ => false,
    };
    admitted && registry.flat_gc_super_idx.contains_key(&ty)
}

fn is_flat_gc_payload_admissible(
    ctx: &yel_core::context::CompilerContext,
    ty: yel_core::Ty,
    registry: &RecordGcTypes,
    visiting: &mut std::collections::HashSet<DefId>,
) -> bool {
    use yel_core::definitions::DefKind;
    use yel_core::types::InternedTyKind;
    match ctx.ty_kind(ty) {
        InternedTyKind::Bool
        | InternedTyKind::S8
        | InternedTyKind::S16
        | InternedTyKind::S32
        | InternedTyKind::S64
        | InternedTyKind::U8
        | InternedTyKind::U16
        | InternedTyKind::U32
        | InternedTyKind::U64
        | InternedTyKind::F32
        | InternedTyKind::F64
        | InternedTyKind::Char
        | InternedTyKind::String => true,
        InternedTyKind::List(_) => is_gc_eligible_list_ty(ctx, ty),
        InternedTyKind::Adt(d) => match ctx.defs.kind(*d) {
            DefKind::Enum(_) => true,
            DefKind::Record(_) => is_dtr_record_for_collapse(ctx, *d),
            DefKind::Variant(_) => is_flat_gc_migrated_recursive(ctx, ty, registry, visiting),
            _ => false,
        },
        InternedTyKind::Option(_) | InternedTyKind::Result { .. } => {
            is_flat_gc_migrated_recursive(ctx, ty, registry, visiting)
        }
        _ => false,
    }
}

/// Phase 5e.5: minimal DTR-record check — used by the free
/// `is_flat_gc_migrated` to mirror the option-of-DTR-record collapse
/// gate. Tracks visited def ids to handle recursive records (none
/// today, but cheap defensiveness).
fn is_dtr_record_for_collapse(ctx: &CompilerContext, def_id: DefId) -> bool {
    let mut seen = std::collections::HashSet::new();
    is_dtr_record_for_collapse_inner(ctx, def_id, &mut seen)
}

fn is_dtr_record_for_collapse_inner(
    ctx: &CompilerContext,
    def_id: DefId,
    seen: &mut std::collections::HashSet<DefId>,
) -> bool {
    let record = match ctx.defs.kind(def_id) {
        DefKind::Record(r) => r.clone(),
        _ => return false,
    };
    if !seen.insert(def_id) {
        return true;
    }
    let result = (|| {
        for &field_def_id in &record.fields {
            let field_ty = match ctx.defs.kind(field_def_id) {
                DefKind::Field(f) => f.ty,
                _ => return false,
            };
            // Allowed field shapes for DTR (mirrors yel-core's
            // `is_dtr_field_ty`): primitives, string, scalar-list,
            // nested DTR record. Conservative on edge cases.
            let ok = matches!(
                ctx.ty_kind(field_ty),
                InternedTyKind::Bool
                    | InternedTyKind::S8
                    | InternedTyKind::S16
                    | InternedTyKind::S32
                    | InternedTyKind::S64
                    | InternedTyKind::U8
                    | InternedTyKind::U16
                    | InternedTyKind::U32
                    | InternedTyKind::U64
                    | InternedTyKind::F32
                    | InternedTyKind::F64
                    | InternedTyKind::Char
                    | InternedTyKind::String
            ) || matches!(
                ctx.ty_kind(field_ty),
                InternedTyKind::Adt(d) if matches!(ctx.defs.kind(*d), DefKind::Enum(_))
            ) || is_gc_eligible_list_ty(ctx, field_ty)
                || (matches!(ctx.ty_kind(field_ty), InternedTyKind::Adt(_))
                    && match ctx.ty_kind(field_ty) {
                        InternedTyKind::Adt(d) => {
                            matches!(ctx.defs.kind(*d), DefKind::Record(_))
                                && is_dtr_record_for_collapse_inner(ctx, *d, seen)
                        }
                        _ => false,
                    });
            if !ok {
                return false;
            }
        }
        true
    })();
    seen.remove(&def_id);
    result
}

/// Phase 5e.5 (Stage 2): per-W3C component-model GC ABI proposal,
/// `bool` / `{s,u}8` / `{s,u}16` payload fields use the packed
/// storage types `i8` / `i16`. Other payload types use the same
/// rules as record fields (`record_field_storage_type` — primitive
/// scalars unboxed, strings/non-typed-array lists boxed in
/// `$fat_value`, records/tuples/typed lists as concrete refs,
/// nested option/result/variant as `(ref null $<sup>)`).
///
/// Readers must use `StructGetS` for signed packed types and
/// `StructGetU` for unsigned/bool/char to recover the full-width
/// value (cf. `struct_get_op_for_payload`).
fn field_storage_for_case_payload(
    ctx: &CompilerContext,
    payload_ty: Ty,
    registry: &RecordGcTypes,
) -> FieldType {
    let storage = match ctx.ty_kind(payload_ty) {
        InternedTyKind::Bool | InternedTyKind::S8 | InternedTyKind::U8 => StorageType::I8,
        InternedTyKind::S16 | InternedTyKind::U16 => StorageType::I16,
        _ => StorageType::Val(record_field_storage_type(ctx, payload_ty, registry)),
    };
    // Stage 8: variant case payloads are immutable. Variants are sum-
    // type values; the payload is set once at struct.new and never
    // mutated.
    FieldType {
        element_type: storage,
        mutable: false,
    }
}

/// Phase 5e.5 (Stage 2): how a reader must dispatch when reading a
/// case payload field — sign-extending, zero-extending, or plain.
/// Centralised here so codegen sites that emit `struct.get` /
/// `StructGetS` / `StructGetU` make consistent choices for the same
/// payload type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StructGetVariant {
    /// `struct.get` — non-packed fields.
    Plain,
    /// `struct.get_s` — signed packed (`s8`, `s16`).
    Signed,
    /// `struct.get_u` — unsigned/bool/char packed (`bool`, `u8`,
    /// `u16` — `char` is value-type i32 so still Plain).
    Unsigned,
}

pub(crate) fn struct_get_op_for_payload(ctx: &CompilerContext, payload_ty: Ty) -> StructGetVariant {
    match ctx.ty_kind(payload_ty) {
        InternedTyKind::Bool | InternedTyKind::U8 | InternedTyKind::U16 => {
            StructGetVariant::Unsigned
        }
        InternedTyKind::S8 | InternedTyKind::S16 => StructGetVariant::Signed,
        _ => StructGetVariant::Plain,
    }
}

/// Phase 5e.5: topologically sort flat-gc parent Tys so that every
/// parent is emitted *after* every flat-gc parent it transitively
/// references through case payloads. Records/tuples/lists referenced
/// by payloads do NOT introduce edges here — they emit later in their
/// own rec group (which can forward-reference any flat-gc supertype
/// already emitted).
///
/// Algorithm: Kahn's. YEL has no recursive variants today, so the
/// graph is a DAG. If a cycle is ever introduced, we panic with a
/// descriptive message — supporting recursive variants requires
/// folding the SCC into one rec group (future work).
fn topo_sort_flat_gc_tys(ctx: &CompilerContext, tys: &[Ty]) -> Vec<Ty> {
    let ty_set: HashSet<Ty> = tys.iter().copied().collect();
    // Edges: `parent → payload_parent` when payload_parent is also a
    // flat-gc Ty. We emit `payload_parent` first, so it's a "deps"
    // edge; in-degree of a node = # of flat-gc parents whose payload
    // references it.
    let mut deps: HashMap<Ty, Vec<Ty>> = HashMap::new();
    let mut in_degree: HashMap<Ty, usize> = tys.iter().map(|&t| (t, 0)).collect();
    for &parent in tys {
        let case_count = match flat_gc_case_count(ctx, parent) {
            Some(n) => n,
            None => continue,
        };
        for i in 0..case_count {
            if let Some(p_ty) = case_payload_ty(ctx, parent, i)
                && ty_set.contains(&p_ty) && p_ty != parent {
                    deps.entry(p_ty).or_default().push(parent);
                    *in_degree.entry(parent).or_insert(0) += 1;
                }
        }
    }
    // Start with all zero-in-degree nodes, in original insertion order.
    let mut queue: VecDeque<Ty> = tys
        .iter()
        .copied()
        .filter(|t| in_degree.get(t).copied() == Some(0))
        .collect();
    let mut out: Vec<Ty> = Vec::with_capacity(tys.len());
    while let Some(n) = queue.pop_front() {
        out.push(n);
        if let Some(succs) = deps.get(&n) {
            for &m in succs {
                let entry = in_degree.get_mut(&m).expect("topo: missing in-degree");
                *entry -= 1;
                if *entry == 0 {
                    queue.push_back(m);
                }
            }
        }
    }
    if out.len() != tys.len() {
        panic!(
            "topo_sort_flat_gc_tys: cycle detected in flat-gc Ty graph \
             (recursive variants are not yet supported — sorted {} of {} tys)",
            out.len(),
            tys.len()
        );
    }
    out
}

/// Stage 6 typed-GC migration re-export — the typed filter helper
/// declares its `item` local with the same shape the array stores per
/// element. Public surface for `record_list.rs::generate_filter_function`.
pub(crate) fn list_element_storage_type_pub(
    ctx: &CompilerContext,
    elem_ty: Ty,
    registry: &RecordGcTypes,
) -> ValType {
    list_element_storage_type(ctx, elem_ty, registry)
}

/// Map a list element type to its Phase 5a GC array element ValType.
/// Mirrors `record_field_storage_type` but for the *element* slot of
/// `(array (mut <elem>))`.
fn list_element_storage_type(
    ctx: &CompilerContext,
    elem_ty: Ty,
    registry: &RecordGcTypes,
) -> ValType {
    // Phase 5e.2: nested lists — when the element is itself a typed
    // GC array (list<scalar>, list<DTR-record>, list<list<...>>),
    // store a concrete `(ref null $<inner_arr>)` so callers can
    // `array.get` directly without going through $fat_value.
    if let InternedTyKind::List(_) = ctx.ty_kind(elem_ty)
        && let Some(&inner_arr_idx) = registry.list_array_type_idx.get(&elem_ty) {
            return ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(inner_arr_idx),
            });
        }
    // Phase 5e.3: tuples — store a concrete `(ref null $tuple_<n>)`
    // typed struct ref. The tuple struct type was emitted alongside
    // record types in this rec group.
    if let InternedTyKind::Tuple(_) = ctx.ty_kind(elem_ty)
        && let Some(&tup_idx) = registry.tuple_struct_type_idx.get(&elem_ty) {
            return ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(tup_idx),
            });
        }
    // Option-of-ref collapse — MUST precede the FlatGcStruct check.
    // `internal_repr(option<record|tuple|scalar-list|collapsing-option>)`
    // collapses to the inner's nullable ref (none = null, some(v) = v), so
    // the array element must be that same concrete ref, NOT the `$opt_*`
    // supertype (which is emitted but unused for the collapse case).
    if let Some(vt) = option_collapse_elem_valtype(ctx, elem_ty, registry) {
        return vt;
    }
    // Phase 5e.5 Stage 8a: FlatGcStruct elements (option/result/user
    // variant) — store the concrete supertype ref so consumers can
    // `array.get` directly to a typed ref and ref.test / ref.cast
    // without going through $fat_value.
    if let Some(&super_idx) = registry.flat_gc_super_idx.get(&elem_ty) {
        return ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Concrete(super_idx),
        });
    }
    // Otherwise reuse record-field rules: scalars unboxed, strings as
    // $str_bytes, records as concrete refs, tuples as concrete refs,
    // non-typed-array list as $fat_value.
    record_field_storage_type(ctx, elem_ty, registry)
}

/// If `ty` is an `option<inner>` that collapses to a single nullable ref
/// (mirroring `internal_repr`'s option-of-ref collapse), return that
/// concrete `(ref null $inner)` ValType. `option<string>` does NOT collapse
/// (a null `$str_bytes` is a valid empty string, ambiguous with `none`) —
/// it stays a FlatGcStruct. Recurses through nested collapsing options.
fn option_collapse_elem_valtype(
    ctx: &CompilerContext,
    ty: Ty,
    registry: &RecordGcTypes,
) -> Option<ValType> {
    let inner = match ctx.ty_kind(ty) {
        InternedTyKind::Option(i) => *i,
        _ => return None,
    };
    let mk = |idx: u32| {
        Some(ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Concrete(idx),
        }))
    };
    match ctx.ty_kind(inner) {
        // option<string> stays FlatGcStruct — no collapse.
        InternedTyKind::String => None,
        // option<record> → the record's GC struct ref.
        InternedTyKind::Adt(d) if matches!(ctx.defs.kind(*d), DefKind::Record(_)) => {
            registry.record_type_idx.get(d).copied().and_then(mk)
        }
        // option<tuple> → the tuple's GC struct ref.
        InternedTyKind::Tuple(_) => registry.tuple_struct_type_idx.get(&inner).copied().and_then(mk),
        // option<scalar-list> → the inner list's typed array ref.
        InternedTyKind::List(_) if is_gc_eligible_list_ty(ctx, inner) => {
            registry.list_array_type_idx.get(&inner).copied().and_then(mk)
        }
        // option<collapsing-option> → recurse.
        InternedTyKind::Option(_) => option_collapse_elem_valtype(ctx, inner, registry),
        _ => None,
    }
}

/// Short, lowercased name fragment used in the emitted `<elem>_list`
/// debug name for a list array type.
fn list_elem_short_name(ctx: &CompilerContext, elem_ty: Ty) -> String {
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
/// Gate for whether a `list<T>` can be stored as a typed
/// `(ref null $<elem>_list)` GC array (single-slot element) vs the
/// fat-pointer fallback. Delegates to the yel-core structural predicate so
/// the registration side stays in EXACT lockstep with the LIR mount-path
/// `is_gc_list` decision and codegen's `is_scalar_list_ty`.
fn is_gc_eligible_list_ty(ctx: &CompilerContext, ty: Ty) -> bool {
    let mut seen = std::collections::HashSet::new();
    yel_core::lower_to_lir::is_scalar_list_ty_struct(ctx, ty, &mut seen)
}

fn record_field_storage_type(ctx: &CompilerContext, ty: Ty, registry: &RecordGcTypes) -> ValType {
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
        // Phase 5e.6: every valid `list<T>` is a typed GC array, stored
        // as a concrete `(ref null $<elem>_list)` ref. A list whose
        // element is not a single-slot value (unit/func) is not a valid
        // list element, so no `list_array_type_idx` entry means the type
        // is structurally impossible here.
        InternedTyKind::List(_) => {
            let arr_idx = registry.list_array_type_idx.get(&ty).copied().expect(
                "record_field_storage_type: list<T> with no GC array type — every valid \
                 list is a typed GC array; a non-single-slot element (unit/func) is not a \
                 valid list element",
            );
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(arr_idx),
            })
        }
        InternedTyKind::String => {
            // strings-to-GC (`plans/strings-to-gc.md`): a string element/field
            // is a `(ref null $str_bytes)` GC byte array, not a `$fat_value`
            // (ptr, len) box. Element read/write is then a plain ref get/set.
            let sb_idx = registry.str_bytes_array_idx.expect(
                "record_field_storage_type: String field but $str_bytes array \
                 type not registered",
            );
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(sb_idx),
            })
        }
        // Phase 5e.5 (Stage 6+): option / result that are migrated
        // to the W3C subtype-hierarchy GC repr store as a concrete
        // `(ref null $<parent>_super)` instead of anyref. Both gates
        // (`is_flat_gc_migrated` here AND `internal_repr ==
        // FlatGcStruct`) MUST agree, otherwise a record's field type
        // is a typed ref but `RecordConstruct` pushes flat-canonical
        // slots (or vice versa).
        InternedTyKind::Option(_) | InternedTyKind::Result { .. } => {
            if is_flat_gc_migrated(ctx, ty, registry)
                && let Some(&super_idx) = registry.flat_gc_super_idx.get(&ty) {
                    return ValType::Ref(RefType {
                        nullable: true,
                        heap_type: HeapType::Concrete(super_idx),
                    });
                }
            // Collapsing option<record|tuple|scalar-list>: store the inner's
            // concrete ref (none = null), matching `internal_repr`'s collapse
            // and the signal-storage / list-element rules — NOT anyref, so a
            // record/tuple field read (`struct.get`) is typed and needs no
            // cast. (`option<string>` / flat-gc options were handled above.)
            if let Some(vt) = option_collapse_elem_valtype(ctx, ty, registry) {
                return vt;
            }
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Abstract {
                    shared: false,
                    ty: AbstractHeapType::Any,
                },
            })
        }
        InternedTyKind::Tuple(_) => {
            if let Some(&tup_idx) = registry.tuple_struct_type_idx.get(&ty) {
                ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(tup_idx),
                })
            } else {
                ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Abstract {
                        shared: false,
                        ty: AbstractHeapType::Any,
                    },
                })
            }
        }
        InternedTyKind::Adt(def_id) => match ctx.defs.kind(*def_id) {
            DefKind::Record(_) => {
                // Nested record: reference the sibling record's type
                // via the registry's pre-assigned index. Phase 7
                // merged flat-gc and records into one rec group so
                // forward refs resolve naturally; the index must be
                // present.
                let inner_idx = *registry.record_type_idx.get(def_id).unwrap_or_else(|| {
                    panic!(
                        "record_field_storage_type: record def_id={:?} missing from registry — \
                         the merged flat-gc/records rec group should have pre-assigned every \
                         record_type_idx before this lookup",
                        def_id
                    )
                });
                ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(inner_idx),
                })
            }
            // Enums collapse to a single i32 discriminant, like the
            // `signal_storage_valtypes` flat path.
            DefKind::Enum(_) => ValType::I32,
            // User variants: typed supertype ref when migrated.
            DefKind::Variant(_) => {
                if is_flat_gc_migrated(ctx, ty, registry)
                    && let Some(&super_idx) = registry.flat_gc_super_idx.get(&ty) {
                        return ValType::Ref(RefType {
                            nullable: true,
                            heap_type: HeapType::Concrete(super_idx),
                        });
                    }
                ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Abstract {
                        shared: false,
                        ty: AbstractHeapType::Any,
                    },
                })
            }
            _ => ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Abstract {
                    shared: false,
                    ty: AbstractHeapType::Any,
                },
            }),
        },
        // Function refs / unit / error / unknown: i32 fallback (matches
        // the `signal_storage_valtypes` default).
        _ => ValType::I32,
    }
}
