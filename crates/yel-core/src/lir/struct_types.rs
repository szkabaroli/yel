//! Resource-level GC type registry.
//!
//! Stage 2 of the LIR-resource-flatten plan: a flat-list mirror of the
//! struct + array types the mount-tree synthesizer plans out. Today this
//! exists *alongside* `ComponentTreeShape` — both are populated, both
//! consumed (codegen still reads `tree_shape`; this registry is the
//! eventual replacement).
//!
//! Stage 3 / 4 will rewrite codegen to read this registry directly and
//! drop the dual representation; Stage 5 deletes `ComponentTreeShape`.
//!
//! For now: every `TreeBoundary` projects to one [`LirStructTypeDecl`],
//! every `ForAnchor`'s children-array projects to one [`LirArrayTypeDecl`].
//! The registry is a flat `Vec` indexed by `LirStructTypeIdx` /
//! `LirArrayTypeIdx`. Cross-references between types (a struct field
//! holding a sub-struct ref, an array's element type) are stored as
//! these indices — no parent_link / SubBoundary chasing.
//!
//! Codegen will consume the registry by allocating one wasm-GC type per
//! entry, in order, and resolving every cross-reference into a wasm
//! type-section index.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use super::block::{LirSlotValType, TreeBoundaryKind};

/// Index into [`LirResource::struct_types`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LirStructTypeIdx(pub u32);

/// Index into [`LirResource::array_types`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LirArrayTypeIdx(pub u32);

/// One GC struct type the resource owns.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirStructTypeDecl {
    /// Debug name surfaced in the wasm name section. Derived from
    /// the originating tree boundary's role (e.g. `tree_root`,
    /// `if_3_then`, `for_0_iter`). Today: per-resource, no
    /// uniqueness across resources required.
    pub name: String,
    /// Field declarations, in struct-field order. The wasm
    /// type-section index of any cross-reference (sub-struct ref,
    /// children-array ref) is computed at codegen time from the
    /// referenced [`LirStructTypeIdx`] / [`LirArrayTypeIdx`].
    pub fields: Vec<LirStructFieldDecl>,
    /// What role this struct plays in the mount tree —
    /// `Root` / `IfAnchor` / `IfBranch` / `ForAnchor` / `ForIterBody`.
    /// Mirrors the projected `TreeBoundary.kind`. Used by codegen to
    /// route Root vs sub-boundary handling and to identify which
    /// structs need a companion `(array)` type (`ForAnchor` only).
    pub kind: TreeBoundaryKind,
    /// Pointer back to the parent struct + the field index on the
    /// parent that holds this struct's ref. `None` for the root and
    /// for `ForIterBody` (reachable only through the for-anchor's
    /// children array, not via a static `LirRefTarget::Struct` field).
    /// Mirrors `TreeBoundary.parent_link`.
    pub parent: Option<LirStructParentLink>,
}

/// Parent link from a struct to its declaring parent struct.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct LirStructParentLink {
    /// Index into [`LirResource::struct_types`].
    pub parent: LirStructTypeIdx,
    /// Field index on `parent`'s struct whose `ref_target =
    /// LirRefTarget::Struct(self_index)` — i.e. the slot that holds
    /// a ref to *this* struct.
    pub field_idx: u32,
}

/// One field of a [`LirStructTypeDecl`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirStructFieldDecl {
    /// Debug name surfaced in the wasm name section.
    pub name: String,
    /// Storage shape of the field. For scalar payloads this drives
    /// the wasm field type directly; for cross-references it's a
    /// `RefNullForBoundary` / `RefNullForChildrenArray` and the
    /// resolved type-section index comes from
    /// [`LirStructFieldDecl::ref_target`].
    pub val_ty: LirSlotValType,
    /// Cross-reference target if this field is a sub-struct ref or
    /// children-array ref. `None` for scalar / DOM-handle fields.
    pub ref_target: Option<LirRefTarget>,
    /// Semantic role of the field. `DomHandle` / `LoopVar` /
    /// `SubBoundary` / `ChildrenArray` / `ActiveTag` mirror the
    /// projected `TreeFieldDecl` variants. Consumers that need to
    /// disambiguate scalar fields by role (e.g. unmount only
    /// detaches `DomHandle`s, not `ActiveTag`s, even though both are
    /// `(mut i32)`) read this discriminator.
    pub role: LirFieldRole,
    /// Always `true` today — the synthesizer never declares immutable
    /// fields. Kept as a field so future passes can tighten select
    /// fields (e.g. once-set children arrays) into immutable
    /// declarations without changing the registry shape.
    pub mutable: bool,
}

/// Semantic role of a [`LirStructFieldDecl`]. Mirrors the projected
/// `TreeFieldDecl` variants. Two `(mut i32)` fields with different
/// roles (`DomHandle` vs `ActiveTag`) are distinguishable here even
/// though their `val_ty` / `ref_target` are identical.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LirFieldRole {
    /// `(mut i32)` field holding a DOM handle. Detached on unmount.
    DomHandle,
    /// Loop-variable field on a `ForIterBody` struct.
    LoopVar,
    /// `(mut (ref null <child_struct>))` — points at a child boundary.
    SubBoundary,
    /// `(mut (ref null <iter_body_arr>))` — for-anchor's children array.
    ChildrenArray,
    /// `(mut i32)` flag tracking the active branch of an `if`.
    ActiveTag,
}

/// Cross-reference from a struct field to another resource type.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum LirRefTarget {
    /// `(ref null <struct>)` — the field holds a nullable ref to
    /// the indexed struct type in the same resource registry.
    Struct(LirStructTypeIdx),
    /// `(ref null <array>)` — the field holds a nullable ref to
    /// the indexed array type in the same resource registry.
    Array(LirArrayTypeIdx),
}

/// One GC array type the resource owns. Element type is always a
/// nullable ref to a struct (today's only use-case is for-anchor
/// children arrays whose element is the iter-body struct).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirArrayTypeDecl {
    /// Debug name surfaced in the wasm name section.
    pub name: String,
    /// Element type — a nullable ref to a struct in the resource
    /// registry. Generalized when other shapes need it.
    pub element: LirArrayElement,
    /// Always `true` today.
    pub mutable: bool,
}

/// Array element type. Currently only struct-ref elements are
/// produced by the synthesizer; widen as needed.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum LirArrayElement {
    /// `(ref null <struct>)`.
    StructRef(LirStructTypeIdx),
}

// ============================================================================
// Stage 2 projection — `ComponentTreeShape` → flat (struct, array) registry
// ============================================================================
//
// One pass over the synthesized boundaries. `TreeBoundary` → one
// `LirStructTypeDecl`; `ForAnchor`'s children-array → one
// `LirArrayTypeDecl`. Cross-references between them resolve via
// `TreeBoundaryId` ↔ `LirStructTypeIdx` 1:1 mapping (boundary index
// becomes struct index; for-anchor's array gets the next free array
// index). The mapping holds because the synthesizer never elides
// boundaries today.

use super::block::{ComponentTreeShape, TreeFieldDecl};

/// Project a [`ComponentTreeShape`] into the flat (struct, array)
/// registry used by Stage 2+. Boundary index `i` becomes
/// [`LirStructTypeIdx`]`(i)`; each `ForAnchor`'s children array gets
/// the next free [`LirArrayTypeIdx`].
pub fn project_tree_shape(
    tree: &ComponentTreeShape,
) -> (Vec<LirStructTypeDecl>, Vec<LirArrayTypeDecl>) {
    let mut structs: Vec<LirStructTypeDecl> = Vec::with_capacity(tree.boundaries.len());
    let mut arrays: Vec<LirArrayTypeDecl> = Vec::new();
    // First pass: allocate one array per ForAnchor so cross-refs from
    // ChildrenArray fields can index by struct boundary.
    // Keyed by iter-body boundary index — that's what the
    // ChildrenArray field decl carries as `arr_target_idx`.
    let mut iter_body_to_array: HashMap<u32, LirArrayTypeIdx> = HashMap::new();
    for boundary in &tree.boundaries {
        if let TreeBoundaryKind::ForAnchor {
            for_id,
            iter_body_idx,
        } = &boundary.kind
        {
            let arr_idx = LirArrayTypeIdx(arrays.len() as u32);
            arrays.push(LirArrayTypeDecl {
                name: format!("for_{}_arr", for_id.0),
                element: LirArrayElement::StructRef(LirStructTypeIdx(*iter_body_idx)),
                mutable: true,
            });
            iter_body_to_array.insert(*iter_body_idx, arr_idx);
        }
    }
    // Second pass: project each boundary into a struct decl.
    for boundary in &tree.boundaries {
        let name = struct_name_for(&boundary.kind);
        let mut fields: Vec<LirStructFieldDecl> = Vec::with_capacity(boundary.fields.len());
        for field in &boundary.fields {
            fields.push(project_field(field, &iter_body_to_array));
        }
        let parent = boundary.parent_link.map(|(pid, fi)| LirStructParentLink {
            parent: LirStructTypeIdx(pid.0),
            field_idx: fi,
        });
        structs.push(LirStructTypeDecl {
            name,
            fields,
            kind: boundary.kind.clone(),
            parent,
        });
    }
    (structs, arrays)
}

fn struct_name_for(kind: &TreeBoundaryKind) -> String {
    match kind {
        TreeBoundaryKind::Root => "tree_root".to_string(),
        TreeBoundaryKind::IfAnchor { if_id, .. } => format!("if_{}", if_id.0),
        TreeBoundaryKind::IfBranch { if_id, branch_idx } => match branch_idx {
            0 => format!("if_{}_then", if_id.0),
            n => format!("if_{}_branch_{}", if_id.0, n),
        },
        TreeBoundaryKind::ForAnchor { for_id, .. } => format!("for_{}", for_id.0),
        TreeBoundaryKind::ForIterBody { for_id } => format!("for_{}_iter", for_id.0),
    }
}

fn project_field(
    decl: &TreeFieldDecl,
    iter_body_to_array: &HashMap<u32, LirArrayTypeIdx>,
) -> LirStructFieldDecl {
    use super::block::LirSlotValType;
    match decl {
        TreeFieldDecl::DomHandle { name } => LirStructFieldDecl {
            name: name.clone(),
            val_ty: LirSlotValType::I32,
            ref_target: None,
            role: LirFieldRole::DomHandle,
            mutable: true,
        },
        TreeFieldDecl::LoopVar { name, val_ty } => LirStructFieldDecl {
            name: name.clone(),
            val_ty: val_ty.clone(),
            ref_target: None,
            role: LirFieldRole::LoopVar,
            mutable: true,
        },
        TreeFieldDecl::SubBoundary { name, target_idx } => LirStructFieldDecl {
            name: name.clone(),
            // Placeholder val_ty: codegen rewrites to a typed ref at
            // emit time using `ref_target`. Stage 3 will refine.
            val_ty: LirSlotValType::I32,
            ref_target: Some(LirRefTarget::Struct(LirStructTypeIdx(*target_idx))),
            role: LirFieldRole::SubBoundary,
            mutable: true,
        },
        TreeFieldDecl::ChildrenArray {
            name,
            arr_target_idx,
        } => {
            // `arr_target_idx` is the iter-body boundary index. The
            // first pass mapped each iter-body to its owning array,
            // so this is a direct lookup.
            let arr_idx = iter_body_to_array
                .get(arr_target_idx)
                .copied()
                .unwrap_or(LirArrayTypeIdx(0));
            LirStructFieldDecl {
                name: name.clone(),
                val_ty: LirSlotValType::I32,
                ref_target: Some(LirRefTarget::Array(arr_idx)),
                role: LirFieldRole::ChildrenArray,
                mutable: true,
            }
        }
        TreeFieldDecl::ActiveTag { name } => LirStructFieldDecl {
            name: name.clone(),
            val_ty: LirSlotValType::I32,
            ref_target: None,
            role: LirFieldRole::ActiveTag,
            mutable: true,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ids::{ForId, IfId, TreeBoundaryId};
    use crate::lir::block::{
        ComponentTreeShape, LirSlotValType, TreeBoundary, TreeBoundaryKind, TreeFieldDecl,
    };

    /// Round-trip projection: each `TreeBoundary` becomes one
    /// [`LirStructTypeDecl`] at the same index, each `ForAnchor`
    /// produces one [`LirArrayTypeDecl`], and `ChildrenArray` field
    /// cross-references resolve to the right array type.
    #[test]
    fn project_tree_shape_one_for_in_root() {
        // Synthesize a hand-built TreeShape:
        //   Root {
        //     for_0_anchor: SubBoundary -> ForAnchor[1]
        //   }
        //   ForAnchor[1] {
        //     parent: DomHandle
        //     anchor: DomHandle
        //     children: ChildrenArray -> iter_body[2]
        //   }
        //   ForIterBody[2] {
        //     loop_var: LoopVar(I32)
        //     wrapper: DomHandle
        //   }
        let mut tree = ComponentTreeShape::default();
        tree.boundaries.push(TreeBoundary {
            id: TreeBoundaryId(0),
            kind: TreeBoundaryKind::Root,
            fields: vec![TreeFieldDecl::SubBoundary {
                name: "for_0".to_string(),
                target_idx: 1,
            }],
            parent_link: None,
        });
        tree.boundaries.push(TreeBoundary {
            id: TreeBoundaryId(1),
            kind: TreeBoundaryKind::ForAnchor {
                for_id: ForId(0),
                iter_body_idx: 2,
            },
            fields: vec![
                TreeFieldDecl::DomHandle {
                    name: "parent".to_string(),
                },
                TreeFieldDecl::DomHandle {
                    name: "anchor".to_string(),
                },
                TreeFieldDecl::ChildrenArray {
                    name: "children".to_string(),
                    arr_target_idx: 2,
                },
            ],
            parent_link: Some((TreeBoundaryId(0), 0)),
        });
        tree.boundaries.push(TreeBoundary {
            id: TreeBoundaryId(2),
            kind: TreeBoundaryKind::ForIterBody { for_id: ForId(0) },
            fields: vec![
                TreeFieldDecl::LoopVar {
                    name: "loop_var".to_string(),
                    val_ty: LirSlotValType::I32,
                },
                TreeFieldDecl::DomHandle {
                    name: "wrapper".to_string(),
                },
            ],
            parent_link: None,
        });
        tree.root_idx = 0;

        let (structs, arrays) = project_tree_shape(&tree);
        assert_eq!(structs.len(), 3, "one struct per boundary");
        assert_eq!(arrays.len(), 1, "one array per ForAnchor");

        // Root struct: SubBoundary field projects to a Struct ref target.
        assert_eq!(structs[0].name, "tree_root");
        assert_eq!(structs[0].fields.len(), 1);
        assert!(matches!(
            structs[0].fields[0].ref_target,
            Some(LirRefTarget::Struct(LirStructTypeIdx(1)))
        ));

        // ForAnchor struct: third field is the ChildrenArray, target = arrays[0].
        assert_eq!(structs[1].name, "for_0");
        assert_eq!(structs[1].fields.len(), 3);
        assert!(matches!(
            structs[1].fields[2].ref_target,
            Some(LirRefTarget::Array(LirArrayTypeIdx(0)))
        ));

        // ForIterBody struct: scalar-only fields, no ref_target.
        assert_eq!(structs[2].name, "for_0_iter");
        assert_eq!(structs[2].fields.len(), 2);
        assert!(structs[2].fields.iter().all(|f| f.ref_target.is_none()));

        // The array's element type points at the iter-body struct.
        assert!(matches!(
            arrays[0].element,
            LirArrayElement::StructRef(LirStructTypeIdx(2))
        ));
    }

    /// Empty tree shape projects to empty registries.
    #[test]
    fn project_tree_shape_empty() {
        let tree = ComponentTreeShape::default();
        let (structs, arrays) = project_tree_shape(&tree);
        assert!(structs.is_empty());
        assert!(arrays.is_empty());
    }

    /// `IfAnchor` + `IfBranch` boundaries project with the right names
    /// (`if_<id>`, `if_<id>_then`, `if_<id>_branch_<n>`).
    #[test]
    fn project_tree_shape_if_naming() {
        let mut tree = ComponentTreeShape::default();
        tree.boundaries.push(TreeBoundary {
            id: TreeBoundaryId(0),
            kind: TreeBoundaryKind::IfAnchor {
                if_id: IfId(7),
                branches: vec![1, 2],
            },
            fields: vec![],
            parent_link: None,
        });
        tree.boundaries.push(TreeBoundary {
            id: TreeBoundaryId(1),
            kind: TreeBoundaryKind::IfBranch {
                if_id: IfId(7),
                branch_idx: 0,
            },
            fields: vec![],
            parent_link: Some((TreeBoundaryId(0), 0)),
        });
        tree.boundaries.push(TreeBoundary {
            id: TreeBoundaryId(2),
            kind: TreeBoundaryKind::IfBranch {
                if_id: IfId(7),
                branch_idx: 1,
            },
            fields: vec![],
            parent_link: Some((TreeBoundaryId(0), 0)),
        });

        let (structs, _) = project_tree_shape(&tree);
        assert_eq!(structs[0].name, "if_7");
        assert_eq!(structs[1].name, "if_7_then");
        assert_eq!(structs[2].name, "if_7_branch_1");
    }
}
