//! The producer half: in-process handles → wire form.

use rustc_hash::FxHashMap;
use serde::Serialize;
use yelc_base::Interner;

use crate::definitions::{Definition, Definitions, MemberDirection, MemberKind};
use crate::ids::DefId;
use crate::types::{Ty, TyKind, TypeInterner};

use super::PackageName;
use super::wire::{
    SerializedDef, SerializedDefPath, SerializedMember, SerializedMemberKind, StructuralTy,
    TypeIndex,
};

/// Rewrites compilation-local handles into artifact-local ones.
///
/// Owns the type table being built, so every `Ty` that reaches the wire goes
/// through [`ArtifactWriter::write_ty`] and arrives as a [`TypeIndex`].
pub struct ArtifactWriter<'a> {
    names: &'a Interner,
    types: &'a TypeInterner,
    defs: &'a Definitions,
    package: &'a PackageName,
    table: Vec<StructuralTy>,
    /// `Ty` → its position in `table`. Preserves the interner's uniquing: two
    /// equal types share a handle in memory and one entry on the wire.
    seen: FxHashMap<Ty, TypeIndex>,
}

impl<'a> ArtifactWriter<'a> {
    pub fn new(
        package: &'a PackageName,
        names: &'a Interner,
        types: &'a TypeInterner,
        defs: &'a Definitions,
    ) -> Self {
        Self {
            names,
            types,
            defs,
            package,
            table: Vec::new(),
            seen: FxHashMap::default(),
        }
    }

    /// The member rows of a definition, in table (= source) order.
    pub fn write_members(&mut self, id: DefId) -> Vec<SerializedMember> {
        let members = self.defs.members(id).to_vec();
        members
            .iter()
            .map(|member| SerializedMember {
                name: self.names.str(member.name).to_string(),
                kind: match member.kind {
                    MemberKind::Field => SerializedMemberKind::Field,
                    MemberKind::Case => SerializedMemberKind::Case,
                    MemberKind::Property { direction } => match direction {
                        MemberDirection::None => SerializedMemberKind::Property,
                        MemberDirection::In => SerializedMemberKind::PropertyIn,
                        MemberDirection::Out => SerializedMemberKind::PropertyOut,
                        MemberDirection::InOut => SerializedMemberKind::PropertyInOut,
                    },
                    MemberKind::Function => SerializedMemberKind::Function,
                },
                ty: member.ty.map(|ty| self.write_ty(ty)),
            })
            .collect()
    }

    /// The finished type table, in the order entries were written.
    ///
    /// Children always precede their parent, because [`write_ty`](Self::write_ty)
    /// pushes a parent only after recursing. The loader relies on that: it can
    /// resolve the table in one forward pass.
    pub fn into_table(self) -> Vec<StructuralTy> {
        self.table
    }

    /// Write a type structurally, returning its artifact-local index.
    pub fn write_ty(&mut self, ty: Ty) -> TypeIndex {
        if let Some(&index) = self.seen.get(&ty) {
            return index;
        }

        let structural = match self.types.kind(ty) {
            TyKind::Bool => StructuralTy::Bool,
            TyKind::S8 => StructuralTy::S8,
            TyKind::S16 => StructuralTy::S16,
            TyKind::S32 => StructuralTy::S32,
            TyKind::S64 => StructuralTy::S64,
            TyKind::U8 => StructuralTy::U8,
            TyKind::U16 => StructuralTy::U16,
            TyKind::U32 => StructuralTy::U32,
            TyKind::U64 => StructuralTy::U64,
            TyKind::F32 => StructuralTy::F32,
            TyKind::F64 => StructuralTy::F64,
            TyKind::Char => StructuralTy::Char,
            TyKind::String => StructuralTy::String,

            TyKind::List(element) => StructuralTy::List(self.write_child(ty, element)),
            TyKind::Option(element) => StructuralTy::Option(self.write_child(ty, element)),
            TyKind::Result { ok, err } => StructuralTy::Result {
                ok: ok.map(|inner| self.write_child(ty, inner)),
                err: err.map(|inner| self.write_child(ty, inner)),
            },
            TyKind::Tuple(elements) => StructuralTy::Tuple(
                elements
                    .into_iter()
                    .map(|element| self.write_child(ty, element))
                    .collect(),
            ),

            TyKind::Adt(def) => StructuralTy::Adt(self.write_def_path(def)),

            TyKind::Func { params, ret } => StructuralTy::Func {
                params: params
                    .into_iter()
                    .map(|param| self.write_child(ty, param))
                    .collect(),
                ret: ret.map(|inner| self.write_child(ty, inner)),
            },

            TyKind::Param(index) => StructuralTy::Param(index),
            TyKind::Infer(index) => StructuralTy::Infer(index),
            TyKind::Error => StructuralTy::Error,
            TyKind::Unit => StructuralTy::Unit,
        };

        let index = TypeIndex::try_from(self.table.len()).expect("type table exceeded u32::MAX");
        self.table.push(structural);
        self.seen.insert(ty, index);
        index
    }

    /// Write a nested type, asserting the interner's acyclicity on the way.
    ///
    /// `TypeInterner::intern` can only be handed handles that already exist, so
    /// a child is always interned before its parent and therefore always has a
    /// smaller handle. That is what makes the recursion in
    /// [`write_ty`](Self::write_ty) terminate without a visited set — and since
    /// it is load-bearing rather than incidental, it is checked rather than
    /// assumed.
    fn write_child(&mut self, parent: Ty, child: Ty) -> TypeIndex {
        assert!(
            child.index() < parent.index(),
            "type {child} is a child of {parent} but was interned after it; \
             the interner is no longer acyclic and write_ty would not terminate",
        );
        self.write_ty(child)
    }

    /// Write a definition's identity as a resolvable path.
    ///
    /// # Panics
    ///
    /// If `def` belongs to another package. That is a compiler bug in the same
    /// class as reading lang-items before resolution
    /// ([`CompilerContext::known`](crate::CompilerContext::known)): a
    /// cross-package `DefId` has no entry in this table to read a name out of,
    /// so there is nothing to write and no honest fallback.
    pub fn write_def_path(&mut self, def: DefId) -> SerializedDefPath {
        assert_eq!(
            def.package,
            self.defs.package(),
            "a DefId from another package reached the artifact writer; \
             cross-package references need that package's Definitions table",
        );
        let definition = self.defs.get(def);
        SerializedDefPath {
            package: self.package.clone(),
            kind: definition.kind,
            segments: vec![self.names.str(definition.name).to_string()],
            // Empty even though `Definitions` now holds an `OverloadKey`: the
            // loader cannot consume one. See `wire::SerializedDefPath::overload`.
            overload: Vec::new(),
        }
    }
}

/// Lowers a value holding compilation-local handles to its wire form.
///
/// # Why this is a trait rather than four functions
///
/// Stage 3 adds HIR nodes and the total `types` map to the artifact. The only
/// thing it must not do is write a `Ty` or a `DefId` — and the only way to
/// obtain a wire value for either is through [`ArtifactWriter`], which is
/// supplied here and nowhere else. Implementing this trait is therefore both the
/// mechanism and the constraint.
///
/// The `Wire` associated type is what keeps the two halves honest: `Ty::Wire` is
/// [`TypeIndex`] and `DefId::Wire` is [`SerializedDefPath`], so a struct
/// deriving its wire form from its field types cannot accidentally carry a
/// handle across.
pub trait ToArtifact {
    /// The compilation-independent form.
    type Wire: Serialize;

    fn to_artifact(&self, writer: &mut ArtifactWriter<'_>) -> Self::Wire;
}

impl ToArtifact for Ty {
    type Wire = TypeIndex;

    fn to_artifact(&self, writer: &mut ArtifactWriter<'_>) -> Self::Wire {
        writer.write_ty(*self)
    }
}

impl ToArtifact for DefId {
    type Wire = SerializedDefPath;

    fn to_artifact(&self, writer: &mut ArtifactWriter<'_>) -> Self::Wire {
        writer.write_def_path(*self)
    }
}

impl ToArtifact for Definition {
    type Wire = SerializedDef;

    fn to_artifact(&self, writer: &mut ArtifactWriter<'_>) -> Self::Wire {
        SerializedDef {
            path: writer.write_def_path(self.id),
            ty: self.ty.to_artifact(writer),
            is_export: self.is_export,
            members: writer.write_members(self.id),
        }
    }
}

impl<T: ToArtifact> ToArtifact for Option<T> {
    type Wire = Option<T::Wire>;

    fn to_artifact(&self, writer: &mut ArtifactWriter<'_>) -> Self::Wire {
        self.as_ref().map(|value| value.to_artifact(writer))
    }
}

impl<T: ToArtifact> ToArtifact for Vec<T> {
    type Wire = Vec<T::Wire>;

    fn to_artifact(&self, writer: &mut ArtifactWriter<'_>) -> Self::Wire {
        self.iter().map(|value| value.to_artifact(writer)).collect()
    }
}
