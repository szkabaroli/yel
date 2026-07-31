//! The consumer half: wire form → this compilation's handles.

use rustc_hash::FxHashMap;
use serde::de::DeserializeOwned;
use yelc_base::{Interner, Span};

use crate::definitions::{Definitions, Member, MemberDirection, MemberKind};
use crate::ids::{DefId, PackageId};
use crate::types::{Ty, TyKind, TypeInterner};

use super::wire::{SerializedDefPath, SerializedMemberKind, StructuralTy, TypeIndex};
use super::{Artifact, LoadError, PackageName};

/// A package read back into *this* compilation.
///
/// Holds the two remaps that make an artifact usable: artifact type index → a
/// [`Ty`] in the consumer's interner, and [`SerializedDefPath`] → a [`DefId`] in
/// the consumer's tables. Both are complete before this value exists, which is
/// why [`FromArtifact`] takes it by shared reference: resolution is finished, so
/// nothing a consumer does can leave it half-built.
pub struct LoadedPackage<'a> {
    names: &'a Interner,
    package: PackageName,
    defs: Definitions,
    type_remap: Vec<Ty>,
    def_remap: FxHashMap<SerializedDefPath, DefId>,
}

impl<'a> LoadedPackage<'a> {
    pub fn package(&self) -> &PackageName {
        &self.package
    }

    /// The loaded definitions, registered under the [`PackageId`] the caller
    /// assigned at load time.
    pub fn defs(&self) -> &Definitions {
        &self.defs
    }

    pub fn into_defs(self) -> Definitions {
        self.defs
    }

    pub fn names(&self) -> &'a Interner {
        self.names
    }

    /// Resolve an artifact type index to a handle in the consumer's interner.
    pub fn ty(&self, index: TypeIndex) -> Result<Ty, LoadError> {
        self.type_remap
            .get(index as usize)
            .copied()
            .ok_or(LoadError::TypeIndexOutOfRange {
                referenced: index,
                table_len: self.type_remap.len(),
            })
    }

    /// Resolve a path to a definition in the consumer's tables.
    ///
    /// The returned [`DefId`]'s index is the consumer's, and in general is not
    /// the producer's — which is the whole reason paths exist.
    pub fn def(&self, path: &SerializedDefPath) -> Result<DefId, LoadError> {
        self.def_remap
            .get(path)
            .copied()
            .ok_or_else(|| LoadError::UnresolvedDefPath(Box::new(path.clone())))
    }

    /// Number of types re-interned from the artifact.
    pub fn type_count(&self) -> usize {
        self.type_remap.len()
    }
}

/// Rebuilds a value from its wire form, resolving every artifact-local
/// reference through the consumer's tables.
///
/// The inverse of [`ToArtifact`](super::ToArtifact), and the half stage 3 needs
/// for HIR nodes. Fallible where the writer is not, because a wire value is
/// input: it can reference a type index that does not exist or a path that does
/// not resolve, and both must be a diagnosed rejection rather than a panic.
pub trait FromArtifact: Sized {
    /// The compilation-independent form.
    type Wire: DeserializeOwned;

    fn from_artifact(wire: &Self::Wire, package: &LoadedPackage<'_>) -> Result<Self, LoadError>;
}

impl FromArtifact for Ty {
    type Wire = TypeIndex;

    fn from_artifact(wire: &Self::Wire, package: &LoadedPackage<'_>) -> Result<Self, LoadError> {
        package.ty(*wire)
    }
}

impl FromArtifact for DefId {
    type Wire = SerializedDefPath;

    fn from_artifact(wire: &Self::Wire, package: &LoadedPackage<'_>) -> Result<Self, LoadError> {
        package.def(wire)
    }
}

impl<T: FromArtifact> FromArtifact for Option<T> {
    type Wire = Option<T::Wire>;

    fn from_artifact(wire: &Self::Wire, package: &LoadedPackage<'_>) -> Result<Self, LoadError> {
        wire.as_ref()
            .map(|value| T::from_artifact(value, package))
            .transpose()
    }
}

impl<T: FromArtifact> FromArtifact for Vec<T> {
    type Wire = Vec<T::Wire>;

    fn from_artifact(wire: &Self::Wire, package: &LoadedPackage<'_>) -> Result<Self, LoadError> {
        wire.iter()
            .map(|value| T::from_artifact(value, package))
            .collect()
    }
}

impl Artifact {
    /// Read this artifact into the consumer's tables.
    ///
    /// `package` is the [`PackageId`] the consumer has assigned to this
    /// dependency; every loaded [`DefId`] is qualified with it.
    ///
    /// # Rejection is total
    ///
    /// A stamp mismatch on either field returns immediately, before any table is
    /// touched. There is no partial load and no best-effort: a compiler change
    /// can alter what a definition *means* without altering its encoding, so a
    /// stale artifact that deserializes cleanly miscompiles with no diagnostic.
    /// The artifact is a cache and the source is always available.
    pub fn load<'a>(
        &self,
        package: PackageId,
        names: &'a Interner,
        types: &TypeInterner,
    ) -> Result<LoadedPackage<'a>, LoadError> {
        self.load_into(Definitions::new(package), names, types)
    }

    /// Read this artifact into a [`Definitions`] the caller already owns.
    ///
    /// The primitive [`Artifact::load`] is built on, and it deliberately does
    /// **not** require the table to be empty. A loaded definition's index is
    /// therefore `existing + n`, not `n` — which is the fact a `DefId` written
    /// to the wire would silently bake in, and the reason a
    /// [`SerializedDefPath`] is written instead. A caller that assumed the two
    /// indices agree would be right only for the empty case.
    pub fn load_into<'a>(
        &self,
        defs: Definitions,
        names: &'a Interner,
        types: &TypeInterner,
    ) -> Result<LoadedPackage<'a>, LoadError> {
        self.stamp.check()?;

        // Pass 1 — register every definition by name and namespace. No types
        // yet: a definition's declared type may mention an ADT, and that ADT's
        // path can only be resolved once the definitions exist. Registration
        // needs neither.
        let mut defs = defs;
        let mut def_remap = FxHashMap::default();
        let mut registered = Vec::with_capacity(self.defs.len());

        for serialized in &self.defs {
            let leaf = serialized
                .path
                .leaf()
                .ok_or_else(|| LoadError::PathWithoutSegments(Box::new(serialized.path.clone())))?;
            let id = defs
                .register(
                    names.intern(leaf),
                    serialized.path.kind,
                    // A producer span cannot be rendered by a consumer that has
                    // not read the producer's sources — see `wire::SerializedDef`.
                    Span::default(),
                    serialized.is_export,
                )
                .map_err(|_| LoadError::DuplicateDefinition(Box::new(serialized.path.clone())))?;
            // `insert`'s return is deliberately dropped. A second entry for a
            // path would mean two definitions with the same leaf name, and
            // `defs.register` above is single-namespace — it rejects that first,
            // for every artifact, always. The guard that used to stand here
            // returned the same error from a branch nothing could reach, which
            // advertised a check that could not be tested; the duplicate rule
            // has exactly one enforcement site and it is the symbol table's.
            def_remap.insert(serialized.path.clone(), id);
            registered.push(id);
        }

        // Pass 2 — walk the type table front to back, re-interning each entry
        // into the consumer's interner. One forward pass suffices because the
        // writer pushes a parent only after its children; `resolve_child`
        // rejects anything that violates that rather than reading a stale slot.
        let mut type_remap: Vec<Ty> = Vec::with_capacity(self.types.len());
        for (position, entry) in self.types.iter().enumerate() {
            let entry_index = TypeIndex::try_from(position).expect("type table exceeded u32::MAX");
            let table_len = self.types.len();
            // Scoped so that `child`'s borrow of `type_remap` ends before the
            // push below extends it.
            let kind = {
                let child = |referenced: TypeIndex| -> Result<Ty, LoadError> {
                    resolve_child(&type_remap, table_len, entry_index, referenced)
                };

                match entry {
                    StructuralTy::Bool => TyKind::Bool,
                    StructuralTy::S8 => TyKind::S8,
                    StructuralTy::S16 => TyKind::S16,
                    StructuralTy::S32 => TyKind::S32,
                    StructuralTy::S64 => TyKind::S64,
                    StructuralTy::U8 => TyKind::U8,
                    StructuralTy::U16 => TyKind::U16,
                    StructuralTy::U32 => TyKind::U32,
                    StructuralTy::U64 => TyKind::U64,
                    StructuralTy::F32 => TyKind::F32,
                    StructuralTy::F64 => TyKind::F64,
                    StructuralTy::Char => TyKind::Char,
                    StructuralTy::String => TyKind::String,

                    StructuralTy::List(element) => TyKind::List(child(*element)?),
                    StructuralTy::Option(element) => TyKind::Option(child(*element)?),
                    StructuralTy::Result { ok, err } => TyKind::Result {
                        ok: ok.map(child).transpose()?,
                        err: err.map(child).transpose()?,
                    },
                    StructuralTy::Tuple(elements) => TyKind::Tuple(
                        elements
                            .iter()
                            .map(|element| child(*element))
                            .collect::<Result<_, _>>()?,
                    ),

                    StructuralTy::Adt(path) => TyKind::Adt(
                        def_remap
                            .get(path)
                            .copied()
                            .ok_or_else(|| LoadError::UnresolvedDefPath(Box::new(path.clone())))?,
                    ),

                    StructuralTy::Func { params, ret } => TyKind::Func {
                        params: params
                            .iter()
                            .map(|param| child(*param))
                            .collect::<Result<_, _>>()?,
                        ret: ret.map(child).transpose()?,
                    },

                    StructuralTy::Param(index) => TyKind::Param(*index),
                    StructuralTy::Infer(index) => TyKind::Infer(*index),
                    StructuralTy::Error => TyKind::Error,
                    StructuralTy::Unit => TyKind::Unit,
                }
            };

            type_remap.push(types.intern(kind));
        }

        // Pass 3 — attach declared types and member rows, now that both
        // remaps exist.
        for (serialized, id) in self.defs.iter().zip(&registered) {
            if let Some(index) = serialized.ty {
                let ty = type_remap.get(index as usize).copied().ok_or(
                    LoadError::TypeIndexOutOfRange {
                        referenced: index,
                        table_len: self.types.len(),
                    },
                )?;
                defs.set_ty(*id, ty);
            }
            for member in &serialized.members {
                let ty = match member.ty {
                    Some(index) => Some(type_remap.get(index as usize).copied().ok_or(
                        LoadError::TypeIndexOutOfRange {
                            referenced: index,
                            table_len: self.types.len(),
                        },
                    )?),
                    None => None,
                };
                let kind = match member.kind {
                    SerializedMemberKind::Field => MemberKind::Field,
                    SerializedMemberKind::Case => MemberKind::Case,
                    SerializedMemberKind::Property => MemberKind::Property {
                        direction: MemberDirection::None,
                    },
                    SerializedMemberKind::PropertyIn => MemberKind::Property {
                        direction: MemberDirection::In,
                    },
                    SerializedMemberKind::PropertyOut => MemberKind::Property {
                        direction: MemberDirection::Out,
                    },
                    SerializedMemberKind::PropertyInOut => MemberKind::Property {
                        direction: MemberDirection::InOut,
                    },
                    SerializedMemberKind::Function => MemberKind::Function,
                };
                defs.add_member(
                    *id,
                    Member {
                        name: names.intern(&member.name),
                        kind,
                        // This compilation has not read that package's sources.
                        span: Span::default(),
                        ty,
                    },
                );
            }
        }

        Ok(LoadedPackage {
            names,
            package: self.package.clone(),
            defs,
            type_remap,
            def_remap,
        })
    }
}

/// Resolve one nested reference during pass 2.
///
/// Distinguishes the two ways a reference can be bad, because they mean
/// different things: past the end of the table is corruption, and inside the
/// table but not yet resolved is a writer that emitted a parent before its
/// child.
fn resolve_child(
    resolved: &[Ty],
    table_len: usize,
    entry: TypeIndex,
    referenced: TypeIndex,
) -> Result<Ty, LoadError> {
    if referenced as usize >= table_len {
        return Err(LoadError::TypeIndexOutOfRange {
            referenced,
            table_len,
        });
    }
    resolved
        .get(referenced as usize)
        .copied()
        .ok_or(LoadError::ForwardTypeReference { entry, referenced })
}
