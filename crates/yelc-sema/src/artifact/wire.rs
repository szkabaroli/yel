//! The types that actually reach the bytes.
//!
//! Everything here is **compilation-independent by construction**: no `Ty`, no
//! `DefId`, no [`Name`](yelc_base::Name), no
//! [`SourceId`](yelc_base::SourceId). Each of those is an index whose meaning is
//! *this* compilation's interning or registration order, and writing one is the
//! bug the whole artifact format exists to make impossible.
//!
//! The rule is enforced two ways. `Ty` cannot appear because it does not derive
//! `Serialize` (decision B1) — a type error. `Name` and `SourceId` *can* appear,
//! because `yelc-base` does derive `Serialize` on both, so for those the rule is
//! carried by review and by this module being the only place wire types are
//! declared. See the module docs on [`super`] for what that costs.

use serde::{Deserialize, Serialize};

use crate::definitions::Namespace;

use super::PackageName;

/// An index into an [`Artifact`](super::Artifact)'s `types` table.
///
/// **Artifact-local.** It means nothing outside the artifact it was read from,
/// which is precisely why it is safe to write: both ends of the wire are the
/// same table.
pub type TypeIndex = u32;

/// A type, written out structurally.
///
/// Mirrors [`TyKind`](crate::TyKind) one variant at a time, with two
/// substitutions that are the entire point of the format:
///
/// | `TyKind` | `StructuralTy` | why |
/// |---|---|---|
/// | `Ty` (a child) | [`TypeIndex`] | the producer's interning order is not the consumer's |
/// | `DefId` | [`SerializedDefPath`] | the producer's registration order is not the consumer's |
///
/// Every other variant carries only plain data and crosses unchanged.
#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub enum StructuralTy {
    Bool,
    S8,
    S16,
    S32,
    S64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Char,
    String,

    List(TypeIndex),
    Option(TypeIndex),
    Result {
        ok: Option<TypeIndex>,
        err: Option<TypeIndex>,
    },
    Tuple(Vec<TypeIndex>),

    /// A user-defined record, enum, variant or component, by path.
    Adt(SerializedDefPath),

    Func {
        params: Vec<TypeIndex>,
        ret: Option<TypeIndex>,
    },

    /// The `T` in a declaration (decision A3).
    Param(u32),

    /// An unsolved inference variable (decision A4).
    ///
    /// # Representable, and still illegal to publish
    ///
    /// [A4 obligation 2](../../../../plans/rewrite/open-decisions.md) says an
    /// artifact containing a hole is a bug, not a state. That is a **policy on
    /// the producer**, checked by
    /// [`Artifact::inference_holes`](super::Artifact::inference_holes) — not a
    /// gap in the encoding. The variant exists so that an `Infer` can never be
    /// silently written *as* a [`StructuralTy::Param`], which is the failure
    /// A3/A4 were split to prevent. An encoding that could not tell them apart
    /// would turn a loud bug into a quiet miscompile.
    Infer(u32),

    /// Recovery. Reaches an artifact only if one was published from a failed
    /// compilation, which the driver's `has_errors()` gate prevents.
    Error,
    Unit,
}

/// A definition's identity, written so a *different* compilation can resolve it.
///
/// # Why this is not [`DefPath`](crate::DefPath)
///
/// `DefPath` is documented as "the serialized form of a definition's identity"
/// and it is not serializable: `package` and `segments` are
/// [`Name`](yelc_base::Name)s — interner indices, the same class of value as a
/// `Ty` handle — and its `overload` is an [`OverloadKey`](crate::OverloadKey)
/// holding `Ty` handles, which `OverloadKey`'s own doc comment says must be
/// written structurally. So `DefPath` is the *resolution-independent in-process*
/// form, one step short of the wire. This is that last step.
///
/// # The `namespace` field is not in the recorded design
///
/// [`Definitions`](crate::Definitions) keys names by `(Name, Namespace)` and its
/// own test asserts that a record and a component may share a name. A path
/// without the namespace therefore cannot name one of them, and `DefPath` has no
/// namespace field. Added here; recorded in `plans/rewrite/seam-changes.md`.
#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub struct SerializedDefPath {
    /// The owning package, by name. Not [`PackageId`](crate::PackageId), which
    /// is an index into the consumer's own dependency list.
    ///
    /// Note the vocabulary clash with [`namespace`](Self::namespace): a package
    /// *namespace* is WIT's first path segment (`yel` in `yel:ui@0.1.0`), a
    /// definition [`Namespace`] is an index space. Both names are the right ones
    /// for their own domain.
    pub package: PackageName,
    /// Which index space the definition lives in.
    pub namespace: Namespace,
    /// Path segments from the package root, outermost first.
    ///
    /// One segment today, because `Definitions` is flat. `module M { }`
    /// (`plans/modules.md` §3) makes it two without a format change.
    pub segments: Vec<String>,
    /// The overload discriminator (decision B3): parameter types, in
    /// declaration order, as indices into the artifact's type table.
    ///
    /// Always empty today — see [`super`] on what `Definitions` cannot yet
    /// represent.
    pub overload: Vec<TypeIndex>,
}

impl SerializedDefPath {
    /// The name the definition is registered under: the last segment.
    ///
    /// `None` for a path with no segments, which is a malformed artifact rather
    /// than a definition without a name.
    pub fn leaf(&self) -> Option<&str> {
        self.segments.last().map(String::as_str)
    }
}

/// One [`Definition`](crate::Definition), written out.
///
/// # What is deliberately absent
///
/// - **`id`.** A `DefId` is the producer's registration index. Reconstructing it
///   on load — by registering the definitions in order — *is* the load.
/// - **`span`.** A [`Span`](yelc_base::Span) is a `SourceId` plus byte offsets,
///   and the `SourceId` indexes the producer's `SourceMap`. A consumer that has
///   not read the producer's sources cannot render it. Loaded definitions get
///   `Span::default()`, whose `SourceId::INVALID` says "synthetic" rather than
///   aliasing the consumer's first file.
#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct SerializedDef {
    pub path: SerializedDefPath,
    /// The declared type, as an index into the artifact's type table.
    pub ty: Option<TypeIndex>,
    pub is_export: bool,
}
