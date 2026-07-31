//! The package artifact: a compiled package, written so a *different*
//! compilation can read it.
//!
//! Specified in [`plans/modules.md` §6.6](../../../../plans/modules.md) and
//! [`directions.md` §6](../../../../plans/rewrite/directions.md). This module is
//! decision [B1](../../../../plans/rewrite/open-decisions.md) made empirical:
//! until something round-trips, "`Ty` is written structurally" is an argument,
//! and a `Ty` written as its interner index is invisible.
//!
//! ```text
//! Artifact {
//!     stamp:   Stamp { compiler, format },
//!     package: PackageName { namespace, name, version },
//!     types:   Vec<StructuralTy>,   // artifact-local table
//!     defs:    Vec<SerializedDef>,  // ty: an index into `types`, never a Ty
//! }
//! ```
//!
//! # The one rule
//!
//! **Nothing whose meaning depends on this compilation's ordering may reach the
//! bytes.** Three such things exist, and the rule is narrower than "no ids":
//!
//! | value | on the wire | why |
//! |---|---|---|
//! | [`Ty`](crate::Ty) | an index into `types` | interning order differs |
//! | [`DefId`](crate::DefId) | a [`SerializedDefPath`] | registration order differs |
//! | [`Name`](yelc_base::Name) | a `String` | interning order differs |
//! | an index *within* the artifact | itself | it only has to agree with itself |
//!
//! `Ty` is enforced by the type system — it does not derive `Serialize`, so
//! writing a handle does not compile. `Name` and `SourceId` are **not**: both
//! derive `Serialize` in `yelc-base`, so for those the rule rests on
//! [`wire`] being the only place artifact types are declared. That asymmetry is
//! recorded in `plans/rewrite/seam-changes.md`, not hidden here.
//!
//! # For stage 3
//!
//! The artifact grows HIR nodes and the total `types` map. The surface to
//! implement is [`ToArtifact`] / [`FromArtifact`], one pair per node type: the
//! only source of a wire value for a `Ty` or a `DefId` is the
//! [`ArtifactWriter`] / [`LoadedPackage`] handed to those methods, so
//! implementing them is simultaneously the mechanism and the constraint. A
//! `HirId` needs neither — the whole HIR travels together, so its ids only have
//! to agree with themselves.
//!
//! # What this cannot represent yet, stated rather than worked around
//!
//! - **Overload sets.** [`Definitions`] can now hold one — it keys by [`Name`]
//!   alone and stores an [`OverloadKey`](crate::OverloadKey) per definition —
//!   but the **loader** cannot rebuild one. Registration happens in pass 1 and
//!   the type table only resolves in pass 2, so the `Ty`s a key is made of do
//!   not exist yet at the moment the key is needed; a key that does not depend
//!   on the type table is a separate decision. [`SerializedDefPath::overload`]
//!   is therefore still always empty, and a colliding artifact is rejected with
//!   [`LoadError::DuplicateDefinition`] rather than silently keeping one.
//! - **Cross-package references.** A `DefId` from another package has no entry
//!   in the producer's `Definitions` to read a name out of. Writing one panics;
//!   it does not emit a guess.

mod encoding;
mod load;
pub mod wire;
mod write;

use std::fmt;

use serde::{Deserialize, Serialize};
use yelc_base::Interner;

use crate::context::CompilerContext;
use crate::definitions::Definitions;
use crate::types::TypeInterner;

pub use encoding::{decode, encode};
pub use load::{FromArtifact, LoadedPackage};
pub use wire::{SerializedDef, SerializedDefPath, StructuralTy, TypeIndex};
pub use write::{ArtifactWriter, ToArtifact};

/// The compiler build that produced an artifact, and the schema it wrote.
///
/// Two fields because they fail for different reasons and a reader should be
/// able to tell which happened: `format` moved because the schema changed,
/// `compiler` moved because the meaning of an unchanged schema may have.
///
/// # Mismatch on either field rejects the artifact outright
///
/// A compiler change can alter what a node *means* without altering its
/// encoding. A stale artifact then deserializes successfully and miscompiles
/// with no diagnostic — the silent-failure shape
/// [A8](../../../../plans/rewrite/anti-spec.md) is about, at the worst possible
/// place to have it. Swift's answer is the same one: `.swiftmodule` is
/// version-locked and refuses on mismatch. Rejecting is cheap, because the
/// artifact is a cache and the source is always available.
#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Stamp {
    /// The producing compiler's own version.
    pub compiler: String,
    /// The artifact schema version. Bumped by hand when [`wire`] changes.
    pub format: u32,
}

impl Stamp {
    /// The current schema version.
    ///
    /// **Bump this whenever anything in [`wire`] changes shape** — a field
    /// added, removed or reordered, an enum variant inserted anywhere but the
    /// end. postcard writes enum variants by index and struct fields by
    /// position, so any of those silently reinterprets old bytes.
    /// History: `1` → `2` on 2026-07-29, when `SerializedDefPath.namespace:
    /// Namespace` became `kind: DefKind` for the single-namespace symbol table.
    /// The two enums have the same four variants in the same order, so postcard
    /// writes **identical bytes** and a stale artifact would have loaded
    /// silently — which is exactly the case this constant exists for.
    /// 3: `SerializedDef` gained `members` (2026-07-31) — postcard writes
    /// fields by position, so the change is invisible in the bytes and only
    /// this number says it happened.
    pub const FORMAT: u32 = 3;

    /// The producing compiler's version.
    ///
    /// # This is weaker than it looks, and deliberately visible
    ///
    /// It is the crate version, which is pinned at `0.1.0` for the whole
    /// workspace and therefore does not move when the compiler does. Every
    /// artifact written during the rewrite carries the same string, so the
    /// `compiler` half of the stamp currently discriminates nothing. It becomes
    /// real when this is a build identity (a git SHA — `shadow-rs` is already a
    /// workspace dependency), which is a change to *this constant* and to
    /// nothing else. Recorded rather than left to be discovered by an artifact
    /// that loads when it should not.
    pub const COMPILER: &'static str = env!("CARGO_PKG_VERSION");

    /// The stamp this compiler writes.
    pub fn current() -> Self {
        Self {
            compiler: Self::COMPILER.to_string(),
            format: Self::FORMAT,
        }
    }

    /// Reject unless both fields match this build.
    pub fn check(&self) -> Result<(), LoadError> {
        if self.compiler != Self::COMPILER {
            return Err(LoadError::CompilerMismatch {
                expected: Self::COMPILER.to_string(),
                found: self.compiler.clone(),
            });
        }
        if self.format != Self::FORMAT {
            return Err(LoadError::FormatMismatch {
                expected: Self::FORMAT,
                found: self.format,
            });
        }
        Ok(())
    }
}

/// A package's identity: WIT's `namespace:name@version`.
///
/// Held as strings rather than [`Name`](yelc_base::Name)s because this is the
/// one thing in an artifact that must be readable without any of the producer's
/// state — including its interner.
#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub struct PackageName {
    /// WIT's first path segment: `yel` in `yel:ui@0.1.0`.
    pub namespace: String,
    pub name: String,
    pub version: String,
}

impl PackageName {
    pub fn new(
        namespace: impl Into<String>,
        name: impl Into<String>,
        version: impl Into<String>,
    ) -> Self {
        Self {
            namespace: namespace.into(),
            name: name.into(),
            version: version.into(),
        }
    }
}

impl fmt::Display for PackageName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}@{}", self.namespace, self.name, self.version)
    }
}

/// A compiled package.
///
/// Built with [`Artifact::build`], encoded with [`encode`], and read back with
/// [`Artifact::load`].
#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Artifact {
    pub stamp: Stamp,
    pub package: PackageName,
    /// Every type the package's definitions mention, structurally.
    ///
    /// Children precede their parent, so the table resolves in one forward pass.
    pub types: Vec<StructuralTy>,
    /// Every definition, in registration order.
    pub defs: Vec<SerializedDef>,
}

impl Artifact {
    /// Build an artifact from a finished compilation's tables.
    pub fn build(
        package: PackageName,
        names: &Interner,
        types: &TypeInterner,
        defs: &Definitions,
    ) -> Self {
        let mut writer = ArtifactWriter::new(&package, names, types, defs);
        // Registration order, so the artifact is byte-deterministic: nothing
        // here is derived from a hash map's iteration
        // ([A6](../../../../plans/rewrite/anti-spec.md)).
        let serialized: Vec<SerializedDef> = defs
            .iter()
            .map(|definition| definition.to_artifact(&mut writer))
            .collect();
        let types = writer.into_table();

        Self {
            stamp: Stamp::current(),
            package,
            types,
            defs: serialized,
        }
    }

    /// Build from a [`CompilerContext`], the usual caller.
    pub fn from_context(package: PackageName, ctx: &CompilerContext) -> Self {
        Self::build(package, &ctx.names, &ctx.types, &ctx.defs)
    }

    /// Type-table positions holding an unsolved inference variable.
    ///
    /// [A4 obligation 2](../../../../plans/rewrite/open-decisions.md): an
    /// artifact containing a hole is a bug, not a state. The check lives on the
    /// producer, not in the encoding — [`StructuralTy::Infer`] exists precisely
    /// so that a hole is *reported* rather than quietly written as a
    /// [`StructuralTy::Param`], which is the confusion A3 and A4 were split to
    /// prevent.
    ///
    /// Empty is the only publishable answer.
    pub fn inference_holes(&self) -> Vec<TypeIndex> {
        self.types
            .iter()
            .enumerate()
            .filter(|(_, entry)| matches!(entry, StructuralTy::Infer(_)))
            .map(|(position, _)| TypeIndex::try_from(position).expect("type table fits in u32"))
            .collect()
    }
}

/// Why an artifact could not be read.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LoadError {
    /// A different compiler build wrote it.
    CompilerMismatch { expected: String, found: String },
    /// A different schema version wrote it.
    FormatMismatch { expected: u32, found: u32 },
    /// The bytes are not a valid encoding.
    Decode(String),
    /// A type entry referenced an index past the end of the table.
    TypeIndexOutOfRange {
        referenced: TypeIndex,
        table_len: usize,
    },
    /// A type entry referenced one that had not been resolved yet — the writer
    /// emitted a parent before its child.
    ForwardTypeReference {
        entry: TypeIndex,
        referenced: TypeIndex,
    },
    /// A path named a definition the artifact does not contain.
    UnresolvedDefPath(Box<SerializedDefPath>),
    /// Two definitions claimed the same name. Legal only as an overload set,
    /// which the *loader* cannot yet rebuild — see
    /// [`SerializedDefPath::overload`].
    DuplicateDefinition(Box<SerializedDefPath>),
    /// A path with nothing in it names no definition.
    PathWithoutSegments(Box<SerializedDefPath>),
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CompilerMismatch { expected, found } => write!(
                f,
                "artifact was written by compiler {found}, this is {expected}; rebuild from source",
            ),
            Self::FormatMismatch { expected, found } => write!(
                f,
                "artifact uses format version {found}, this compiler reads {expected}; \
                 rebuild from source",
            ),
            Self::Decode(message) => write!(f, "artifact is not decodable: {message}"),
            Self::TypeIndexOutOfRange {
                referenced,
                table_len,
            } => write!(
                f,
                "artifact type index {referenced} is past the end of a {table_len}-entry table",
            ),
            Self::ForwardTypeReference { entry, referenced } => write!(
                f,
                "artifact type entry {entry} references {referenced}, which is not resolved yet; \
                 the type table is not in child-before-parent order",
            ),
            Self::UnresolvedDefPath(path) => write!(
                f,
                "artifact references {}, which it does not define",
                DisplayPath(path),
            ),
            Self::DuplicateDefinition(path) => {
                write!(f, "artifact defines {} twice", DisplayPath(path))
            }
            Self::PathWithoutSegments(path) => {
                write!(
                    f,
                    "artifact contains a path with no segments in {}",
                    path.package
                )
            }
        }
    }
}

impl std::error::Error for LoadError {}

struct DisplayPath<'a>(&'a SerializedDefPath);

impl fmt::Display for DisplayPath<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.0.package, self.0.segments.join("."))
    }
}
