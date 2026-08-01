//! Definition identity.
//!
//! Two representations of the same thing, and the split is the point:
//!
//! | | used | shape |
//! |---|---|---|
//! | [`DefId`] | in-process, everywhere | dense index, O(1) |
//! | [`DefPath`] | serialized, across packages | structural, resolvable on load |
//!
//! A `DefId` must never reach a serialized position — its meaning depends on
//! how many definitions were registered before it, which is not stable across
//! compilations. See `plans/rewrite/stage-3-hir-build.md` § Designed for
//! serialization.

use yelc_base::Name;

/// Identifies a package — **the compilation unit**.
///
/// Not derived from a file path: a package is a *directory* of files that merge
/// into one namespace, the way a WIT package and a Go package do
/// ([D8](../../../plans/rewrite/stage-3-hir-build.md),
/// [`plans/modules.md`](../../../plans/modules.md)).
///
/// # Not to be confused with a `module`
///
/// A *module* is a namespace **within** a package, mapping 1:1 onto a WIT
/// `interface`. A package holds several. This type was called `ModuleId` until
/// 2026-07-29, which named the wrong level — the thing that is compiled,
/// versioned and serialized is the package.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct PackageId(pub u32);

impl PackageId {
    /// The package currently being compiled.
    pub const LOCAL: PackageId = PackageId(0);

    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// Identifies one module node in a [`Definitions`](crate::Definitions) tree.
///
/// # Why a module is not addressed by [`PackageId`]
///
/// A module is a namespace *within* a package and a package holds several, so a
/// `PackageId` cannot tell two `include`s of the same package apart — and
/// [`plans/modules.md` §4.1](../../../plans/modules.md) settled that an
/// `include` names a module, one node per `include`. Nor is it a [`DefId`]: a
/// module has no declared type, no export flag and no row in the definition
/// table.
///
/// What resolution actually needs from `Sym::Module` is *a scope to look the
/// next segment up in*. So this indexes the symbol table's own module arena, and
/// the node it reaches carries the [`PackageId`] its definitions belong to —
/// which is what lets a [`DefId`] resolved through the module be read out of
/// that package's own `Definitions` (see
/// [`LoadedPackage`](crate::artifact::LoadedPackage)).
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ModuleId(pub u32);

impl ModuleId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// Identifies one definition, qualified by the package that owns it.
///
/// # Why the package is here from day one (decision B2)
///
/// The alternative — a plain index now, qualified once serialization lands —
/// requires retrofitting every holder of a `DefId`, which is the whole
/// compiler. The field costs 4 bytes; adding it later costs a migration.
///
/// # Why this is not in `yelc-base`
///
/// Package identity is a semantic concept, and `yelc-base` is deliberately
/// mechanism-only. A `DefId` that cannot name its package is precisely the shape
/// B2 exists to avoid, so there is no version of this type that belongs one
/// layer down.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct DefId {
    pub package: PackageId,
    pub index: u32,
}

impl DefId {
    pub fn new(package: PackageId, index: u32) -> Self {
        Self { package, index }
    }

    /// A definition in the package being compiled.
    pub fn local(index: u32) -> Self {
        Self::new(PackageId::LOCAL, index)
    }

    pub fn is_local(self) -> bool {
        self.package == PackageId::LOCAL
    }
}

/// Distinguishes definitions that share a name.
///
/// A name does not identify a definition under overloading: `len` is both
/// `list<T> -> s32` and `string -> s32`. Swift's `XREF_VALUE_PATH_PIECE`
/// carries the type for exactly this reason.
///
/// **One key, two consumers** (decision B3 / S6): [`DefPath`] needs it to name a
/// definition across a package boundary, and monomorphization needs it to mangle
/// an instantiation. Two mechanisms would be two things that must agree, checked
/// by nothing — which is [F12](../../../plans/rewrite/findings.md)'s shape.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub struct OverloadKey {
    /// Parameter types, in declaration order. Empty for anything unoverloadable.
    ///
    /// Held as [`crate::Ty`] handles for in-process comparison. A `DefPath`
    /// carrying this key writes them **structurally**, never as handles — see
    /// [`crate::Ty`]'s note on serialization.
    pub params: Vec<crate::Ty>,
}

impl OverloadKey {
    pub const NONE: Self = Self { params: Vec::new() };

    pub fn is_none(&self) -> bool {
        self.params.is_empty()
    }
}

/// The serialized form of a definition's identity.
///
/// Resolvable on load without knowing anything about the emitting compilation's
/// registration order — which is what makes it correct where a [`DefId`] is not.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct DefPath {
    /// The owning package, by name rather than by index.
    pub package: Name,
    /// Path segments from the package root, outermost first.
    pub segments: Vec<Name>,
    /// Disambiguates overloads. Empty when the name is unique.
    pub overload: OverloadKey,
}
