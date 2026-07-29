//! Definition identity.
//!
//! Two representations of the same thing, and the split is the point:
//!
//! | | used | shape |
//! |---|---|---|
//! | [`DefId`] | in-process, everywhere | dense index, O(1) |
//! | [`DefPath`] | serialized, across modules | structural, resolvable on load |
//!
//! A `DefId` must never reach a serialized position — its meaning depends on
//! how many definitions were registered before it, which is not stable across
//! compilations. See `plans/rewrite/stage-3-hir-build.md` § Designed for
//! serialization.

use yelc_base::Name;

/// Identifies a module. Not derived from a file path — a module may span
/// several files ([D8](../../../plans/rewrite/stage-3-hir-build.md)).
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ModuleId(pub u32);

impl ModuleId {
    /// The module currently being compiled.
    pub const LOCAL: ModuleId = ModuleId(0);

    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// Identifies one definition, qualified by the module that owns it.
///
/// # Why the module is here from day one (decision B2)
///
/// The alternative — a plain index now, qualified once serialization lands —
/// requires retrofitting every holder of a `DefId`, which is the whole
/// compiler. The field costs 4 bytes; adding it later costs a migration.
///
/// # Why this is not in `yelc-base`
///
/// Module identity is a semantic concept, and `yelc-base` is deliberately
/// mechanism-only. A `DefId` that cannot name its module is precisely the shape
/// B2 exists to avoid, so there is no version of this type that belongs one
/// layer down.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct DefId {
    pub module: ModuleId,
    pub index: u32,
}

impl DefId {
    pub fn new(module: ModuleId, index: u32) -> Self {
        Self { module, index }
    }

    /// A definition in the module being compiled.
    pub fn local(index: u32) -> Self {
        Self::new(ModuleId::LOCAL, index)
    }

    pub fn is_local(self) -> bool {
        self.module == ModuleId::LOCAL
    }
}

/// Distinguishes definitions that share a name.
///
/// A name does not identify a definition under overloading: `len` is both
/// `list<T> -> s32` and `string -> s32`. Swift's `XREF_VALUE_PATH_PIECE`
/// carries the type for exactly this reason.
///
/// **One key, two consumers** (decision B3 / S6): [`DefPath`] needs it to name a
/// definition across a module boundary, and monomorphization needs it to mangle
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
    /// The owning module, by name rather than by index.
    pub module: Name,
    /// Path segments from module root, outermost first.
    pub segments: Vec<Name>,
    /// Disambiguates overloads. Empty when the name is unique.
    pub overload: OverloadKey,
}
