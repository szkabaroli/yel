//! The state every phase threads.
//!
//! Replaces the frozen `context.rs` (963 lines). The keep-list keeps context
//! *threading*, not the god object — so this holds six fields, and the test for
//! each was: **is it produced and consumed within sema, or is it a later
//! stage's state parked here?**

use yelc_base::{Diagnostics, NameInterner, SourceMap};

use crate::definitions::Definitions;
use crate::ids::PackageId;
use crate::intrinsics::IntrinsicTable;
use crate::known::KnownItems;
use crate::types::TypeInterner;

/// Shared compiler state, threaded through every phase (decision D0).
///
/// # The six, and why there is no seventh
///
/// | field | why it is sema's |
/// |---|---|
/// | [`names`](Self::names) | interning is mechanism every phase needs |
/// | [`types`](Self::types) | one place a type's structure lives |
/// | [`defs`](Self::defs) | what the program declares |
/// | [`builtins`](Self::builtins) | what the language provides |
/// | [`sources`](Self::sources) | spans mean nothing without it |
/// | [`diagnostics`](Self::diagnostics) | accumulate-and-continue, everywhere |
///
/// [`known`](Self::known) is the seventh field and is not a seventh *decision*:
/// it is a projection of `defs`, empty until [`CompilerContext::resolve_known`]
/// runs, and it exists so that lowering can name a definition without a lookup.
///
/// # What cannot be here, enforced rather than remembered
///
/// `block_id_counter`, `block_names`, `component_lifecycle_blocks` and the
/// fanout table sit on the frozen context. They are `yelc-lir` types and
/// `sema → lir` is forbidden by the crate graph, so they **do not compile
/// here** — the boundary is a build error, not a review finding.
///
/// `signal_deps` is the one that would look defensible: it is cited as the
/// positive precedent for side tables, and it stays one. But a side table is a
/// *shape*, not an *address* — reactivity analysis is produced by the frontend
/// and consumed by lowering, so it fails the produced-and-consumed-here test and
/// lives in `yelc-hir` (decision D0a). Keeping it because that is where it
/// happens to live today is how the god object re-forms, one justified field at
/// a time.
pub struct CompilerContext {
    pub names: NameInterner,
    pub types: TypeInterner,
    pub defs: Definitions,
    /// Definition tables of every **included** package, loaded from module
    /// artifacts. Each owns the ids of its own package; `defs` owns the local
    /// ones. Anything that must describe a `DefId` it did not mint goes
    /// through [`CompilerContext::definition`], which dispatches on the id's
    /// package — the per-table accessors assert ownership and panic on a
    /// foreign id, deliberately.
    pub imported: Vec<Definitions>,
    /// The compilation's structure — packages, modules, file assignments
    /// (dora's arenas; `plans/rewrite/definition-arenas.md` step 1).
    pub compilation: crate::compilation::Compilation,
    pub intrinsics: IntrinsicTable,
    pub sources: SourceMap,
    pub diagnostics: Diagnostics,
    /// Resolved lang-items. `None` until [`CompilerContext::resolve_known`].
    known: Option<KnownItems>,
}

impl CompilerContext {
    pub fn new(package: PackageId) -> Self {
        Self {
            names: NameInterner::new(),
            types: TypeInterner::new(),
            defs: Definitions::new(package),
            imported: Vec::new(),
            compilation: crate::compilation::Compilation::new(),
            intrinsics: IntrinsicTable::new(),
            sources: SourceMap::new(),
            diagnostics: Diagnostics::new(),
            known: None,
        }
    }

    /// **The constructor a compilation uses**: builtins registered, lang-items
    /// resolved.
    ///
    /// [`CompilerContext::new`] gives an *empty* context — no builtin resolves
    /// in it and [`CompilerContext::known`] panics. Both exist because the
    /// difference is the thing worth being able to state: the sequence
    /// `new → register_intrinsics → resolve_known` is what
    /// [`crate::known`] documents, and this is the one place it is written down
    /// as code instead of as prose that every caller re-derives.
    ///
    /// # Panics
    ///
    /// If a [`Known`](crate::known::Known) entry has no definition afterwards.
    /// [`register_intrinsics`](crate::stdlib::register_intrinsics) loops the same
    /// inventory `resolve` reads, so this cannot fire from a language change —
    /// only from the registration being removed, which is exactly what it is
    /// here to catch.
    pub fn with_intrinsics(package: PackageId) -> Self {
        let mut ctx = Self::new(package);
        crate::stdlib::register_intrinsics(&mut ctx);
        ctx.resolve_known().unwrap_or_else(|missing| {
            panic!("register_intrinsics left the lang-item table incomplete: {missing}")
        });
        ctx
    }

    /// Resolve the lang-items. Call once, after builtin registration and before
    /// any lowering.
    pub fn resolve_known(&mut self) -> Result<(), crate::known::MissingKnownItems> {
        self.known = Some(KnownItems::resolve(&self.defs, &self.names)?);
        Ok(())
    }

    /// The resolved lang-items.
    ///
    /// # Panics
    ///
    /// If [`CompilerContext::resolve_known`] has not run. That is a **compiler
    /// bug**, not a user error: it means a lowering ran before registration
    /// finished. Panicking beats returning an `Option` that 47 call sites would
    /// each unwrap-or-diagnose for a case none of them can actually observe —
    /// see [`crate::known`] and decision C2.
    /// The table owning a `DefId`, local or imported. `None` for a package
    /// this compilation never loaded — a bug upstream, but a renderer should
    /// degrade rather than panic.
    pub fn tables_of(&self, def: crate::ids::DefId) -> Option<&Definitions> {
        self.package_table(def.package)
    }

    /// A definition by id, wherever it lives.
    pub fn definition(&self, def: crate::ids::DefId) -> Option<&crate::definitions::Definition> {
        self.tables_of(def).map(|table| table.get(def))
    }

    /// A definition's member rows, wherever it lives.
    pub fn members_of(&self, def: crate::ids::DefId) -> &[crate::definitions::Member] {
        self.tables_of(def).map_or(&[], |table| table.members(def))
    }

    /// The table owning a package's declarations — local or loaded.
    pub fn package_table(&self, package: crate::ids::PackageId) -> Option<&Definitions> {
        if package == self.defs.package() {
            return Some(&self.defs);
        }
        self.imported
            .iter()
            .find(|table| table.package() == package)
    }

    /// Look a name up **inside** a bound module — ark's `table_for_module`
    /// walk: module row → its package → that package's own table. Members are
    /// never copied into the consumer; a module is looked *into*.
    pub fn module_member(
        &self,
        module: crate::ids::ModuleId,
        name: yelc_base::Name,
    ) -> Option<crate::ids::DefId> {
        let package = self.defs.module(module).package;
        self.package_table(package)?
            .lookup(name)
            .iter()
            .find_map(|sym| sym.def())
    }

    pub fn known(&self) -> &KnownItems {
        self.known
            .as_ref()
            .expect("lang-items read before resolve_known(); registration is incomplete")
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }
}

impl Default for CompilerContext {
    fn default() -> Self {
        Self::new(PackageId::LOCAL)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::known::Known;
    use yelc_base::{SourceId, Span};

    /// **Through the real registration, not a fixture.**
    ///
    /// The version of this test that stood until 2026-07-30 registered the
    /// lang-items itself, from a helper in this module — so it passed while
    /// `register_intrinsics` touched no definition table at all, and every
    /// non-test caller of [`CompilerContext::known`] would have panicked. A
    /// fixture that reimplements the step under test measures the fixture.
    #[test]
    fn known_items_resolve_after_registration() {
        let ctx = CompilerContext::with_intrinsics(PackageId::LOCAL);
        let _ = ctx.known().get(Known::Color);
    }

    #[test]
    fn resolving_before_registration_reports_what_is_missing() {
        let mut ctx = CompilerContext::default();
        assert!(ctx.resolve_known().is_err());
    }

    /// Reading lang-items early is a compiler bug and must be loud. A silent
    /// `None` here is what produced the frozen tree's 47 unwrap-or-diagnostic
    /// sites (decision C2).
    #[test]
    #[should_panic(expected = "registration is incomplete")]
    fn reading_lang_items_before_resolution_panics() {
        let ctx = CompilerContext::default();
        let _ = ctx.known();
    }

    #[test]
    fn diagnostics_accumulate_rather_than_stopping() {
        let mut ctx = CompilerContext::default();
        assert!(!ctx.has_errors());
        let span = Span::new(SourceId::new(0), 0, 1);
        ctx.diagnostics
            .error(span, yelc_base::ErrorCode::TypeMismatch, "first");
        ctx.diagnostics
            .error(span, yelc_base::ErrorCode::TypeMismatch, "second");
        assert!(ctx.has_errors());
        assert_eq!(ctx.diagnostics.error_count(), 2);
    }

    /// D0's boundary, asserted by the type system: a context that could hold
    /// LIR state would need a dependency this crate does not have. Documented
    /// here because a compile error leaves no trace in the test suite.
    ///
    /// `imported` was admitted 2026-07-31 — the tables of included packages,
    /// the same class of state as `defs` itself. `compilation` was admitted
    /// the same day — dora's package/module arenas
    /// (`plans/rewrite/definition-arenas.md` step 1). The guard fired on
    /// both, which is the guard working: every addition gets named here or
    /// does not land.
    #[test]
    fn the_context_has_eight_fields_plus_a_projection() {
        let ctx = CompilerContext::default();
        let CompilerContext {
            names: _,
            types: _,
            defs: _,
            imported: _,
            compilation: _,
            intrinsics: _,
            sources: _,
            diagnostics: _,
            known: _,
        } = ctx;
        // Destructuring is exhaustive: a ninth decision-bearing field cannot
        // be added without this failing to compile.
    }
}
