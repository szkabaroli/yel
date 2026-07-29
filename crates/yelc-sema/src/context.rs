//! The state every phase threads.
//!
//! Replaces the frozen `context.rs` (963 lines). The keep-list keeps context
//! *threading*, not the god object — so this holds six fields, and the test for
//! each was: **is it produced and consumed within sema, or is it a later
//! stage's state parked here?**

use yelc_base::{Diagnostics, Interner, SourceMap};

use crate::builtins::BuiltinTable;
use crate::definitions::Definitions;
use crate::ids::ModuleId;
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
    pub names: Interner,
    pub types: TypeInterner,
    pub defs: Definitions,
    pub builtins: BuiltinTable,
    pub sources: SourceMap,
    pub diagnostics: Diagnostics,
    /// Resolved lang-items. `None` until [`CompilerContext::resolve_known`].
    known: Option<KnownItems>,
}

impl CompilerContext {
    pub fn new(module: ModuleId) -> Self {
        Self {
            names: Interner::new(),
            types: TypeInterner::new(),
            defs: Definitions::new(module),
            builtins: BuiltinTable::new(),
            sources: SourceMap::new(),
            diagnostics: Diagnostics::new(),
            known: None,
        }
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
        Self::new(ModuleId::LOCAL)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::known::Known;
    use yelc_base::{SourceId, Span};

    fn register_known(ctx: &mut CompilerContext) {
        for &item in Known::ALL {
            let name = ctx.names.intern(item.source_name());
            ctx.defs
                .register(
                    name,
                    item.namespace(),
                    Span::new(SourceId::new(0), 0, 1),
                    false,
                )
                .unwrap();
        }
    }

    #[test]
    fn known_items_resolve_after_registration() {
        let mut ctx = CompilerContext::default();
        register_known(&mut ctx);
        ctx.resolve_known().unwrap();
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
    #[test]
    fn the_context_has_six_fields_plus_a_projection() {
        let ctx = CompilerContext::default();
        let CompilerContext {
            names: _,
            types: _,
            defs: _,
            builtins: _,
            sources: _,
            diagnostics: _,
            known: _,
        } = ctx;
        // Destructuring is exhaustive: a seventh decision-bearing field cannot
        // be added without this failing to compile.
    }
}
