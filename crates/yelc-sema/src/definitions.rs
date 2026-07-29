//! The symbol table: every name the program declares, and what it names.
//!
//! Replaces the frozen `definitions.rs` (742 lines). Registration happens before
//! any body is lowered, which is what makes forward references work in both
//! directions — see stage 3's register-then-lower invariant.
//!
//! # One namespace, not four
//!
//! The frozen compiler keys names by `(Name, Namespace)`, so a record and a
//! component may share a name and neither shadows the other. This table keys by
//! [`Name`] alone: **a name binds to one thing**. That is a deliberate,
//! user-approved narrowing of the surface language — it rejects programs the
//! frozen compiler accepts — and it is recorded as such in
//! `plans/rewrite/scope.md` (2026-07-29), with the boundary enumerated against
//! the frozen compiler in `tests/single_namespace.rs`.
//!
//! What survives is the *tag*, not the *key*: a [`Definition`] still knows
//! whether it is a type, a value, a component or a global ([`DefKind`]), because
//! a diagnostic has to say so and a loaded artifact has to be rebuilt as the
//! right thing. What dies is looking a name up *in* a kind — [`lookup`] takes no
//! kind, and two declarations of one name are a [`Collision`] whichever kinds
//! they are.
//!
//! [`lookup`]: Definitions::lookup
//!
//! # The table is a two-level tree
//!
//! A [`Scope`] holds `Name → [Sym]`. There is one root scope — this package's
//! own declarations — plus one child scope per `include`, held as a [`Module`]
//! and named from the root by [`Sym::Module`].
//!
//! **Two levels, and no deeper.** WIT interfaces do not nest, and
//! [`plans/modules.md` §3](../../../plans/modules.md) refuses source-level module
//! nesting for that reason. The depth limit is carried by the signatures rather
//! than by a check: [`Definitions::bind_in_module`] takes a [`DefKind`], and
//! `DefKind` has no `Module` variant, so a module inside a module does not
//! compile.
//!
//! Nothing populates a module scope yet — `include` does not parse. The shape is
//! here so that the thing which will populate it has somewhere correct to go.

use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;
use yelc_base::{Diagnostic, ErrorCode, Interner, Name, SourceMap, Span};

use crate::ids::{DefId, ModuleId, OverloadKey, PackageId};
use crate::types::Ty;

/// What a definition *is*.
///
/// # Why this is not [`Sym`], and why it has no `Module` variant
///
/// `Sym` answers "what does this name bind to" and carries the thing it binds
/// to. `DefKind` answers "what kind of definition is this" and carries nothing —
/// which is what makes it the right thing to put on the wire, and the right
/// parameter for every registration entry point. A module is not a definition:
/// it has no declared type, no export flag and no row in [`Definitions`], so a
/// fifth variant here would be four dead columns on one row. Its absence is also
/// what makes module nesting a compile error rather than a runtime check — see
/// the module docs.
///
/// # Why this one derives `Serialize` when [`Ty`] does not
///
/// It carries no index. A `DefKind` means the same thing in every compilation,
/// so writing it is writing a fact rather than a handle — the distinction
/// decision B1 turns on. See [`crate::artifact::wire`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub enum DefKind {
    /// Records, enums, variants.
    Type,
    /// Functions, constants, properties.
    Value,
    /// Components, extern components, elements.
    Component,
    /// Global singletons.
    Global,
}

impl DefKind {
    /// Every kind, for exhaustive tests.
    pub const ALL: &'static [DefKind] = &[
        DefKind::Type,
        DefKind::Value,
        DefKind::Component,
        DefKind::Global,
    ];

    /// The symbol a definition of this kind binds its name to.
    ///
    /// The single bridge between the two enums, so they cannot drift.
    pub const fn sym(self, id: DefId) -> Sym {
        match self {
            DefKind::Type => Sym::Type(id),
            DefKind::Value => Sym::Value(id),
            DefKind::Component => Sym::Component(id),
            DefKind::Global => Sym::Global(id),
        }
    }

    /// How a diagnostic names this kind, with its article.
    pub const fn describe(self) -> &'static str {
        match self {
            DefKind::Type => "a type",
            DefKind::Value => "a value",
            DefKind::Component => "a component",
            DefKind::Global => "a global",
        }
    }
}

/// What a name binds to in a [`Scope`].
///
/// One namespace: a scope maps a [`Name`] to symbols, and the variant — not a
/// separate key — says what kind of thing was found.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Sym {
    Type(DefId),
    Value(DefId),
    Component(DefId),
    Global(DefId),
    /// A module brought in by one `include`.
    ///
    /// One node per `include`, not per package: `plans/modules.md` §4.1 settled
    /// that an `include` names a *module*, so a package contributing three
    /// modules and included once puts exactly one node here — which keeps the
    /// tree matching the emitted world's import list one-to-one.
    Module(ModuleId),
}

impl Sym {
    /// The definition this symbol names, or `None` for a module.
    pub const fn def(self) -> Option<DefId> {
        match self {
            Sym::Type(id) | Sym::Value(id) | Sym::Component(id) | Sym::Global(id) => Some(id),
            Sym::Module(_) => None,
        }
    }

    /// The kind of definition this symbol names, or `None` for a module.
    pub const fn kind(self) -> Option<DefKind> {
        match self {
            Sym::Type(_) => Some(DefKind::Type),
            Sym::Value(_) => Some(DefKind::Value),
            Sym::Component(_) => Some(DefKind::Component),
            Sym::Global(_) => Some(DefKind::Global),
            Sym::Module(_) => None,
        }
    }

    /// How a diagnostic names this symbol, with its article.
    pub const fn describe(self) -> &'static str {
        match self.kind() {
            Some(kind) => kind.describe(),
            None => "a module",
        }
    }
}

/// One name-resolution scope: the root, or one module.
///
/// Multi-valued because a name may carry an **overload set** — several values
/// distinguished by their parameter types (decision B3). Every other kind of
/// reuse is a [`Collision`].
#[derive(Default, Debug)]
struct Scope {
    by_name: FxHashMap<Name, SmallVec<[Sym; 1]>>,
}

impl Scope {
    fn get(&self, name: Name) -> &[Sym] {
        self.by_name.get(&name).map_or(&[], SmallVec::as_slice)
    }

    fn insert(&mut self, name: Name, sym: Sym) {
        self.by_name.entry(name).or_default().push(sym);
    }
}

/// One module node: the names one `include` brings in.
///
/// Its definitions belong to `package`, not to this one — a loaded package owns
/// its own [`Definitions`] (see [`crate::artifact::LoadedPackage`]), and the
/// [`DefId`]s bound here are qualified with that package's id so they read out
/// of the right table.
#[derive(Debug)]
pub struct Module {
    /// The name the module is bound to in the root scope.
    pub name: Name,
    /// The package the module's definitions live in.
    pub package: PackageId,
    /// Where the `include` was written.
    pub span: Span,
    scope: Scope,
}

/// One registered definition.
///
/// Deliberately thin: what a definition *is*, not what analysis later concluded
/// about it ([B3](../../../plans/rewrite/anti-spec.md)). `ty` is the **declared**
/// type, filled at registration from the syntax — not an inference result.
#[derive(Clone, Debug)]
pub struct Definition {
    pub id: DefId,
    pub name: Name,
    pub kind: DefKind,
    /// Where the name was written. Every diagnostic about this definition
    /// points here.
    pub span: Span,
    /// The declared type, where the syntax gives one. `None` until stage 3
    /// resolves it, never a placeholder.
    pub ty: Option<Ty>,
    /// Whether the definition is published in the package interface.
    pub is_export: bool,
    /// What distinguishes this definition from others of the same name
    /// (decision B3). [`OverloadKey::NONE`] for anything unoverloadable, which
    /// is everything a program can currently declare.
    pub overload: OverloadKey,
}

/// Every definition in the package being compiled, and the scopes that name
/// them.
pub struct Definitions {
    package: PackageId,
    defs: Vec<Definition>,
    root: Scope,
    modules: Vec<Module>,
}

/// Returned when a name is already taken. The caller pushes the diagnostic —
/// this table reports the collision and keeps the original, rather than deciding
/// how to complain about it.
///
/// It carries everything a good message needs, because there is exactly **one**
/// place a collision is detected and it should not take a second lookup to
/// describe it.
#[derive(Clone, Copy, Debug)]
pub struct Collision {
    /// The name both declarations claim.
    pub name: Name,
    /// The symbol already holding it.
    pub existing: Sym,
    /// Where that one was declared. `Span::default()` when it belongs to
    /// another package, whose sources this compilation has not read — the same
    /// answer [`crate::artifact::wire`] gives for a loaded definition.
    pub existing_span: Span,
    /// What the rejected declaration would have been. `None` for a module: a
    /// module is not a definition, which is why this is an `Option` rather than
    /// a fifth [`DefKind`].
    pub attempted: Option<DefKind>,
    /// Where the rejected declaration is.
    pub span: Span,
}

impl Collision {
    /// How a diagnostic names the rejected declaration, with its article.
    pub fn attempted_description(&self) -> &'static str {
        match self.attempted {
            Some(kind) => kind.describe(),
            None => "a module",
        }
    }

    /// Whether the two declarations are of different kinds.
    ///
    /// The frozen compiler accepted exactly this case; it is the one worth an
    /// extra note, because the user has no reason to expect the rejection.
    pub fn is_cross_kind(&self) -> bool {
        self.attempted != self.existing.kind()
    }

    /// Render the collision, naming both declarations.
    ///
    /// One check, one place, one message: the primary span is the declaration
    /// being rejected and a note carries the file and line of the one that won,
    /// which is the shape [`Diagnostic`] supports (it has one span and a list of
    /// notes).
    pub fn diagnostic(&self, names: &Interner, sources: &SourceMap) -> Diagnostic {
        let name = names.str(self.name);
        let previously = match sources.get(self.existing_span.source) {
            Some(source) => format!(
                "previously defined as {} at {}:{}",
                self.existing.describe(),
                source.name(),
                source.line_col(self.existing_span.start).0,
            ),
            None => format!("previously defined as {}", self.existing.describe()),
        };

        let mut diagnostic = Diagnostic::error(format!("duplicate definition of `{name}`"))
            .with_span(self.span)
            .with_code(ErrorCode::DuplicateDefinition)
            .with_note(previously);

        if self.is_cross_kind() {
            diagnostic = diagnostic.with_note(format!(
                "a name may name only one definition; `{name}` cannot be both {} and {}",
                self.existing.describe(),
                self.attempted_description(),
            ));
        }

        diagnostic
    }
}

impl Definitions {
    pub fn new(package: PackageId) -> Self {
        Self {
            package,
            defs: Vec::new(),
            root: Scope::default(),
            modules: Vec::new(),
        }
    }

    pub fn package(&self) -> PackageId {
        self.package
    }

    /// Register a name. Returns `Err(Collision)` if it is taken, leaving the
    /// original in place.
    ///
    /// Registration **continues** after a collision — the caller pushes a
    /// diagnostic and carries on, so one duplicate name does not hide every
    /// later error in the file (`yelc-base`'s accumulate-and-continue policy).
    pub fn register(
        &mut self,
        name: Name,
        kind: DefKind,
        span: Span,
        is_export: bool,
    ) -> Result<DefId, Collision> {
        self.register_definition(name, kind, span, is_export, OverloadKey::NONE)
    }

    /// Register an **overload**: another definition sharing an existing name,
    /// told apart by its parameter types (decision B3).
    ///
    /// Takes no [`DefKind`] because only values overload, and rejects an empty
    /// key because an empty key means "unoverloadable" — two of those are a
    /// [`Collision`], not an overload set.
    pub fn register_overload(
        &mut self,
        name: Name,
        span: Span,
        is_export: bool,
        overload: OverloadKey,
    ) -> Result<DefId, Collision> {
        self.register_definition(name, DefKind::Value, span, is_export, overload)
    }

    fn register_definition(
        &mut self,
        name: Name,
        kind: DefKind,
        span: Span,
        is_export: bool,
        overload: OverloadKey,
    ) -> Result<DefId, Collision> {
        self.check(name, Some(kind), &overload, span)?;

        let id = DefId::new(self.package, self.defs.len() as u32);
        self.defs.push(Definition {
            id,
            name,
            kind,
            span,
            ty: None,
            is_export,
            overload,
        });
        self.root.insert(name, kind.sym(id));
        Ok(id)
    }

    /// Register a module node: one `include`, one child scope.
    pub fn register_module(
        &mut self,
        name: Name,
        package: PackageId,
        span: Span,
    ) -> Result<ModuleId, Collision> {
        self.check(name, None, &OverloadKey::NONE, span)?;

        let id = ModuleId::new(self.modules.len() as u32);
        self.modules.push(Module {
            name,
            package,
            span,
            scope: Scope::default(),
        });
        self.root.insert(name, Sym::Module(id));
        Ok(id)
    }

    /// The one place a name collision is decided.
    ///
    /// `attempted` is `None` for a module. A name may carry more than one symbol
    /// only when every one of them is a value with a distinct, non-empty
    /// [`OverloadKey`]; everything else — including the record/component reuse
    /// the frozen compiler allowed — is a collision.
    fn check(
        &self,
        name: Name,
        attempted: Option<DefKind>,
        overload: &OverloadKey,
        span: Span,
    ) -> Result<(), Collision> {
        for &existing in self.root.get(name) {
            let compatible = attempted == Some(DefKind::Value)
                && existing.kind() == Some(DefKind::Value)
                && !overload.is_none()
                && self.definition_of(existing).is_some_and(|definition| {
                    !definition.overload.is_none() && definition.overload != *overload
                });
            if !compatible {
                return Err(Collision {
                    name,
                    existing,
                    existing_span: self.span_of(existing),
                    attempted,
                    span,
                });
            }
        }
        Ok(())
    }

    /// Bind a name inside a module scope.
    ///
    /// `def` belongs to the module's package, not to this one. Takes a
    /// [`DefKind`] rather than a [`Sym`] on purpose: `DefKind` has no `Module`
    /// variant, so a module inside a module is a compile error and the tree
    /// cannot grow a third level.
    pub fn bind_in_module(
        &mut self,
        module: ModuleId,
        name: Name,
        kind: DefKind,
        def: DefId,
    ) -> Result<(), Collision> {
        let span = self.modules[module.index()].span;
        if let Some(&existing) = self.modules[module.index()].scope.get(name).first() {
            return Err(Collision {
                name,
                existing,
                existing_span: self.span_of(existing),
                attempted: Some(kind),
                span,
            });
        }
        self.modules[module.index()]
            .scope
            .insert(name, kind.sym(def));
        Ok(())
    }

    /// Every symbol bound to `name` in the root scope, in registration order.
    ///
    /// More than one only for an overload set. Takes no kind — that is the whole
    /// content of the single-namespace change.
    pub fn lookup(&self, name: Name) -> &[Sym] {
        self.root.get(name)
    }

    /// The definition `name` binds to, if it is of `kind`.
    ///
    /// Not a namespaced lookup: the name resolves to at most one non-overloaded
    /// symbol, and this reports `None` when that symbol is something else. A
    /// `Color` declared as a global means there is no `Color` record, which is
    /// exactly what [`crate::known`] needs to hear.
    pub fn lookup_def(&self, name: Name, kind: DefKind) -> Option<DefId> {
        self.root
            .get(name)
            .iter()
            .find(|sym| sym.kind() == Some(kind))
            .and_then(|sym| sym.def())
    }

    /// Every symbol bound to `name` inside a module scope.
    pub fn lookup_in_module(&self, module: ModuleId, name: Name) -> &[Sym] {
        self.modules[module.index()].scope.get(name)
    }

    pub fn module(&self, id: ModuleId) -> &Module {
        &self.modules[id.index()]
    }

    /// Every module node, in registration order.
    pub fn modules(&self) -> impl Iterator<Item = (ModuleId, &Module)> {
        self.modules
            .iter()
            .enumerate()
            .map(|(index, module)| (ModuleId::new(index as u32), module))
    }

    /// Where a symbol was declared.
    ///
    /// A definition owned by another package reports `Span::default()`: this
    /// compilation has not read that package's sources, so there is no span it
    /// could render — the same answer `wire::SerializedDef` gives.
    pub fn span_of(&self, sym: Sym) -> Span {
        match sym {
            Sym::Module(id) => self.modules[id.index()].span,
            _ => self
                .definition_of(sym)
                .map_or_else(Span::default, |definition| definition.span),
        }
    }

    /// The definition a symbol names, when it is one of *this* package's.
    fn definition_of(&self, sym: Sym) -> Option<&Definition> {
        sym.def()
            .filter(|id| id.package == self.package)
            .map(|id| self.get(id))
    }

    pub fn get(&self, id: DefId) -> &Definition {
        debug_assert_eq!(
            id.package, self.package,
            "DefId from another package read out of this table",
        );
        &self.defs[id.index as usize]
    }

    /// Record the declared type discovered during resolution.
    pub fn set_ty(&mut self, id: DefId, ty: Ty) {
        debug_assert_eq!(id.package, self.package);
        self.defs[id.index as usize].ty = Some(ty);
    }

    pub fn len(&self) -> usize {
        self.defs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.defs.is_empty()
    }

    /// Every definition, in **registration order**.
    ///
    /// Registration order rather than scope order, because anything derived
    /// from a hash map must be deterministic before it reaches output
    /// ([A6](../../../plans/rewrite/anti-spec.md)) — and this iterator feeds
    /// WIT emission.
    pub fn iter(&self) -> impl Iterator<Item = &Definition> {
        self.defs.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use yelc_base::{Interner, SourceId};

    fn span() -> Span {
        Span::new(SourceId::new(0), 0, 1)
    }

    /// **Inverted deliberately on 2026-07-29.** This test used to be
    /// `namespaces_do_not_collide` and asserted the opposite — that a record and
    /// a component may share a name, which the frozen compiler accepts. The
    /// single-namespace symbol table rejects it: that is the narrowing, not a
    /// regression, and it is recorded in `plans/rewrite/scope.md` with the
    /// boundary enumerated against the frozen compiler in
    /// `tests/single_namespace.rs`. The assertion was flipped rather than
    /// deleted so the reversal stays visible.
    #[test]
    fn kinds_share_one_namespace_and_therefore_collide() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let name = interner.intern("Panel");
        let first = defs.register(name, DefKind::Type, span(), false).unwrap();
        let collision = defs
            .register(name, DefKind::Component, span(), false)
            .unwrap_err();
        assert_eq!(collision.existing, Sym::Type(first));
        assert_eq!(collision.attempted, Some(DefKind::Component));
        assert!(collision.is_cross_kind());
        assert_eq!(defs.len(), 1, "the second declaration must not register");
    }

    /// Every ordered pair of distinct kinds, not just the record/component one
    /// the frozen compiler's own test named.
    #[test]
    fn every_pair_of_distinct_kinds_collides() {
        let interner = Interner::new();
        for &first in DefKind::ALL {
            for &second in DefKind::ALL {
                if first == second {
                    continue;
                }
                let mut defs = Definitions::new(PackageId::LOCAL);
                let name = interner.intern("N");
                defs.register(name, first, span(), false).unwrap();
                let collision = defs.register(name, second, span(), false).unwrap_err();
                assert!(
                    collision.is_cross_kind(),
                    "{first:?} then {second:?} must be reported as a cross-kind collision",
                );
                assert_eq!(defs.len(), 1, "{first:?} then {second:?}");
            }
        }
    }

    #[test]
    fn a_duplicate_reports_and_keeps_the_original() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let name = interner.intern("R");
        let first = defs.register(name, DefKind::Type, span(), false).unwrap();
        let collision = defs
            .register(name, DefKind::Type, span(), false)
            .unwrap_err();
        assert_eq!(collision.existing.def(), Some(first));
        assert!(!collision.is_cross_kind());
        assert_eq!(defs.lookup(name), [Sym::Type(first)]);
        assert_eq!(defs.len(), 1, "the duplicate must not be registered");
    }

    /// B3: the scope is multi-valued, and this is the API that fills it.
    #[test]
    fn values_with_distinct_overload_keys_share_a_name() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let name = interner.intern("len");
        let on_list = defs
            .register_overload(
                name,
                span(),
                false,
                OverloadKey {
                    params: vec![Ty::ERROR],
                },
            )
            .unwrap();
        let on_string = defs
            .register_overload(
                name,
                span(),
                false,
                OverloadKey {
                    params: vec![Ty::STRING],
                },
            )
            .unwrap();
        assert_eq!(
            defs.lookup(name),
            [Sym::Value(on_list), Sym::Value(on_string)],
        );
    }

    #[test]
    fn an_overload_set_rejects_a_repeated_key() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let name = interner.intern("len");
        let key = OverloadKey {
            params: vec![Ty::STRING],
        };
        defs.register_overload(name, span(), false, key.clone())
            .unwrap();
        assert!(defs.register_overload(name, span(), false, key).is_err());
        assert_eq!(defs.len(), 1);
    }

    /// An empty key means "unoverloadable", so it collides with everything —
    /// including another empty key on the same kind.
    #[test]
    fn an_empty_overload_key_never_forms_a_set() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let name = interner.intern("len");
        defs.register(name, DefKind::Value, span(), false).unwrap();
        assert!(
            defs.register_overload(
                name,
                span(),
                false,
                OverloadKey {
                    params: vec![Ty::STRING],
                },
            )
            .is_err(),
            "an unoverloadable value must not acquire overloads later",
        );
        assert!(
            defs.register_overload(name, span(), false, OverloadKey::NONE)
                .is_err(),
            "an empty key is not a discriminator",
        );
    }

    /// A module occupies the same namespace as everything else.
    #[test]
    fn a_module_collides_with_a_definition_of_the_same_name() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let name = interner.intern("Hash");
        let module = defs
            .register_module(name, PackageId::new(4), span())
            .unwrap();
        assert_eq!(defs.lookup(name), [Sym::Module(module)]);
        let collision = defs
            .register(name, DefKind::Type, span(), false)
            .unwrap_err();
        assert_eq!(collision.existing, Sym::Module(module));
        assert_eq!(collision.existing.describe(), "a module");

        let mut other = Definitions::new(PackageId::LOCAL);
        other.register(name, DefKind::Type, span(), false).unwrap();
        let collision = other
            .register_module(name, PackageId::new(4), span())
            .unwrap_err();
        assert_eq!(collision.attempted, None);
        assert_eq!(collision.attempted_description(), "a module");
    }

    /// The tree is two levels: a module scope resolves independently of the
    /// root, and its definitions are the *module's* package's.
    #[test]
    fn a_module_scope_is_a_second_level_that_does_not_leak() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let foreign = PackageId::new(4);
        let module = defs
            .register_module(interner.intern("Sha256"), foreign, span())
            .unwrap();
        let hash = interner.intern("hash");
        defs.bind_in_module(module, hash, DefKind::Value, DefId::new(foreign, 9))
            .unwrap();

        assert_eq!(
            defs.lookup_in_module(module, hash),
            [Sym::Value(DefId::new(foreign, 9))],
        );
        assert!(
            defs.lookup(hash).is_empty(),
            "a module's names must not reach the root scope",
        );
        assert_eq!(defs.module(module).package, foreign);
    }

    /// A foreign definition has no span this compilation can render, and the
    /// table says so instead of aliasing one of its own.
    #[test]
    fn a_foreign_symbol_reports_no_span() {
        let defs = Definitions::new(PackageId::LOCAL);
        let foreign = Sym::Value(DefId::new(PackageId::new(4), 0));
        assert_eq!(defs.span_of(foreign), Span::default());
    }

    /// DefIds carry their package, so a table can catch a foreign one rather
    /// than silently indexing with it (decision B2).
    #[test]
    fn defids_are_package_qualified() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let id = defs
            .register(interner.intern("R"), DefKind::Type, span(), false)
            .unwrap();
        assert_eq!(id.package, PackageId::LOCAL);
        assert!(id.is_local());
        assert_ne!(id, DefId::new(PackageId(1), id.index));
    }

    /// A6: iteration order must not come from the hash map.
    #[test]
    fn iteration_is_registration_order() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        for name in ["zeta", "alpha", "mu"] {
            defs.register(interner.intern(name), DefKind::Type, span(), false)
                .unwrap();
        }
        let order: Vec<_> = defs
            .iter()
            .map(|d| interner.str(d.name).to_string())
            .collect();
        assert_eq!(order, vec!["zeta", "alpha", "mu"]);
    }

    #[test]
    fn declared_types_start_absent_not_placeholder() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let id = defs
            .register(interner.intern("x"), DefKind::Value, span(), false)
            .unwrap();
        assert_eq!(defs.get(id).ty, None, "no Ty::ERROR placeholder");
        defs.set_ty(id, Ty::S32);
        assert_eq!(defs.get(id).ty, Some(Ty::S32));
    }

    /// `DefKind` and `Sym` are one fact stored twice; `DefKind::sym` is the only
    /// bridge, so the two cannot drift.
    #[test]
    fn a_definitions_kind_agrees_with_the_symbol_bound_to_its_name() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        for (index, &kind) in DefKind::ALL.iter().enumerate() {
            let name = interner.intern(&format!("n{index}"));
            let id = defs.register(name, kind, span(), false).unwrap();
            assert_eq!(defs.lookup(name), [kind.sym(id)]);
            assert_eq!(defs.get(id).kind, kind);
            assert_eq!(kind.sym(id).kind(), Some(kind));
            assert_eq!(kind.sym(id).def(), Some(id));
        }
    }

    /// `lookup_def` is a filter on one binding, not a namespaced lookup: a name
    /// taken by another kind resolves to nothing at all.
    #[test]
    fn lookup_def_reports_nothing_when_the_name_is_another_kind() {
        let interner = Interner::new();
        let mut defs = Definitions::new(PackageId::LOCAL);
        let name = interner.intern("Color");
        defs.register(name, DefKind::Global, span(), false).unwrap();
        assert_eq!(defs.lookup_def(name, DefKind::Type), None);
        assert!(defs.lookup_def(name, DefKind::Global).is_some());
    }
}
