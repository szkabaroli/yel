//! AST → HIR: register, collect, lower — invariant H1's three phases.
//!
//! Each phase sweeps **every file** before the next begins, which is what makes
//! a reference legal regardless of source order and file order — the frozen
//! driver merged fully-lowered files in a loop, so cross-file references
//! resolved in one direction only (F4). Phase-major sweeping is the fix.
//!
//! | phase | does, across all files | may not |
//! |---|---|---|
//! | 1 · register | a `DefId` + member rows for every item | call `type_of` |
//! | 2 · collect | resolve every **declared** type into the tables | look at any body |
//! | 3 · lower | lower bodies (globals, then components — D5) | register new items |
//!
//! # Registration order is kind-major, and that is load-bearing
//!
//! Within phase 1 items register **records → enums → variants → elements →
//! extern components → globals → components**, each kind in source order —
//! the frozen compiler's order, kept because `DefId`s are ordinals that reach
//! output ordering and the differential compares the table *and its order*.
//! (The frozen tree gets this order from its AST, which pre-groups items by
//! kind; ours keeps source order, so the grouping is explicit here.)
//!
//! # The ten UI primitive spellings have no type — a recorded gap
//!
//! `length`, `physical-length`, `angle`, `duration`, `percent`,
//! `relative-font-size`, `color`, `brush`, `image`, `easing` parse
//! ([`ast::PrimitiveType`]) and have **no [`TyKind`] variant** — the frozen
//! interner has one for each. [`type_of`](LoweringContext::type_of) answers
//! `None` for them, exactly as it does for an unresolved name. Measured before
//! deciding this was tolerable: not one of the checked-in corpus, fixture or
//! example programs writes one as an annotation, and `yel-smith` deliberately
//! generates them only as attribute *values*. The decision — distinct types
//! (frozen), stdlib ADTs, or aliases — is owed before stage 4 types a program
//! that uses one.

use rustc_hash::FxHashMap;
use yelc_base::{Diagnostic, Diagnostics, ErrorCode, Name, Span};
use yelc_sema::definitions::{Member, MemberDirection, MemberKind};
use yelc_sema::{CompilerContext, DefId, DefKind, Sym, Ty, TyKind};
use yelc_syntax::{ParsedFile, ast};

use crate::ids::{HirId, SourceNodeId, TypeId};
use crate::module::HirModule;

mod bodies;
mod ui;

/// Lower the whole file set into one package's HIR.
///
/// Diagnostics accumulate in `sema.diagnostics` and lowering continues — no
/// `Result`, per the accumulate-and-continue policy.
pub fn lower_files(parsed: &[ParsedFile], sema: &mut CompilerContext) -> HirModule {
    let package = sema.defs.package();
    let sources = parsed.iter().map(|file| file.source).collect();
    let mut lowering = LoweringContext {
        sema,
        module: HirModule::new(package, sources),
        item_defs: FxHashMap::default(),
        type_memo: FxHashMap::default(),
    };
    lowering.register(parsed); // phase 1
    lowering.collect(parsed); // phase 2
    lowering.lower(parsed); // phase 3

    // Signal dependencies — the frozen `signalck`, one stage earlier: needs
    // names only, so it runs before checking, on the desugared bodies.
    let mut module = lowering.module;
    for (body, dependencies) in crate::signalck::compute(&module, lowering.sema) {
        module.dependencies.insert(body, dependencies);
    }
    module
}

/// **The owner of `type_of`** — the type the seam contract left unnamed, named
/// 2026-07-30 (`plans/rewrite/stage-3-hir-build.md` § what the seam could not
/// be written as). It exists between phases 1 and 3 and dies with the
/// lowering, so its memo cannot carry stale entries between compilations.
///
/// `type_of` cannot live on [`CompilerContext`]: it is keyed by [`TypeId`],
/// which is this crate's, and `yelc-sema` sits below `yelc-syntax` in the
/// crate graph. Not a style call — a build error.
pub(crate) struct LoweringContext<'a> {
    pub sema: &'a mut CompilerContext,
    pub module: HirModule,
    /// The `DefId` phase 1 gave each item node, for phases 2 and 3 to revisit.
    item_defs: FxHashMap<SourceNodeId, DefId>,
    /// `type_of`'s memo, keyed by [`TypeId`] — **not** a `NodeMap<Ty>`, which
    /// keys `HirId`; the key-space mismatch was the second contract defect.
    /// The memoized value is the whole answer, `None` included: an unresolved
    /// annotation is unresolved every time it is asked about.
    type_memo: FxHashMap<TypeId, Option<Ty>>,
}

/// Walk one file's items of one shape, in source order.
macro_rules! for_items {
    ($file:expr, $pat:pat => $body:expr) => {
        for item in &$file.ast.items {
            if let $pat = item {
                $body
            }
        }
    };
}

impl<'a> LoweringContext<'a> {
    fn node(&self, file: &ParsedFile, id: yelc_syntax::NodeId) -> SourceNodeId {
        SourceNodeId::new(file.source, id)
    }

    // ------------------------------------------------------------------
    // Phase 1 · register
    // ------------------------------------------------------------------

    fn register(&mut self, parsed: &[ParsedFile]) {
        use ast::ItemKind as I;
        // Kind-major, the frozen order. `Package` was consumed by
        // `check_package_identity`; `Error` carries its parse diagnostic (H5's
        // "or a diagnostic" arm).
        for file in parsed {
            for_items!(file, I::Record(decl) => self.register_record(file, decl));
        }
        for file in parsed {
            for_items!(file, I::Enum(decl) => self.register_enum(file, decl));
        }
        for file in parsed {
            for_items!(file, I::Variant(decl) => self.register_variant(file, decl));
        }
        for file in parsed {
            for_items!(file, I::Element(decl) => self.register_element(file, decl));
        }
        for file in parsed {
            for_items!(file, I::ExternComponent(decl) => self.register_extern(file, decl));
        }
        for file in parsed {
            for_items!(file, I::Global(decl) => self.register_global(file, decl));
        }
        for file in parsed {
            for_items!(file, I::Component(decl) => self.register_component(file, decl));
        }
    }

    /// Register one item's name. `None` when the name is a parse hole (its
    /// diagnostic exists; there is nothing to bind) or the name is taken (the
    /// duplicate is reported here, matching the frozen message).
    ///
    /// Also attaches the item's doc comment (D6): the nearest preceding `//`
    /// run with no blank line between it and `span`'s first line.
    fn register_item(
        &mut self,
        file: &ParsedFile,
        node: yelc_syntax::NodeId,
        span: Span,
        name: &ast::MaybeIdent,
        kind: DefKind,
        is_export: bool,
    ) -> Option<DefId> {
        let ident = name.present()?;
        match self
            .sema
            .defs
            .register(ident.name, kind, ident.span, is_export)
        {
            Ok(def) => {
                self.item_defs.insert(self.node(file, node), def);
                let doc = self
                    .sema
                    .sources
                    .get(span.source)
                    .and_then(|source| doc_comment(&source.content, span.start));
                if let Some(doc) = doc {
                    let doc = self.sema.names.intern(&doc);
                    self.module.attach_doc(def, doc);
                }
                Some(def)
            }
            Err(collision) => {
                report_duplicate(
                    &mut self.sema.diagnostics,
                    &self.sema.sources,
                    &self.sema.names,
                    &collision,
                );
                None
            }
        }
    }

    fn member(&mut self, owner: DefId, name: Name, kind: MemberKind, span: Span) {
        self.sema.defs.add_member(
            owner,
            Member {
                name,
                kind,
                span,
                ty: None,
            },
        );
    }

    fn register_record(&mut self, file: &ParsedFile, decl: &ast::RecordDecl) {
        let Some(def) =
            self.register_item(file, decl.id, decl.span, &decl.name, DefKind::Type, false)
        else {
            return;
        };
        for field in decl.present_fields() {
            if let Some(ident) = field.name.present() {
                self.member(def, ident.name, MemberKind::Field, ident.span);
            }
        }
    }

    fn register_enum(&mut self, file: &ParsedFile, decl: &ast::EnumDecl) {
        let Some(def) =
            self.register_item(file, decl.id, decl.span, &decl.name, DefKind::Type, false)
        else {
            return;
        };
        for case in &decl.cases {
            if let Some(ident) = case.present() {
                self.member(def, ident.name, MemberKind::Case, ident.span);
            }
        }
    }

    fn register_variant(&mut self, file: &ParsedFile, decl: &ast::VariantDecl) {
        let Some(def) =
            self.register_item(file, decl.id, decl.span, &decl.name, DefKind::Type, false)
        else {
            return;
        };
        for case in &decl.cases {
            let Some(case) = case.present() else { continue };
            if let Some(ident) = case.name.present() {
                self.member(def, ident.name, MemberKind::Case, ident.span);
            }
        }
    }

    fn register_element(&mut self, file: &ParsedFile, decl: &ast::ElementDecl) {
        let Some(def) = self.register_item(
            file,
            decl.id,
            decl.span,
            &decl.name,
            DefKind::Component,
            false,
        ) else {
            return;
        };
        for property in decl.properties() {
            if let Some(ident) = property.name.present() {
                self.member(
                    def,
                    ident.name,
                    MemberKind::Property {
                        direction: MemberDirection::None,
                    },
                    ident.span,
                );
            }
        }
    }

    fn register_extern(&mut self, file: &ParsedFile, decl: &ast::ExternComponentDecl) {
        let Some(def) = self.register_item(
            file,
            decl.id,
            decl.span,
            &decl.name,
            DefKind::Component,
            false,
        ) else {
            return;
        };
        for member in &decl.members {
            match member {
                ast::ExternMember::Property(property) => {
                    if let Some(ident) = property.name.present() {
                        self.member(
                            def,
                            ident.name,
                            MemberKind::Property {
                                direction: MemberDirection::None,
                            },
                            ident.span,
                        );
                    }
                }
                ast::ExternMember::Method(method) => {
                    if let Some(ident) = method.name.present() {
                        self.member(def, ident.name, MemberKind::Function, ident.span);
                    }
                }
                // The single-slot rule is checked where slots are lowered; an
                // extern component's marker registers nothing.
                ast::ExternMember::Children { .. } => {}
                // Parse recovery: its diagnostic exists (H5).
                ast::ExternMember::Error { .. } => {}
            }
        }
    }

    fn register_global(&mut self, file: &ParsedFile, decl: &ast::GlobalDecl) {
        let Some(def) = self.register_item(
            file,
            decl.id,
            decl.span,
            &decl.name,
            DefKind::Global,
            decl.is_export,
        ) else {
            return;
        };
        for member in &decl.members {
            match member {
                ast::GlobalMember::Property(property) => {
                    if let Some(ident) = property.name.present() {
                        self.member(
                            def,
                            ident.name,
                            MemberKind::Property {
                                direction: direction(property.direction),
                            },
                            ident.span,
                        );
                    }
                }
                ast::GlobalMember::Callback(callback) => {
                    if let Some(ident) = callback.name.present() {
                        self.member(def, ident.name, MemberKind::Function, ident.span);
                    }
                }
                ast::GlobalMember::Error { .. } => {}
            }
        }
    }

    fn register_component(&mut self, file: &ParsedFile, decl: &ast::ComponentDecl) {
        let Some(def) = self.register_item(
            file,
            decl.id,
            decl.span,
            &decl.name,
            DefKind::Component,
            decl.is_export,
        ) else {
            return;
        };
        for member in &decl.members {
            match member {
                ast::ComponentMember::Property(property) => {
                    if let Some(ident) = property.name.present() {
                        self.member(
                            def,
                            ident.name,
                            MemberKind::Property {
                                direction: MemberDirection::None,
                            },
                            ident.span,
                        );
                    }
                }
                ast::ComponentMember::Function(function) => {
                    if let Some(ident) = function.name.present() {
                        self.member(def, ident.name, MemberKind::Function, ident.span);
                    }
                }
                // The UI tree is phase 3's; nothing about it registers.
                ast::ComponentMember::Node(_) => {}
                ast::ComponentMember::Error { .. } => {}
            }
        }
    }

    // ------------------------------------------------------------------
    // Phase 2 · collect declared types
    // ------------------------------------------------------------------
    //
    // ⚠️ Member **indices** here must mirror phase 1's registration exactly —
    // both walk the same lists with the same `present()` filters. The
    // `debug_assert_eq!` on each item's row count is the tripwire.

    fn collect(&mut self, parsed: &[ParsedFile]) {
        use ast::ItemKind as I;
        for file in parsed {
            for item in &file.ast.items {
                match item {
                    I::Record(decl) => self.collect_record(file, decl),
                    I::Enum(decl) => self.collect_enum(file, decl),
                    I::Variant(decl) => self.collect_variant(file, decl),
                    I::Element(decl) => self.collect_element(file, decl),
                    I::ExternComponent(decl) => self.collect_extern(file, decl),
                    I::Global(decl) => self.collect_global(file, decl),
                    I::Component(decl) => self.collect_component(file, decl),
                    // An include was consumed by the driver — the module
                    // binding and any not-found diagnostic exist before this
                    // sweep starts (H5's diagnostic arm, one level up).
                    I::Package(_) | I::Include(_) | I::Error { .. } => {}
                }
            }
        }
    }

    /// The item's own registered def, if phase 1 gave it one.
    fn def_of(&self, file: &ParsedFile, node: yelc_syntax::NodeId) -> Option<DefId> {
        self.item_defs.get(&self.node(file, node)).copied()
    }

    fn set_own_ty(&mut self, def: DefId) {
        let ty = self.sema.types.intern(TyKind::Adt(def));
        self.sema.defs.set_ty(def, ty);
    }

    fn set_member(&mut self, def: DefId, index: &mut u32, ty: Option<Ty>) {
        if let Some(ty) = ty {
            self.sema.defs.set_member_ty(def, *index, ty);
        }
        *index += 1;
    }

    fn collect_record(&mut self, file: &ParsedFile, decl: &ast::RecordDecl) {
        let Some(def) = self.def_of(file, decl.id) else {
            return;
        };
        self.set_own_ty(def);
        let mut index = 0;
        for field in decl.present_fields() {
            if field.name.present().is_some() {
                let ty = self.type_of(file, &field.ty, &[]);
                self.set_member(def, &mut index, ty);
            }
        }
        debug_assert_eq!(index as usize, self.sema.defs.members(def).len());
    }

    fn collect_enum(&mut self, file: &ParsedFile, decl: &ast::EnumDecl) {
        let Some(def) = self.def_of(file, decl.id) else {
            return;
        };
        self.set_own_ty(def);
    }

    fn collect_variant(&mut self, file: &ParsedFile, decl: &ast::VariantDecl) {
        let Some(def) = self.def_of(file, decl.id) else {
            return;
        };
        self.set_own_ty(def);
        let mut index = 0;
        for case in &decl.cases {
            let Some(case) = case.present() else { continue };
            if case.name.present().is_some() {
                let ty = case
                    .payload
                    .as_ref()
                    .and_then(|ty| self.type_of(file, ty, &[]));
                self.set_member(def, &mut index, ty);
            }
        }
        debug_assert_eq!(index as usize, self.sema.defs.members(def).len());
    }

    fn collect_element(&mut self, file: &ParsedFile, decl: &ast::ElementDecl) {
        let Some(def) = self.def_of(file, decl.id) else {
            return;
        };
        self.set_own_ty(def);
        let mut index = 0;
        for property in decl.properties() {
            if property.name.present().is_some() {
                let ty = self.type_of(file, &property.ty, &[]);
                self.set_member(def, &mut index, ty);
            }
        }
        debug_assert_eq!(index as usize, self.sema.defs.members(def).len());
    }

    fn collect_extern(&mut self, file: &ParsedFile, decl: &ast::ExternComponentDecl) {
        let Some(def) = self.def_of(file, decl.id) else {
            return;
        };
        self.set_own_ty(def);
        let mut index = 0;
        for member in &decl.members {
            match member {
                ast::ExternMember::Property(property) => {
                    if property.name.present().is_some() {
                        let ty = self.type_of(file, &property.ty, &[]);
                        self.set_member(def, &mut index, ty);
                    }
                }
                ast::ExternMember::Method(method) => {
                    if method.name.present().is_some() {
                        let ty = self.signature_ty(file, &method.signature);
                        self.set_member(def, &mut index, ty);
                    }
                }
                ast::ExternMember::Children { .. } | ast::ExternMember::Error { .. } => {}
            }
        }
        debug_assert_eq!(index as usize, self.sema.defs.members(def).len());
    }

    fn collect_global(&mut self, file: &ParsedFile, decl: &ast::GlobalDecl) {
        let Some(def) = self.def_of(file, decl.id) else {
            return;
        };
        self.set_own_ty(def);
        let mut index = 0;
        for member in &decl.members {
            match member {
                ast::GlobalMember::Property(property) => {
                    if property.name.present().is_some() {
                        let ty = self.type_of(file, &property.ty, &[]);
                        self.set_member(def, &mut index, ty);
                    }
                }
                ast::GlobalMember::Callback(callback) => {
                    if callback.name.present().is_some() {
                        let ty = self.signature_ty(file, &callback.signature);
                        self.set_member(def, &mut index, ty);
                    }
                }
                ast::GlobalMember::Error { .. } => {}
            }
        }
        debug_assert_eq!(index as usize, self.sema.defs.members(def).len());
    }

    fn collect_component(&mut self, file: &ParsedFile, decl: &ast::ComponentDecl) {
        let Some(def) = self.def_of(file, decl.id) else {
            return;
        };
        self.set_own_ty(def);
        let mut index = 0;
        for member in &decl.members {
            match member {
                ast::ComponentMember::Property(property) => {
                    if property.name.present().is_some() {
                        let ty = self.type_of(file, &property.ty, &[]);
                        self.set_member(def, &mut index, ty);
                    }
                }
                ast::ComponentMember::Function(function) => {
                    if function.name.present().is_some() {
                        let ty = self.signature_ty(file, &function.signature);
                        self.set_member(def, &mut index, ty);
                    }
                }
                ast::ComponentMember::Node(_) | ast::ComponentMember::Error { .. } => {}
            }
        }
        debug_assert_eq!(index as usize, self.sema.defs.members(def).len());
    }

    // ------------------------------------------------------------------
    // type_of
    // ------------------------------------------------------------------

    /// The one syntax → [`Ty`] function. Resolves a written annotation against
    /// the definition tables and interns. Memoized by [`TypeId`].
    ///
    /// `None` means *the annotation does not resolve* — an unknown name, a
    /// recovery node, a spelling with no type yet — and it is the answer, not
    /// a failure: the definition keeps `ty: None` (never a placeholder) and
    /// stage 4 reports against the written syntax.
    ///
    /// `params` is the generic-parameter scope of the enclosing signature —
    /// `T` in `func<T>(a: T)` resolves to [`TyKind::Param`]. Non-empty scope
    /// bypasses the memo: the same written `T` means a different thing in a
    /// different signature, and the memo key cannot say which.
    ///
    /// **Callable only after phase 1** — it reads the tables phase 1 builds.
    /// It is structurally unreachable earlier: the receiver is this context,
    /// and phase 1 is a method on the same context that never calls it.
    pub(crate) fn type_of(
        &mut self,
        file: &ParsedFile,
        ty: &ast::TypeRef,
        params: &[Name],
    ) -> Option<Ty> {
        let key = TypeId::new(self.node(file, ty.id));
        if params.is_empty()
            && let Some(&memoized) = self.type_memo.get(&key)
        {
            return memoized;
        }
        let resolved = self.resolve_type(file, ty, params);
        if params.is_empty() {
            self.type_memo.insert(key, resolved);
        }
        resolved
    }

    fn resolve_type(
        &mut self,
        file: &ParsedFile,
        ty: &ast::TypeRef,
        params: &[Name],
    ) -> Option<Ty> {
        use ast::PrimitiveType as P;
        match &ty.kind {
            ast::TypeKind::Primitive(primitive) => match primitive {
                P::Bool => Some(Ty::BOOL),
                P::S8 => Some(Ty::S8),
                P::S16 => Some(Ty::S16),
                P::S32 => Some(Ty::S32),
                P::S64 => Some(Ty::S64),
                P::U8 => Some(Ty::U8),
                P::U16 => Some(Ty::U16),
                P::U32 => Some(Ty::U32),
                P::U64 => Some(Ty::U64),
                P::F32 => Some(Ty::F32),
                P::F64 => Some(Ty::F64),
                P::Char => Some(Ty::CHAR),
                P::String => Some(Ty::STRING),
                // The recorded gap — see the module doc.
                P::Length
                | P::PhysicalLength
                | P::Angle
                | P::Duration
                | P::Percent
                | P::RelativeFontSize
                | P::Color
                | P::Brush
                | P::Image
                | P::Easing => None,
            },
            ast::TypeKind::Named(name) => {
                if let Some(position) = params.iter().position(|param| param == name) {
                    return Some(self.sema.types.intern(TyKind::Param(position as u32)));
                }
                // Only a type-kinded symbol becomes an `Adt` — the frozen tree
                // looks the name up in `Namespace::Type` alone, and `x: Counter`
                // (a component) staying unresolved preserves that behaviour
                // under one namespace.
                self.sema
                    .defs
                    .lookup(*name)
                    .iter()
                    .find_map(|sym| match sym {
                        Sym::Type(def) => Some(self.sema.types.intern(TyKind::Adt(*def))),
                        Sym::Value(_) | Sym::Component(_) | Sym::Global(_) | Sym::Module(_) => None,
                    })
            }
            ast::TypeKind::List(inner) => {
                let inner = self.type_of(file, inner, params)?;
                Some(self.sema.types.intern(TyKind::List(inner)))
            }
            ast::TypeKind::Option(inner) => {
                let inner = self.type_of(file, inner, params)?;
                Some(self.sema.types.intern(TyKind::Option(inner)))
            }
            ast::TypeKind::Result { args } => {
                // Stored as written (S5); `result<a,b,c>` was reported by the
                // parser, and the first two argument positions are the type.
                // An absent position is `None`-the-type (bare `result`); an
                // unresolvable one fails the whole annotation, like any child.
                let mut ok = None;
                if let Some(arg) = args.first() {
                    ok = Some(self.type_of(file, arg, params)?);
                }
                let mut err = None;
                if let Some(arg) = args.get(1) {
                    err = Some(self.type_of(file, arg, params)?);
                }
                Some(self.sema.types.intern(TyKind::Result { ok, err }))
            }
            ast::TypeKind::Tuple(elements) => {
                let elements: Option<Vec<Ty>> = elements
                    .iter()
                    .map(|element| self.type_of(file, element, params))
                    .collect();
                Some(self.sema.types.intern(TyKind::Tuple(elements?)))
            }
            ast::TypeKind::Func(signature) => self.func_ty(file, signature, params),
            ast::TypeKind::Error => None,
        }
    }

    /// A declared function member's `Func` type, from its signature.
    fn signature_ty(
        &mut self,
        file: &ParsedFile,
        signature: &ast::Recovered<ast::FuncSignature>,
    ) -> Option<Ty> {
        let signature = signature.present()?;
        let params: Vec<Name> = signature
            .present_type_params()
            .filter_map(|param| param.name.present().map(|ident| ident.name))
            .collect();
        self.func_ty(file, signature, &params)
    }

    fn func_ty(
        &mut self,
        file: &ParsedFile,
        signature: &ast::FuncSignature,
        params: &[Name],
    ) -> Option<Ty> {
        let declared: Option<Vec<Ty>> = signature
            .present_params()
            .map(|param| self.type_of(file, &param.ty, params))
            .collect();
        let ret = match &signature.return_type {
            Some(ty) => Some(self.type_of(file, ty, params)?),
            None => None,
        };
        Some(self.sema.types.intern(TyKind::Func {
            params: declared?,
            ret,
        }))
    }

    // ------------------------------------------------------------------
    // Phase 3 · lower bodies — in bodies.rs / ui.rs
    // ------------------------------------------------------------------

    fn lower(&mut self, parsed: &[ParsedFile]) {
        use ast::ItemKind as I;
        // D5: globals before components — the dependency direction (components
        // call globals; the grammar gives a global no place to use a
        // component), reversing the frozen lowering order deliberately.
        for file in parsed {
            for_items!(file, I::Global(decl) => bodies::lower_global(self, file, decl));
        }
        for file in parsed {
            for_items!(file, I::Component(decl) => bodies::lower_component(self, file, decl));
        }
    }

    /// Allocate a **primary** id: this HIR node is the lowering of that AST
    /// node.
    pub(crate) fn primary(&mut self, node: SourceNodeId) -> HirId {
        self.module.map.next_hir_id(node)
    }

    /// Allocate a **synthesized** id: a desugaring product whose origin is
    /// that AST node. Forward-mapped only — see `HirMap::synthesize`.
    pub(crate) fn synthesize(&mut self, origin: SourceNodeId) -> HirId {
        self.module.map.synthesize(origin)
    }
}

fn direction(direction: Option<ast::PropertyDirection>) -> MemberDirection {
    match direction {
        None => MemberDirection::None,
        Some(ast::PropertyDirection::In) => MemberDirection::In,
        Some(ast::PropertyDirection::Out) => MemberDirection::Out,
        Some(ast::PropertyDirection::InOut) => MemberDirection::InOut,
    }
}

/// D6's extraction rule, over the source text (which the green tree
/// reconstructs byte-for-byte — S1 — so text scanning *is* reading the green
/// trivia, without a token walk):
///
/// - **only `///` lines are documentation** — Rust's line, adopted 2026-07-31
///   after reading WIT's parser, where `//` and `///` blur together at
///   resolve time (`trim_start_matches('/')`) and any comment becomes docs.
///   A plain `//` line is commentary: it never attaches, and one sitting
///   between a `///` run and the item ends the run;
/// - the run is the contiguous `///` lines directly above the item's first
///   line;
/// - a blank line between run and item breaks attachment entirely;
/// - lines join in source order, `///` and one leading space stripped;
/// - a run with no item after it attaches to nothing, silently.
fn doc_comment(text: &str, item_start: usize) -> Option<String> {
    let item_start = item_start.min(text.len());
    // Start of the line the item begins on; only full lines above are read.
    let line_start = text[..item_start].rfind('\n').map_or(0, |index| index + 1);

    let mut run: Vec<&str> = Vec::new();
    let mut rest = &text[..line_start];
    loop {
        if rest.is_empty() {
            break;
        }
        let without_newline = rest.strip_suffix('\n').unwrap_or(rest);
        let previous_start = without_newline.rfind('\n').map_or(0, |index| index + 1);
        let line = without_newline[previous_start..].trim_end_matches('\r');
        let trimmed = line.trim_start();
        if let Some(doc) = trimmed.strip_prefix("///") {
            run.push(doc.strip_prefix(' ').unwrap_or(doc));
            rest = &rest[..previous_start];
        } else {
            // Blank, code, or a plain `//` comment: the line directly above
            // the item decides attachment; higher lines merely end the run.
            break;
        }
    }
    if run.is_empty() {
        return None;
    }
    run.reverse();
    Some(run.join("\n"))
}

/// The collision reporter, context-shaped — `includes` binds modules into the
/// same scope and must collide with the same words.
pub(crate) fn report_duplicate_collision(
    context: &mut CompilerContext,
    collision: &yelc_sema::Collision,
) {
    report_duplicate(
        &mut context.diagnostics,
        &context.sources,
        &context.names,
        collision,
    );
}

/// The frozen duplicate-definition headline, kept word for word — the
/// diagnostic differential compares meaning, and there is no reason to spend
/// a divergence on phrasing. The **note** carries what ark's
/// `report_sym_shadow_span` taught: name what was shadowed, kind-specifically
/// — "previously defined as a component" points at the right declaration;
/// bare "previously defined" makes the reader find both. `Collision` always
/// carried the kinds; this is the first consumer of `describe()`.
fn report_duplicate(
    diagnostics: &mut Diagnostics,
    sources: &yelc_base::SourceMap,
    names: &yelc_base::Interner,
    collision: &yelc_sema::Collision,
) {
    let name = names.str(collision.name);
    let location = sources
        .get(collision.existing_span.source)
        .map(|source| {
            format!(
                "{}:{}",
                source.name(),
                source.line_col(collision.existing_span.start).0
            )
        })
        .unwrap_or_else(|| "<unknown>".to_string());
    diagnostics.push(
        Diagnostic::error(format!("duplicate definition of `{name}`"))
            .with_span(collision.span)
            .with_code(ErrorCode::DuplicateDefinition)
            .with_note(format!(
                "previously defined as {} at {location}",
                collision.existing.describe()
            )),
    );
}
