//! Central compiler context.

use rustc_hash::FxHashMap as HashMap;
use std::cell::RefCell;
use std::sync::Arc;

use crate::definitions::{Definitions, Namespace};
use crate::diagnostic::Diagnostics;
use crate::dom_imports::DomImports;
use crate::ids::{BlockId, DefId};
use crate::interner::{Interner, Name};
use crate::known::KnownDefinitions;
use crate::source::SourceMap;
use crate::syntax::ast::TyKind as AstTyKind;
use crate::thir::signalck::SignalDependencies;
use crate::types::{InternedTyKind, Ty, TypeInterner};

/// Stores global compiler state.
///
/// This is the equivalent of rustc's TyCtxt - the single source of truth
/// for all compiler data.
pub struct CompilerContext {
    /// String interner (shared, thread-safe).
    pub interner: Arc<Interner>,
    /// Type interner.
    pub types: TypeInterner,
    /// All definitions.
    pub defs: Definitions,
    /// Known builtin definitions.
    pub known: KnownDefinitions,
    /// DefId table for `meshx-ui/dom` WIT imports. `None` until
    /// `lookup_known_definitions` has run; afterwards always `Some`.
    /// Phase 2.2 will emit `LirOp::CallFunction` against these DefIds;
    /// codegen resolves them back to wasm import indices.
    dom_global: Option<DefId>,
    /// Source file management.
    pub source_map: SourceMap,
    /// Accumulated diagnostics.
    pub diagnostics: Diagnostics,
    /// Debug names for LIR blocks: (component DefId, BlockId) ->
    /// structured `BlockDebugName`. Uses RefCell for interior
    /// mutability since blocks are named during lowering.
    block_names: RefCell<HashMap<(DefId, BlockId), BlockDebugName>>,
    /// Module-wide BlockId counter. Allocated via `alloc_block_id`
    /// so every block across every `LirResource` in a compilation
    /// gets a unique id. Lets codegen key `block_func_indices` by
    /// `BlockId` alone (no component index tuple) and lets
    /// `LirOp::CallBlock` call across components uniformly.
    block_id_counter: std::cell::Cell<u32>,
    /// Per-component lifecycle BlockId table populated as
    /// `LirResource`s are lowered. Lets the THIR→LIR mount-site
    /// lowering (`lower_mount_component`) resolve a child
    /// component's `internal_constructor_block` and `mount_block`
    /// to global BlockIds without needing the child's full
    /// `LirResource` in hand. Requires children to be lowered
    /// before parents (which is the source/typeck-resolved order
    /// in current pipelines).
    component_lifecycle_blocks: RefCell<HashMap<DefId, ComponentLifecycleBlocks>>,
    /// Per-(observing component, global signal) fanout block table.
    /// Populated by `synth_global_fanout_blocks` when the module-level
    /// `resolve_global_triggers` pass runs (after every component is
    /// lowered). Read by that pass to expand `TriggerEffects`
    /// placeholders into per-observer `CallBlock`s, and by codegen's
    /// `emit_trigger_effects` for codegen-synthesized global writers
    /// (binding setters).
    ///
    /// Key: `(observing_component_def_id, global_signal_def_id)`. Value:
    /// module-wide BlockId allocated via `alloc_block_id`.
    global_fanout_blocks: RefCell<HashMap<(DefId, DefId), BlockId>>,
    /// Per-component / per-global signal-dependency analysis, keyed by the
    /// owning component/global `DefId`. Produced by [`crate::thir::signalck`]
    /// after typeck. This is a DefId-keyed *side table*, not a field on the
    /// THIR node: signalck is a read-only analysis (it does not mutate THIR),
    /// so its output belongs next to the other analysis tables rather than
    /// bolted onto the node it analysed. Staged for the Phase 1.1c
    /// effect-fanout consumer.
    signal_deps: HashMap<DefId, SignalDependencies>,
}

/// Snapshot of the lifecycle BlockIds for a lowered component,
/// captured at `lower_component` return time and consulted by
/// `lower_mount_component` when a parent mounts this component as
/// a child.
#[derive(Debug, Clone, Copy)]
pub struct ComponentLifecycleBlocks {
    pub internal_constructor_block: Option<BlockId>,
    pub mount_block: BlockId,
}

/// Structured debug name for a lowered block. Stored once at lowering
/// time and consumed by both the WASM name section (which formats as
/// `<comp>-<kind>[-b<bid>]*[-s<sid>]#<block_id>`) and the DOT signal
/// graph renderer (which uses the same fields to build human-friendly
/// labels). Holding the structured form rather than a pre-formatted
/// string keeps consumers from having to re-parse strings to recover
/// what the lowering pass already knew.
///
/// Boundary ids are NOT stored here — they live on
/// `LirBlock.boundary_params` and are read directly by consumers, so
/// adding/removing a boundary param doesn't require updating the
/// debug name.
#[derive(Debug, Clone)]
pub struct BlockDebugName {
    /// Conceptual role of the block — one of:
    ///   `constructor`, `mount`, `update`, `noop-update`,
    ///   `derived-update`, `if-update`, `if-branch-mount`,
    ///   `if-branch-unmount`, `for-update`, `for-item-mount`,
    ///   `for-item-unmount`, `handle-<event>`.
    /// Stored as `Cow` so common kinds are static literals
    /// (no allocation) while parameterised ones (like
    /// `handle-<event>`) own their formatted strings.
    pub kind: std::borrow::Cow<'static, str>,
    /// Signal id this block is keyed to. `Some` only for the
    /// per-(boundary, signal) update fns (`kind == "update"`).
    pub signal: Option<DefId>,
}

impl BlockDebugName {
    pub fn kind(kind: &'static str) -> Self {
        Self {
            kind: std::borrow::Cow::Borrowed(kind),
            signal: None,
        }
    }

    pub fn update(signal: DefId) -> Self {
        Self {
            kind: std::borrow::Cow::Borrowed("update"),
            signal: Some(signal),
        }
    }

    pub fn handle(event: &str) -> Self {
        Self {
            kind: std::borrow::Cow::Owned(format!("handle-{}", event)),
            signal: None,
        }
    }
}

impl Default for CompilerContext {
    fn default() -> Self {
        Self::new()
    }
}

impl CompilerContext {
    /// Create a new compiler context.
    pub fn new() -> Self {
        Self {
            interner: Arc::new(Interner::new()),
            types: TypeInterner::new(),
            defs: Definitions::new(),
            known: KnownDefinitions::new(),
            source_map: SourceMap::new(),
            diagnostics: Diagnostics::new(),
            dom_global: None,
            block_names: RefCell::new(HashMap::default()),
            block_id_counter: std::cell::Cell::new(0),
            component_lifecycle_blocks: RefCell::new(HashMap::default()),
            global_fanout_blocks: RefCell::new(HashMap::default()),
            signal_deps: HashMap::default(),
        }
    }

    /// Create with a shared interner.
    pub fn with_interner(interner: Arc<Interner>) -> Self {
        Self {
            interner,
            types: TypeInterner::new(),
            defs: Definitions::new(),
            known: KnownDefinitions::new(),
            source_map: SourceMap::new(),
            diagnostics: Diagnostics::new(),
            dom_global: None,
            block_names: RefCell::new(HashMap::default()),
            block_id_counter: std::cell::Cell::new(0),
            component_lifecycle_blocks: RefCell::new(HashMap::default()),
            global_fanout_blocks: RefCell::new(HashMap::default()),
            signal_deps: HashMap::default(),
        }
    }

    /// Record the signal-dependency analysis for a component or global,
    /// keyed by its `DefId`. Called by the typeck driver right after
    /// [`crate::thir::signalck`] runs.
    pub fn set_signal_deps(&mut self, owner: DefId, deps: SignalDependencies) {
        self.signal_deps.insert(owner, deps);
    }

    /// Look up the signal-dependency analysis for a component or global.
    pub fn signal_deps(&self, owner: DefId) -> Option<&SignalDependencies> {
        self.signal_deps.get(&owner)
    }

    // ========================================================================
    // String interning
    // ========================================================================

    /// Intern a string.
    pub fn intern(&self, s: &str) -> Name {
        self.interner.intern(s)
    }

    /// Get the string for an interned name. Returns an `ArcStr`
    /// (cheaply cloneable, derefs to `&str`) — avoids the per-call
    /// allocation that `.to_string()` would incur in hot paths like
    /// name-section emission.
    pub fn str(&self, name: Name) -> crate::interner::ArcStr {
        self.interner.str(name)
    }

    // ========================================================================
    // Type interning
    // ========================================================================

    /// Intern a type.
    pub fn intern_ty(&mut self, kind: InternedTyKind) -> Ty {
        self.types.intern(kind)
    }

    /// Get the kind of an interned type.
    pub fn ty_kind(&self, ty: Ty) -> &InternedTyKind {
        self.types.kind(ty)
    }

    /// Intern a list type.
    pub fn mk_list(&mut self, elem: Ty) -> Ty {
        self.types.intern_list(elem)
    }

    /// Intern an option type.
    pub fn mk_option(&mut self, inner: Ty) -> Ty {
        self.types.intern_option(inner)
    }

    /// Intern a result type.
    pub fn mk_result(&mut self, ok: Ty, err: Ty) -> Ty {
        self.types.intern(InternedTyKind::Result {
            ok: Some(ok),
            err: Some(err),
        })
    }

    /// Intern a user-defined type.
    pub fn mk_adt(&mut self, def_id: DefId) -> Ty {
        self.types.intern_adt(def_id)
    }

    /// Intern a function type.
    pub fn mk_func(&mut self, params: Vec<Ty>, ret: Option<Ty>) -> Ty {
        self.types.intern_func(params, ret)
    }

    /// Intern an AST type, resolving named types to their definitions.
    ///
    /// This is the preferred way to convert AST types to interned types,
    /// as it properly resolves named types (records, enums, variants) to
    /// ADT types with their DefId.
    pub fn intern_ast_ty(&mut self, ast_ty: &AstTyKind) -> Ty {
        match ast_ty {
            AstTyKind::Bool => Ty::BOOL,
            AstTyKind::S8 => self.types.intern(InternedTyKind::S8),
            AstTyKind::S16 => self.types.intern(InternedTyKind::S16),
            AstTyKind::S32 => Ty::S32,
            AstTyKind::S64 => self.types.intern(InternedTyKind::S64),
            AstTyKind::U8 => self.types.intern(InternedTyKind::U8),
            AstTyKind::U16 => self.types.intern(InternedTyKind::U16),
            AstTyKind::U32 => self.types.intern(InternedTyKind::U32),
            AstTyKind::U64 => self.types.intern(InternedTyKind::U64),
            AstTyKind::F32 => self.types.intern(InternedTyKind::F32),
            AstTyKind::F64 => self.types.intern(InternedTyKind::F64),
            AstTyKind::Char => self.types.intern(InternedTyKind::Char),
            AstTyKind::String => Ty::STRING,

            AstTyKind::List(inner) => {
                let inner_ty = self.intern_ast_ty(&inner.kind);
                self.types.intern_list(inner_ty)
            }
            AstTyKind::Option(inner) => {
                let inner_ty = self.intern_ast_ty(&inner.kind);
                self.types.intern_option(inner_ty)
            }
            AstTyKind::Result { ok, err } => {
                let ok_ty = ok.as_ref().map(|t| self.intern_ast_ty(&t.kind));
                let err_ty = err.as_ref().map(|t| self.intern_ast_ty(&t.kind));
                self.types.intern(InternedTyKind::Result {
                    ok: ok_ty,
                    err: err_ty,
                })
            }
            AstTyKind::Tuple(elems) => {
                let elem_tys: Vec<_> = elems.iter().map(|t| self.intern_ast_ty(&t.kind)).collect();
                self.types.intern_tuple(elem_tys)
            }

            AstTyKind::Length => self.types.intern(InternedTyKind::Length),
            AstTyKind::PhysicalLength => self.types.intern(InternedTyKind::PhysicalLength),
            AstTyKind::Angle => self.types.intern(InternedTyKind::Angle),
            AstTyKind::Duration => self.types.intern(InternedTyKind::Duration),
            AstTyKind::Percent => self.types.intern(InternedTyKind::Percent),
            AstTyKind::RelativeFontSize => self.types.intern(InternedTyKind::RelativeFontSize),
            AstTyKind::Color => self.types.intern(InternedTyKind::Color),
            AstTyKind::Brush => self.types.intern(InternedTyKind::Brush),
            AstTyKind::Image => self.types.intern(InternedTyKind::Image),
            AstTyKind::Easing => self.types.intern(InternedTyKind::Easing),

            AstTyKind::Func {
                params,
                return_type,
            } => {
                let param_tys: Vec<_> = params
                    .iter()
                    .map(|(_, t)| self.intern_ast_ty(&t.kind))
                    .collect();
                let ret_ty = return_type.as_ref().map(|t| self.intern_ast_ty(&t.kind));
                self.types.intern_func(param_tys, ret_ty)
            }

            AstTyKind::Named(name) => {
                // Look up the named type in definitions
                let name_interned = self.interner.intern(name);
                if let Some(def_id) = self.defs.lookup(name_interned, Namespace::Type) {
                    // Create an ADT type with the definition's DefId
                    self.types.intern_adt(def_id)
                } else {
                    // Type not found - return Unknown and let type checker report error
                    self.types.intern(InternedTyKind::Unknown)
                }
            }

            AstTyKind::Unknown => self.types.intern(InternedTyKind::Unknown),
        }
    }

    // ========================================================================
    // Definition access
    // ========================================================================

    /// Get the type of a definition.
    pub fn type_of(&self, def_id: DefId) -> Option<Ty> {
        self.defs.type_of(def_id)
    }

    /// Look up a type definition by name.
    pub fn lookup_type(&self, name: &str) -> Option<DefId> {
        let n = self.interner.intern(name);
        self.defs.lookup(n, Namespace::Type)
    }

    /// Look up a component by name.
    pub fn lookup_component(&self, name: &str) -> Option<DefId> {
        let n = self.interner.intern(name);
        self.defs.lookup(n, Namespace::Component)
    }

    // ========================================================================
    // DOM imports
    // ========================================================================

    /// Build the module's **import-side boundary contract** and its
    /// **host-import registry** in one pass — the single source of truth for
    /// every function the core module imports and the WIT import interfaces
    /// that declare them.
    ///
    /// Returns `(interfaces, imports)`:
    /// - `interfaces` — one [`LirInterface`] per import interface, with its
    ///   final kebab name, package, owned ADTs (foreign only), and the full
    ///   WIT function surface (`functions`). Three kinds:
    ///   * `{component}-callbacks` — a component's callbacks (receiver
    ///     `Borrow(component)`), one per component that declares any;
    ///   * `{global}` — a local (module-package) global's host-boundary
    ///     members: `set-<prop>` / `on-<prop>-changed` accessors (from
    ///     non-`Inline` property directions) plus its callbacks;
    ///   * a foreign-package global's interface (the built-in `Dom`), whose
    ///     ADTs are owned inline.
    /// - `imports` — the ordered host-import registry: every function the
    ///   *core module* imports (component + global + DOM callbacks — NOT the
    ///   WIT-only property setters/notifiers), in import-index order
    ///   (component callbacks first, then global callbacks). Each references
    ///   its interface by [`InterfaceId`].
    ///
    /// `component_def_ids` supplies the components in resource order so the
    /// import indices are stable and match codegen's component iteration.
    pub fn build_import_contract(
        &self,
        component_def_ids: &[crate::ids::DefId],
    ) -> (
        crate::index_vec::IndexVec<crate::ids::InterfaceId, crate::lir::LirInterface>,
        Vec<crate::lir::LirImport>,
    ) {
        use crate::ids::InterfaceId;
        use crate::lir::{
            InterfaceDirection, LirIfaceFn, LirImport, LirInterface, LirReceiver,
        };
        use crate::naming::to_kebab_case;
        use crate::types::{InternedTyKind, Ty};

        let ctx = self;
        let mut interfaces: crate::index_vec::IndexVec<InterfaceId, LirInterface> =
            crate::index_vec::IndexVec::new();
        let mut imports: Vec<LirImport> = Vec::new();

        // Resolve a callback `DefId` to a `(name, params, result)` signature,
        // skipping non-function defs. Component/global callbacks share this.
        let signature_of = |cb: crate::ids::DefId| -> Option<(Name, Vec<(Name, Ty)>, Option<Ty>)> {
            let f = ctx.defs.as_function(cb)?;
            let mut params = Vec::new();
            for p in &f.params {
                let Some(pty) = ctx.defs.type_of(*p) else {
                    continue;
                };
                params.push((ctx.defs.name(*p), pty));
            }
            let result = if f.ret_ty == Ty::UNIT {
                None
            } else {
                Some(f.ret_ty)
            };
            Some((f.name, params, result))
        };

        // --- Component callbacks: one `{component}-callbacks` interface per
        // component that declares any. Receiver is `Borrow(component)`. These
        // are allocated first so their import indices lead the registry.
        for &comp_id in component_def_ids {
            let Some(comp) = ctx.defs.as_component(comp_id) else {
                continue;
            };
            let comp_name = ctx.str(comp.name).to_string();
            let mut functions = Vec::new();
            let mut seen: rustc_hash::FxHashSet<String> = rustc_hash::FxHashSet::default();
            for &cb in &comp.callbacks {
                let Some((fname, params, result)) = signature_of(cb) else {
                    continue;
                };
                // Defence-in-depth: reject duplicate callback names within a
                // single component (mirrors the old ImportLayout guard).
                if !seen.insert(to_kebab_case(ctx.str(fname).as_ref())) {
                    continue;
                }
                functions.push(LirIfaceFn {
                    name: fname,
                    params,
                    result,
                    receiver: LirReceiver::Borrow(comp_id),
                    def: cb,
                });
            }
            if functions.is_empty() {
                continue;
            }
            let iface_id = interfaces.push(LirInterface {
                name: ctx.intern(&format!("{}-callbacks", to_kebab_case(&comp_name))),
                direction: InterfaceDirection::Import,
                package: None,
                owned_types: Vec::new(),
                resources: vec![comp_id],
                functions: functions.clone(),
            });
            for f in functions {
                imports.push(LirImport {
                    def_id: f.def,
                    name: f.name,
                    interface: iface_id,
                    params: f.params,
                    result: f.result,
                    receiver: f.receiver,
                });
            }
        }

        // --- Globals: one interface per global with host-boundary members.
        // Property setters/notifiers are WIT-only (no core import); callbacks
        // are both WIT functions and core imports. Foreign globals (DOM) own
        // their ADTs inline; local globals `use` shared types.
        for g_id in ctx.defs.globals().collect::<Vec<_>>() {
            let Some(g) = ctx.defs.as_global(g_id) else {
                continue;
            };
            let is_foreign = g.package.is_some();
            let mut functions = Vec::new();
            let mut owned_types: Vec<Ty> = Vec::new();
            let note_adt = |ty: Ty, owned: &mut Vec<Ty>| {
                if is_foreign
                    && matches!(ctx.ty_kind(ty), InternedTyKind::Adt(_))
                    && !owned.contains(&ty)
                {
                    owned.push(ty);
                }
            };

            // Property setters / notifiers (WIT-only, non-`Inline`).
            for (idx, &prop_id) in g.properties.iter().enumerate() {
                let direction = g
                    .property_directions
                    .get(idx)
                    .copied()
                    .unwrap_or(crate::definitions::GlobalPropDirection::Inline);
                if direction == crate::definitions::GlobalPropDirection::Inline {
                    continue;
                }
                let Some(prop_ty) = ctx.defs.type_of(prop_id) else {
                    continue;
                };
                note_adt(prop_ty, &mut owned_types);
                let prop_name = to_kebab_case(ctx.str(ctx.defs.name(prop_id)).as_ref());
                let v = ctx.intern("v");
                use crate::definitions::GlobalPropDirection as D;
                if matches!(direction, D::In | D::InOut) {
                    functions.push(LirIfaceFn {
                        name: ctx.intern(&format!("set-{}", prop_name)),
                        params: vec![(v, prop_ty)],
                        result: None,
                        receiver: LirReceiver::None,
                        def: prop_id,
                    });
                }
                if matches!(direction, D::Out | D::InOut) {
                    functions.push(LirIfaceFn {
                        name: ctx.intern(&format!("on-{}-changed", prop_name)),
                        params: vec![(v, prop_ty)],
                        result: None,
                        receiver: LirReceiver::None,
                        def: prop_id,
                    });
                }
            }

            // Callbacks (WIT function + core import).
            let mut callback_imports: Vec<LirIfaceFn> = Vec::new();
            let mut seen: rustc_hash::FxHashSet<String> = rustc_hash::FxHashSet::default();
            for &cb in &g.callbacks {
                let Some((fname, params, result)) = signature_of(cb) else {
                    continue;
                };
                if !seen.insert(to_kebab_case(ctx.str(fname).as_ref())) {
                    continue;
                }
                for (_, pty) in &params {
                    note_adt(*pty, &mut owned_types);
                }
                if let Some(r) = result {
                    note_adt(r, &mut owned_types);
                }
                let f = LirIfaceFn {
                    name: fname,
                    params,
                    result,
                    receiver: LirReceiver::None,
                    def: cb,
                };
                functions.push(f.clone());
                callback_imports.push(f);
            }

            if functions.is_empty() {
                continue;
            }
            let iface_id = interfaces.push(LirInterface {
                name: g.name,
                direction: InterfaceDirection::Import,
                package: g.package.clone(),
                owned_types,
                resources: Vec::new(),
                functions,
            });
            for f in callback_imports {
                imports.push(LirImport {
                    def_id: f.def,
                    name: f.name,
                    interface: iface_id,
                    params: f.params,
                    result: f.result,
                    receiver: f.receiver,
                });
            }
        }

        (interfaces, imports)
    }

    /// The `to_string` builtin function for converting a value of `ty` to a
    /// string: type-specific helpers for primitives, `object_to_string` for
    /// everything else. Shared by interpolation lowering and the
    /// `set-text-content` / dynamic-text paths so a value is stringified by
    /// a real `Call`, not a bespoke codegen dispatch.
    pub fn to_string_func_for(&self, ty: Ty) -> DefId {
        match self.types.kind(ty) {
            crate::types::InternedTyKind::Bool => self.known.functions.bool_to_string(),
            crate::types::InternedTyKind::S8
            | crate::types::InternedTyKind::S16
            | crate::types::InternedTyKind::S32 => self.known.functions.s32_to_string(),
            crate::types::InternedTyKind::U8
            | crate::types::InternedTyKind::U16
            | crate::types::InternedTyKind::U32 => self.known.functions.u32_to_string(),
            crate::types::InternedTyKind::S64 => self.known.functions.s64_to_string(),
            crate::types::InternedTyKind::U64 => self.known.functions.u64_to_string(),
            crate::types::InternedTyKind::F32 => self.known.functions.f32_to_string(),
            crate::types::InternedTyKind::F64 => self.known.functions.f64_to_string(),
            crate::types::InternedTyKind::Char => self.known.functions.char_to_string(),
            _ => self.known.functions.object_to_string(),
        }
    }

    /// Record the built-in `Dom` global's `DefId`. Called once during
    /// `lookup_known_definitions`. The global's `callbacks` ARE the DOM
    /// functions — the single source of truth — so we store only the
    /// global id, not a separate table.
    pub fn set_dom_global(&mut self, dom_global: DefId) {
        self.dom_global = Some(dom_global);
    }

    /// The DOM functions as a typed view, reconstructed on demand from the
    /// `Dom` global's `callbacks`. DOM is nothing more than that global's
    /// callbacks; `DomImports` is just a typed lens over them. Panics if
    /// called before `lookup_known_definitions` — every pipeline
    /// initialises the stdlib before any LIR/codegen runs, so a missing
    /// `Dom` global here is a programmer error, not a recoverable state.
    pub fn dom_imports(&self) -> DomImports {
        let dom_global = self
            .dom_global
            .expect("Dom global not initialised — call lookup_known_definitions(ctx) first");
        let global = self
            .defs
            .as_global(dom_global)
            .expect("Dom global id does not resolve to a global def");
        DomImports::from_callbacks(&global.callbacks)
    }

    // ========================================================================
    // Diagnostics
    // ========================================================================

    /// Check if there are any errors.
    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }

    /// Get the error count.
    pub fn error_count(&self) -> usize {
        self.diagnostics.error_count()
    }

    /// Render all diagnostics.
    pub fn render_diagnostics(&self) -> String {
        self.diagnostics.render(&self.source_map)
    }

    // ========================================================================
    // Block debug names
    // ========================================================================

    /// Register a structured debug name for a block. Interior
    /// mutability lets lowering passes name blocks while iterating.
    pub fn set_block_name(&self, comp_def_id: DefId, block_id: BlockId, name: BlockDebugName) {
        self.block_names
            .borrow_mut()
            .insert((comp_def_id, block_id), name);
    }

    /// Get the structured debug name for a block. Returns an owned
    /// clone (callers can't borrow into a RefCell across the
    /// inevitable subsequent `borrow_mut`).
    pub fn get_block_name(&self, comp_def_id: DefId, block_id: BlockId) -> Option<BlockDebugName> {
        self.block_names
            .borrow()
            .get(&(comp_def_id, block_id))
            .cloned()
    }

    // ========================================================================
    // Module-wide BlockId allocation
    // ========================================================================

    /// Allocate a fresh, module-wide unique `BlockId`. Lowering passes
    /// route every block creation through this so that `block_func_indices`
    /// can be keyed by `BlockId` alone.
    pub fn alloc_block_id(&self) -> BlockId {
        let id = BlockId(self.block_id_counter.get());
        self.block_id_counter.set(id.0 + 1);
        id
    }

    /// Record a freshly-lowered component's lifecycle BlockIds so
    /// parents that mount it can look up the callee BlockIds at lowering
    /// time. Called from `lower_component` after `synth_internal_*`
    /// passes run.
    pub fn register_component_lifecycle_blocks(
        &self,
        def_id: DefId,
        blocks: ComponentLifecycleBlocks,
    ) {
        self.component_lifecycle_blocks
            .borrow_mut()
            .insert(def_id, blocks);
    }

    /// Look up a previously-registered component's lifecycle BlockIds.
    pub fn lookup_component_lifecycle_blocks(
        &self,
        def_id: DefId,
    ) -> Option<ComponentLifecycleBlocks> {
        self.component_lifecycle_blocks
            .borrow()
            .get(&def_id)
            .copied()
    }

    /// Phase 1.1c-l (#97): register the synthesized fanout block for an
    /// (observing component, global signal) pair. Called from
    /// `synth_global_fanout_blocks` once per (comp, signal) where a
    /// fanout block was emitted.
    pub fn register_global_fanout_block(
        &self,
        observing_comp: DefId,
        global_signal: DefId,
        block_id: BlockId,
    ) {
        self.global_fanout_blocks
            .borrow_mut()
            .insert((observing_comp, global_signal), block_id);
    }

    /// Phase 1.1c-l (#97): look up a previously-registered fanout
    /// BlockId. Returns `None` if the observing component was not yet
    /// lowered, or if its observers did not match the supported "simple
    /// shape" criteria.
    pub fn lookup_global_fanout_block(
        &self,
        observing_comp: DefId,
        global_signal: DefId,
    ) -> Option<BlockId> {
        self.global_fanout_blocks
            .borrow()
            .get(&(observing_comp, global_signal))
            .copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_basic() {
        let ctx = CompilerContext::new();

        let name = ctx.intern("test");
        assert_eq!(ctx.str(name), "test");
    }

    #[test]
    fn test_context_types() {
        let mut ctx = CompilerContext::new();

        let list_s32 = ctx.mk_list(Ty::S32);
        assert!(matches!(
            ctx.ty_kind(list_s32),
            InternedTyKind::List(Ty::S32)
        ));
    }

    #[test]
    fn test_context_shared_interner() {
        let interner = Arc::new(Interner::new());
        let name1 = interner.intern("shared");

        let ctx = CompilerContext::with_interner(interner.clone());
        let name2 = ctx.intern("shared");

        assert_eq!(name1, name2);
    }
}
