//! DefId table for the `meshx-ui/dom` WIT imports.
//!
//! Phase 2.1 of the LIR-flattening refactor pre-allocates a `DefId` for
//! each function in the `yel:ui/dom@0.1.0` WIT interface (see
//! `crates/yel-host/wit/deps/meshx-ui/dom.wit`). Phase 2.2 will lower
//! the UI-specific `LirOp::CreateElement` / `LirOp::SetAttribute` /
//! ... variants into the generic `LirOp::CallFunction { func, args }`
//! op against these DefIds. Codegen resolves them back to wasm import
//! indices (see `IMPORT_CREATE_ELEMENT` and friends in
//! `yel-wasm-codegen::wasm`).
//!
//! The fields here mirror the dom.wit functions verbatim (kebab → snake).
//! The order of registration is significant: the 1:1 mapping to wasm
//! import indices is owned by the codegen-side round-trip helper, not
//! by this struct.
//!
//! These DefIds are intentionally NOT registered in any name namespace —
//! they are internal entries reachable only via `CompilerContext::dom_imports()`,
//! never by name resolution from user code.

use crate::context::CompilerContext;
use crate::definitions::{DefKind, FunctionDef, GlobalDef, ParameterDef};
use crate::ids::{DefId, ParamIdx};
use crate::source::Span;
use crate::syntax::ast::PackageId;
use crate::types::{InternedTyKind, Ty};

/// DefId table for the `meshx-ui/dom` WIT imports.
///
/// Populated once at `CompilerContext` initialization via
/// [`register_dom_imports`]. Each field is the `DefId` of a synthetic
/// `DefKind::Function` entry whose name matches the kebab-case name in
/// dom.wit. Phase 2.2 will emit `LirOp::CallFunction` against these
/// DefIds; codegen maps them back to wasm import indices.
#[derive(Debug, Clone, Copy)]
pub struct DomImports {
    // Node creation
    pub create_element: DefId,
    pub create_text: DefId,
    pub create_comment: DefId,
    pub create_fragment: DefId,

    // Node manipulation
    pub set_attribute: DefId,
    pub remove_attribute: DefId,
    pub set_text_content: DefId,
    pub set_style: DefId,
    pub set_class: DefId,

    // Tree operations
    pub append_child: DefId,
    pub insert_before: DefId,
    pub insert_after: DefId,
    pub remove_child: DefId,
    pub remove: DefId,

    // Navigation
    pub get_parent: DefId,
    pub get_next_sibling: DefId,

    // Events
    pub add_event_listener: DefId,
    pub remove_event_listener: DefId,
}

impl DomImports {
    /// All 18 DOM import `DefId`s in a single canonical order — the one
    /// source of truth for enumerating them (import-index allocation and
    /// the import-section emission both iterate this, so the order lives
    /// in exactly one place rather than being duplicated).
    pub fn all(&self) -> [DefId; 18] {
        [
            self.create_element,
            self.create_text,
            self.create_comment,
            self.set_attribute,
            self.remove_attribute,
            self.set_text_content,
            self.set_style,
            self.set_class,
            self.append_child,
            self.insert_before,
            self.remove_child,
            self.remove,
            self.get_parent,
            self.get_next_sibling,
            self.add_event_listener,
            self.remove_event_listener,
            self.insert_after,
            self.create_fragment,
        ]
    }

    /// Reconstruct the typed view from the `Dom` global's `callbacks`
    /// slice — the strict inverse of [`all`](Self::all), so the two share
    /// one canonical order. The global's callbacks are the single source
    /// of truth; this is just a typed lens over them, so DOM functions
    /// are nothing more than that global's callbacks.
    pub fn from_callbacks(cb: &[DefId]) -> Self {
        assert_eq!(cb.len(), 18, "Dom global must have 18 callbacks");
        DomImports {
            create_element: cb[0],
            create_text: cb[1],
            create_comment: cb[2],
            set_attribute: cb[3],
            remove_attribute: cb[4],
            set_text_content: cb[5],
            set_style: cb[6],
            set_class: cb[7],
            append_child: cb[8],
            insert_before: cb[9],
            remove_child: cb[10],
            remove: cb[11],
            get_parent: cb[12],
            get_next_sibling: cb[13],
            add_event_listener: cb[14],
            remove_event_listener: cb[15],
            insert_after: cb[16],
            create_fragment: cb[17],
        }
    }
}

/// Register the 19 `meshx-ui/dom` WIT imports as synthetic function
/// DefIds. Called once from `lookup_known_definitions`.
///
/// Each DefId carries the function's **real** signature from
/// `yel:ui/dom@0.1.0` (`node` = `u32`, `attribute-value` = the builtin
/// variant). The signatures' canonical-ABI flattening reproduces the
/// wasm import types; codegen currently still maps each DefId directly to
/// a fixed wasm import index, but the signatures are now the single
/// source of truth (and let DOM eventually flow through the generic
/// global-import path). Requires `register_builtin_variants` to have run
/// first (for `AttributeValue`).
/// Returns the `DefId` of the registered `Dom` global; its `callbacks`
/// are the 18 DOM functions and are the single source of truth (the typed
/// [`DomImports`] view is reconstructed from them on demand via
/// [`DomImports::from_callbacks`]).
pub fn register_dom_imports(ctx: &mut CompilerContext) -> DefId {
    let node = ctx.types.intern(InternedTyKind::U32);
    let string = ctx.types.intern(InternedTyKind::String);
    let unit = Ty::UNIT;
    let attr_value = {
        let avd = ctx.known.variants.attribute_value();
        ctx.types.intern_adt(avd)
    };

    let dom = DomImports {
        create_element: alloc_dom_fn(ctx, "create-element", &[("tag", string)], node),
        create_text: alloc_dom_fn(ctx, "create-text", &[("content", string)], node),
        create_comment: alloc_dom_fn(ctx, "create-comment", &[("content", string)], node),
        create_fragment: alloc_dom_fn(ctx, "create-fragment", &[], node),

        set_attribute: alloc_dom_fn(
            ctx,
            "set-attribute",
            &[("node", node), ("name", string), ("value", attr_value)],
            unit,
        ),
        remove_attribute: alloc_dom_fn(
            ctx,
            "remove-attribute",
            &[("node", node), ("name", string)],
            unit,
        ),
        set_text_content: alloc_dom_fn(
            ctx,
            "set-text-content",
            &[("node", node), ("content", string)],
            unit,
        ),
        set_style: alloc_dom_fn(
            ctx,
            "set-style",
            &[("node", node), ("property", string), ("value", string)],
            unit,
        ),
        set_class: alloc_dom_fn(
            ctx,
            "set-class",
            &[("node", node), ("class-name", string)],
            unit,
        ),

        append_child: alloc_dom_fn(
            ctx,
            "append-child",
            &[("parent", node), ("child", node)],
            unit,
        ),
        insert_before: alloc_dom_fn(
            ctx,
            "insert-before",
            &[("parent", node), ("node", node), ("reference", node)],
            unit,
        ),
        insert_after: alloc_dom_fn(
            ctx,
            "insert-after",
            &[("parent", node), ("node", node), ("anchor", node)],
            unit,
        ),
        remove_child: alloc_dom_fn(
            ctx,
            "remove-child",
            &[("parent", node), ("child", node)],
            unit,
        ),
        remove: alloc_dom_fn(ctx, "remove", &[("node", node)], unit),

        get_parent: alloc_dom_fn(ctx, "get-parent", &[("node", node)], node),
        get_next_sibling: alloc_dom_fn(ctx, "get-next-sibling", &[("node", node)], node),

        add_event_listener: alloc_dom_fn(
            ctx,
            "add-event-listener",
            &[("node", node), ("event", string), ("handler-id", node)],
            unit,
        ),
        remove_event_listener: alloc_dom_fn(
            ctx,
            "remove-event-listener",
            &[("node", node), ("event", string), ("handler-id", node)],
            unit,
        ),
    };

    // Register DOM as a built-in foreign-package global: its 18 functions
    // are the global's callbacks, so DOM imports flow through the same
    // generic global-callback machinery (import emission + type interning)
    // as any user `global`'s host functions — no DOM-specific import path.
    // It lives in the foreign `yel:ui` package, matching the host's
    // `yel:ui/dom@0.1.0` interface. Not name-registered: it is reachable
    // only via `ctx.dom_imports()`, never by user name resolution.
    let dom_name = ctx.interner.intern("Dom");
    let dom_global_id = ctx.defs.alloc(
        dom_name,
        DefKind::Global(GlobalDef {
            def_id: DefId::INVALID,
            name: dom_name,
            is_export: false,
            properties: vec![],
            property_directions: vec![],
            property_defaults: vec![],
            callbacks: dom.all().to_vec(),
            package: Some(PackageId {
                namespace: "yel".to_string(),
                name: "ui".to_string(),
                version: Some("0.1.0".to_string()),
            }),
        }),
        Span::default(),
    );
    if let Some(g) = ctx.defs.as_global_mut(dom_global_id) {
        g.def_id = dom_global_id;
    }

    dom_global_id
}

/// Allocate a `DefKind::Function` for one `dom.wit` import with its real
/// signature: a `ParameterDef` per param (typed) and the declared return
/// type, plus the matching `Func` type. `Ty::UNIT` return means `-> ()`.
fn alloc_dom_fn(
    ctx: &mut CompilerContext,
    kebab_name: &str,
    params: &[(&str, Ty)],
    ret: Ty,
) -> DefId {
    let name = ctx.interner.intern(kebab_name);
    let param_ids: Vec<DefId> = params
        .iter()
        .enumerate()
        .map(|(idx, (pname, pty))| {
            let pname_i = ctx.interner.intern(pname);
            let pid = ctx.defs.alloc(
                pname_i,
                DefKind::Parameter(ParameterDef {
                    owner: DefId::INVALID,
                    name: pname_i,
                    ty: *pty,
                    idx: ParamIdx::new(idx as u32),
                }),
                Span::default(),
            );
            ctx.defs.set_type(pid, *pty);
            pid
        })
        .collect();

    let ret_opt = if ret == Ty::UNIT { None } else { Some(ret) };
    let func_ty = ctx.types.intern(InternedTyKind::Func {
        params: params.iter().map(|(_, t)| *t).collect(),
        ret: ret_opt,
    });

    let def_id = ctx.defs.alloc(
        name,
        DefKind::Function(FunctionDef {
            def_id: DefId::INVALID,
            name,
            params: param_ids.clone(),
            ret_ty: ret,
            is_export: false,
        }),
        Span::default(),
    );
    for pid in &param_ids {
        if let DefKind::Parameter(p) = &mut ctx.defs.get_mut(*pid).kind {
            p.owner = def_id;
        }
    }
    if let Some(f) = ctx.defs.as_function_mut(def_id) {
        f.def_id = def_id;
    }
    ctx.defs.set_type(def_id, func_ty);
    def_id
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::stdlib_lookup::lookup_known_definitions;

    /// The `Dom` global's `callbacks` are the single source of truth, and
    /// the typed view is derived from them. `from_callbacks` is the strict
    /// inverse of `all`, so the canonical order must agree in both places;
    /// guard that here (the two are hand-written in separate spots).
    #[test]
    fn dom_view_round_trips_through_the_global_callbacks() {
        let mut ctx = CompilerContext::new();
        lookup_known_definitions(&mut ctx);
        // `ctx.dom_imports()` is reconstructed from the global's callbacks;
        // re-flattening it with `all()` must reproduce that exact slice.
        let view = ctx.dom_imports();
        assert_eq!(
            view.all().to_vec(),
            DomImports::from_callbacks(&view.all()).all().to_vec(),
            "all() and from_callbacks() disagree on the canonical DOM order"
        );
    }

    /// The DOM imports must carry their real `yel:ui/dom` signatures
    /// (`node` = `u32`, value = the `AttributeValue` variant). This is the
    /// frontend's contract; the canonical-ABI flattening of these types is
    /// the back-end's concern and lives there.
    #[test]
    fn dom_imports_carry_real_signatures() {
        let mut ctx = CompilerContext::new();
        lookup_known_definitions(&mut ctx);
        let dom = ctx.dom_imports().clone();

        let sig = |def_id: DefId| -> (Vec<InternedTyKind>, InternedTyKind) {
            let f = ctx
                .defs
                .as_function(def_id)
                .expect("dom import is a function");
            let params = f
                .params
                .iter()
                .map(|p| {
                    ctx.ty_kind(ctx.defs.type_of(*p).expect("param type"))
                        .clone()
                })
                .collect();
            (params, ctx.ty_kind(f.ret_ty).clone())
        };

        // create-element: (string) -> u32 (node)
        assert_eq!(
            sig(dom.create_element),
            (vec![InternedTyKind::String], InternedTyKind::U32)
        );
        // create-fragment: () -> u32
        assert_eq!(sig(dom.create_fragment), (vec![], InternedTyKind::U32));
        // append-child: (u32, u32) -> ()
        assert_eq!(
            sig(dom.append_child),
            (
                vec![InternedTyKind::U32, InternedTyKind::U32],
                InternedTyKind::Unit
            )
        );
        // set-attribute: (u32, string, attribute-value) -> ()
        let (params, ret) = sig(dom.set_attribute);
        assert_eq!(ret, InternedTyKind::Unit);
        assert_eq!(params.len(), 3);
        assert_eq!(params[0], InternedTyKind::U32);
        assert_eq!(params[1], InternedTyKind::String);
        assert!(
            matches!(params[2], InternedTyKind::Adt(d) if d == ctx.known.variants.attribute_value()),
            "set-attribute value param must be the AttributeValue variant"
        );
    }

    /// The `AttributeValue` variant mirrors `yel:ui/dom`'s 14 cases.
    #[test]
    fn attribute_value_variant_has_14_cases() {
        let mut ctx = CompilerContext::new();
        lookup_known_definitions(&mut ctx);
        let av = ctx.known.variants.attribute_value();
        let var = ctx
            .defs
            .as_variant(av)
            .expect("AttributeValue is a variant");
        assert_eq!(var.cases.len(), 14);
    }
}
