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
use crate::definitions::{DefKind, FunctionDef};
use crate::ids::DefId;
use crate::source::Span;
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

/// Register the 19 `meshx-ui/dom` WIT imports as synthetic function
/// DefIds. Called once from `lookup_known_definitions`.
///
/// The synthetic DefIds carry only a name + (placeholder) function
/// signature; they are not type-checked or name-resolved against user
/// code. The point of the entries is to give Phase 2.2 a stable DefId
/// to put on `LirOp::CallFunction { func, .. }`.
pub fn register_dom_imports(ctx: &mut CompilerContext) -> DomImports {
    DomImports {
        create_element: alloc_dom_import(ctx, "create-element"),
        create_text: alloc_dom_import(ctx, "create-text"),
        create_comment: alloc_dom_import(ctx, "create-comment"),
        create_fragment: alloc_dom_import(ctx, "create-fragment"),

        set_attribute: alloc_dom_import(ctx, "set-attribute"),
        remove_attribute: alloc_dom_import(ctx, "remove-attribute"),
        set_text_content: alloc_dom_import(ctx, "set-text-content"),
        set_style: alloc_dom_import(ctx, "set-style"),
        set_class: alloc_dom_import(ctx, "set-class"),

        append_child: alloc_dom_import(ctx, "append-child"),
        insert_before: alloc_dom_import(ctx, "insert-before"),
        insert_after: alloc_dom_import(ctx, "insert-after"),
        remove_child: alloc_dom_import(ctx, "remove-child"),
        remove: alloc_dom_import(ctx, "remove"),

        get_parent: alloc_dom_import(ctx, "get-parent"),
        get_next_sibling: alloc_dom_import(ctx, "get-next-sibling"),

        add_event_listener: alloc_dom_import(ctx, "add-event-listener"),
        remove_event_listener: alloc_dom_import(ctx, "remove-event-listener"),
    }
}

/// Allocate a synthetic `DefKind::Function` for a single dom.wit import.
///
/// The function's params/return are left as a non-meaningful placeholder
/// (`func() -> ()`); codegen does not consult them — it maps the DefId
/// directly to a known wasm import index. Phase 2.2 can fill in real
/// signatures if the lowering needs them for type-checking.
fn alloc_dom_import(ctx: &mut CompilerContext, kebab_name: &str) -> DefId {
    let name = ctx.interner.intern(kebab_name);
    let placeholder_ty = ctx.types.intern(InternedTyKind::Func {
        params: vec![],
        ret: None,
    });
    let def_id = ctx.defs.alloc(
        name,
        DefKind::Function(FunctionDef {
            def_id: DefId::INVALID,
            name,
            params: vec![],
            ret_ty: Ty::ERROR,
            is_export: false,
        }),
        Span::default(),
    );
    if let Some(f) = ctx.defs.as_function_mut(def_id) {
        f.def_id = def_id;
    }
    ctx.defs.set_type(def_id, placeholder_ty);
    def_id
}
