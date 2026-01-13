//! Standard library lookup and registration.
//!
//! This module registers builtin elements and types into the compiler's
//! definition tables during initialization.

use crate::context::CompilerContext;
use crate::definitions::{ComponentDef, DefKind, EnumDef, FieldDef, FunctionDef, Namespace, VariantCaseDef, VariantDef};
use crate::ids::{DefId, FieldIdx, VariantIdx};
use crate::source::Span;
use crate::types::{InternedTyKind, Ty};

/// Register all known fundamental types and elements.
pub fn lookup_known_definitions(ctx: &mut CompilerContext) {
    // Register builtin generic types first (option, result)
    register_builtin_types(ctx);

    // Register builtin enums (elements reference them)
    register_builtin_enums(ctx);

    // Register builtin variants
    register_builtin_variants(ctx);

    // Register builtin elements
    register_builtin_elements(ctx);

    // Register builtin functions
    register_builtin_functions(ctx);
}

fn register_builtin_types(ctx: &mut CompilerContext) {
    // Register option variant: option { some(T), none }
    // Note: This is a template variant - actual types are option<T>
    let option_name = ctx.interner.intern("option");
    let option_def_id = ctx.defs.alloc(
        option_name,
        DefKind::Variant(VariantDef {
            def_id: DefId::INVALID,
            name: option_name,
            cases: vec![],
        }),
        dummy_span(),
    );
    if let Some(v) = ctx.defs.as_variant_mut(option_def_id) {
        v.def_id = option_def_id;
    }

    // Register option::some case
    let some_name = ctx.interner.intern("some");
    let some_def_id = ctx.defs.alloc(
        some_name,
        DefKind::VariantCase(VariantCaseDef {
            owner: option_def_id,
            name: some_name,
            payload: Some(Ty::ERROR), // Generic placeholder
            idx: VariantIdx::new(0),
        }),
        dummy_span(),
    );

    // Register option::none case
    let none_name = ctx.interner.intern("none");
    let none_def_id = ctx.defs.alloc(
        none_name,
        DefKind::VariantCase(VariantCaseDef {
            owner: option_def_id,
            name: none_name,
            payload: None,
            idx: VariantIdx::new(1),
        }),
        dummy_span(),
    );

    // Update option with its cases
    if let Some(v) = ctx.defs.as_variant_mut(option_def_id) {
        v.cases = vec![some_def_id, none_def_id];
    }

    ctx.known.builtin_types.option = Some(option_def_id);
    ctx.known.builtin_types.option_some = Some(some_def_id);
    ctx.known.builtin_types.option_none = Some(none_def_id);

    // Register result variant: result { ok(T), err(E) }
    let result_name = ctx.interner.intern("result");
    let result_def_id = ctx.defs.alloc(
        result_name,
        DefKind::Variant(VariantDef {
            def_id: DefId::INVALID,
            name: result_name,
            cases: vec![],
        }),
        dummy_span(),
    );
    if let Some(v) = ctx.defs.as_variant_mut(result_def_id) {
        v.def_id = result_def_id;
    }

    // Register result::ok case
    let ok_name = ctx.interner.intern("ok");
    let ok_def_id = ctx.defs.alloc(
        ok_name,
        DefKind::VariantCase(VariantCaseDef {
            owner: result_def_id,
            name: ok_name,
            payload: Some(Ty::ERROR), // Generic placeholder
            idx: VariantIdx::new(0),
        }),
        dummy_span(),
    );

    // Register result::err case
    let err_name = ctx.interner.intern("err");
    let err_def_id = ctx.defs.alloc(
        err_name,
        DefKind::VariantCase(VariantCaseDef {
            owner: result_def_id,
            name: err_name,
            payload: Some(Ty::ERROR), // Generic placeholder
            idx: VariantIdx::new(1),
        }),
        dummy_span(),
    );

    // Update result with its cases
    if let Some(v) = ctx.defs.as_variant_mut(result_def_id) {
        v.cases = vec![ok_def_id, err_def_id];
    }

    ctx.known.builtin_types.result = Some(result_def_id);
    ctx.known.builtin_types.result_ok = Some(ok_def_id);
    ctx.known.builtin_types.result_err = Some(err_def_id);
}

fn register_builtin_enums(ctx: &mut CompilerContext) {
    ctx.known.enums.direction = Some(register_enum(ctx, "Direction", &["column", "row"]));

    ctx.known.enums.button_variant = Some(register_enum(
        ctx,
        "ButtonVariant",
        &["default", "destructive", "outline", "secondary", "ghost", "link"],
    ));

    ctx.known.enums.align = Some(register_enum(
        ctx,
        "Align",
        &["start", "center", "end", "stretch", "baseline"],
    ));

    ctx.known.enums.justify = Some(register_enum(
        ctx,
        "Justify",
        &["start", "center", "end", "space-between", "space-around", "space-evenly"],
    ));

    ctx.known.enums.weight = Some(register_enum(
        ctx,
        "Weight",
        &["thin", "extra-light", "light", "normal", "medium", "semi-bold", "bold", "extra-bold", "black"],
    ));
}

fn register_builtin_variants(ctx: &mut CompilerContext) {
    // Color: red, green, blue, white, black, transparent, rgba(tuple<u8, u8, u8, u8>)
    let u8_ty = ctx.types.intern(InternedTyKind::U8);
    let rgba_ty = ctx.types.intern_tuple(vec![u8_ty, u8_ty, u8_ty, u8_ty]);
    ctx.known.variants.color = Some(register_variant(
        ctx,
        "Color",
        &[
            ("red", None),
            ("green", None),
            ("blue", None),
            ("white", None),
            ("black", None),
            ("transparent", None),
            ("rgba", Some(rgba_ty)),
        ],
    ));

    // Brush: color(Color), gradient, image
    let color_ty = ctx.types.intern_adt(ctx.known.variants.color.unwrap());
    ctx.known.variants.brush = Some(register_variant(
        ctx,
        "Brush",
        &[
            ("color", Some(color_ty)),
            ("gradient", None),
            ("image", None),
        ],
    ));
}

fn register_builtin_functions(ctx: &mut CompilerContext) {
    // concat: func(string...) -> string
    // Note: Variadic, but we model it as taking multiple string args
    ctx.known.functions.concat = Some(register_function(
        ctx,
        "concat",
        &[], // Variadic - accepts any number of string args
        Ty::STRING,
    ));

    // Type-specific to-string conversions
    // These are used by interpolation lowering to convert values to strings

    // bool_to_string: func(bool) -> string
    ctx.known.functions.bool_to_string = Some(register_function(
        ctx,
        "bool-to-string",
        &[Ty::BOOL],
        Ty::STRING,
    ));

    // s32_to_string: func(s32) -> string
    ctx.known.functions.s32_to_string = Some(register_function(
        ctx,
        "s32-to-string",
        &[Ty::S32],
        Ty::STRING,
    ));

    // u32_to_string: func(u32) -> string
    let u32_ty = ctx.types.intern(InternedTyKind::U32);
    ctx.known.functions.u32_to_string = Some(register_function(
        ctx,
        "u32-to-string",
        &[u32_ty],
        Ty::STRING,
    ));

    // f32_to_string: func(f32) -> string
    let f32_ty = ctx.types.intern(InternedTyKind::F32);
    ctx.known.functions.f32_to_string = Some(register_function(
        ctx,
        "f32-to-string",
        &[f32_ty],
        Ty::STRING,
    ));

    // f64_to_string: func(f64) -> string
    let f64_ty = ctx.types.intern(InternedTyKind::F64);
    ctx.known.functions.f64_to_string = Some(register_function(
        ctx,
        "f64-to-string",
        &[f64_ty],
        Ty::STRING,
    ));

    // char_to_string: func(char) -> string
    let char_ty = ctx.types.intern(InternedTyKind::Char);
    ctx.known.functions.char_to_string = Some(register_function(
        ctx,
        "char-to-string",
        &[char_ty],
        Ty::STRING,
    ));

    // object_to_string: func(any) -> string
    // Generic fallback for complex types (records, variants, lists, etc.)
    // Returns JS-like representation: "[object]", "[Array]", etc.
    ctx.known.functions.object_to_string = Some(register_function(
        ctx,
        "object-to-string",
        &[Ty::ERROR], // any -> we use ERROR as placeholder
        Ty::STRING,
    ));

    // len: func(list<T> | string) -> s32
    // Note: Polymorphic over list element type
    ctx.known.functions.len = Some(register_function(
        ctx,
        "len",
        &[Ty::ERROR], // list<T> | string - polymorphic
        Ty::S32,
    ));

    // list_get: func(list<T>, s32) -> option<T>
    // Safe element access that returns option::none on out-of-bounds.
    // Note: The actual return type is polymorphic - codegen uses the list's element type.
    ctx.known.functions.list_get = Some(register_function(
        ctx,
        "list-get",
        &[Ty::ERROR, Ty::S32], // list<T>, index - polymorphic over T
        Ty::ERROR,             // option<T> - polymorphic
    ));
}

fn register_builtin_elements(ctx: &mut CompilerContext) {
    // Layout elements
    ctx.known.elements.vstack = Some(register_stack_element(ctx, "VStack"));
    ctx.known.elements.hstack = Some(register_stack_element(ctx, "HStack"));
    ctx.known.elements.zstack = Some(register_zstack_element(ctx, "ZStack"));

    // Container elements
    ctx.known.elements.list = Some(register_list_element(ctx));
    ctx.known.elements.scroll_view = Some(register_scroll_view_element(ctx));
    ctx.known.elements.div = Some(register_box_element(ctx, "div"));
    ctx.known.elements.r#box = Some(register_box_element(ctx, "Box"));

    // Text elements
    ctx.known.elements.text = Some(register_text_element(ctx));

    // Input elements
    ctx.known.elements.button = Some(register_button_element(ctx));
    ctx.known.elements.text_field = Some(register_text_field_element(ctx));
    ctx.known.elements.checkbox = Some(register_checkbox_element(ctx));

    // Media elements
    ctx.known.elements.image = Some(register_image_element(ctx));

    // Utility elements
    ctx.known.elements.spacer = Some(register_simple_element(ctx, "Spacer", &layout_props()));
    ctx.known.elements.divider = Some(register_divider_element(ctx));
    ctx.known.elements.badge = Some(register_badge_element(ctx));
    ctx.known.elements.fragment = Some(register_simple_element(ctx, "Fragment", &[]));
    ctx.known.elements.portal = Some(register_portal_element(ctx));
    ctx.known.elements.group = Some(register_simple_element(ctx, "Group", &[]));
}

// ============================================================================
// Helper functions
// ============================================================================

fn dummy_span() -> Span {
    Span::default()
}

/// Register an enum with cases.
fn register_enum(ctx: &mut CompilerContext, name: &str, cases: &[&str]) -> DefId {
    let name_interned = ctx.interner.intern(name);

    let enum_def_id = ctx.defs.alloc(
        name_interned,
        DefKind::Enum(EnumDef {
            def_id: DefId::INVALID, // Will be updated
            name: name_interned,
            cases: vec![],
        }),
        dummy_span(),
    );

    // Register enum cases
    let mut case_ids = Vec::new();
    for (idx, case_name) in cases.iter().enumerate() {
        let case_name_interned = ctx.interner.intern(case_name);
        let case_id = ctx.defs.alloc(
            case_name_interned,
            DefKind::VariantCase(crate::definitions::VariantCaseDef {
                owner: enum_def_id,
                name: case_name_interned,
                payload: None,
                idx: crate::ids::VariantIdx::new(idx as u32),
            }),
            dummy_span(),
        );
        case_ids.push(case_id);
    }

    // Update enum with case IDs
    if let Some(e) = ctx.defs.as_enum_mut(enum_def_id) {
        e.def_id = enum_def_id;
        e.cases = case_ids;
    }

    // Register in namespace
    ctx.defs.register_name(name_interned, Namespace::Type, enum_def_id);

    // Create type for this enum
    let enum_ty = ctx.types.intern_adt(enum_def_id);
    ctx.defs.set_type(enum_def_id, enum_ty);

    enum_def_id
}

/// Register a variant with cases (some may have payloads).
fn register_variant(ctx: &mut CompilerContext, name: &str, cases: &[(&str, Option<Ty>)]) -> DefId {
    let name_interned = ctx.interner.intern(name);

    let variant_def_id = ctx.defs.alloc(
        name_interned,
        DefKind::Variant(VariantDef {
            def_id: DefId::INVALID,
            name: name_interned,
            cases: vec![],
        }),
        dummy_span(),
    );

    // Register variant cases
    let mut case_ids = Vec::new();
    for (idx, (case_name, payload)) in cases.iter().enumerate() {
        let case_name_interned = ctx.interner.intern(case_name);
        let case_id = ctx.defs.alloc(
            case_name_interned,
            DefKind::VariantCase(VariantCaseDef {
                owner: variant_def_id,
                name: case_name_interned,
                payload: *payload,
                idx: VariantIdx::new(idx as u32),
            }),
            dummy_span(),
        );
        case_ids.push(case_id);
    }

    // Update variant with case IDs
    if let Some(v) = ctx.defs.as_variant_mut(variant_def_id) {
        v.def_id = variant_def_id;
        v.cases = case_ids;
    }

    // Register in namespace
    ctx.defs.register_name(name_interned, Namespace::Type, variant_def_id);

    // Create type for this variant
    let variant_ty = ctx.types.intern_adt(variant_def_id);
    ctx.defs.set_type(variant_def_id, variant_ty);

    variant_def_id
}

/// Register a function.
fn register_function(ctx: &mut CompilerContext, name: &str, params: &[Ty], ret_ty: Ty) -> DefId {
    let name_interned = ctx.interner.intern(name);

    // Create function type
    let func_ty = ctx.types.intern(InternedTyKind::Func {
        params: params.to_vec(),
        ret: Some(ret_ty),
    });

    let func_def_id = ctx.defs.alloc(
        name_interned,
        DefKind::Function(FunctionDef {
            def_id: DefId::INVALID,
            name: name_interned,
            params: vec![], // Builtin functions don't have param DefIds
            ret_ty,
            is_export: false,
        }),
        dummy_span(),
    );

    // Update def_id
    if let Some(f) = ctx.defs.as_function_mut(func_def_id) {
        f.def_id = func_def_id;
    }

    // Register in namespace
    ctx.defs.register_name(name_interned, Namespace::Value, func_def_id);
    ctx.defs.set_type(func_def_id, func_ty);

    func_def_id
}

/// Property descriptor for element registration.
struct PropDesc {
    name: &'static str,
    ty: PropType,
}

enum PropType {
    Length,
    Color,
    Brush,
    Bool,
    String,
    S32,
    F32,
    Image,
    Named(&'static str),
}

impl PropType {
    fn to_ty(&self, ctx: &mut CompilerContext) -> Ty {
        match self {
            PropType::Length => ctx.types.intern(InternedTyKind::Length),
            PropType::Color => ctx.types.intern(InternedTyKind::Color),
            PropType::Brush => ctx.types.intern(InternedTyKind::Brush),
            PropType::Bool => Ty::BOOL,
            PropType::String => Ty::STRING,
            PropType::S32 => Ty::S32,
            PropType::F32 => ctx.types.intern(InternedTyKind::F32),
            PropType::Image => ctx.types.intern(InternedTyKind::Image),
            PropType::Named(name) => {
                let name_interned = ctx.interner.intern(name);
                if let Some(def_id) = ctx.defs.lookup(name_interned, Namespace::Type) {
                    ctx.types.intern_adt(def_id)
                } else {
                    Ty::ERROR
                }
            }
        }
    }
}

/// Common layout properties.
fn layout_props() -> Vec<PropDesc> {
    vec![
        PropDesc { name: "width", ty: PropType::Length },
        PropDesc { name: "height", ty: PropType::Length },
        PropDesc { name: "min-width", ty: PropType::Length },
        PropDesc { name: "min-height", ty: PropType::Length },
        PropDesc { name: "max-width", ty: PropType::Length },
        PropDesc { name: "max-height", ty: PropType::Length },
        PropDesc { name: "padding", ty: PropType::Length },
        PropDesc { name: "margin", ty: PropType::Length },
        PropDesc { name: "visible", ty: PropType::Bool },
        PropDesc { name: "opacity", ty: PropType::F32 },
    ]
}

/// Common style properties.
fn style_props() -> Vec<PropDesc> {
    vec![
        PropDesc { name: "background", ty: PropType::Brush },
        PropDesc { name: "border-color", ty: PropType::Color },
        PropDesc { name: "border-width", ty: PropType::Length },
        PropDesc { name: "corner-radius", ty: PropType::Length },
    ]
}

/// Register an element with properties.
fn register_element(ctx: &mut CompilerContext, name: &str, props: &[PropDesc]) -> DefId {
    let name_interned = ctx.interner.intern(name);

    let comp_def_id = ctx.defs.alloc(
        name_interned,
        DefKind::Component(ComponentDef {
            def_id: DefId::INVALID,
            name: name_interned,
            properties: vec![],
            callbacks: vec![],
            is_export: false,
        }),
        dummy_span(),
    );

    // Register properties
    let mut prop_ids = Vec::new();
    for (idx, prop) in props.iter().enumerate() {
        let prop_name = ctx.interner.intern(prop.name);
        let prop_ty = prop.ty.to_ty(ctx);

        let prop_id = ctx.defs.alloc(
            prop_name,
            DefKind::Field(FieldDef {
                owner: comp_def_id,
                name: prop_name,
                ty: prop_ty,
                idx: FieldIdx::new(idx as u32),
            }),
            dummy_span(),
        );
        ctx.defs.set_type(prop_id, prop_ty);
        prop_ids.push(prop_id);
    }

    // Update component with property IDs
    if let Some(comp) = ctx.defs.as_component_mut(comp_def_id) {
        comp.def_id = comp_def_id;
        comp.properties = prop_ids;
    }

    // Register in namespace
    ctx.defs.register_name(name_interned, Namespace::Component, comp_def_id);

    comp_def_id
}

fn register_simple_element(ctx: &mut CompilerContext, name: &str, extra_props: &[PropDesc]) -> DefId {
    let mut props = layout_props();
    props.extend(extra_props.iter().map(|p| PropDesc { name: p.name, ty: p.ty.clone() }));
    register_element(ctx, name, &props)
}

// ============================================================================
// Element-specific registrations
// ============================================================================

fn register_stack_element(ctx: &mut CompilerContext, name: &str) -> DefId {
    let mut props = vec![
        PropDesc { name: "spacing", ty: PropType::Length },
        PropDesc { name: "gap", ty: PropType::Length },
        PropDesc { name: "align", ty: PropType::Named("Align") },
        PropDesc { name: "justify", ty: PropType::Named("Justify") },
    ];
    props.extend(layout_props());
    props.extend(style_props());
    register_element(ctx, name, &props)
}

fn register_zstack_element(ctx: &mut CompilerContext, name: &str) -> DefId {
    let mut props = vec![
        PropDesc { name: "align", ty: PropType::Named("Align") },
    ];
    props.extend(layout_props());
    props.extend(style_props());
    register_element(ctx, name, &props)
}

fn register_list_element(ctx: &mut CompilerContext) -> DefId {
    let mut props = vec![
        PropDesc { name: "direction", ty: PropType::Named("Direction") },
        PropDesc { name: "divide", ty: PropType::String },
        PropDesc { name: "spacing", ty: PropType::Length },
    ];
    props.extend(layout_props());
    props.extend(style_props());
    register_element(ctx, "List", &props)
}

fn register_scroll_view_element(ctx: &mut CompilerContext) -> DefId {
    let mut props = vec![
        PropDesc { name: "direction", ty: PropType::String },
    ];
    props.extend(layout_props());
    register_element(ctx, "ScrollView", &props)
}

fn register_box_element(ctx: &mut CompilerContext, name: &str) -> DefId {
    let mut props = layout_props();
    props.extend(style_props());
    register_element(ctx, name, &props)
}

fn register_text_element(ctx: &mut CompilerContext) -> DefId {
    let mut props = vec![
        PropDesc { name: "content", ty: PropType::String },
        PropDesc { name: "text", ty: PropType::String },
        PropDesc { name: "line-clamp", ty: PropType::S32 },
        PropDesc { name: "color", ty: PropType::Color },
        PropDesc { name: "font-size", ty: PropType::Length },
        PropDesc { name: "font-weight", ty: PropType::Named("Weight") },
        PropDesc { name: "font-family", ty: PropType::String },
    ];
    props.extend(layout_props());
    register_element(ctx, "Text", &props)
}

fn register_button_element(ctx: &mut CompilerContext) -> DefId {
    let mut props = vec![
        PropDesc { name: "label", ty: PropType::String },
        PropDesc { name: "variant", ty: PropType::Named("ButtonVariant") },
        PropDesc { name: "disabled", ty: PropType::Bool },
    ];
    props.extend(layout_props());
    props.extend(style_props());
    // TODO: Add clicked callback
    register_element(ctx, "Button", &props)
}

fn register_text_field_element(ctx: &mut CompilerContext) -> DefId {
    let mut props = vec![
        PropDesc { name: "value", ty: PropType::String },
        PropDesc { name: "placeholder", ty: PropType::String },
        PropDesc { name: "disabled", ty: PropType::Bool },
    ];
    props.extend(layout_props());
    // TODO: Add changed, submitted callbacks
    register_element(ctx, "TextField", &props)
}

fn register_checkbox_element(ctx: &mut CompilerContext) -> DefId {
    let mut props = vec![
        PropDesc { name: "checked", ty: PropType::Bool },
        PropDesc { name: "label", ty: PropType::String },
        PropDesc { name: "disabled", ty: PropType::Bool },
    ];
    props.extend(layout_props());
    // TODO: Add toggled callback
    register_element(ctx, "Checkbox", &props)
}

fn register_image_element(ctx: &mut CompilerContext) -> DefId {
    let mut props = vec![
        PropDesc { name: "source", ty: PropType::Image },
        PropDesc { name: "src", ty: PropType::String },
        PropDesc { name: "alt", ty: PropType::String },
        PropDesc { name: "fit", ty: PropType::String },
    ];
    props.extend(layout_props());
    register_element(ctx, "Image", &props)
}

fn register_divider_element(ctx: &mut CompilerContext) -> DefId {
    let mut props = vec![
        PropDesc { name: "orientation", ty: PropType::String },
    ];
    props.extend(layout_props());
    props.extend(style_props());
    register_element(ctx, "Divider", &props)
}

fn register_badge_element(ctx: &mut CompilerContext) -> DefId {
    let mut props = vec![
        PropDesc { name: "content", ty: PropType::String },
        PropDesc { name: "text", ty: PropType::String },
        PropDesc { name: "variant", ty: PropType::String },
    ];
    props.extend(layout_props());
    props.extend(style_props());
    register_element(ctx, "Badge", &props)
}

fn register_portal_element(ctx: &mut CompilerContext) -> DefId {
    let props = vec![
        PropDesc { name: "target", ty: PropType::String },
    ];
    register_element(ctx, "Portal", &props)
}

// Clone implementation for PropType
impl Clone for PropType {
    fn clone(&self) -> Self {
        match self {
            PropType::Length => PropType::Length,
            PropType::Color => PropType::Color,
            PropType::Brush => PropType::Brush,
            PropType::Bool => PropType::Bool,
            PropType::String => PropType::String,
            PropType::S32 => PropType::S32,
            PropType::F32 => PropType::F32,
            PropType::Image => PropType::Image,
            PropType::Named(s) => PropType::Named(s),
        }
    }
}
