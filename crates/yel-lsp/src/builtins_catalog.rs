//! Built-in elements, enums, and variants — kept in sync with `yel_core::stdlib_lookup`.
//! Used for completions and hovers without duplicating the compiler driver at request time.

use std::sync::LazyLock;

use yel_core::syntax::ast::TyKind;

/// One property on a built-in component.
#[derive(Debug, Clone)]
pub struct BuiltinProperty {
    pub name: &'static str,
    pub ty: TyKind,
}

/// Built-in UI component (kebab / PascalCase names as in source).
#[derive(Debug, Clone)]
pub struct BuiltinElement {
    pub name: &'static str,
    pub properties: Vec<BuiltinProperty>,
    pub functions: Vec<BuiltinFunction>,
    pub accepts_children: bool,
}

/// Callback slot on a built-in (reserved for future parity with compiler).
#[derive(Debug, Clone)]
pub struct BuiltinFunction {
    pub name: &'static str,
}

#[derive(Debug, Clone)]
pub struct BuiltinEnum {
    pub name: &'static str,
    pub cases: &'static [&'static str],
}

#[derive(Debug, Clone)]
pub struct BuiltinVariantCase {
    pub name: String,
    pub payload: Option<TyKind>,
}

#[derive(Debug, Clone)]
pub struct BuiltinVariant {
    pub name: String,
    pub cases: Vec<BuiltinVariantCase>,
}

fn layout_props() -> Vec<BuiltinProperty> {
    vec![
        mp("width", TyKind::Length),
        mp("height", TyKind::Length),
        mp("min-width", TyKind::Length),
        mp("min-height", TyKind::Length),
        mp("max-width", TyKind::Length),
        mp("max-height", TyKind::Length),
        mp("padding", TyKind::Length),
        mp("margin", TyKind::Length),
        mp("visible", TyKind::Bool),
        mp("opacity", TyKind::F32),
    ]
}

fn style_props() -> Vec<BuiltinProperty> {
    vec![
        mp("background", TyKind::Brush),
        mp("border-color", TyKind::Color),
        mp("border-width", TyKind::Length),
        mp("corner-radius", TyKind::Length),
    ]
}

fn mp(name: &'static str, ty: TyKind) -> BuiltinProperty {
    BuiltinProperty { name, ty }
}

fn named(s: &str) -> TyKind {
    TyKind::Named(s.to_string())
}

fn stack(name: &'static str) -> BuiltinElement {
    let mut props = vec![
        mp("spacing", TyKind::Length),
        mp("gap", TyKind::Length),
        mp("align", named("Align")),
        mp("justify", named("Justify")),
    ];
    props.extend(layout_props());
    props.extend(style_props());
    BuiltinElement {
        name,
        properties: props,
        functions: vec![],
        accepts_children: true,
    }
}

fn zstack() -> BuiltinElement {
    let mut props = vec![mp("align", named("Align"))];
    props.extend(layout_props());
    props.extend(style_props());
    BuiltinElement {
        name: "ZStack",
        properties: props,
        functions: vec![],
        accepts_children: true,
    }
}

fn list_el() -> BuiltinElement {
    let mut props = vec![
        mp("direction", named("Direction")),
        mp("divide", TyKind::String),
        mp("spacing", TyKind::Length),
    ];
    props.extend(layout_props());
    props.extend(style_props());
    BuiltinElement {
        name: "List",
        properties: props,
        functions: vec![],
        accepts_children: true,
    }
}

fn scroll_view() -> BuiltinElement {
    let mut props = vec![mp("direction", TyKind::String)];
    props.extend(layout_props());
    BuiltinElement {
        name: "ScrollView",
        properties: props,
        functions: vec![],
        accepts_children: true,
    }
}

fn box_el() -> BuiltinElement {
    let mut props = layout_props();
    props.extend(style_props());
    BuiltinElement {
        name: "Box",
        properties: props,
        functions: vec![],
        accepts_children: true,
    }
}

fn text_el() -> BuiltinElement {
    let mut props = vec![
        mp("content", TyKind::String),
        mp("text", TyKind::String),
        mp("line-clamp", TyKind::S32),
        mp("color", TyKind::Color),
        mp("font-size", TyKind::Length),
        mp("font-weight", named("Weight")),
        mp("font-family", TyKind::String),
    ];
    props.extend(layout_props());
    BuiltinElement {
        name: "Text",
        properties: props,
        functions: vec![],
        accepts_children: false,
    }
}

fn button() -> BuiltinElement {
    let mut props = vec![
        mp("label", TyKind::String),
        mp("variant", named("ButtonVariant")),
        mp("disabled", TyKind::Bool),
    ];
    props.extend(layout_props());
    props.extend(style_props());
    BuiltinElement {
        name: "Button",
        properties: props,
        functions: vec![],
        accepts_children: false,
    }
}

fn text_field() -> BuiltinElement {
    let mut props = vec![
        mp("value", TyKind::String),
        mp("placeholder", TyKind::String),
        mp("disabled", TyKind::Bool),
    ];
    props.extend(layout_props());
    BuiltinElement {
        name: "TextField",
        properties: props,
        functions: vec![],
        accepts_children: false,
    }
}

fn text_input() -> BuiltinElement {
    let mut props = vec![
        mp("value", TyKind::String),
        mp("placeholder", TyKind::String),
        mp("disabled", TyKind::Bool),
    ];
    props.extend(layout_props());
    BuiltinElement {
        name: "TextInput",
        properties: props,
        functions: vec![],
        accepts_children: false,
    }
}

fn integer_input() -> BuiltinElement {
    let mut props = vec![
        mp("value", TyKind::S32),
        mp("placeholder", TyKind::String),
        mp("disabled", TyKind::Bool),
    ];
    props.extend(layout_props());
    BuiltinElement {
        name: "IntegerInput",
        properties: props,
        functions: vec![],
        accepts_children: false,
    }
}

fn float_input() -> BuiltinElement {
    let mut props = vec![
        mp("value", TyKind::F32),
        mp("placeholder", TyKind::String),
        mp("disabled", TyKind::Bool),
    ];
    props.extend(layout_props());
    BuiltinElement {
        name: "FloatInput",
        properties: props,
        functions: vec![],
        accepts_children: false,
    }
}

fn checkbox() -> BuiltinElement {
    let mut props = vec![
        mp("checked", TyKind::Bool),
        mp("label", TyKind::String),
        mp("disabled", TyKind::Bool),
    ];
    props.extend(layout_props());
    BuiltinElement {
        name: "Checkbox",
        properties: props,
        functions: vec![],
        accepts_children: false,
    }
}

fn select_el() -> BuiltinElement {
    let mut props = vec![
        mp("value", TyKind::S32),
        mp("size", TyKind::S32),
        mp("disabled", TyKind::Bool),
    ];
    props.extend(layout_props());
    BuiltinElement {
        name: "Select",
        properties: props,
        functions: vec![],
        accepts_children: false,
    }
}

fn option_el() -> BuiltinElement {
    BuiltinElement {
        name: "Option",
        properties: vec![mp("value", TyKind::S32), mp("disabled", TyKind::Bool)],
        functions: vec![],
        accepts_children: false,
    }
}

fn image_el() -> BuiltinElement {
    let mut props = vec![
        mp("source", TyKind::Image),
        mp("src", TyKind::String),
        mp("alt", TyKind::String),
        mp("fit", TyKind::String),
    ];
    props.extend(layout_props());
    BuiltinElement {
        name: "Image",
        properties: props,
        functions: vec![],
        accepts_children: false,
    }
}

fn spacer() -> BuiltinElement {
    BuiltinElement {
        name: "Spacer",
        properties: layout_props(),
        functions: vec![],
        accepts_children: false,
    }
}

fn divider() -> BuiltinElement {
    let mut props = vec![mp("orientation", TyKind::String)];
    props.extend(layout_props());
    props.extend(style_props());
    BuiltinElement {
        name: "Divider",
        properties: props,
        functions: vec![],
        accepts_children: false,
    }
}

fn badge() -> BuiltinElement {
    let mut props = vec![
        mp("content", TyKind::String),
        mp("text", TyKind::String),
        mp("variant", TyKind::String),
    ];
    props.extend(layout_props());
    props.extend(style_props());
    BuiltinElement {
        name: "Badge",
        properties: props,
        functions: vec![],
        accepts_children: false,
    }
}

fn fragment() -> BuiltinElement {
    BuiltinElement {
        name: "Fragment",
        properties: layout_props(),
        functions: vec![],
        accepts_children: true,
    }
}

fn portal() -> BuiltinElement {
    BuiltinElement {
        name: "Portal",
        properties: vec![mp("target", TyKind::String)],
        functions: vec![],
        accepts_children: true,
    }
}

fn group() -> BuiltinElement {
    BuiltinElement {
        name: "Group",
        properties: layout_props(),
        functions: vec![],
        accepts_children: true,
    }
}

static ELEMENTS_VEC: LazyLock<Vec<BuiltinElement>> = LazyLock::new(|| {
    vec![
        stack("VStack"),
        stack("HStack"),
        zstack(),
        list_el(),
        scroll_view(),
        box_el(),
        text_el(),
        button(),
        text_field(),
        text_input(),
        integer_input(),
        float_input(),
        checkbox(),
        select_el(),
        option_el(),
        image_el(),
        spacer(),
        divider(),
        badge(),
        fragment(),
        portal(),
        group(),
    ]
});

/// All registered built-in components (for completion snippets).
pub fn builtin_components() -> impl Iterator<Item = &'static BuiltinElement> {
    ELEMENTS_VEC.iter()
}

pub fn get_builtin(name: &str) -> Option<BuiltinElement> {
    ELEMENTS_VEC.iter().find(|e| e.name == name).cloned()
}

pub fn accepts_children(name: &str) -> bool {
    ELEMENTS_VEC
        .iter()
        .find(|e| e.name == name)
        .map(|e| e.accepts_children)
        .unwrap_or(true)
}

pub fn get_prop_type(element: &str, prop: &str) -> Option<TyKind> {
    let el = ELEMENTS_VEC.iter().find(|e| e.name == element)?;
    el.properties
        .iter()
        .find(|p| p.name == prop)
        .map(|p| p.ty.clone())
}

static ENUMS_VEC: LazyLock<Vec<BuiltinEnum>> = LazyLock::new(|| {
    vec![
        BuiltinEnum {
            name: "Direction",
            cases: &["column", "row"],
        },
        BuiltinEnum {
            name: "ButtonVariant",
            cases: &[
                "default",
                "destructive",
                "outline",
                "secondary",
                "ghost",
                "link",
            ],
        },
        BuiltinEnum {
            name: "Align",
            cases: &["start", "center", "end", "stretch", "baseline"],
        },
        BuiltinEnum {
            name: "Justify",
            cases: &[
                "start",
                "center",
                "end",
                "space-between",
                "space-around",
                "space-evenly",
            ],
        },
        BuiltinEnum {
            name: "Weight",
            cases: &[
                "thin",
                "extra-light",
                "light",
                "normal",
                "medium",
                "semi-bold",
                "bold",
                "extra-bold",
                "black",
            ],
        },
    ]
});

pub fn builtin_enums() -> impl Iterator<Item = &'static BuiltinEnum> {
    ENUMS_VEC.iter()
}

pub fn get_builtin_enum(name: &str) -> Option<&'static BuiltinEnum> {
    ENUMS_VEC.iter().find(|e| e.name == name)
}

static VARIANTS_VEC: LazyLock<Vec<BuiltinVariant>> = LazyLock::new(|| {
    vec![
        BuiltinVariant {
            name: "Color".into(),
            cases: vec![
                vc("red", false),
                vc("green", false),
                vc("blue", false),
                vc("white", false),
                vc("black", false),
                vc("transparent", false),
                vc("rgba", true),
            ],
        },
        BuiltinVariant {
            name: "Brush".into(),
            cases: vec![vc("color", true), vc("gradient", false), vc("image", false)],
        },
    ]
});

fn vc(name: &str, has_payload: bool) -> BuiltinVariantCase {
    BuiltinVariantCase {
        name: name.into(),
        payload: if has_payload {
            Some(TyKind::String)
        } else {
            None
        },
    }
}

pub fn builtin_variants() -> impl Iterator<Item = &'static BuiltinVariant> {
    VARIANTS_VEC.iter()
}

pub fn get_builtin_variant(name: &str) -> Option<BuiltinVariant> {
    VARIANTS_VEC.iter().find(|v| v.name == name).cloned()
}
