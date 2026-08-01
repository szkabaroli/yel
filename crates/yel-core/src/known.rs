//! Known definitions for the Yel compiler.
//!
//! This module tracks DefIds for builtin elements and types that are
//! registered during compiler initialization. The pattern follows the
//! lazy-lookup approach where DefIds are populated during stdlib registration
//! and accessed via getters that panic if uninitialized.

use crate::ids::DefId;

/// Known definitions tracked by the compiler.
#[derive(Debug, Default)]
pub struct KnownDefinitions {
    pub elements: KnownElements,
    pub enums: KnownEnums,
    pub variants: KnownVariants,
    pub functions: KnownFunctions,
    pub builtin_types: KnownBuiltinTypes,
}

impl KnownDefinitions {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Known builtin elements (VStack, HStack, Button, etc.).
/// These are primitive UI elements provided by the host runtime.
#[derive(Debug, Default)]
pub struct KnownElements {
    // Layout elements
    pub vstack: Option<DefId>,
    pub hstack: Option<DefId>,
    pub zstack: Option<DefId>,

    // Container elements
    pub list: Option<DefId>,
    pub scroll_view: Option<DefId>,
    pub r#box: Option<DefId>,

    // Text elements
    pub text: Option<DefId>,

    // Input elements
    pub text_field: Option<DefId>,
    pub text_input: Option<DefId>,
    pub integer_input: Option<DefId>,
    pub float_input: Option<DefId>,

    // Media elements
    pub image: Option<DefId>,

    // Utility elements
    pub fragment: Option<DefId>,
    pub portal: Option<DefId>,
    pub group: Option<DefId>,

    /// Additional builtin elements that don't need named codegen access
    /// (e.g. the MeshX design-system components: `Card`, `Alert`, `Tabs`, …).
    /// Registered as create-element primitives; membership here is what
    /// `is_builtin` consults for them.
    pub others: Vec<DefId>,
}

impl KnownElements {
    pub fn vstack(&self) -> DefId {
        self.vstack.expect("VStack not initialized")
    }

    pub fn hstack(&self) -> DefId {
        self.hstack.expect("HStack not initialized")
    }

    pub fn zstack(&self) -> DefId {
        self.zstack.expect("ZStack not initialized")
    }

    pub fn list(&self) -> DefId {
        self.list.expect("List not initialized")
    }

    pub fn scroll_view(&self) -> DefId {
        self.scroll_view.expect("ScrollView not initialized")
    }

    pub fn r#box(&self) -> DefId {
        self.r#box.expect("Box not initialized")
    }

    pub fn text(&self) -> DefId {
        self.text.expect("Text not initialized")
    }

    pub fn text_field(&self) -> DefId {
        self.text_field.expect("TextField not initialized")
    }

    pub fn text_input(&self) -> DefId {
        self.text_input.expect("TextInput not initialized")
    }

    pub fn integer_input(&self) -> DefId {
        self.integer_input.expect("IntegerInput not initialized")
    }

    pub fn float_input(&self) -> DefId {
        self.float_input.expect("FloatInput not initialized")
    }

    pub fn image(&self) -> DefId {
        self.image.expect("Image not initialized")
    }

    pub fn fragment(&self) -> DefId {
        self.fragment.expect("Fragment not initialized")
    }

    pub fn portal(&self) -> DefId {
        self.portal.expect("Portal not initialized")
    }

    pub fn group(&self) -> DefId {
        self.group.expect("Group not initialized")
    }

    /// Check if a DefId is a known builtin element.
    pub fn is_builtin(&self, def_id: DefId) -> bool {
        self.vstack == Some(def_id)
            || self.hstack == Some(def_id)
            || self.zstack == Some(def_id)
            || self.list == Some(def_id)
            || self.scroll_view == Some(def_id)
            || self.r#box == Some(def_id)
            || self.text == Some(def_id)
            || self.text_field == Some(def_id)
            || self.text_input == Some(def_id)
            || self.integer_input == Some(def_id)
            || self.float_input == Some(def_id)
            || self.image == Some(def_id)
            || self.fragment == Some(def_id)
            || self.portal == Some(def_id)
            || self.group == Some(def_id)
            || self.others.contains(&def_id)
    }
}

/// Known builtin enums (Direction, Align, etc.).
#[derive(Debug, Default)]
pub struct KnownEnums {
    pub direction: Option<DefId>,
    pub button_variant: Option<DefId>,
    pub align: Option<DefId>,
    pub justify: Option<DefId>,
    pub weight: Option<DefId>,
}

impl KnownEnums {
    pub fn direction(&self) -> DefId {
        self.direction.expect("Direction not initialized")
    }

    pub fn button_variant(&self) -> DefId {
        self.button_variant.expect("ButtonVariant not initialized")
    }

    pub fn align(&self) -> DefId {
        self.align.expect("Align not initialized")
    }

    pub fn justify(&self) -> DefId {
        self.justify.expect("Justify not initialized")
    }

    pub fn weight(&self) -> DefId {
        self.weight.expect("Weight not initialized")
    }
}

/// Known builtin variants (Color, Brush, AttributeValue).
#[derive(Debug, Default)]
pub struct KnownVariants {
    pub color: Option<DefId>,
    pub brush: Option<DefId>,
    /// The `attribute-value` variant from `yel:ui/dom` — the value type
    /// of `set-attribute`. Its canonical-ABI flattening backs the DOM
    /// `set-attribute` import signature.
    pub attribute_value: Option<DefId>,
    /// Interned `Ty` for [`Self::attribute_value`], cached so THIR→LIR
    /// (which holds `ctx` immutably) can build an `attribute-value`
    /// `VariantCtor` without re-interning.
    pub attribute_value_ty: Option<crate::types::Ty>,
    /// The `event-value` variant from `yel:ui/dispatch` — the payload type
    /// of the module `dispatch` export. Its canonical-ABI flattening backs
    /// the core `dispatch` function signature; registering it as a real
    /// variant (like `attribute-value`) is what lets the WIT + core ABI
    /// derive from one source instead of two hardcoded copies.
    pub event_value: Option<DefId>,
    /// Interned `Ty` for [`Self::event_value`].
    pub event_value_ty: Option<crate::types::Ty>,
}

impl KnownVariants {
    pub fn color(&self) -> DefId {
        self.color.expect("Color not initialized")
    }

    pub fn attribute_value(&self) -> DefId {
        self.attribute_value
            .expect("AttributeValue not initialized")
    }

    pub fn attribute_value_ty(&self) -> crate::types::Ty {
        self.attribute_value_ty
            .expect("AttributeValue Ty not initialized")
    }

    pub fn event_value_ty(&self) -> crate::types::Ty {
        self.event_value_ty.expect("EventValue Ty not initialized")
    }

    pub fn brush(&self) -> DefId {
        self.brush.expect("Brush not initialized")
    }
}

/// Known builtin functions (concat, type-specific conversions, len).
/// These are the only function types supported currently.
/// User-defined functions are not supported yet - only handlers and callbacks
/// (extern functions with no bodies that codegen generates logic for).
#[derive(Debug, Default)]
pub struct KnownFunctions {
    /// Concatenate strings: func(string...) -> string
    pub concat: Option<DefId>,

    // Type-specific to-string conversions
    /// Convert bool to string: func(bool) -> string ("true" / "false")
    pub bool_to_string: Option<DefId>,
    /// Convert s32 to string: func(s32) -> string
    pub s32_to_string: Option<DefId>,
    /// Convert u32 to string: func(u32) -> string
    pub u32_to_string: Option<DefId>,
    /// Convert s64 to string: func(s64) -> string
    pub s64_to_string: Option<DefId>,
    /// Convert u64 to string: func(u64) -> string
    pub u64_to_string: Option<DefId>,
    /// Convert f32 to string: func(f32) -> string
    pub f32_to_string: Option<DefId>,
    /// Convert f64 to string: func(f64) -> string
    pub f64_to_string: Option<DefId>,
    /// Convert char to string: func(char) -> string
    pub char_to_string: Option<DefId>,
    /// Generic object to string (for complex types): func(any) -> string
    /// Returns JS-like "[object]" for records, "[Array]" for lists, etc.
    pub object_to_string: Option<DefId>,

    /// Get length of list or string: func(list<T> | string) -> s32
    pub len: Option<DefId>,

    /// Safe list element access: func(list<T>, s32) -> option<T>
    /// Returns some(element) if index in bounds, none if out of bounds.
    pub list_get: Option<DefId>,

    // Math functions
    /// Minimum of two values: func(s32, s32) -> s32
    pub min: Option<DefId>,
    /// Maximum of two values: func(s32, s32) -> s32
    pub max: Option<DefId>,

    // Collection methods
    /// Filter list by predicate: func(list<T>, func(T) -> bool) -> list<T>
    pub filter: Option<DefId>,

    /// Append element to list (immutable, returns new list):
    /// `func(list<T>, T) -> list<T>`.
    pub append: Option<DefId>,

    // String methods
    /// Check if string starts with prefix: func(string, string) -> bool
    pub starts_with: Option<DefId>,
}

impl KnownFunctions {
    pub fn concat(&self) -> DefId {
        self.concat.expect("concat not initialized")
    }

    pub fn bool_to_string(&self) -> DefId {
        self.bool_to_string.expect("bool_to_string not initialized")
    }

    pub fn s32_to_string(&self) -> DefId {
        self.s32_to_string.expect("s32_to_string not initialized")
    }

    pub fn u32_to_string(&self) -> DefId {
        self.u32_to_string.expect("u32_to_string not initialized")
    }

    pub fn s64_to_string(&self) -> DefId {
        self.s64_to_string.expect("s64_to_string not initialized")
    }

    pub fn u64_to_string(&self) -> DefId {
        self.u64_to_string.expect("u64_to_string not initialized")
    }

    pub fn f32_to_string(&self) -> DefId {
        self.f32_to_string.expect("f32_to_string not initialized")
    }

    pub fn f64_to_string(&self) -> DefId {
        self.f64_to_string.expect("f64_to_string not initialized")
    }

    pub fn char_to_string(&self) -> DefId {
        self.char_to_string.expect("char_to_string not initialized")
    }

    pub fn object_to_string(&self) -> DefId {
        self.object_to_string
            .expect("object_to_string not initialized")
    }

    pub fn len(&self) -> DefId {
        self.len.expect("len not initialized")
    }

    pub fn list_get(&self) -> DefId {
        self.list_get.expect("list_get not initialized")
    }

    pub fn min(&self) -> DefId {
        self.min.expect("min not initialized")
    }

    pub fn max(&self) -> DefId {
        self.max.expect("max not initialized")
    }

    pub fn filter(&self) -> DefId {
        self.filter.expect("filter not initialized")
    }

    pub fn append(&self) -> DefId {
        self.append.expect("append not initialized")
    }

    pub fn starts_with(&self) -> DefId {
        self.starts_with.expect("starts_with not initialized")
    }

    /// Check if a DefId is a known builtin function.
    pub fn is_builtin(&self, def_id: DefId) -> bool {
        self.concat == Some(def_id)
            || self.bool_to_string == Some(def_id)
            || self.s32_to_string == Some(def_id)
            || self.u32_to_string == Some(def_id)
            || self.s64_to_string == Some(def_id)
            || self.u64_to_string == Some(def_id)
            || self.f32_to_string == Some(def_id)
            || self.f64_to_string == Some(def_id)
            || self.char_to_string == Some(def_id)
            || self.object_to_string == Some(def_id)
            || self.len == Some(def_id)
            || self.list_get == Some(def_id)
            || self.min == Some(def_id)
            || self.max == Some(def_id)
            || self.filter == Some(def_id)
            || self.append == Some(def_id)
            || self.starts_with == Some(def_id)
    }
}

/// Known builtin generic types (option, result).
/// These are template variants whose cases (some/none, ok/err) can be used
/// as constructors.
#[derive(Debug, Default)]
pub struct KnownBuiltinTypes {
    // Option type
    pub option: Option<DefId>,
    pub option_some: Option<DefId>,
    pub option_none: Option<DefId>,

    // Result type
    pub result: Option<DefId>,
    pub result_ok: Option<DefId>,
    pub result_err: Option<DefId>,
}

impl KnownBuiltinTypes {
    pub fn option(&self) -> DefId {
        self.option.expect("option not initialized")
    }

    pub fn option_some(&self) -> DefId {
        self.option_some.expect("option::some not initialized")
    }

    pub fn option_none(&self) -> DefId {
        self.option_none.expect("option::none not initialized")
    }

    pub fn result(&self) -> DefId {
        self.result.expect("result not initialized")
    }

    pub fn result_ok(&self) -> DefId {
        self.result_ok.expect("result::ok not initialized")
    }

    pub fn result_err(&self) -> DefId {
        self.result_err.expect("result::err not initialized")
    }
}
