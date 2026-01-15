//! Completion provider for Yel DSL.

use yel_core::syntax::ast::TyKind;
use tower_lsp::lsp_types::*;

use crate::document::Document;

// Stub module for removed stdlib functionality
mod stdlib {
    pub struct BuiltinEnum {
        pub name: String,
        pub cases: Vec<String>,
    }

    pub struct BuiltinVariant {
        pub name: String,
        pub cases: Vec<BuiltinVariantCase>,
    }

    pub struct BuiltinVariantCase {
        pub name: String,
        pub payload: Option<yel_core::syntax::ast::TyKind>,
    }

    pub struct BuiltinElement {
        pub name: String,
        pub properties: Vec<BuiltinProperty>,
    }

    pub struct BuiltinProperty {
        pub name: String,
        pub ty: yel_core::syntax::ast::TyKind,
    }

    pub fn get_builtin(_name: &str) -> Option<BuiltinElement> {
        None
    }

    pub fn get_builtin_enum(_name: &str) -> Option<BuiltinEnum> {
        None
    }

    pub fn get_builtin_variant(_name: &str) -> Option<BuiltinVariant> {
        None
    }

    pub fn builtin_enums() -> Vec<BuiltinEnum> {
        vec![]
    }

    pub fn builtin_variants() -> Vec<BuiltinVariant> {
        vec![]
    }

    pub fn builtin_components() -> Vec<BuiltinElement> {
        vec![]
    }
}

/// Keywords in Yel DSL.
const KEYWORDS: &[(&str, &str)] = &[
    ("component", "Define a new component"),
    ("property", "Declare a component property"),
    ("callback", "Declare a callback"),
    ("import", "Import a component from another file"),
    ("from", "Specify import source"),
    ("if", "Conditional rendering or statement"),
    ("else", "Alternative branch"),
    ("for", "List rendering loop"),
    ("as", "Bind loop item"),
    ("in", "Input property direction"),
    ("out", "Output property direction"),
    ("inout", "Bidirectional property direction"),
    ("true", "Boolean true"),
    ("false", "Boolean false"),
];

/// Built-in types.
const TYPES: &[(&str, &str)] = &[
    ("bool", "Boolean type"),
    ("string", "String type"),
    ("int", "Integer type"),
    ("float", "Floating point type"),
    ("length", "Length value (e.g., 10px)"),
    ("physical-length", "Physical length"),
    ("angle", "Angle value (e.g., 45deg)"),
    ("duration", "Duration value (e.g., 100ms)"),
    ("percent", "Percentage value"),
    ("relative-font-size", "Relative font size (rem)"),
    ("color", "Color value"),
    ("brush", "Brush for fills"),
    ("image", "Image reference"),
    ("easing", "Animation easing function"),
    ("list", "List container"),
];

/// Provide completions at the given position.
pub fn provide_completions(doc: &Document, position: Position) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    // Try to get context-aware completions first
    if let Some(ctx_completions) = get_contextual_completions(doc, position) {
        return ctx_completions;
    }

    // Add keywords
    for (name, detail) in KEYWORDS {
        completions.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(detail.to_string()),
            insert_text: Some(name.to_string()),
            ..Default::default()
        });
    }

    // Add types
    for (name, detail) in TYPES {
        completions.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::TYPE_PARAMETER),
            detail: Some(detail.to_string()),
            insert_text: Some(name.to_string()),
            ..Default::default()
        });
    }

    // Add built-in elements from stdlib
    for builtin in stdlib::builtin_components() {
        completions.push(CompletionItem {
            label: builtin.name.to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("Built-in element".to_string()),
            insert_text: Some(format!("{} {{\n    $0\n}}", builtin.name)),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    // Add component snippet
    completions.push(CompletionItem {
        label: "component".to_string(),
        kind: Some(CompletionItemKind::SNIPPET),
        detail: Some("Create a new component".to_string()),
        insert_text: Some(
            "component ${1:ComponentName} {\n    ${2:// properties}\n\n    ${3:// body}\n}"
                .to_string(),
        ),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        sort_text: Some("0component".to_string()), // Sort before keyword
        ..Default::default()
    });

    // Add property snippet
    completions.push(CompletionItem {
        label: "property".to_string(),
        kind: Some(CompletionItemKind::SNIPPET),
        detail: Some("Create a property declaration".to_string()),
        insert_text: Some("property ${1:name}: ${2:type} = ${3:value};".to_string()),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        sort_text: Some("0property".to_string()),
        ..Default::default()
    });

    // Add import snippet
    completions.push(CompletionItem {
        label: "import".to_string(),
        kind: Some(CompletionItemKind::SNIPPET),
        detail: Some("Import a component".to_string()),
        insert_text: Some("import ${1:Component} from \"${2:file.yel}\";".to_string()),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        sort_text: Some("0import".to_string()),
        ..Default::default()
    });

    completions
}

/// Try to provide context-aware completions based on cursor position.
fn get_contextual_completions(doc: &Document, position: Position) -> Option<Vec<CompletionItem>> {
    let content = doc.content();
    let content_str = content.as_str();
    let offset = doc.position_to_offset(position);

    // Parse the document to get user-defined types
    let parse_result = yel_core::syntax::parser::parse_file(content_str).ok();
    let parsed = parse_result.as_ref().map(|r| &r.file);

    // Get the text before cursor on the current line
    let line_start = content_str[..offset].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let line_text = &content_str[line_start..offset];

    // Check for dot-completion (member access)
    if let Some(completions) = get_dot_completions(content_str, offset, parsed) {
        return Some(completions);
    }

    // Check if we're after "property_name:" pattern (property value context)
    if let Some((element_name, prop_name)) = parse_property_context(content_str, offset) {
        // Look up the element and property type
        if let Some(prop_type) = get_property_type(&element_name, &prop_name, parsed) {
            // If it's a named type (enum or variant), suggest cases
            if let TyKind::Named(type_name) = prop_type {
                return Some(get_type_value_completions(&type_name, parsed));
            }
            // For bool type, suggest true/false
            if matches!(prop_type, TyKind::Bool) {
                return Some(vec![
                    CompletionItem {
                        label: "true".to_string(),
                        kind: Some(CompletionItemKind::VALUE),
                        detail: Some("Boolean true".to_string()),
                        ..Default::default()
                    },
                    CompletionItem {
                        label: "false".to_string(),
                        kind: Some(CompletionItemKind::VALUE),
                        detail: Some("Boolean false".to_string()),
                        ..Default::default()
                    },
                ]);
            }
        }
    }

    // Check if we're just typing an identifier that might be an enum case
    let word_start = line_text.rfind(|c: char| !c.is_alphanumeric() && c != '-' && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);
    let current_word = &line_text[word_start..];

    if !current_word.is_empty() {
        // Suggest matching enum/variant cases from all types (builtin + user-defined)
        let mut type_completions = Vec::new();

        // Builtin enums (stub - always empty)
        for builtin_enum in stdlib::builtin_enums() {
            for case in &builtin_enum.cases {
                if case.starts_with(current_word) {
                    type_completions.push(CompletionItem {
                        label: case.clone(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(format!("{} enum case", builtin_enum.name)),
                        ..Default::default()
                    });
                }
            }
        }

        // Builtin variants (stub - always empty)
        for builtin_variant in stdlib::builtin_variants() {
            for case in &builtin_variant.cases {
                if case.name.starts_with(current_word) {
                    let has_payload = case.payload.is_some();
                    type_completions.push(CompletionItem {
                        label: case.name.clone(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(format!("{} variant", builtin_variant.name)),
                        insert_text: if has_payload {
                            Some(format!("{}($0)", case.name))
                        } else {
                            Some(case.name.clone())
                        },
                        insert_text_format: if has_payload {
                            Some(InsertTextFormat::SNIPPET)
                        } else {
                            None
                        },
                        ..Default::default()
                    });
                }
            }
        }

        // User-defined enums
        if let Some(ref file) = parsed {
            for enum_def in &file.enums {
                for case in &enum_def.node.cases {
                    if case.node.starts_with(current_word) {
                        type_completions.push(CompletionItem {
                            label: case.node.clone(),
                            kind: Some(CompletionItemKind::ENUM_MEMBER),
                            detail: Some(format!("{} enum case", enum_def.node.name)),
                            ..Default::default()
                        });
                    }
                }
            }

            // User-defined variants
            for variant_def in &file.variants {
                for case in &variant_def.node.cases {
                    if case.node.name.starts_with(current_word) {
                        let has_payload = case.node.payload.is_some();
                        type_completions.push(CompletionItem {
                            label: case.node.name.clone(),
                            kind: Some(CompletionItemKind::ENUM_MEMBER),
                            detail: Some(format!("{} variant", variant_def.node.name)),
                            insert_text: if has_payload {
                                Some(format!("{}($0)", case.node.name))
                            } else {
                                Some(case.node.name.clone())
                            },
                            insert_text_format: if has_payload {
                                Some(InsertTextFormat::SNIPPET)
                            } else {
                                None
                            },
                            ..Default::default()
                        });
                    }
                }
            }
        }

        if !type_completions.is_empty() {
            // Don't return only type completions, let it fall through to general completions
            // but we could prioritize these
        }
    }

    None
}

/// Parse the context to find element name and property name.
/// Returns (element_name, property_name) if we're in a property value context.
fn parse_property_context(content: &str, offset: usize) -> Option<(String, String)> {
    let before_cursor = &content[..offset];

    // Find the property name by looking for "prop_name:" pattern
    // First, find the last colon before cursor
    let colon_pos = before_cursor.rfind(':')?;

    // Check that there's only whitespace or partial identifier between colon and cursor
    // This allows completing after "direction: " or "direction: col"
    let between = &before_cursor[colon_pos + 1..];
    let trimmed = between.trim_start();

    // Allow if empty, whitespace only, or a partial identifier (no special chars like comma, brace)
    if !trimmed.is_empty() && trimmed.contains([',', '{', '}', ';']) {
        return None;
    }

    // Find the property name before the colon
    let before_colon = &before_cursor[..colon_pos];
    let prop_start = before_colon.rfind(|c: char| !c.is_alphanumeric() && c != '-' && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);
    let prop_name = before_colon[prop_start..].trim().to_string();

    if prop_name.is_empty() {
        return None;
    }

    // Now find the enclosing element
    // Look for "ElementName {" pattern before this property
    let element_name = find_enclosing_element(before_cursor)?;

    Some((element_name, prop_name))
}

/// Find the enclosing element name by looking for "Name {" pattern.
fn find_enclosing_element(text: &str) -> Option<String> {
    // Simple approach: look for the most recent "Name {" pattern
    // by tracking brace depth
    let mut depth = 0;
    let mut i = text.len();

    while i > 0 {
        i -= 1;
        let c = text.chars().nth(i)?;

        if c == '}' {
            depth += 1;
        } else if c == '{' {
            if depth == 0 {
                // This is our opening brace, find the name before it
                let before_brace = text[..i].trim_end();
                let name_start = before_brace.rfind(|c: char| !c.is_alphanumeric() && c != '-' && c != '_')
                    .map(|i| i + 1)
                    .unwrap_or(0);
                let name = before_brace[name_start..].trim();
                if !name.is_empty() && !["if", "else", "for", "component"].contains(&name) {
                    return Some(name.to_string());
                }
                // Keep looking for a non-keyword element
            } else {
                depth -= 1;
            }
        }
    }

    None
}

/// Get the type of a property on an element.
fn get_property_type(element: &str, property: &str, parsed: Option<&yel_core::syntax::ast::File>) -> Option<TyKind> {
    // Check builtin components (stub - always returns None)
    if let Some(comp) = stdlib::get_builtin(element) {
        if let Some(prop) = comp.properties.iter().find(|p| p.name == property) {
            return Some(prop.ty.clone());
        }
    }

    // Check user-defined components
    if let Some(file) = parsed {
        for comp in &file.components {
            if comp.node.name == element {
                if let Some(prop) = comp.node.properties.iter().find(|p| p.node.name == property) {
                    return Some(prop.node.ty.kind.clone());
                }
            }
        }
    }

    None
}

/// Get completions for enum or variant cases.
fn get_type_value_completions(type_name: &str, parsed: Option<&yel_core::syntax::ast::File>) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    // Check builtin enums (stub - always returns None)
    if let Some(enum_def) = stdlib::get_builtin_enum(type_name) {
        for case in &enum_def.cases {
            completions.push(CompletionItem {
                label: case.clone(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some(format!("{} value", type_name)),
                ..Default::default()
            });
        }
        return completions;
    }

    // Check builtin variants (stub - always returns None)
    if let Some(variant_def) = stdlib::get_builtin_variant(type_name) {
        for case in &variant_def.cases {
            let has_payload = case.payload.is_some();
            completions.push(CompletionItem {
                label: case.name.clone(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some(format!("{} variant", type_name)),
                insert_text: if has_payload {
                    Some(format!("{}($0)", case.name))
                } else {
                    Some(case.name.clone())
                },
                insert_text_format: if has_payload {
                    Some(InsertTextFormat::SNIPPET)
                } else {
                    None
                },
                ..Default::default()
            });
        }
        return completions;
    }

    // Check user-defined enums
    if let Some(file) = parsed {
        for enum_def in &file.enums {
            if enum_def.node.name == type_name {
                for case in &enum_def.node.cases {
                    completions.push(CompletionItem {
                        label: case.node.clone(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(format!("{} value", type_name)),
                        ..Default::default()
                    });
                }
                return completions;
            }
        }

        // Check user-defined variants
        for variant_def in &file.variants {
            if variant_def.node.name == type_name {
                for case in &variant_def.node.cases {
                    let has_payload = case.node.payload.is_some();
                    completions.push(CompletionItem {
                        label: case.node.name.clone(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(format!("{} variant", type_name)),
                        insert_text: if has_payload {
                            Some(format!("{}($0)", case.node.name))
                        } else {
                            Some(case.node.name.clone())
                        },
                        insert_text_format: if has_payload {
                            Some(InsertTextFormat::SNIPPET)
                        } else {
                            None
                        },
                        ..Default::default()
                    });
                }
                return completions;
            }
        }
    }

    completions
}

/// Get completions after a dot (member access).
fn get_dot_completions(
    content: &str,
    offset: usize,
    parsed: Option<&yel_core::syntax::ast::File>,
) -> Option<Vec<CompletionItem>> {
    let before = &content[..offset];

    // Check if we just typed a dot or are after a dot with partial identifier
    let dot_pos = before.rfind('.')?;

    // Make sure there's no special characters between dot and cursor (allow partial identifier)
    let after_dot = &before[dot_pos + 1..];
    if after_dot.contains(|c: char| c.is_whitespace() || c == '(' || c == ')' || c == '{' || c == '}' || c == ';' || c == ':') {
        return None;
    }

    // Get the identifier before the dot
    let before_dot = &before[..dot_pos];
    let ident_start = before_dot
        .rfind(|c: char| !c.is_alphanumeric() && c != '-' && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);
    let ident = before_dot[ident_start..].trim();

    if ident.is_empty() {
        return None;
    }

    // Find the type of this identifier
    let file = parsed?;

    // Find which component we're in
    let comp = find_component_at_offset(file, offset)?;

    // Look up the identifier type
    let ident_type = find_identifier_type(ident, comp, file)?;

    // Generate completions based on the type
    Some(get_completions_for_type(&ident_type, file))
}

/// Find which component contains the given offset.
fn find_component_at_offset(
    file: &yel_core::syntax::ast::File,
    offset: usize,
) -> Option<&yel_core::syntax::ast::Component> {
    for comp in &file.components {
        if comp.span.start <= offset && offset <= comp.span.end {
            return Some(&comp.node);
        }
    }
    None
}

/// Find the type of an identifier in a component.
fn find_identifier_type(
    ident: &str,
    comp: &yel_core::syntax::ast::Component,
    _file: &yel_core::syntax::ast::File,
) -> Option<TyKind> {
    // Check component properties
    for prop in &comp.properties {
        if prop.node.name == ident {
            return Some(prop.node.ty.kind.clone());
        }
    }

    // Could also check for loop variables, locals, etc. but properties cover most cases
    None
}

/// Get completions for a given type.
fn get_completions_for_type(
    ty: &TyKind,
    file: &yel_core::syntax::ast::File,
) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    match ty {
        TyKind::Option(inner) => {
            // Option methods
            completions.push(CompletionItem {
                label: "is_some".to_string(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some("bool - Returns true if the option has a value".to_string()),
                ..Default::default()
            });
            completions.push(CompletionItem {
                label: "is_none".to_string(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some("bool - Returns true if the option is empty".to_string()),
                ..Default::default()
            });
            completions.push(CompletionItem {
                label: "unwrap".to_string(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some(format!("{} - Returns the value (panics if none)", format_type(&inner.kind))),
                ..Default::default()
            });
            // Also add fields from inner type for convenience (optional chaining style)
            if let TyKind::Named(type_name) = &inner.kind {
                for rec in &file.records {
                    if rec.node.name == *type_name {
                        for field in &rec.node.fields {
                            completions.push(CompletionItem {
                                label: field.node.name.clone(),
                                kind: Some(CompletionItemKind::FIELD),
                                detail: Some(format!("{} - Field of {} (requires unwrap)", format_type(&field.node.ty.kind), type_name)),
                                ..Default::default()
                            });
                        }
                    }
                }
            }
        }
        TyKind::List(inner) => {
            // List methods
            completions.push(CompletionItem {
                label: "len".to_string(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some("s32 - Returns the number of elements".to_string()),
                ..Default::default()
            });
            completions.push(CompletionItem {
                label: "is_empty".to_string(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some("bool - Returns true if the list is empty".to_string()),
                ..Default::default()
            });
            completions.push(CompletionItem {
                label: "first".to_string(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some(format!("option<{}> - Returns the first element", format_type(&inner.kind))),
                ..Default::default()
            });
            completions.push(CompletionItem {
                label: "last".to_string(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some(format!("option<{}> - Returns the last element", format_type(&inner.kind))),
                ..Default::default()
            });
        }
        TyKind::String => {
            completions.push(CompletionItem {
                label: "len".to_string(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some("s32 - Returns the length of the string".to_string()),
                ..Default::default()
            });
            completions.push(CompletionItem {
                label: "is_empty".to_string(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some("bool - Returns true if the string is empty".to_string()),
                ..Default::default()
            });
        }
        TyKind::Named(type_name) => {
            // Look up record fields
            for rec in &file.records {
                if rec.node.name == *type_name {
                    for field in &rec.node.fields {
                        completions.push(CompletionItem {
                            label: field.node.name.clone(),
                            kind: Some(CompletionItemKind::FIELD),
                            detail: Some(format!("{} - Field of {}", format_type(&field.node.ty.kind), type_name)),
                            ..Default::default()
                        });
                    }
                    return completions;
                }
            }
        }
        _ => {}
    }

    completions
}

/// Format a type for display.
fn format_type(ty: &TyKind) -> String {
    ty.to_string()
}
