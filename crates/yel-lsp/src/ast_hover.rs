//! AST-based hover information provider.

use yel_core::source::{SourceId, Span};
use yel_core::syntax::ast::*;
use yel_core::syntax::parser;
use tower_lsp::lsp_types::*;

// Stub types for removed analyzer module functionality
#[derive(Clone, Debug)]
pub enum ResolvedType {
    Bool,
    S8,
    S16,
    S32,
    S64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Char,
    String,
    List(Box<ResolvedType>),
    Option(Box<ResolvedType>),
    Named(std::string::String),
    Unknown,
}

impl std::fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Bool => write!(f, "bool"),
            ResolvedType::S8 => write!(f, "s8"),
            ResolvedType::S16 => write!(f, "s16"),
            ResolvedType::S32 => write!(f, "s32"),
            ResolvedType::S64 => write!(f, "s64"),
            ResolvedType::U8 => write!(f, "u8"),
            ResolvedType::U16 => write!(f, "u16"),
            ResolvedType::U32 => write!(f, "u32"),
            ResolvedType::U64 => write!(f, "u64"),
            ResolvedType::F32 => write!(f, "f32"),
            ResolvedType::F64 => write!(f, "f64"),
            ResolvedType::Char => write!(f, "char"),
            ResolvedType::String => write!(f, "string"),
            ResolvedType::List(inner) => write!(f, "list<{}>", inner),
            ResolvedType::Option(inner) => write!(f, "option<{}>", inner),
            ResolvedType::Named(name) => write!(f, "{}", name),
            ResolvedType::Unknown => write!(f, "unknown"),
        }
    }
}

pub struct TypeNarrowings;

pub struct NarrowedScope {
    pub variable: String,
    pub narrowed_type: ResolvedType,
}

impl TypeNarrowings {
    pub fn new() -> Self {
        TypeNarrowings
    }

    #[allow(dead_code)]
    pub fn get(&self, _var: &str) -> Option<&ResolvedType> {
        None
    }

    pub fn get_narrowed_type(&self, _offset: usize, _var: &str) -> Option<&ResolvedType> {
        None
    }

    pub fn get_narrowings_at(&self, _offset: usize) -> Vec<NarrowedScope> {
        vec![]
    }
}

/// Convert an AST TyKind to a ResolvedType.
fn resolve_ast_type(ty: &TyKind) -> ResolvedType {
    match ty {
        TyKind::Bool => ResolvedType::Bool,
        TyKind::S8 => ResolvedType::S8,
        TyKind::S16 => ResolvedType::S16,
        TyKind::S32 => ResolvedType::S32,
        TyKind::S64 => ResolvedType::S64,
        TyKind::U8 => ResolvedType::U8,
        TyKind::U16 => ResolvedType::U16,
        TyKind::U32 => ResolvedType::U32,
        TyKind::U64 => ResolvedType::U64,
        TyKind::F32 => ResolvedType::F32,
        TyKind::F64 => ResolvedType::F64,
        TyKind::Char => ResolvedType::Char,
        TyKind::String => ResolvedType::String,
        TyKind::List(inner) => ResolvedType::List(Box::new(resolve_ast_type(&inner.kind))),
        TyKind::Option(inner) => ResolvedType::Option(Box::new(resolve_ast_type(&inner.kind))),
        TyKind::Named(name) => ResolvedType::Named(name.clone()),
        _ => ResolvedType::Unknown,
    }
}

// Stub module for removed stdlib functionality
mod stdlib {
    #[allow(dead_code)]
    pub struct BuiltinElement {
        pub name: String,
        pub properties: Vec<BuiltinProperty>,
        pub functions: Vec<BuiltinFunction>,
    }

    #[allow(dead_code)]
    pub struct BuiltinProperty {
        pub name: String,
        pub ty: BuiltinTy,
    }

    #[allow(dead_code)]
    pub struct BuiltinTy {
        pub kind: yel_core::syntax::ast::TyKind,
    }

    #[allow(dead_code)]
    pub struct BuiltinFunction {
        pub name: String,
        pub params: Vec<(String, BuiltinTy)>,
        pub return_type: Option<BuiltinTy>,
    }

    #[allow(dead_code)]
    pub struct BuiltinEnum {
        pub name: String,
        pub cases: Vec<String>,
    }

    #[allow(dead_code)]
    pub struct BuiltinVariant {
        pub name: String,
        pub cases: Vec<BuiltinVariantCase>,
    }

    #[allow(dead_code)]
    pub struct BuiltinVariantCase {
        pub name: String,
        pub payload: Option<yel_core::syntax::ast::TyKind>,
    }

    pub fn get_builtin(_name: &str) -> Option<BuiltinElement> {
        None
    }

    pub fn accepts_children(_name: &str) -> bool {
        true
    }

    #[allow(dead_code)]
    pub fn builtin_enums() -> Vec<BuiltinEnum> {
        vec![]
    }

    #[allow(dead_code)]
    pub fn builtin_variants() -> Vec<BuiltinVariant> {
        vec![]
    }

    #[allow(dead_code)]
    pub fn get_prop_type(_element: &str, _prop: &str) -> Option<yel_core::syntax::ast::TyKind> {
        None
    }
}

/// Parse document and find AST node at position, returning hover info.
pub fn get_ast_hover(content: &str, offset: usize) -> Option<Hover> {
    let source_id = SourceId(0);
    let parse_result = parser::parse_file_with_source_id(content, source_id).ok()?;
    let file = parse_result.file;

    let narrowings = TypeNarrowings::new();
    let hover_content = find_hover_content(&file, offset, source_id, &narrowings)?;
    Some(markdown_hover(&hover_content))
}

fn markdown_hover(content: &str) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content.to_string(),
        }),
        range: None,
    }
}

fn contains(span: &Span, offset: usize, source_id: SourceId) -> bool {
    span.source == source_id && span.start <= offset && offset < span.end
}

/// Find hover content by traversing the AST.
fn find_hover_content(
    file: &File,
    offset: usize,
    source_id: SourceId,
    narrowings: &TypeNarrowings,
) -> Option<String> {
    // Check records first
    for rec in &file.records {
        if contains(&rec.node.name_span, offset, source_id) {
            return Some(format_record(&rec.node));
        }
        if contains(&rec.span, offset, source_id) {
            // Check record fields
            for field in &rec.node.fields {
                if contains(&field.node.name_span, offset, source_id) {
                    return Some(format_record_field(&field.node, &rec.node.name));
                }
                if contains(&field.node.ty.span, offset, source_id) {
                    return Some(format_type(&field.node.ty.kind));
                }
            }
            return Some(format_record(&rec.node));
        }
    }

    // Check components
    for comp in &file.components {
        // Component name
        if contains(&comp.node.name_span, offset, source_id) {
            return Some(format_component(&comp.node));
        }
        if contains(&comp.span, offset, source_id) {
            if let Some(content) = find_in_component(&comp.node, file, offset, source_id, narrowings)
            {
                return Some(content);
            }
        }
    }
    None
}

fn find_in_component(
    comp: &Component,
    file: &File,
    offset: usize,
    sid: SourceId,
    narrowings: &TypeNarrowings,
) -> Option<String> {
    // Properties
    for prop in &comp.properties {
        if contains(&prop.span, offset, sid) {
            if contains(&prop.node.name_span, offset, sid) {
                return Some(format_property(&prop.node, &comp.name));
            }
            if contains(&prop.node.ty.span, offset, sid) {
                return Some(format_type(&prop.node.ty.kind));
            }
            if let Some(ref def) = prop.node.default {
                if contains(&def.span, offset, sid) {
                    return find_in_expr(&def.node, comp, file, offset, sid, narrowings);
                }
            }
            return Some(format_property(&prop.node, &comp.name));
        }
    }

    // Functions (callbacks)
    for func in &comp.functions {
        if contains(&func.span, offset, sid) {
            if contains(&func.node.name_span, offset, sid) {
                return Some(format_function(&func.node, &comp.name));
            }
            for (_, ty) in &func.node.params {
                if contains(&ty.span, offset, sid) {
                    return Some(format_type(&ty.kind));
                }
            }
            return Some(format_function(&func.node, &comp.name));
        }
    }

    // Body nodes
    for node in &comp.body {
        if contains(&node.span, offset, sid) {
            if let Some(c) = find_in_node(&node.node, comp, file, offset, sid, narrowings) {
                return Some(c);
            }
        }
    }
    None
}

fn find_in_node(
    node: &Node,
    comp: &Component,
    file: &File,
    offset: usize,
    sid: SourceId,
    narrowings: &TypeNarrowings,
) -> Option<String> {
    match node {
        Node::Element(el) => {
            if contains(&el.name_span, offset, sid) {
                // Custom component or built-in?
                if let Some(ref_comp) = file.components.iter().find(|c| c.node.name == el.name) {
                    return Some(format_component_use(&ref_comp.node));
                }
                // Check stdlib
                if let Some(builtin) = stdlib::get_builtin(&el.name) {
                    return Some(format_builtin(&builtin));
                }
                return None;
            }
            for b in &el.bindings {
                if contains(&b.span, offset, sid) {
                    if contains(&b.node.name_span, offset, sid) {
                        return format_binding(&b.node.name, &el.name, file);
                    }
                    if contains(&b.node.value.span, offset, sid) {
                        return find_in_expr(&b.node.value.node, comp, file, offset, sid, narrowings);
                    }
                }
            }
            for h in &el.handlers {
                if contains(&h.span, offset, sid) {
                    if contains(&h.node.name_span, offset, sid) {
                        return format_handler(&h.node.name, &el.name, file);
                    }
                    for stmt in &h.node.body {
                        if contains(&stmt.span, offset, sid) {
                            return find_in_stmt(&stmt.node, comp, file, offset, sid, narrowings);
                        }
                    }
                }
            }
            for child in &el.children {
                if contains(&child.span, offset, sid) {
                    return find_in_node(&child.node, comp, file, offset, sid, narrowings);
                }
            }
        }
        Node::Text(text_node) => {
            if contains(&text_node.content.span, offset, sid) {
                return find_in_expr(&text_node.content.node, comp, file, offset, sid, narrowings);
            }
        }
        Node::If(if_node) => {
            if contains(&if_node.condition.span, offset, sid) {
                return find_in_expr(&if_node.condition.node, comp, file, offset, sid, narrowings);
            }
            for child in &if_node.then_branch {
                if contains(&child.span, offset, sid) {
                    return find_in_node(&child.node, comp, file, offset, sid, narrowings);
                }
            }
            for (cond, branch) in &if_node.else_if_branches {
                if contains(&cond.span, offset, sid) {
                    return find_in_expr(&cond.node, comp, file, offset, sid, narrowings);
                }
                for child in branch {
                    if contains(&child.span, offset, sid) {
                        return find_in_node(&child.node, comp, file, offset, sid, narrowings);
                    }
                }
            }
            if let Some(ref else_b) = if_node.else_branch {
                for child in else_b {
                    if contains(&child.span, offset, sid) {
                        return find_in_node(&child.node, comp, file, offset, sid, narrowings);
                    }
                }
            }
        }
        Node::For(for_node) => {
            if contains(&for_node.iterable.span, offset, sid) {
                return find_in_expr(&for_node.iterable.node, comp, file, offset, sid, narrowings);
            }
            if contains(&for_node.item_name_span, offset, sid) {
                return Some(format!(
                    "```yel\n{}: <item>\n```\n---\nLoop variable.",
                    for_node.item_name
                ));
            }
            if let Some(ref key) = for_node.key {
                if contains(&key.span, offset, sid) {
                    return find_in_expr(&key.node, comp, file, offset, sid, narrowings);
                }
            }
            for child in &for_node.body {
                if contains(&child.span, offset, sid) {
                    return find_in_node(&child.node, comp, file, offset, sid, narrowings);
                }
            }
        }
    }
    None
}

fn find_in_expr(
    expr: &Expr,
    comp: &Component,
    file: &File,
    offset: usize,
    sid: SourceId,
    narrowings: &TypeNarrowings,
) -> Option<String> {
    match expr {
        Expr::Ident(name) => {
            // Check if it's a property reference
            if let Some(prop) = comp.properties.iter().find(|p| &p.node.name == name) {
                // Check if there's a narrowed type at this position
                if let Some(narrowed_ty) = narrowings.get_narrowed_type(offset, name) {
                    return Some(format_property_ref_narrowed(
                        &prop.node,
                        &comp.name,
                        narrowed_ty,
                    ));
                }
                // Debug: show available narrowings
                let all_at_offset: Vec<_> = narrowings
                    .get_narrowings_at(offset)
                    .iter()
                    .map(|s| format!("{}:{}", s.variable, format_resolved_type(&s.narrowed_type)))
                    .collect();
                let debug_info = if all_at_offset.is_empty() {
                    format!(" (no narrowings at offset {})", offset)
                } else {
                    format!(" (narrowings: {})", all_at_offset.join(", "))
                };
                return Some(format!(
                    "```yel\n{}: {}\n```\n---\nProperty of `{}`.{}",
                    prop.name, prop.ty.kind, comp.name, debug_info
                ));
            }
            // Check if it's an enum case (builtin)
            if let Some(info) = format_enum_case(name, file) {
                return Some(info);
            }
            // Check if it's a variant case (builtin)
            if let Some(info) = format_variant_case(name, file) {
                return Some(info);
            }
            Some(format!("```yel\n{}\n```\n---\nIdentifier.", name))
        }
        Expr::Literal(lit) => Some(format_literal(lit)),
        Expr::Binary(lhs, _, rhs) => {
            if contains(&lhs.span, offset, sid) {
                return find_in_expr(&lhs.node, comp, file, offset, sid, narrowings);
            }
            if contains(&rhs.span, offset, sid) {
                return find_in_expr(&rhs.node, comp, file, offset, sid, narrowings);
            }
            None
        }
        Expr::Unary(_, inner) => {
            if contains(&inner.span, offset, sid) {
                return find_in_expr(&inner.node, comp, file, offset, sid, narrowings);
            }
            None
        }
        Expr::Call(name, args) => {
            // Check if it's a function call
            if let Some(func) = comp.functions.iter().find(|f| &f.node.name == name) {
                return Some(format_function_call(&func.node, &comp.name));
            }
            for arg in args {
                if contains(&arg.span, offset, sid) {
                    return find_in_expr(&arg.node, comp, file, offset, sid, narrowings);
                }
            }
            None
        }
        Expr::Member(base, field) => {
            if contains(&base.span, offset, sid) {
                return find_in_expr(&base.node, comp, file, offset, sid, narrowings);
            }
            // Cursor is on the field name - provide field info based on base type
            Some(format_member_field(base, field, comp, file, offset, narrowings))
        }
        Expr::OptionalMember(base, field) => {
            if contains(&base.span, offset, sid) {
                return find_in_expr(&base.node, comp, file, offset, sid, narrowings);
            }
            // Cursor is on the field name - provide field info for optional chaining
            Some(format_optional_member_field(base, field, comp, file, offset, narrowings))
        }
        Expr::Index(base, idx) => {
            if contains(&base.span, offset, sid) {
                return find_in_expr(&base.node, comp, file, offset, sid, narrowings);
            }
            if contains(&idx.span, offset, sid) {
                return find_in_expr(&idx.node, comp, file, offset, sid, narrowings);
            }
            None
        }
        Expr::Interpolation(parts) => {
            for part in parts {
                if let InterpolationPart::Expr(e) = part {
                    if contains(&e.span, offset, sid) {
                        return find_in_expr(&e.node, comp, file, offset, sid, narrowings);
                    }
                }
            }
            None
        }
        Expr::Range { start, end, .. } => {
            if contains(&start.span, offset, sid) {
                return find_in_expr(&start.node, comp, file, offset, sid, narrowings);
            }
            if contains(&end.span, offset, sid) {
                return find_in_expr(&end.node, comp, file, offset, sid, narrowings);
            }
            None
        }
        Expr::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            if contains(&condition.span, offset, sid) {
                return find_in_expr(&condition.node, comp, file, offset, sid, narrowings);
            }
            if contains(&then_expr.span, offset, sid) {
                return find_in_expr(&then_expr.node, comp, file, offset, sid, narrowings);
            }
            if contains(&else_expr.span, offset, sid) {
                return find_in_expr(&else_expr.node, comp, file, offset, sid, narrowings);
            }
            Some(
                "```yel\ncondition ? then : else\n```\n---\nTernary conditional expression."
                    .to_string(),
            )
        }
        Expr::Closure { params: _, body } => {
            // Check statements in closure body
            for stmt in body {
                if let Some(result) = find_in_stmt(&stmt.node, comp, file, offset, sid, narrowings)
                {
                    return Some(result);
                }
            }
            Some("```yel\n{ statements }\n```\n---\nClosure expression.".to_string())
        }
        Expr::PathCall { base, member, args } => {
            // Path call like `base::member(args)`
            if contains(&base.span, offset, sid) {
                return find_in_expr(&base.node, comp, file, offset, sid, narrowings);
            }
            for arg in args {
                if contains(&arg.span, offset, sid) {
                    return find_in_expr(&arg.node, comp, file, offset, sid, narrowings);
                }
            }
            Some(format!("```yel\n::{}(...)\n```\n---\nPath call.", member))
        }
    }
}

fn find_in_stmt(
    stmt: &Statement,
    comp: &Component,
    file: &File,
    offset: usize,
    sid: SourceId,
    narrowings: &TypeNarrowings,
) -> Option<String> {
    match stmt {
        Statement::Expr(e) if contains(&e.span, offset, sid) => {
            find_in_expr(&e.node, comp, file, offset, sid, narrowings)
        }
        Statement::Assign(t, v) | Statement::CompoundAssign(t, _, v) => {
            if contains(&t.span, offset, sid) {
                return find_in_expr(&t.node, comp, file, offset, sid, narrowings);
            }
            if contains(&v.span, offset, sid) {
                return find_in_expr(&v.node, comp, file, offset, sid, narrowings);
            }
            None
        }
        Statement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            if contains(&condition.span, offset, sid) {
                return find_in_expr(&condition.node, comp, file, offset, sid, narrowings);
            }
            for s in then_branch {
                if contains(&s.span, offset, sid) {
                    return find_in_stmt(&s.node, comp, file, offset, sid, narrowings);
                }
            }
            if let Some(else_b) = else_branch {
                for s in else_b {
                    if contains(&s.span, offset, sid) {
                        return find_in_stmt(&s.node, comp, file, offset, sid, narrowings);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

// Formatting functions

fn format_record(rec: &Record) -> String {
    let mut s = format!("```yel\nrecord {}\n```\n---\n", rec.name);
    if !rec.fields.is_empty() {
        s.push_str("**Fields:**\n");
        for f in &rec.fields {
            s.push_str(&format!("- `{}`: `{}`\n", f.node.name, f.node.ty.kind));
        }
    }
    s
}

fn format_record_field(field: &RecordField, record_name: &str) -> String {
    format!(
        "```yel\n{}: {}\n```\n---\nField of `{}`.",
        field.name, field.ty.kind, record_name
    )
}

fn format_component(comp: &Component) -> String {
    let mut s = format!("```yel\ncomponent {}\n```\n---\n", comp.name);
    if !comp.properties.is_empty() {
        s.push_str("**Properties:**\n");
        for p in &comp.properties {
            s.push_str(&format!("- `{}`: `{}`\n", p.node.name, p.node.ty.kind));
        }
    }
    if !comp.functions.is_empty() {
        s.push_str("\n**Functions:**\n");
        for f in &comp.functions {
            let params: Vec<_> = f.node.params.iter().map(|(n, t)| format!("{}: {}", n, t.kind)).collect();
            let export = if f.node.is_export { "export " } else { "" };
            s.push_str(&format!("- `{}{}({})`\n", export, f.node.name, params.join(", ")));
        }
    }
    s
}

fn format_component_use(comp: &Component) -> String {
    let mut s = format!("```yel\n{}\n```\n---\nComponent instantiation.\n\n", comp.name);
    if !comp.properties.is_empty() {
        s.push_str("**Properties:**\n");
        for p in &comp.properties {
            let req = if p.node.default.is_some() { "" } else { " *(required)*" };
            s.push_str(&format!("- `{}`: `{}`{}\n", p.node.name, p.node.ty.kind, req));
        }
    }
    s
}

fn format_property(prop: &Property, comp_name: &str) -> String {
    let def = if prop.default.is_some() { " = ..." } else { "" };
    format!("```yel\n{}: {}{}\n```\n---\nProperty of `{}`.", prop.name, prop.ty.kind, def, comp_name)
}

fn format_property_ref_narrowed(prop: &Property, comp_name: &str, narrowed_ty: &ResolvedType) -> String {
    format!(
        "```yel\n{}: {}\n```\n---\nProperty of `{}`\n\n*Narrowed from `{}`*",
        prop.name,
        format_resolved_type(narrowed_ty),
        comp_name,
        prop.ty.kind
    )
}

/// Format hover info for a member field access (e.g., `obj.field`).
fn format_member_field(
    base: &Spanned<Expr>,
    field: &str,
    comp: &Component,
    file: &File,
    offset: usize,
    narrowings: &TypeNarrowings,
) -> String {
    // Infer the type of the base expression
    let base_ty = infer_expr_type(&base.node, comp, file, offset, narrowings);

    match &base_ty {
        ResolvedType::Named(type_name) => {
            // Look up in user-defined records
            if let Some(rec) = file.records.iter().find(|r| &r.node.name == type_name) {
                if let Some(field_def) = rec.node.fields.iter().find(|f| f.node.name == field) {
                    return format!(
                        "```yel\n{}: {}\n```\n---\nField of `{}`",
                        field, field_def.node.ty.kind, type_name
                    );
                }
            }
            format!(
                "```yel\n.{}\n```\n---\nField access on `{}`",
                field, type_name
            )
        }
        ResolvedType::Option(inner) => {
            match field {
                "is_some" => "```yel\nis_some: bool\n```\n---\nReturns `true` if the option contains a value.".to_string(),
                "is_none" => "```yel\nis_none: bool\n```\n---\nReturns `true` if the option is empty.".to_string(),
                "unwrap" => format!(
                    "```yel\nunwrap: {}\n```\n---\nReturns the contained value. Panics if empty.",
                    format_resolved_type(inner)
                ),
                _ => format!("```yel\n.{}\n```\n---\nField on `option`", field),
            }
        }
        ResolvedType::List(inner) => {
            match field {
                "len" => "```yel\nlen: s32\n```\n---\nReturns the number of elements.".to_string(),
                "is_empty" => "```yel\nis_empty: bool\n```\n---\nReturns `true` if the list has no elements.".to_string(),
                "first" => format!(
                    "```yel\nfirst: option<{}>\n```\n---\nReturns the first element, or `none` if empty.",
                    format_resolved_type(inner)
                ),
                "last" => format!(
                    "```yel\nlast: option<{}>\n```\n---\nReturns the last element, or `none` if empty.",
                    format_resolved_type(inner)
                ),
                _ => format!("```yel\n.{}\n```\n---\nField on `list`", field),
            }
        }
        ResolvedType::String => {
            match field {
                "len" => "```yel\nlen: s32\n```\n---\nReturns the length of the string.".to_string(),
                "is_empty" => "```yel\nis_empty: bool\n```\n---\nReturns `true` if the string is empty.".to_string(),
                _ => format!("```yel\n.{}\n```\n---\nField on `string`", field),
            }
        }
        _ => format!(
            "```yel\n.{}\n```\n---\nField access on `{}`",
            field,
            format_resolved_type(&base_ty)
        ),
    }
}

/// Format hover info for an optional member field access (e.g., `obj?.field`).
fn format_optional_member_field(
    base: &Spanned<Expr>,
    field: &str,
    comp: &Component,
    file: &File,
    offset: usize,
    narrowings: &TypeNarrowings,
) -> String {
    // Infer the type of the base expression (should be Option<T>)
    let base_ty = infer_expr_type(&base.node, comp, file, offset, narrowings);

    match &base_ty {
        ResolvedType::Option(inner) => {
            // Get the field type from the inner type
            let field_ty = infer_member_type(inner, field, file);
            let result_ty = ResolvedType::Option(Box::new(field_ty.clone()));

            match inner.as_ref() {
                ResolvedType::Named(type_name) => {
                    // Look up in user-defined records
                    if let Some(rec) = file.records.iter().find(|r| &r.node.name == type_name) {
                        if let Some(field_def) = rec.node.fields.iter().find(|f| f.node.name == field) {
                            return format!(
                                "```yel\n?.{}: {}\n```\n---\nOptional chaining on `option<{}>`. Returns `{}`.",
                                field, field_def.node.ty.kind, type_name, format_resolved_type(&result_ty)
                            );
                        }
                    }
                    format!(
                        "```yel\n?.{}: {}\n```\n---\nOptional field access on `option<{}>`",
                        field, format_resolved_type(&result_ty), type_name
                    )
                }
                _ => format!(
                    "```yel\n?.{}: {}\n```\n---\nOptional chaining. Returns `{}`.",
                    field, format_resolved_type(&result_ty), format_resolved_type(&result_ty)
                ),
            }
        }
        _ => format!(
            "```yel\n?.{}\n```\n---\nOptional chaining (base type is not `option`)",
            field
        ),
    }
}

/// Infer the type of an expression for hover purposes.
fn infer_expr_type(
    expr: &Expr,
    comp: &Component,
    file: &File,
    offset: usize,
    narrowings: &TypeNarrowings,
) -> ResolvedType {
    match expr {
        Expr::Ident(name) => {
            // Check narrowed types first
            if let Some(narrowed_ty) = narrowings.get_narrowed_type(offset, name) {
                return narrowed_ty.clone();
            }
            // Check properties
            if let Some(prop) = comp.properties.iter().find(|p| &p.node.name == name) {
                return resolve_ast_type(&prop.node.ty.kind);
            }
            ResolvedType::Unknown
        }
        Expr::Member(base, field) => {
            let base_ty = infer_expr_type(&base.node, comp, file, offset, narrowings);
            infer_member_type(&base_ty, field, file)
        }
        Expr::OptionalMember(base, field) => {
            // For optional chaining, unwrap Option, get field type, wrap in Option
            let base_ty = infer_expr_type(&base.node, comp, file, offset, narrowings);
            match base_ty {
                ResolvedType::Option(inner) => {
                    let field_ty = infer_member_type(&inner, field, file);
                    ResolvedType::Option(Box::new(field_ty))
                }
                _ => ResolvedType::Unknown,
            }
        }
        Expr::Literal(lit) => match lit {
            Literal::Int(_) => ResolvedType::S32,
            Literal::Float(_) => ResolvedType::F32,
            Literal::String(_) => ResolvedType::String,
            Literal::Bool(_) => ResolvedType::Bool,
            Literal::Color(_) => ResolvedType::Named("Color".to_string()),
            Literal::List(_) => ResolvedType::List(Box::new(ResolvedType::Unknown)),
            _ => ResolvedType::Unknown,
        },
        _ => ResolvedType::Unknown,
    }
}

/// Infer the type of a member access.
fn infer_member_type(base_ty: &ResolvedType, field: &str, file: &File) -> ResolvedType {
    match base_ty {
        ResolvedType::Named(type_name) => {
            // Look up in user-defined records
            if let Some(rec) = file.records.iter().find(|r| &r.node.name == type_name) {
                if let Some(field_def) = rec.node.fields.iter().find(|f| f.node.name == field) {
                    return resolve_ast_type(&field_def.node.ty.kind);
                }
            }
            ResolvedType::Unknown
        }
        ResolvedType::Option(inner) => match field {
            "is_some" | "is_none" => ResolvedType::Bool,
            "unwrap" => (**inner).clone(),
            _ => ResolvedType::Unknown,
        },
        ResolvedType::List(inner) => match field {
            "len" => ResolvedType::S32,
            "is_empty" => ResolvedType::Bool,
            "first" | "last" => ResolvedType::Option(inner.clone()),
            _ => ResolvedType::Unknown,
        },
        ResolvedType::String => match field {
            "len" => ResolvedType::S32,
            "is_empty" => ResolvedType::Bool,
            _ => ResolvedType::Unknown,
        },
        _ => ResolvedType::Unknown,
    }
}

/// Format a resolved type as a string.
///
/// Uses the Display impl from the compiler.
fn format_resolved_type(ty: &ResolvedType) -> String {
    ty.to_string()
}

fn format_function(func: &FunctionDecl, comp_name: &str) -> String {
    let params: Vec<_> = func.params.iter().map(|(n, t)| format!("{}: {}", n, t.kind)).collect();
    let export = if func.is_export { "export " } else { "" };
    let ret = func.return_type.as_ref().map(|t| format!(" -> {}", t.kind)).unwrap_or_default();
    format!(
        "```yel\n{}{}: func({}){}\n```\n---\nCallback in `{}`.",
        export,
        func.name,
        params.join(", "),
        ret,
        comp_name
    )
}

fn format_function_call(func: &FunctionDecl, comp_name: &str) -> String {
    let params: Vec<_> = func.params.iter().map(|(n, t)| format!("{}: {}", n, t.kind)).collect();
    let ret = func.return_type.as_ref().map(|t| format!(" -> {}", t.kind)).unwrap_or_default();
    format!(
        "```yel\n{}: func({}){}\n```\n---\nCallback call. Defined in `{}`.",
        func.name,
        params.join(", "),
        ret,
        comp_name
    )
}

fn format_type(ty: &TyKind) -> String {
    let desc = match ty {
        TyKind::Bool => "Boolean — `true` or `false`.",
        TyKind::S32 => "32-bit signed integer.",
        TyKind::S64 => "64-bit signed integer.",
        TyKind::U32 => "32-bit unsigned integer.",
        TyKind::U64 => "64-bit unsigned integer.",
        TyKind::F32 => "32-bit floating-point.",
        TyKind::F64 => "64-bit floating-point.",
        TyKind::String => "Text string. Supports `{interpolation}`.",
        TyKind::Char => "Unicode character.",
        TyKind::S8 => "8-bit signed integer.",
        TyKind::S16 => "16-bit signed integer.",
        TyKind::U8 => "8-bit unsigned integer.",
        TyKind::U16 => "16-bit unsigned integer.",
        TyKind::Length => "Length (px, pt, rem, %).",
        TyKind::PhysicalLength => "Physical length (phx).",
        TyKind::Angle => "Angle (deg, rad, turn).",
        TyKind::Duration => "Duration (ms, s).",
        TyKind::Percent => "Percentage (0-100%).",
        TyKind::RelativeFontSize => "Relative font size.",
        TyKind::Color => "Color (#rgb, #rrggbb).",
        TyKind::Brush => "Brush — color, gradient, or pattern.",
        TyKind::Image => "Image resource.",
        TyKind::Easing => "Animation easing.",
        TyKind::List(_) => "List type.",
        TyKind::Option(_) => "Optional type.",
        TyKind::Result { .. } => "Result type.",
        TyKind::Tuple(_) => "Tuple type.",
        TyKind::Func { params, return_type } => {
            let param_strs: Vec<_> = params
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, ty.kind))
                .collect();
            let ret = return_type
                .as_ref()
                .map(|t| format!(" -> {}", t.kind))
                .unwrap_or_default();
            return format!(
                "```yel\nfunc({}){}\n```\n---\nFunction/callback type.",
                param_strs.join(", "),
                ret
            );
        }
        TyKind::Named(n) => return format!("```yel\n{}\n```\n---\nNamed type.", n),
        TyKind::Unknown => "Unknown type.",
    };
    format!("```yel\n{}\n```\n---\n{}", ty, desc)
}

fn format_literal(lit: &Literal) -> String {
    match lit {
        Literal::Int(v) => format!("```yel\ns32\n```\n---\n`{}` (`0x{:X}`)", v, v),
        Literal::Float(v) => format!("```yel\nf32\n```\n---\n`{}`", v),
        Literal::String(s) => format!("```yel\nstring\n```\n---\n\"{}\"", if s.len() > 40 { &s[..37] } else { s }),
        Literal::Bool(v) => format!("```yel\nbool\n```\n---\n`{}`", v),
        Literal::Unit(v, u) => {
            let ty = match u.as_str() {
                "px"|"pt"|"rem"|"in"|"mm"|"cm" => "length",
                "phx" => "physical-length",
                "deg"|"rad"|"turn" => "angle",
                "ms"|"s" => "duration",
                "%" => "percent",
                _ => "unit",
            };
            format!("```yel\n{}\n```\n---\n`{}{}`", ty, v, u)
        }
        Literal::Color(hex) => {
            let h = hex.trim_start_matches('#');
            let (r, g, b) = match h.len() {
                3 => (u8::from_str_radix(&h[0..1], 16).unwrap_or(0)*17, u8::from_str_radix(&h[1..2], 16).unwrap_or(0)*17, u8::from_str_radix(&h[2..3], 16).unwrap_or(0)*17),
                _ => (u8::from_str_radix(h.get(0..2).unwrap_or("0"), 16).unwrap_or(0), u8::from_str_radix(h.get(2..4).unwrap_or("0"), 16).unwrap_or(0), u8::from_str_radix(h.get(4..6).unwrap_or("0"), 16).unwrap_or(0)),
            };
            format!("```yel\ncolor\n```\n---\nrgb({}, {}, {})\n`{}`", r, g, b, hex)
        }
        Literal::List(elements) => {
            format!("```yel\nlist\n```\n---\nList with {} element{}", elements.len(), if elements.len() == 1 { "" } else { "s" })
        }
        Literal::Tuple(elements) => {
            format!("```yel\ntuple\n```\n---\nTuple with {} element{}", elements.len(), if elements.len() == 1 { "" } else { "s" })
        }
        Literal::Record { fields } => {
            format!("```yel\nrecord\n```\n---\nAnonymous record literal with {} field{}", fields.len(), if fields.len() == 1 { "" } else { "s" })
        }
        Literal::Char(c) => format!("```yel\nchar\n```\n---\n`'{}'`", c),
    }
}

fn format_handler(handler_name: &str, element_name: &str, file: &File) -> Option<String> {
    // Check if element is a user-defined component
    if let Some(comp) = file.components.iter().find(|c| c.node.name == element_name) {
        if let Some(func) = comp.node.functions.iter().find(|f| f.node.name == handler_name) {
            let params: Vec<_> = func.node.params.iter()
                .map(|(n, t)| format!("{}: {}", n, t.kind))
                .collect();
            let ret = func.node.return_type.as_ref()
                .map(|t| format!(" -> {}", t.kind))
                .unwrap_or_default();
            return Some(format!(
                "```yel\n{}: func({}){}\n```\n---\nCallback on `{}`.",
                handler_name, params.join(", "), ret, element_name
            ));
        }
    }

    // TODO: stdlib lookup not available in current API
    None
}

fn format_binding(prop_name: &str, element_name: &str, file: &File) -> Option<String> {
    // Check if element is a user-defined component
    if let Some(comp) = file.components.iter().find(|c| c.node.name == element_name) {
        if let Some(prop) = comp.node.properties.iter().find(|p| p.node.name == prop_name) {
            return Some(format!(
                "```yel\n{}: {}\n```\n---\nProperty of `{}`.",
                prop_name, prop.node.ty.kind, element_name
            ));
        }
    }

    // Check if element is a stdlib builtin (stub - always returns None)
    if let Some(builtin) = stdlib::get_builtin(element_name) {
        if let Some(prop) = builtin.properties.iter().find(|p| p.name == prop_name) {
            return Some(format!(
                "```yel\n{}: {}\n```\n---\nProperty of `{}`.",
                prop_name, prop.ty.kind, element_name
            ));
        }
    }

    None
}

fn format_builtin(builtin: &stdlib::BuiltinElement) -> String {
    let mut s = format!("```yel\n{}\n```\n---\n", builtin.name);

    s.push_str("Built-in component\n\n");

    // Show main properties (not common layout props)
    let main_props: Vec<_> = builtin.properties.iter()
        .filter(|p| !is_common_prop(&p.name))
        .collect();

    if !main_props.is_empty() {
        s.push_str("**Properties:**\n");
        for p in main_props {
            s.push_str(&format!("- `{}`: `{}`\n", p.name, p.ty.kind));
        }
    }

    if !builtin.functions.is_empty() {
        s.push_str("\n**Callbacks:**\n");
        for c in &builtin.functions {
            let params: Vec<_> = c.params.iter()
                .map(|(n, t)| format!("{}: {}", n, t.kind))
                .collect();
            s.push_str(&format!("- `{}({})`\n", c.name, params.join(", ")));
        }
    }

    if !stdlib::accepts_children(&builtin.name) {
        s.push_str("\n*Does not accept children.*");
    }

    s
}

fn is_common_prop(name: &str) -> bool {
    matches!(name,
        "width" | "height" | "min-width" | "min-height" | "max-width" | "max-height" |
        "padding" | "margin" | "visible" | "opacity" |
        "background" | "border-color" | "border-width" | "border-radius"
    )
}

/// Format hover for an enum case (builtin or user-defined).
fn format_enum_case(case_name: &str, file: &File) -> Option<String> {
    // Check builtin option/result cases FIRST (highest priority)
    match case_name {
        "none" => return Some("```yel\noption.none\n```\n---\n`option<T>` empty case".to_string()),
        "some" => return Some("```yel\noption.some(T)\n```\n---\n`option<T>` value case".to_string()),
        "ok" => return Some("```yel\nresult.ok(T)\n```\n---\n`result<T, E>` success case".to_string()),
        "err" => return Some("```yel\nresult.err(E)\n```\n---\n`result<T, E>` error case".to_string()),
        _ => {}
    }
    // Check builtin enums (stub - always empty)
    for builtin_enum in stdlib::builtin_enums() {
        if builtin_enum.cases.iter().any(|c| c == case_name) {
            return Some(format!(
                "```yel\n{}.{}\n```\n---\n`{}` enum value",
                builtin_enum.name, case_name, builtin_enum.name
            ));
        }
    }
    // Check user-defined enums
    for enum_def in &file.enums {
        if enum_def.node.cases.iter().any(|c| c.node == case_name) {
            return Some(format!(
                "```yel\n{}.{}\n```\n---\n`{}` enum value",
                enum_def.node.name, case_name, enum_def.node.name
            ));
        }
    }
    None
}

/// Format hover for a variant case (builtin or user-defined).
fn format_variant_case(case_name: &str, file: &File) -> Option<String> {
    // Skip option/result cases - handled in format_enum_case
    if matches!(case_name, "none" | "some" | "ok" | "err") {
        return None;
    }
    // Check builtin variants (stub - always empty)
    for builtin_variant in stdlib::builtin_variants() {
        if let Some(case) = builtin_variant.cases.iter().find(|c| c.name == case_name) {
            let payload = case.payload.as_ref()
                .map(|p| format!("({})", p))
                .unwrap_or_default();
            return Some(format!(
                "```yel\n{}.{}{}\n```\n---\n`{}` variant case",
                builtin_variant.name, case_name, payload, builtin_variant.name
            ));
        }
    }
    // Check user-defined variants
    for variant_def in &file.variants {
        if let Some(case) = variant_def.node.cases.iter().find(|c| c.node.name == case_name) {
            let payload = case.node.payload.as_ref()
                .map(|p| format!("({})", p.kind))
                .unwrap_or_default();
            return Some(format!(
                "```yel\n{}.{}{}\n```\n---\n`{}` variant case",
                variant_def.node.name, case_name, payload, variant_def.node.name
            ));
        }
    }
    None
}
