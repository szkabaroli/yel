//! Debug dumps of the syntax stage: the green tree (`--emit-green`) and the
//! typed AST (`--emit-ast`), one indented line per node.
//!
//! Nothing in `tests/` asserts on this text — it is a debugging instrument,
//! free to change shape.

use std::fmt::Write as _;

use yelc_base::{NameInterner, Span};
use yelc_syntax::NodeId;
use yelc_syntax::ast::visit::{self, Visitor};
use yelc_syntax::ast::*;
use yelc_syntax::green::GreenNode;

// ---------------------------------------------------------------------------
// Green tree
// ---------------------------------------------------------------------------

/// Kinds and widths, trivia included.
pub fn green_tree(node: &GreenNode) -> String {
    let mut out = String::new();
    green_node(node, 0, &mut out);
    out
}

fn green_node(node: &GreenNode, depth: usize, out: &mut String) {
    let _ = writeln!(
        out,
        "{:>pad$}{:?} {}",
        "",
        node.kind(),
        node.len(),
        pad = depth * 2
    );
    for child in node.children() {
        match child.to_node() {
            Some(inner) => green_node(&inner, depth + 1, out),
            None => {
                let _ = writeln!(out, "{:>pad$}{:?}", "", child.kind(), pad = (depth + 1) * 2);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Typed AST
// ---------------------------------------------------------------------------

/// Walk the typed AST, one indented line per node.
///
/// `filter` restricts the dump to a single top-level item, matched on its
/// name: the whole AST of a real file is not readable, and the question is
/// almost always about one declaration.
pub fn ast(
    file: &File,
    interner: &NameInterner,
    filter: Option<&str>,
    identified: bool,
    spans: bool,
) -> String {
    let mut dumper = Dumper {
        out: String::new(),
        depth: 0,
        interner,
        filter,
        identified,
        spans,
    };
    dumper.visit_file(file);
    for mark in &file.recovery_marks {
        dumper.line("RecoveryMark", mark.id, mark.span, "");
    }
    dumper.out
}

struct Dumper<'a> {
    out: String,
    depth: usize,
    interner: &'a NameInterner,
    filter: Option<&'a str>,
    identified: bool,
    spans: bool,
}

impl Dumper<'_> {
    fn line(&mut self, label: &str, id: NodeId, span: Span, extra: &str) {
        let _ = write!(self.out, "{:>pad$}{label}", "", pad = self.depth * 2);
        if !extra.is_empty() {
            let _ = write!(self.out, " {extra}");
        }
        if self.identified {
            let _ = write!(self.out, " #{}", id.0);
        }
        if self.spans {
            let _ = write!(self.out, " @{}..{}", span.start, span.end);
        }
        self.out.push('\n');
    }
}

/// Generates one `visit_*` override per node type that carries `id` + `span`:
/// print a line, indent, continue the walk, outdent.
///
/// A macro rather than 31 hand-written bodies, and *every* such type is listed
/// — a dump that silently omits a node type is worse than no dump, because the
/// reader concludes the node is not in the tree. `visit.rs`'s exhaustive matches
/// mean a new AST variant is a compile error there; this list is the reason it
/// also has to be added here.
macro_rules! dump_nodes {
    ($($visit:ident($ty:ident) via $walk:ident;)*) => {
        $(
            fn $visit(&mut self, node: &$ty) {
                self.line(stringify!($ty), node.id, node.span, &label(node));
                self.depth += 1;
                visit::$walk(self, node);
                self.depth -= 1;
            }
        )*
    };
}

impl Visitor for Dumper<'_> {
    fn visit_file(&mut self, node: &File) {
        self.line("File", node.id, node.span, "");
        self.depth += 1;
        for item in &node.items {
            if self.keeps(item) {
                self.visit_item(item);
            }
        }
        self.depth -= 1;
    }

    fn visit_ident(&mut self, node: &Ident) {
        let name = self.interner.str(node.name).to_string();
        self.line("Ident", node.id, node.span, &name);
    }

    fn visit_error(&mut self, id: NodeId, span: Span) {
        self.line("Error", id, span, "");
    }

    fn visit_ui_node(&mut self, node: &UiNode) {
        // `Children` is the one UI position with no struct behind it, so the
        // generated arms cannot reach it and it would vanish from the dump.
        if let UiNode::Children { id, span } = node {
            self.line("Children", *id, *span, "");
            return;
        }
        visit::walk_ui_node(self, node);
    }

    fn visit_interpolation_part(&mut self, node: &InterpolationPart) {
        if let InterpolationPart::Literal(name) = node {
            let text = self.interner.str(*name).to_string();
            let _ = writeln!(
                self.out,
                "{:>pad$}Literal {text:?}",
                "",
                pad = self.depth * 2
            );
            return;
        }
        visit::walk_interpolation_part(self, node);
    }

    dump_nodes! {
        visit_attribute_list(AttributeList) via walk_attribute_list;
        visit_attribute(Attribute) via walk_attribute;
        visit_attribute_arg(AttributeArg) via walk_attribute_arg;
        visit_package_decl(PackageDecl) via walk_package_decl;
        visit_record_decl(RecordDecl) via walk_record_decl;
        visit_record_field(RecordField) via walk_record_field;
        visit_enum_decl(EnumDecl) via walk_enum_decl;
        visit_variant_decl(VariantDecl) via walk_variant_decl;
        visit_variant_case(VariantCase) via walk_variant_case;
        visit_element_decl(ElementDecl) via walk_element_decl;
        visit_extern_component_decl(ExternComponentDecl) via walk_extern_component_decl;
        visit_global_decl(GlobalDecl) via walk_global_decl;
        visit_global_property(GlobalProperty) via walk_global_property;
        visit_component_decl(ComponentDecl) via walk_component_decl;
        visit_property_decl(PropertyDecl) via walk_property_decl;
        visit_function_decl(FunctionDecl) via walk_function_decl;
        visit_func_signature(FuncSignature) via walk_func_signature;
        visit_func_param(FuncParam) via walk_func_param;
        visit_type_param(TypeParam) via walk_type_param;
        visit_type_ref(TypeRef) via walk_type_ref;
        visit_element_node(ElementNode) via walk_element_node;
        visit_named_prop(NamedProp) via walk_named_prop;
        visit_text_node(TextNode) via walk_text_node;
        visit_if_node(IfNode) via walk_if_node;
        visit_else_if_branch(ElseIfBranch) via walk_else_if_branch;
        visit_for_node(ForNode) via walk_for_node;
        visit_block(Block) via walk_block;
        visit_let_stmt(LetStmt) via walk_let_stmt;
        visit_if_stmt(IfStmt) via walk_if_stmt;
        visit_return_stmt(ReturnStmt) via walk_return_stmt;
        visit_assign_stmt(AssignStmt) via walk_assign_stmt;
        visit_expr_stmt(ExprStmt) via walk_expr_stmt;
        visit_expr(Expr) via walk_expr;
        visit_record_field_init(RecordFieldInit) via walk_record_field_init;
        visit_closure_expr(ClosureExpr) via walk_closure_expr;
        visit_closure_param(ClosureParam) via walk_closure_param;
    }
}

impl Dumper<'_> {
    /// Does this top-level item pass `--emit-ast=<ITEM>`?
    ///
    /// Matched on the item's **first** `Ident`, which for every `ItemKind` is
    /// its declared name. Reading it off the walk rather than off a match over
    /// `ItemKind` keeps the driver from carrying a second copy of the item list
    /// that would silently go stale.
    fn keeps(&self, item: &ItemKind) -> bool {
        let Some(filter) = self.filter else {
            return true;
        };
        let mut first = FirstIdent(None);
        visit::walk_item(&mut first, item);
        first
            .0
            .is_some_and(|name| &*self.interner.str(name) == filter)
    }
}

struct FirstIdent(Option<yelc_base::Name>);

impl Visitor for FirstIdent {
    fn visit_ident(&mut self, node: &Ident) {
        self.0.get_or_insert(node.name);
    }
}

// ---------------------------------------------------------------------------
// Kind labels
// ---------------------------------------------------------------------------
//
// `Expr` and `TypeRef` are single structs over a big kind enum, so the struct
// name alone carries no information. Written as matches rather than scraped out
// of `{:?}` — a `Debug` string is a representation, and reading structure out of
// one is the shape anti-spec A3 is about.

trait Labelled {
    fn label(&self) -> String;
}

fn label<T: Labelled>(node: &T) -> String {
    node.label()
}

impl Labelled for Expr {
    fn label(&self) -> String {
        let kind = match &self.kind {
            ExprKind::Int(_) => "Int",
            ExprKind::Float(_) => "Float",
            ExprKind::Unit { .. } => "Unit",
            ExprKind::Color(_) => "Color",
            ExprKind::Char(_) => "Char",
            ExprKind::Bool(_) => "Bool",
            ExprKind::String(_) => "String",
            ExprKind::Interpolation(_) => "Interpolation",
            ExprKind::List(_) => "List",
            ExprKind::Tuple(_) => "Tuple",
            ExprKind::Record { .. } => "Record",
            ExprKind::Closure(_) => "Closure",
            ExprKind::Ident(_) => "Ident",
            ExprKind::Unary { .. } => "Unary",
            ExprKind::Binary { .. } => "Binary",
            ExprKind::Ternary { .. } => "Ternary",
            ExprKind::Range { .. } => "Range",
            ExprKind::Call { .. } => "Call",
            ExprKind::PathCall { .. } => "PathCall",
            ExprKind::Member { .. } => "Member",
            ExprKind::OptionalMember { .. } => "OptionalMember",
            ExprKind::Index { .. } => "Index",
            ExprKind::Match(_) => "Match",
            ExprKind::Error => "Error",
        };
        kind.to_string()
    }
}

impl Labelled for TypeRef {
    fn label(&self) -> String {
        let kind = match &self.kind {
            TypeKind::Primitive(_) => "Primitive",
            TypeKind::Named(_) => "Named",
            TypeKind::List(_) => "List",
            TypeKind::Option(_) => "Option",
            TypeKind::Result { .. } => "Result",
            TypeKind::Tuple(_) => "Tuple",
            TypeKind::Func(_) => "Func",
            TypeKind::Error => "Error",
        };
        kind.to_string()
    }
}

macro_rules! unlabelled {
    ($($ty:ident),* $(,)?) => {
        $(impl Labelled for $ty {
            fn label(&self) -> String { String::new() }
        })*
    };
}

unlabelled!(
    AttributeList,
    Attribute,
    AttributeArg,
    PackageDecl,
    RecordDecl,
    RecordField,
    EnumDecl,
    VariantDecl,
    VariantCase,
    ElementDecl,
    ExternComponentDecl,
    GlobalDecl,
    GlobalProperty,
    ComponentDecl,
    PropertyDecl,
    FunctionDecl,
    FuncSignature,
    FuncParam,
    TypeParam,
    ElementNode,
    NamedProp,
    TextNode,
    IfNode,
    ElseIfBranch,
    ForNode,
    Block,
    LetStmt,
    IfStmt,
    ReturnStmt,
    AssignStmt,
    ExprStmt,
    RecordFieldInit,
    ClosureExpr,
    ClosureParam,
);
