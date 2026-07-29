//! One visitor owns recursion over the AST — anti-spec A3.
//!
//! `Visitor::visit_*` defaults to the free `walk_*` function of the same name;
//! a pass overrides the arms it cares about and calls `walk_*` to continue.
//!
//! **Every `walk_*` match is exhaustive with no `_` arm.** Adding an AST
//! variant is therefore a compile error in exactly one file — the one that has
//! to learn the new shape — rather than a silently skipped subtree.
//!
//! # Holes are visited
//!
//! A [`Recovered::Missing`] position routes to [`Visitor::visit_error`], which
//! is what makes "every recovery point has an `Error` node" a property the tree
//! *carries* rather than one the parser promises. [`ErrorNodeCounter`] is the
//! measurement the corpus tests assert on.

use super::*;

/// Grow the stack when less than this remains. Sized well above the measured
/// per-link cost of the walk (~2.6 KB in a debug build), so the check cannot be
/// passed by a frame that then overflows.
const RED_ZONE: usize = 256 * 1024;
/// How much stack to allocate when the red zone is entered.
const NEW_SEGMENT: usize = 2 * 1024 * 1024;

#[allow(unused_variables)]
pub trait Visitor: Sized {
    fn visit_file(&mut self, node: &File) {
        walk_file(self, node);
    }
    fn visit_item(&mut self, node: &ItemKind) {
        walk_item(self, node);
    }
    fn visit_ident(&mut self, node: &Ident) {}
    fn visit_package_decl(&mut self, node: &PackageDecl) {
        walk_package_decl(self, node);
    }
    fn visit_record_decl(&mut self, node: &RecordDecl) {
        walk_record_decl(self, node);
    }
    fn visit_record_field(&mut self, node: &RecordField) {
        walk_record_field(self, node);
    }
    fn visit_enum_decl(&mut self, node: &EnumDecl) {
        walk_enum_decl(self, node);
    }
    fn visit_variant_decl(&mut self, node: &VariantDecl) {
        walk_variant_decl(self, node);
    }
    fn visit_variant_case(&mut self, node: &VariantCase) {
        walk_variant_case(self, node);
    }
    fn visit_element_decl(&mut self, node: &ElementDecl) {
        walk_element_decl(self, node);
    }
    fn visit_extern_component_decl(&mut self, node: &ExternComponentDecl) {
        walk_extern_component_decl(self, node);
    }
    fn visit_extern_member(&mut self, node: &ExternMember) {
        walk_extern_member(self, node);
    }
    fn visit_global_decl(&mut self, node: &GlobalDecl) {
        walk_global_decl(self, node);
    }
    fn visit_global_member(&mut self, node: &GlobalMember) {
        walk_global_member(self, node);
    }
    fn visit_global_property(&mut self, node: &GlobalProperty) {
        walk_global_property(self, node);
    }
    fn visit_component_decl(&mut self, node: &ComponentDecl) {
        walk_component_decl(self, node);
    }
    fn visit_component_member(&mut self, node: &ComponentMember) {
        walk_component_member(self, node);
    }
    fn visit_property_decl(&mut self, node: &PropertyDecl) {
        walk_property_decl(self, node);
    }
    fn visit_function_decl(&mut self, node: &FunctionDecl) {
        walk_function_decl(self, node);
    }
    fn visit_func_signature(&mut self, node: &FuncSignature) {
        walk_func_signature(self, node);
    }
    fn visit_func_param(&mut self, node: &FuncParam) {
        walk_func_param(self, node);
    }
    fn visit_type_param(&mut self, node: &TypeParam) {
        walk_type_param(self, node);
    }
    fn visit_type_ref(&mut self, node: &TypeRef) {
        walk_type_ref(self, node);
    }
    fn visit_ui_node(&mut self, node: &UiNode) {
        walk_ui_node(self, node);
    }
    fn visit_element_node(&mut self, node: &ElementNode) {
        walk_element_node(self, node);
    }
    fn visit_named_prop(&mut self, node: &NamedProp) {
        walk_named_prop(self, node);
    }
    fn visit_text_node(&mut self, node: &TextNode) {
        walk_text_node(self, node);
    }
    fn visit_if_node(&mut self, node: &IfNode) {
        walk_if_node(self, node);
    }
    fn visit_else_if_branch(&mut self, node: &ElseIfBranch) {
        walk_else_if_branch(self, node);
    }
    fn visit_for_node(&mut self, node: &ForNode) {
        walk_for_node(self, node);
    }
    fn visit_stmt(&mut self, node: &Stmt) {
        walk_stmt(self, node);
    }
    fn visit_let_stmt(&mut self, node: &LetStmt) {
        walk_let_stmt(self, node);
    }
    fn visit_if_stmt(&mut self, node: &IfStmt) {
        walk_if_stmt(self, node);
    }
    fn visit_assign_stmt(&mut self, node: &AssignStmt) {
        walk_assign_stmt(self, node);
    }
    fn visit_expr_stmt(&mut self, node: &ExprStmt) {
        walk_expr_stmt(self, node);
    }
    fn visit_expr(&mut self, node: &Expr) {
        walk_expr(self, node);
    }
    fn visit_interpolation_part(&mut self, node: &InterpolationPart) {
        walk_interpolation_part(self, node);
    }
    fn visit_record_field_init(&mut self, node: &RecordFieldInit) {
        walk_record_field_init(self, node);
    }
    fn visit_closure_expr(&mut self, node: &ClosureExpr) {
        walk_closure_expr(self, node);
    }
    fn visit_closure_param(&mut self, node: &ClosureParam) {
        walk_closure_param(self, node);
    }
    /// Called for every recovery node in the tree, whatever its category —
    /// including every [`Recovered::Missing`] hole.
    fn visit_error(&mut self, id: NodeId, span: Span) {
        let _ = (id, span);
    }
}

/// Walk a position that may be a hole.
///
/// The `Missing` arm is the *only* way a hole is reachable from a walk, so it
/// cannot be forgotten at a call site.
pub fn walk_recovered<V: Visitor, T>(
    visitor: &mut V,
    node: &Recovered<T>,
    walk_present: impl FnOnce(&mut V, &T),
) {
    match node {
        Recovered::Present(value) => walk_present(visitor, value),
        Recovered::Missing { id, span } => visitor.visit_error(*id, *span),
    }
}

fn walk_maybe_ident<V: Visitor>(visitor: &mut V, node: &MaybeIdent) {
    walk_recovered(visitor, node, |visitor, ident| visitor.visit_ident(ident));
}

fn walk_block<V: Visitor, T>(
    visitor: &mut V,
    node: &Block<T>,
    mut walk_item: impl FnMut(&mut V, &T),
) {
    walk_recovered(visitor, node, |visitor, items| {
        for item in items {
            walk_item(visitor, item);
        }
    });
}

pub fn walk_file<V: Visitor>(v: &mut V, node: &File) {
    for item in &node.items {
        v.visit_item(item);
    }
    // Recovery positions with no slot in the typed tree — see `RecoveryMark`.
    // Visited here and nowhere else, so a pass that overrides `visit_error`
    // sees them without any construct having to lie about its arity.
    for mark in &node.recovery_marks {
        v.visit_error(mark.id, mark.span);
    }
}

pub fn walk_item<V: Visitor>(v: &mut V, node: &ItemKind) {
    match node {
        ItemKind::Package(it) => v.visit_package_decl(it),
        ItemKind::Record(it) => v.visit_record_decl(it),
        ItemKind::Enum(it) => v.visit_enum_decl(it),
        ItemKind::Variant(it) => v.visit_variant_decl(it),
        ItemKind::Element(it) => v.visit_element_decl(it),
        ItemKind::ExternComponent(it) => v.visit_extern_component_decl(it),
        ItemKind::Global(it) => v.visit_global_decl(it),
        ItemKind::Component(it) => v.visit_component_decl(it),
        ItemKind::Error { id, span } => v.visit_error(*id, *span),
    }
}

pub fn walk_package_decl<V: Visitor>(v: &mut V, node: &PackageDecl) {
    walk_maybe_ident(v, &node.namespace);
    walk_maybe_ident(v, &node.name);
}

pub fn walk_record_decl<V: Visitor>(v: &mut V, node: &RecordDecl) {
    walk_maybe_ident(v, &node.name);
    for field in &node.fields {
        walk_recovered(v, field, |v, field| v.visit_record_field(field));
    }
}

pub fn walk_record_field<V: Visitor>(v: &mut V, node: &RecordField) {
    walk_maybe_ident(v, &node.name);
    v.visit_type_ref(&node.ty);
}

pub fn walk_enum_decl<V: Visitor>(v: &mut V, node: &EnumDecl) {
    walk_maybe_ident(v, &node.name);
    for case in &node.cases {
        walk_maybe_ident(v, case);
    }
}

pub fn walk_variant_decl<V: Visitor>(v: &mut V, node: &VariantDecl) {
    walk_maybe_ident(v, &node.name);
    for case in &node.cases {
        walk_recovered(v, case, |v, case| v.visit_variant_case(case));
    }
}

pub fn walk_variant_case<V: Visitor>(v: &mut V, node: &VariantCase) {
    walk_maybe_ident(v, &node.name);
    if let Some(payload) = &node.payload {
        v.visit_type_ref(payload);
    }
}

pub fn walk_element_decl<V: Visitor>(v: &mut V, node: &ElementDecl) {
    walk_maybe_ident(v, &node.name);
    for member in &node.members {
        walk_recovered(v, member, |v, property| v.visit_property_decl(property));
    }
}

pub fn walk_extern_component_decl<V: Visitor>(v: &mut V, node: &ExternComponentDecl) {
    walk_maybe_ident(v, &node.name);
    for member in &node.members {
        v.visit_extern_member(member);
    }
}

pub fn walk_extern_member<V: Visitor>(v: &mut V, node: &ExternMember) {
    match node {
        ExternMember::Property(property) => v.visit_property_decl(property),
        ExternMember::Method(method) => v.visit_function_decl(method),
        ExternMember::Children { .. } => {}
        ExternMember::Error { id, span } => v.visit_error(*id, *span),
    }
}

pub fn walk_global_decl<V: Visitor>(v: &mut V, node: &GlobalDecl) {
    walk_maybe_ident(v, &node.name);
    for member in &node.members {
        v.visit_global_member(member);
    }
}

pub fn walk_global_member<V: Visitor>(v: &mut V, node: &GlobalMember) {
    match node {
        GlobalMember::Property(property) => v.visit_global_property(property),
        GlobalMember::Callback(callback) => v.visit_function_decl(callback),
        GlobalMember::Error { id, span } => v.visit_error(*id, *span),
    }
}

pub fn walk_global_property<V: Visitor>(v: &mut V, node: &GlobalProperty) {
    walk_maybe_ident(v, &node.name);
    v.visit_type_ref(&node.ty);
    if let Some(default) = &node.default {
        v.visit_expr(default);
    }
}

pub fn walk_component_decl<V: Visitor>(v: &mut V, node: &ComponentDecl) {
    walk_maybe_ident(v, &node.name);
    for member in &node.members {
        v.visit_component_member(member);
    }
}

pub fn walk_component_member<V: Visitor>(v: &mut V, node: &ComponentMember) {
    match node {
        ComponentMember::Property(property) => v.visit_property_decl(property),
        ComponentMember::Function(function) => v.visit_function_decl(function),
        ComponentMember::Node(ui_node) => v.visit_ui_node(ui_node),
        ComponentMember::Error { id, span } => v.visit_error(*id, *span),
    }
}

pub fn walk_property_decl<V: Visitor>(v: &mut V, node: &PropertyDecl) {
    walk_maybe_ident(v, &node.name);
    v.visit_type_ref(&node.ty);
    if let Some(default) = &node.default {
        v.visit_expr(default);
    }
}

pub fn walk_function_decl<V: Visitor>(v: &mut V, node: &FunctionDecl) {
    walk_maybe_ident(v, &node.name);
    walk_recovered(v, &node.signature, |v, signature| {
        v.visit_func_signature(signature)
    });
}

pub fn walk_func_signature<V: Visitor>(v: &mut V, node: &FuncSignature) {
    // Type parameters first: they are in scope for everything below.
    for param in &node.type_params {
        walk_recovered(v, param, |v, param| v.visit_type_param(param));
    }
    walk_recovered(v, &node.params, |v, params| {
        for param in params {
            walk_recovered(v, param, |v, param| v.visit_func_param(param));
        }
    });
    if let Some(ret) = &node.return_type {
        v.visit_type_ref(ret);
    }
}

/// A type parameter's only child is its name, which may be a recovery hole.
pub fn walk_type_param<V: Visitor>(v: &mut V, node: &TypeParam) {
    walk_recovered(v, &node.name, |v, name| v.visit_ident(name));
}

pub fn walk_func_param<V: Visitor>(v: &mut V, node: &FuncParam) {
    walk_maybe_ident(v, &node.name);
    v.visit_type_ref(&node.ty);
}

pub fn walk_type_ref<V: Visitor>(v: &mut V, node: &TypeRef) {
    match &node.kind {
        TypeKind::Primitive(_) => {}
        TypeKind::Named(_) => {}
        TypeKind::List(inner) => v.visit_type_ref(inner),
        TypeKind::Option(inner) => v.visit_type_ref(inner),
        TypeKind::Result { args } => {
            for arg in args {
                v.visit_type_ref(arg);
            }
        }
        TypeKind::Tuple(items) => {
            for item in items {
                v.visit_type_ref(item);
            }
        }
        TypeKind::Func(signature) => v.visit_func_signature(signature),
        TypeKind::Error => v.visit_error(node.id, node.span),
    }
}

pub fn walk_ui_node<V: Visitor>(v: &mut V, node: &UiNode) {
    match node {
        UiNode::Element(n) => v.visit_element_node(n),
        UiNode::Text(n) => v.visit_text_node(n),
        UiNode::If(n) => v.visit_if_node(n),
        UiNode::For(n) => v.visit_for_node(n),
        UiNode::Children { .. } => {}
        UiNode::Error { id, span } => v.visit_error(*id, *span),
    }
}

pub fn walk_element_node<V: Visitor>(v: &mut V, node: &ElementNode) {
    walk_maybe_ident(v, &node.name);
    for prop in &node.props {
        v.visit_named_prop(prop);
    }
    for child in &node.children {
        v.visit_ui_node(child);
    }
}

pub fn walk_named_prop<V: Visitor>(v: &mut V, node: &NamedProp) {
    walk_maybe_ident(v, &node.name);
    v.visit_expr(&node.value);
}

pub fn walk_text_node<V: Visitor>(v: &mut V, node: &TextNode) {
    v.visit_expr(&node.content);
}

pub fn walk_if_node<V: Visitor>(v: &mut V, node: &IfNode) {
    v.visit_expr(&node.condition);
    walk_block(v, &node.then_branch, |v, child| v.visit_ui_node(child));
    for branch in &node.else_if_branches {
        v.visit_else_if_branch(branch);
    }
    if let Some(else_branch) = &node.else_branch {
        walk_block(v, else_branch, |v, child| v.visit_ui_node(child));
    }
}

pub fn walk_else_if_branch<V: Visitor>(v: &mut V, node: &ElseIfBranch) {
    v.visit_expr(&node.condition);
    walk_block(v, &node.body, |v, child| v.visit_ui_node(child));
}

pub fn walk_for_node<V: Visitor>(v: &mut V, node: &ForNode) {
    walk_maybe_ident(v, &node.item);
    v.visit_expr(&node.iterable);
    if let Some(key) = &node.key {
        v.visit_expr(key);
    }
    walk_block(v, &node.body, |v, child| v.visit_ui_node(child));
}

pub fn walk_stmt<V: Visitor>(v: &mut V, node: &Stmt) {
    match node {
        Stmt::Let(s) => v.visit_let_stmt(s),
        Stmt::If(s) => v.visit_if_stmt(s),
        Stmt::Assign(s) => v.visit_assign_stmt(s),
        Stmt::Expr(s) => v.visit_expr_stmt(s),
        Stmt::Error { id, span } => v.visit_error(*id, *span),
    }
}

pub fn walk_let_stmt<V: Visitor>(v: &mut V, node: &LetStmt) {
    walk_maybe_ident(v, &node.name);
    if let Some(ty) = &node.ty {
        v.visit_type_ref(ty);
    }
    v.visit_expr(&node.value);
}

pub fn walk_if_stmt<V: Visitor>(v: &mut V, node: &IfStmt) {
    v.visit_expr(&node.condition);
    walk_block(v, &node.then_branch, |v, stmt| v.visit_stmt(stmt));
    if let Some(else_branch) = &node.else_branch {
        walk_block(v, else_branch, |v, stmt| v.visit_stmt(stmt));
    }
}

pub fn walk_assign_stmt<V: Visitor>(v: &mut V, node: &AssignStmt) {
    v.visit_expr(&node.target);
    v.visit_expr(&node.value);
}

pub fn walk_expr_stmt<V: Visitor>(v: &mut V, node: &ExprStmt) {
    v.visit_expr(&node.expr);
}

/// # The one walk that must survive an unbounded structure
///
/// `parse_binary` and `parse_postfix` are **loops**, so `a.b.b.b…` and
/// `1 + 1 + 1…` build an `Expr` spine as long as the user cares to type while
/// the parser's own `MAX_NESTING_DEPTH` counter reads 2 (anti-spec A11). Every
/// other recursive edge out of an expression passes through a guarded `parse_*`
/// entry point and is bounded at 256.
///
/// A worklist here cannot help: the recursion runs
/// `walk_expr → v.visit_expr → walk_expr`, through a hook an arbitrary pass may
/// override, so flattening the spine locally would stop calling that hook on
/// spine nodes — the silent skip anti-spec A3 forbids. [`stacker::maybe_grow`]
/// is the mechanism rustc uses for exactly this shape
/// (`rustc_data_structures::stack::ensure_sufficient_stack`): when less than
/// [`RED_ZONE`] of stack remains, the walk continues on a freshly allocated
/// segment, on the same thread, with no bound on the visitor type.
///
/// Measured, debug `cargo test` thread, `component A { x: s32 = a.b.b…; }`:
/// this aborted at n ≈ 3,126 before the guard, and completes at n = 200,000
/// after it. The frozen pest parser `abort()`s on the same inputs at n ≈ 14,544,
/// so a bound below that would have been a language narrowing.
pub fn walk_expr<V: Visitor>(v: &mut V, node: &Expr) {
    stacker::maybe_grow(RED_ZONE, NEW_SEGMENT, || walk_expr_inner(v, node))
}

fn walk_expr_inner<V: Visitor>(v: &mut V, node: &Expr) {
    match &node.kind {
        ExprKind::Int(_) => {}
        ExprKind::Float(_) => {}
        ExprKind::Unit { .. } => {}
        ExprKind::Color(_) => {}
        ExprKind::Char(_) => {}
        ExprKind::Bool(_) => {}
        ExprKind::String(_) => {}
        ExprKind::Interpolation(parts) => {
            for part in parts {
                v.visit_interpolation_part(part);
            }
        }
        ExprKind::List(items) => {
            for item in items {
                v.visit_expr(item);
            }
        }
        ExprKind::Tuple(items) => {
            for item in items {
                v.visit_expr(item);
            }
        }
        ExprKind::Record(fields) => {
            for field in fields {
                walk_recovered(v, field, |v, field| v.visit_record_field_init(field));
            }
        }
        ExprKind::Closure(closure) => v.visit_closure_expr(closure),
        ExprKind::Ident(_) => {}
        ExprKind::Unary { op: _, operand } => v.visit_expr(operand),
        ExprKind::Binary { op: _, lhs, rhs } => {
            v.visit_expr(lhs);
            v.visit_expr(rhs);
        }
        ExprKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            v.visit_expr(condition);
            v.visit_expr(then_expr);
            v.visit_expr(else_expr);
        }
        ExprKind::Range {
            start,
            end,
            inclusive: _,
        } => {
            v.visit_expr(start);
            v.visit_expr(end);
        }
        ExprKind::Call { callee, args } => {
            v.visit_ident(callee);
            for arg in args {
                v.visit_expr(arg);
            }
        }
        ExprKind::PathCall { base, member, args } => {
            v.visit_expr(base);
            walk_maybe_ident(v, member);
            for arg in args {
                v.visit_expr(arg);
            }
        }
        ExprKind::Member { base, member } => {
            v.visit_expr(base);
            walk_maybe_ident(v, member);
        }
        ExprKind::OptionalMember { base, member } => {
            v.visit_expr(base);
            walk_maybe_ident(v, member);
        }
        ExprKind::Index { base, index } => {
            v.visit_expr(base);
            v.visit_expr(index);
        }
        ExprKind::Error => v.visit_error(node.id, node.span),
    }
}

pub fn walk_interpolation_part<V: Visitor>(v: &mut V, node: &InterpolationPart) {
    match node {
        InterpolationPart::Literal(_) => {}
        InterpolationPart::Expr(expr) => v.visit_expr(expr),
    }
}

pub fn walk_record_field_init<V: Visitor>(v: &mut V, node: &RecordFieldInit) {
    walk_maybe_ident(v, &node.name);
    v.visit_expr(&node.value);
}

pub fn walk_closure_expr<V: Visitor>(v: &mut V, node: &ClosureExpr) {
    for param in &node.params {
        walk_recovered(v, param, |v, param| v.visit_closure_param(param));
    }
    for stmt in &node.body {
        v.visit_stmt(stmt);
    }
}

pub fn walk_closure_param<V: Visitor>(v: &mut V, node: &ClosureParam) {
    walk_maybe_ident(v, &node.name);
    if let Some(ty) = &node.ty {
        v.visit_type_ref(ty);
    }
}

/// Counts every recovery node in a parsed file — `Error` variants and
/// `Recovered::Missing` holes alike.
///
/// This is the measurement invariant S5 is asserted with: a corpus program that
/// produces a non-zero count is a grammar regression, and an ill-formed input
/// that produces zero while emitting a diagnostic is an S5 violation.
#[derive(Default)]
pub struct ErrorNodeCounter {
    pub count: usize,
    pub first_span: Option<Span>,
    /// Every recovery span, in visit order. The **per-construct** half of S5 is
    /// asserted against these rather than against `count`: a file-level count
    /// lets a report-without-mark and a mark-without-report cancel out
    /// (anti-spec A12).
    pub spans: Vec<Span>,
}

impl Visitor for ErrorNodeCounter {
    fn visit_error(&mut self, _id: NodeId, span: Span) {
        self.count += 1;
        self.first_span.get_or_insert(span);
        self.spans.push(span);
    }
}

impl ErrorNodeCounter {
    pub fn run(file: &File) -> ErrorNodeCounter {
        let mut counter = ErrorNodeCounter::default();
        counter.visit_file(file);
        counter
    }
}
