//! Typed AST — the view stage 2 consumes.
//!
//! # Contract
//!
//! - Every node carries `NodeId` + `Span` (invariant S3).
//! - Names are interned `Name`, never `String` (invariant S4).
//! - `Error` variants exist at every recovery point (invariant S5).
//!
//! The node *set* is stage 1's to design against the frozen grammar
//! (`yel-core/src/syntax/grammar.pest`) — AST node shapes are explicitly free in
//! `plans/rewrite/scope.md`. Only `File` and the invariants above are the seam.
//!
//! `visit.rs` carries the walk/visit split: a `Visitor` trait whose `visit_*`
//! methods default to free `walk_*` functions. `walk_*` is **exhaustive with no
//! `_` arm**, so a new node variant is a compile error at the one place that
//! must learn the new shape (anti-spec A3).
//!
//! # Holes are unrepresentable-as-valid
//!
//! Every position the parser can fail to fill is a [`Recovered`], and every list
//! it can fail to parse an element of holds a type implementing [`Recovery`].
//! There is no fabricated name, no defaulted-to-empty parameter list, and no
//! silently truncated argument list anywhere below.
//!
//! This replaces an earlier design that interned `""` for an unreadable name.
//! That is a *value*: `package ;` produced a `PackageDecl` whose namespace and
//! name were both a real `Name` — and equal to each other — while a doc comment
//! two lines up claimed the parser "does not guess at what was meant". A
//! consumer that never reads `Diagnostics` could not tell the hole from a name.
//! `Recovered::Missing` carries a span and nothing else.
//!
//! # Deliberate departures from the frozen AST
//!
//! * **No handler/binding split.** The frozen parser inspected a binding's value
//!   and re-filed closure-valued bindings as `Handler`s. That is analysis, and
//!   analysis does not belong on the node it describes (anti-spec B3). Every
//!   `name: value` inside an element is one [`NamedProp`]; classification is
//!   stage 2's job.
//! * **One member list per declaration, in source order.** The frozen AST split
//!   a component into `properties` / `functions` / `body`, losing their relative
//!   order and leaving nowhere to put a recovery node. [`ComponentDecl::members`]
//!   is the uniform spine (anti-spec D1); the split views are accessors.

pub mod visit;

use crate::{NodeId, green::GreenNode};
use yelc_base::{Name, SourceId, Span};

// ---------------------------------------------------------------------------
// Holes
// ---------------------------------------------------------------------------

/// A node position the parser may not have been able to fill.
///
/// `Missing` is a **hole**, not a value: it carries the span of the text that
/// could not be parsed and nothing else. There is no `Name`, no default, and
/// nothing a consumer can accidentally treat as well-formed.
#[derive(Debug)]
pub enum Recovered<T> {
    Present(T),
    Missing { id: NodeId, span: Span },
}

impl<T> Recovered<T> {
    pub fn present(&self) -> Option<&T> {
        match self {
            Recovered::Present(value) => Some(value),
            Recovered::Missing { .. } => None,
        }
    }

    pub fn is_missing(&self) -> bool {
        matches!(self, Recovered::Missing { .. })
    }
}

/// A type that can stand in for a list element the parser could not read.
///
/// [`crate::parser`]'s `parse_list` requires this, which is what makes it
/// *impossible* to add a list production whose recovery path pushes nothing.
/// The previous revision returned `Option<R>` and dropped the `None` case,
/// which is how six recovery positions ended up with a diagnostic and no
/// `Error` node anywhere in the tree.
pub trait Recovery {
    fn recovery(id: NodeId, span: Span) -> Self;
}

impl<T> Recovery for Recovered<T> {
    fn recovery(id: NodeId, span: Span) -> Self {
        Recovered::Missing { id, span }
    }
}

// ---------------------------------------------------------------------------
// File and items
// ---------------------------------------------------------------------------

/// Root of one parsed file.
pub struct File {
    pub id: NodeId,
    pub source: SourceId,
    pub span: Span,
    pub green: GreenNode,
    pub items: Vec<ItemKind>,
    /// Recovery positions with **no slot** in the typed tree, in source order.
    /// See [`RecoveryMark`].
    pub recovery_marks: Vec<RecoveryMark>,
}

/// A recovery position that describes a missing **token**, not a missing node.
///
/// The `>` in `list<s32`, the `in` in `for x xs { … }`, the extra comma a
/// `TrailingSep::Forbidden` list refused: each is a real recovery point, and the
/// typed AST models nodes rather than tokens, so none of them has a field to
/// live in. Invariant S5 still requires a recovery node, so they live here — a
/// side table keyed by span, which is where analysis about a program belongs
/// (anti-spec B3).
///
/// # Why not a sibling element in the nearest list
///
/// That is what the predecessor did, and it **corrupted list arity**. The hole
/// drained into whichever list closed first — usually an inner, well-formed one
/// — and arrived as a genuine element at a position chosen by drain timing
/// rather than by source position:
///
/// | input | wrong result |
/// |---|---|
/// | `f: func(a list<s32>, b: s32)` | 3 parameters for the 2 written |
/// | `f: func(a tuple<s32, string>)` | `tuple<S32, ERR, String>` — an error *between* the two written types |
/// | `record R { a: list<s32 }` | 2 fields for the 1 written |
///
/// A consumer counting parameters got the wrong number, and sibling source order
/// — the one ordering guarantee the seam does make — did not hold.
pub struct RecoveryMark {
    pub id: NodeId,
    pub span: Span,
}

/// Top-level declarations, per the frozen grammar's `top_level_item`:
/// records, enums, variants, elements, imported components, globals,
/// components — plus the package declaration and a recovery variant.
#[derive(Debug)]
pub enum ItemKind {
    Package(PackageDecl),
    Record(RecordDecl),
    Enum(EnumDecl),
    Variant(VariantDecl),
    Element(ElementDecl),
    ExternComponent(ExternComponentDecl),
    Global(GlobalDecl),
    Component(ComponentDecl),
    /// Recovery node: the parser could not match a top-level item here.
    /// Carries the span it consumed so the text is still attributable.
    Error {
        id: NodeId,
        span: Span,
    },
}

impl ItemKind {
    pub fn id(&self) -> NodeId {
        match self {
            ItemKind::Package(it) => it.id,
            ItemKind::Record(it) => it.id,
            ItemKind::Enum(it) => it.id,
            ItemKind::Variant(it) => it.id,
            ItemKind::Element(it) => it.id,
            ItemKind::ExternComponent(it) => it.id,
            ItemKind::Global(it) => it.id,
            ItemKind::Component(it) => it.id,
            ItemKind::Error { id, .. } => *id,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            ItemKind::Package(it) => it.span,
            ItemKind::Record(it) => it.span,
            ItemKind::Enum(it) => it.span,
            ItemKind::Variant(it) => it.span,
            ItemKind::Element(it) => it.span,
            ItemKind::ExternComponent(it) => it.span,
            ItemKind::Global(it) => it.span,
            ItemKind::Component(it) => it.span,
            ItemKind::Error { span, .. } => *span,
        }
    }
}

impl Recovery for ItemKind {
    fn recovery(id: NodeId, span: Span) -> Self {
        ItemKind::Error { id, span }
    }
}

/// An interned name together with the span it was written at.
#[derive(Clone, Copy, Debug)]
pub struct Ident {
    pub id: NodeId,
    pub span: Span,
    pub name: Name,
}

/// A name position: either a real interned name, or a hole.
pub type MaybeIdent = Recovered<Ident>;

// ---------------------------------------------------------------------------
// Declarations
// ---------------------------------------------------------------------------

/// `package ns:name@1.0.0;`
#[derive(Debug)]
pub struct PackageDecl {
    pub id: NodeId,
    pub span: Span,
    pub namespace: MaybeIdent,
    pub name: MaybeIdent,
    /// The version text without its leading `@`, e.g. `1.0.0`.
    pub version: Option<Name>,
    pub version_span: Option<Span>,
}

/// `record Name { field: type, … }`
#[derive(Debug)]
pub struct RecordDecl {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub fields: Vec<Recovered<RecordField>>,
}

impl RecordDecl {
    pub fn present_fields(&self) -> impl Iterator<Item = &RecordField> {
        self.fields.iter().filter_map(Recovered::present)
    }
}

#[derive(Debug)]
pub struct RecordField {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub ty: TypeRef,
}

/// `enum Name { case-a, case-b }`
#[derive(Debug)]
pub struct EnumDecl {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub cases: Vec<MaybeIdent>,
}

/// `variant Name { case-a, case-b(type) }`
#[derive(Debug)]
pub struct VariantDecl {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub cases: Vec<Recovered<VariantCase>>,
}

#[derive(Debug)]
pub struct VariantCase {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub payload: Option<TypeRef>,
}

/// `element Name { prop: type; }` — an intrinsic UI primitive.
#[derive(Debug)]
pub struct ElementDecl {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub members: Vec<Recovered<PropertyDecl>>,
}

impl ElementDecl {
    pub fn properties(&self) -> impl Iterator<Item = &PropertyDecl> {
        self.members.iter().filter_map(Recovered::present)
    }
}

/// `extern component Name { … }`
#[derive(Debug)]
pub struct ExternComponentDecl {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub members: Vec<ExternMember>,
}

#[derive(Debug)]
pub enum ExternMember {
    Property(PropertyDecl),
    Method(FunctionDecl),
    /// An `@children` slot marker.
    Children {
        id: NodeId,
        span: Span,
    },
    Error {
        id: NodeId,
        span: Span,
    },
}

impl Recovery for ExternMember {
    fn recovery(id: NodeId, span: Span) -> Self {
        ExternMember::Error { id, span }
    }
}

impl ExternComponentDecl {
    pub fn properties(&self) -> impl Iterator<Item = &PropertyDecl> {
        self.members.iter().filter_map(|member| match member {
            ExternMember::Property(property) => Some(property),
            ExternMember::Method(_)
            | ExternMember::Children { .. }
            | ExternMember::Error { .. } => None,
        })
    }

    pub fn methods(&self) -> impl Iterator<Item = &FunctionDecl> {
        self.members.iter().filter_map(|member| match member {
            ExternMember::Method(method) => Some(method),
            ExternMember::Property(_)
            | ExternMember::Children { .. }
            | ExternMember::Error { .. } => None,
        })
    }

    /// The span of the first `@children` marker, if the body declared one.
    pub fn children_slot(&self) -> Option<Span> {
        self.members.iter().find_map(|member| match member {
            ExternMember::Children { span, .. } => Some(*span),
            ExternMember::Property(_) | ExternMember::Method(_) | ExternMember::Error { .. } => {
                None
            }
        })
    }
}

/// `[export] global Name { … }`
#[derive(Debug)]
pub struct GlobalDecl {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub is_export: bool,
    pub members: Vec<GlobalMember>,
}

#[derive(Debug)]
pub enum GlobalMember {
    Property(GlobalProperty),
    Callback(FunctionDecl),
    Error { id: NodeId, span: Span },
}

impl Recovery for GlobalMember {
    fn recovery(id: NodeId, span: Span) -> Self {
        GlobalMember::Error { id, span }
    }
}

impl GlobalDecl {
    pub fn properties(&self) -> impl Iterator<Item = &GlobalProperty> {
        self.members.iter().filter_map(|member| match member {
            GlobalMember::Property(property) => Some(property),
            GlobalMember::Callback(_) | GlobalMember::Error { .. } => None,
        })
    }

    pub fn callbacks(&self) -> impl Iterator<Item = &FunctionDecl> {
        self.members.iter().filter_map(|member| match member {
            GlobalMember::Callback(callback) => Some(callback),
            GlobalMember::Property(_) | GlobalMember::Error { .. } => None,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PropertyDirection {
    In,
    Out,
    InOut,
}

/// `[in|out|in-out] name: type [= default];`
#[derive(Debug)]
pub struct GlobalProperty {
    pub id: NodeId,
    pub span: Span,
    /// `None` when no direction was written. The frozen compiler defaults this
    /// to `In` at lowering time; the parser records what was written.
    pub direction: Option<PropertyDirection>,
    pub name: MaybeIdent,
    pub ty: TypeRef,
    pub default: Option<Expr>,
}

/// `[export] component Name { … }`
#[derive(Debug)]
pub struct ComponentDecl {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub is_export: bool,
    /// Members in **source order**, properties and nodes interleaved as
    /// written. The split views below are derived.
    pub members: Vec<ComponentMember>,
}

#[derive(Debug)]
pub enum ComponentMember {
    Property(PropertyDecl),
    Function(FunctionDecl),
    Node(UiNode),
    Error { id: NodeId, span: Span },
}

impl Recovery for ComponentMember {
    fn recovery(id: NodeId, span: Span) -> Self {
        ComponentMember::Error { id, span }
    }
}

impl ComponentDecl {
    pub fn properties(&self) -> impl Iterator<Item = &PropertyDecl> {
        self.members.iter().filter_map(|member| match member {
            ComponentMember::Property(property) => Some(property),
            ComponentMember::Function(_)
            | ComponentMember::Node(_)
            | ComponentMember::Error { .. } => None,
        })
    }

    pub fn functions(&self) -> impl Iterator<Item = &FunctionDecl> {
        self.members.iter().filter_map(|member| match member {
            ComponentMember::Function(function) => Some(function),
            ComponentMember::Property(_)
            | ComponentMember::Node(_)
            | ComponentMember::Error { .. } => None,
        })
    }

    pub fn body(&self) -> impl Iterator<Item = &UiNode> {
        self.members.iter().filter_map(|member| match member {
            ComponentMember::Node(node) => Some(node),
            ComponentMember::Property(_)
            | ComponentMember::Function(_)
            | ComponentMember::Error { .. } => None,
        })
    }
}

/// `name: type [= default];`
///
/// Note a func-typed property (`on-click: func(a: s32);`) lands here and *not*
/// in [`FunctionDecl`]: the frozen grammar tries `property_decl` before
/// `function_decl`, and `type_annotation` includes `func_type`. Only the
/// `export`-prefixed form is a `function_decl`. Globals invert this.
#[derive(Debug)]
pub struct PropertyDecl {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub ty: TypeRef,
    pub default: Option<Expr>,
}

/// `[export] name: func(params) -> ret;`, `func name(params) -> ret;`
/// (import methods) and `callback name(params) -> ret;`.
#[derive(Debug)]
pub struct FunctionDecl {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub is_export: bool,
    /// `Missing` when the `func` keyword itself was absent — which is different
    /// from a function with no parameters, and used to be indistinguishable
    /// from it. `component A { export x: s32; }` produced a `FunctionDecl`
    /// named `x` with zero parameters and the written `s32` silently orphaned.
    pub signature: Recovered<FuncSignature>,
}

/// `( params? ) ( -> type )?`
#[derive(Debug)]
pub struct FuncSignature {
    pub id: NodeId,
    pub span: Span,
    /// `Missing` when the `(` was absent: no parameter list was read at all,
    /// which is not the same as an empty one.
    pub params: Recovered<Vec<Recovered<FuncParam>>>,
    pub return_type: Option<TypeRef>,
}

impl FuncSignature {
    pub fn present_params(&self) -> impl Iterator<Item = &FuncParam> {
        self.params
            .present()
            .into_iter()
            .flatten()
            .filter_map(Recovered::present)
    }
}

#[derive(Debug)]
pub struct FuncParam {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub ty: TypeRef,
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------
#[derive(Debug)]
pub struct TypeRef {
    pub id: NodeId,
    pub span: Span,
    pub kind: TypeKind,
}

impl Recovery for TypeRef {
    fn recovery(id: NodeId, span: Span) -> Self {
        TypeRef {
            id,
            span,
            kind: TypeKind::Error,
        }
    }
}

/// The WIT primitive spellings the frozen grammar recognises.
///
/// These are **not** reserved words. The type parser matches them by text; the
/// same spelling in any other position is an ordinary identifier.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PrimitiveType {
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
    Length,
    PhysicalLength,
    Angle,
    Duration,
    Percent,
    RelativeFontSize,
    Color,
    Brush,
    Image,
    Easing,
}

impl PrimitiveType {
    /// `int` and `float` are aliases for `s32` / `f32` in the frozen grammar.
    pub fn from_spelling(text: &str) -> Option<PrimitiveType> {
        Some(match text {
            "bool" => PrimitiveType::Bool,
            "s8" => PrimitiveType::S8,
            "s16" => PrimitiveType::S16,
            "s32" | "int" => PrimitiveType::S32,
            "s64" => PrimitiveType::S64,
            "u8" => PrimitiveType::U8,
            "u16" => PrimitiveType::U16,
            "u32" => PrimitiveType::U32,
            "u64" => PrimitiveType::U64,
            "f32" | "float" => PrimitiveType::F32,
            "f64" => PrimitiveType::F64,
            "char" => PrimitiveType::Char,
            "string" => PrimitiveType::String,
            "length" => PrimitiveType::Length,
            "physical-length" => PrimitiveType::PhysicalLength,
            "angle" => PrimitiveType::Angle,
            "duration" => PrimitiveType::Duration,
            "percent" => PrimitiveType::Percent,
            "relative-font-size" => PrimitiveType::RelativeFontSize,
            "color" => PrimitiveType::Color,
            "brush" => PrimitiveType::Brush,
            "image" => PrimitiveType::Image,
            "easing" => PrimitiveType::Easing,
            _ => return None,
        })
    }
}

#[derive(Debug)]
pub enum TypeKind {
    Primitive(PrimitiveType),
    /// A user-defined name. Explicitly **unresolved** — resolution is stage 2's
    /// job and nothing here pretends otherwise (anti-spec B2).
    Named(Name),
    List(Box<TypeRef>),
    Option(Box<TypeRef>),
    /// `result`, `result<T>`, `result<T, E>`.
    ///
    /// Stored as the argument list **as written** rather than as `ok`/`err`
    /// fields. `result<a, b, c>` is a real input; truncating it to two would
    /// drop a subtree the user typed, which invariant S5 forbids. Over-long
    /// lists are reported and kept.
    Result {
        args: Vec<TypeRef>,
    },
    Tuple(Vec<TypeRef>),
    /// Boxed for the same reason [`UiNode::If`] is: `FuncSignature` holds an
    /// `Option<TypeRef>` return type, so `TypeRef → TypeKind → FuncSignature →
    /// TypeRef` is a cycle and the variant needs one indirection to have a size.
    Func(Box<FuncSignature>),
    /// Recovery node.
    Error,
}

// ---------------------------------------------------------------------------
// UI tree
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub enum UiNode {
    Element(ElementNode),
    Text(TextNode),
    If(Box<IfNode>),
    For(Box<ForNode>),
    Children { id: NodeId, span: Span },
    Error { id: NodeId, span: Span },
}

impl Recovery for UiNode {
    fn recovery(id: NodeId, span: Span) -> Self {
        UiNode::Error { id, span }
    }
}

impl UiNode {
    pub fn span(&self) -> Span {
        match self {
            UiNode::Element(node) => node.span,
            UiNode::Text(node) => node.span,
            UiNode::If(node) => node.span,
            UiNode::For(node) => node.span,
            UiNode::Children { span, .. } | UiNode::Error { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub struct ElementNode {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub props: Vec<NamedProp>,
    pub children: Vec<UiNode>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PropModifier {
    None,
    Set,
    Bind,
}

/// `[set|bind] attr-name: expr` inside an element body.
#[derive(Debug)]
pub struct NamedProp {
    pub id: NodeId,
    pub span: Span,
    pub modifier: PropModifier,
    pub name: MaybeIdent,
    pub value: Expr,
}

/// A bare string used as element content: `"Hello {name}"`.
#[derive(Debug)]
pub struct TextNode {
    pub id: NodeId,
    pub span: Span,
    pub content: Expr,
}

/// A braced body.
///
/// `Missing` means the `{` itself was absent. An `if` with no block is not an
/// `if` with an empty block, and `component A { if x "a" }` used to be
/// indistinguishable from `if x { }`.
pub type Block<T> = Recovered<Vec<T>>;

#[derive(Debug)]
pub struct IfNode {
    pub id: NodeId,
    pub span: Span,
    pub condition: Expr,
    pub then_branch: Block<UiNode>,
    pub else_if_branches: Vec<ElseIfBranch>,
    pub else_branch: Option<Block<UiNode>>,
}

#[derive(Debug)]
pub struct ElseIfBranch {
    pub id: NodeId,
    pub span: Span,
    pub condition: Expr,
    pub body: Block<UiNode>,
}

#[derive(Debug)]
pub struct ForNode {
    pub id: NodeId,
    pub span: Span,
    pub item: MaybeIdent,
    pub iterable: Expr,
    pub key: Option<Expr>,
    pub body: Block<UiNode>,
}

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub enum Stmt {
    Let(LetStmt),
    If(Box<IfStmt>),
    Assign(AssignStmt),
    Expr(ExprStmt),
    Error { id: NodeId, span: Span },
}

impl Recovery for Stmt {
    fn recovery(id: NodeId, span: Span) -> Self {
        Stmt::Error { id, span }
    }
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Let(stmt) => stmt.span,
            Stmt::If(stmt) => stmt.span,
            Stmt::Assign(stmt) => stmt.span,
            Stmt::Expr(stmt) => stmt.span,
            Stmt::Error { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub struct LetStmt {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub ty: Option<TypeRef>,
    pub value: Expr,
}

#[derive(Debug)]
pub struct IfStmt {
    pub id: NodeId,
    pub span: Span,
    pub condition: Expr,
    pub then_branch: Block<Stmt>,
    pub else_branch: Option<Block<Stmt>>,
}

/// `target = value;` and `target += value;`
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AssignOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub struct AssignStmt {
    pub id: NodeId,
    pub span: Span,
    pub op: AssignOp,
    pub target: Expr,
    pub value: Expr,
}

/// An expression used as a statement. `has_semicolon` is false for the
/// `trailing_expr` position at the end of a closure body.
#[derive(Debug)]
pub struct ExprStmt {
    pub id: NodeId,
    pub span: Span,
    pub expr: Expr,
    pub has_semicolon: bool,
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct Expr {
    pub id: NodeId,
    pub span: Span,
    pub kind: ExprKind,
}

impl Recovery for Expr {
    fn recovery(id: NodeId, span: Span) -> Self {
        Expr {
            id,
            span,
            kind: ExprKind::Error,
        }
    }
}

/// Iterative teardown of an expression chain.
///
/// `parse_binary` and `parse_postfix` are **loops**, so they build an
/// arbitrarily long `Box<Expr>` spine while `MAX_NESTING_DEPTH` — which counts
/// recursion inside `parse_*` — reads 2. `a.b.b.b…` with n = 4,979 is a valid,
/// diagnostic-free 10 KB file that `abort()`ed a debug build in the *derived*
/// `Drop` glue, long after `parse` had returned successfully (anti-spec A11).
///
/// Only the `Expr → Expr` edges are flattened here. Every other recursive edge
/// out of an expression (a closure body's statements, a nested UI node) is
/// reached through a guarded `parse_*` entry point and so is bounded by
/// `MAX_NESTING_DEPTH`; those drop normally.
impl Drop for Expr {
    fn drop(&mut self) {
        let mut worklist = vec![detach(self)];
        while let Some(kind) = worklist.pop() {
            flatten(kind, &mut worklist);
        }
    }
}

/// Replace an expression's payload with the trivially-droppable `Error`, and
/// hand the payload back. Dropping the husk afterwards costs one empty loop.
fn detach(expr: &mut Expr) -> ExprKind {
    std::mem::replace(&mut expr.kind, ExprKind::Error)
}

fn detach_owned(mut expr: Expr) -> ExprKind {
    detach(&mut expr)
}

impl Expr {
    /// Consume the expression and hand back its payload.
    ///
    /// `let ExprKind::Member { base, .. } = expr.kind` no longer compiles now
    /// that [`Expr`] has a `Drop` impl (E0509), and this is the replacement.
    /// Read `id` / `span` off the expression first if you need them.
    pub fn into_kind(self) -> ExprKind {
        detach_owned(self)
    }
}

/// Push every directly-owned sub-expression's payload onto `worklist`.
///
/// Exhaustive with no `_` arm, for the same reason `walk_expr` is: a new
/// `ExprKind` carrying a `Box<Expr>` must be a compile error here, not a silent
/// return of the stack overflow this exists to prevent.
fn flatten(kind: ExprKind, worklist: &mut Vec<ExprKind>) {
    match kind {
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Unit { .. }
        | ExprKind::Color(_)
        | ExprKind::Char(_)
        | ExprKind::Bool(_)
        | ExprKind::String(_)
        | ExprKind::Ident(_)
        | ExprKind::Error => {}
        // A closure body is statements, which are depth-bounded by the parser's
        // guarded `parse_stmt`; the derived glue is safe there.
        ExprKind::Closure(_) => {}
        ExprKind::Interpolation(parts) => {
            worklist.extend(parts.into_iter().filter_map(|part| match part {
                InterpolationPart::Literal(_) => None,
                InterpolationPart::Expr(expr) => Some(detach_owned(expr)),
            }));
        }
        ExprKind::List(items) | ExprKind::Tuple(items) => {
            worklist.extend(items.into_iter().map(detach_owned));
        }
        ExprKind::Record(fields) => {
            worklist.extend(fields.into_iter().filter_map(|field| match field {
                Recovered::Present(field) => Some(detach_owned(field.value)),
                Recovered::Missing { .. } => None,
            }));
        }
        ExprKind::Unary { op: _, operand } => worklist.push(detach_owned(*operand)),
        ExprKind::Binary { op: _, lhs, rhs } => {
            worklist.push(detach_owned(*lhs));
            worklist.push(detach_owned(*rhs));
        }
        ExprKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            worklist.push(detach_owned(*condition));
            worklist.push(detach_owned(*then_expr));
            worklist.push(detach_owned(*else_expr));
        }
        ExprKind::Range {
            start,
            end,
            inclusive: _,
        } => {
            worklist.push(detach_owned(*start));
            worklist.push(detach_owned(*end));
        }
        ExprKind::Call { callee: _, args } => {
            worklist.extend(args.into_iter().map(detach_owned));
        }
        ExprKind::PathCall {
            base,
            member: _,
            args,
        } => {
            worklist.push(detach_owned(*base));
            worklist.extend(args.into_iter().map(detach_owned));
        }
        ExprKind::Member { base, member: _ } => worklist.push(detach_owned(*base)),
        ExprKind::OptionalMember { base, member: _ } => worklist.push(detach_owned(*base)),
        ExprKind::Index { base, index } => {
            worklist.push(detach_owned(*base));
            worklist.push(detach_owned(*index));
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[derive(Debug)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    /// `8px` — the numeric part and the suffix text (`px`).
    Unit {
        value: f64,
        suffix: Name,
    },
    /// `#ff0000`, stored with its leading `#`.
    Color(Name),
    Char(char),
    Bool(bool),
    /// A string with no interpolation.
    String(Name),
    /// A string with at least one `{expr}` part.
    Interpolation(Vec<InterpolationPart>),
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    Record(Vec<Recovered<RecordFieldInit>>),
    Closure(Box<ClosureExpr>),
    Ident(Name),
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Ternary {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        inclusive: bool,
    },
    /// `f(args)` — a bare identifier callee.
    Call {
        callee: Ident,
        args: Vec<Expr>,
    },
    /// `base.member(args)`.
    PathCall {
        base: Box<Expr>,
        member: MaybeIdent,
        args: Vec<Expr>,
    },
    Member {
        base: Box<Expr>,
        member: MaybeIdent,
    },
    OptionalMember {
        base: Box<Expr>,
        member: MaybeIdent,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    /// Recovery node.
    Error,
}

#[derive(Debug)]
pub enum InterpolationPart {
    Literal(Name),
    Expr(Expr),
}

#[derive(Debug)]
pub struct RecordFieldInit {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    pub value: Expr,
}

#[derive(Debug)]
pub struct ClosureExpr {
    pub id: NodeId,
    pub span: Span,
    pub params: Vec<Recovered<ClosureParam>>,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct ClosureParam {
    pub id: NodeId,
    pub span: Span,
    pub name: MaybeIdent,
    /// `None` for `{ p -> … }`, where the type comes from context. Explicitly
    /// absent rather than a placeholder — anti-spec B2.
    pub ty: Option<TypeRef>,
}
