//! The HIR expression and statement vocabulary.
//!
//! Name-resolved, untyped, desugared. What distinguishes it from the AST:
//!
//! - **Names are resolved where resolution is name-decidable** — a reference is
//!   a [`LocalId`], a [`DefId`], a builtin name, or explicitly
//!   [unresolved](HirExprKind::Unresolved) (H4: never `Unknown`-and-hoped-over,
//!   and never an error here — resolution is partial on purpose; the checker
//!   reports).
//! - **The five desugarings have happened** — `x += 1` is an assignment of a
//!   `Binary`, `#ff0000` is `Color.rgba(…)`, `bind value: x` is a getter plus
//!   an empty setter, `else if` chains are nested (D7), and `Foo.bar(…)` is a
//!   member call when `Foo` names a definition.
//! - **`Match` is the only conditional.** `Ternary` and statement `if` lower
//!   into it, and UI `if` lowers into it *directly*
//!   (`plans/rewrite/directions.md` §9). There is no `If` and no `Ternary`
//!   here.
//! - **No `String` anywhere** (S4, carried from stage 1) and **no `Ty` on any
//!   node** (H3): a written annotation is a [`TypeId`] — a reference to the
//!   syntax — and resolved types live in the definition tables or in stage 4's
//!   side table.
//!
//! # Every node's `hir_id` answers to the map
//!
//! [`HirMap::node_of`](crate::HirMap::node_of) is total over ids allocated
//! here: primary nodes via `next_hir_id`, synthesized ones via `synthesize`
//! with the origin they were desugared from. That is the provenance mechanism
//! the diagnostic obligation requires — a renderer asks the map, not the node.

use yelc_base::Name;
use yelc_sema::DefId;

use crate::ids::{HirId, LocalId, TypeId};

/// One expression.
#[derive(Debug)]
pub struct HirExpr {
    pub hir_id: HirId,
    pub kind: HirExprKind,
}

/// What an expression is.
///
/// UI vocabulary ([`Instantiate`](HirExprKind::Instantiate),
/// [`UiText`](HirExprKind::UiText), [`Fragment`](HirExprKind::Fragment),
/// [`Boundary`](HirExprKind::Boundary)) is legal here — stage 3 is a frontend
/// stage. C1 forbids it below the LIR seam, not in HIR.
#[derive(Debug)]
pub enum HirExprKind {
    /// A body-scoped local: parameter, `let`, loop or arm binder.
    Local(LocalId),
    /// A resolved top-level definition.
    Def(DefId),
    /// A member of the **enclosing** component or global, referenced bare:
    /// `count` inside the component that declares `count`. The frozen tree
    /// modelled this by defining every property as a local before the body
    /// (`lower.rs:894–911`) — a duplicate of the member table kept because
    /// scope lookup was the only resolution path. An explicit form is the D3
    /// fix applied one level up: resolve through the structure that owns the
    /// name, and store the resolution, not a copy of the table.
    Prop {
        owner: DefId,
        member: Name,
    },
    /// A name in the intrinsic table: a compiler-internal symbol declared in
    /// source as an `extern func` and **inlined** at codegen — a call to one
    /// never becomes a wasm call. Which **row** — `len` has two — is
    /// type-directed and stage 4's; the name is the resolution.
    Intrinsic(Name),
    /// A name that resolved to nothing. Kept as itself (H4); stage 4 reports
    /// it. Never constructed for a name that *did* resolve.
    Unresolved(Name),
    Literal(HirLiteral),
    List(Vec<HirExpr>),
    Tuple(Vec<HirExpr>),
    Record {
        fields: Vec<HirFieldInit>,
    },
    Field {
        base: Box<HirExpr>,
        field: Name,
    },
    OptionalField {
        base: Box<HirExpr>,
        field: Name,
    },
    Index {
        base: Box<HirExpr>,
        index: Box<HirExpr>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<HirExpr>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<HirExpr>,
        rhs: Box<HirExpr>,
    },
    /// `start..end` / `start..=end`. Still a node: the struct-literal desugar
    /// is scheduled behind the stdlib `Range` type
    /// (`stage-3-hir-build.md` § Candidates).
    Range {
        start: Box<HirExpr>,
        end: Box<HirExpr>,
        inclusive: bool,
    },
    /// A call. `x.f(a)` arrives here as `f` with args `[x, a]` — pure UFCS,
    /// `plans/modules.md` §8; the receiver origin is readable off the argument's
    /// `hir_id`. `MethodCall` deliberately does not exist.
    Call {
        callee: HirCallee,
        args: Vec<HirExpr>,
    },
    /// A closure. Its parameters and every local inside it live in the
    /// **enclosing body's arena**, scoped — a closure is a value inside a body,
    /// not a body of its own, and its captures are references to enclosing
    /// locals, which per-closure arenas could not express without a capture
    /// vocabulary nothing needs yet (directions §4). The frozen tree allocates
    /// the same way, which is what D1's local-order caveat is measured against.
    Closure(Box<HirClosure>),
    /// A string with `{…}` parts. Survives to stage 4, which picks each part's
    /// `*-to-string` — the type-directed half of the desugar.
    Interpolation(Vec<HirInterpolationPart>),
    /// The general conditional (§9). Today only desugarings construct it, with
    /// boolean-literal patterns; the surface `match` adds pattern forms when
    /// its grammar lands, without moving this node.
    Match(Box<HirMatch>),
    /// A block in expression position — a `match` arm, a desugared branch. Its
    /// `tail` is its value.
    Block(Box<HirBlock>),
    /// A UI element or component instantiation, desugared to a builder call:
    /// the target resolves like any name, the props are one uniform list (D1),
    /// the children are builder expressions.
    Instantiate(Box<HirInstantiate>),
    /// A bare string child — text content.
    UiText(Box<HirExpr>),
    /// A dynamic region of the tree — see [`HirBoundary`]. Everything in a
    /// build body outside a boundary is static structure that runs once at
    /// mount; a boundary is the unit that mounts, unmounts and reconciles
    /// afterwards.
    Boundary(Box<HirBoundary>),
    /// A run of builder children with no element of their own — a UI `if`
    /// arm's node list.
    Fragment(Vec<HirExpr>),
    /// A recovery node the parser produced, carried through (H5: lowered, not
    /// skipped). The parse diagnostic already exists.
    Error,
}

/// A closure: parameters (locals of the enclosing arena) and a block.
#[derive(Debug)]
pub struct HirClosure {
    pub hir_id: HirId,
    pub params: Vec<LocalId>,
    pub block: HirBlock,
}

/// What a call's function position resolved to.
#[derive(Debug)]
pub enum HirCallee {
    /// A local — a closure held in a variable, called.
    Local(LocalId),
    /// One definition: a member function, a global callback, a component.
    Def(DefId),
    /// An intrinsic overload set, by name. The row is stage 4's pick.
    Intrinsic(Name),
    /// `Base.member(…)` where `Base` names a definition: a variant case
    /// constructor, a global's callback. Which member is a lookup in the
    /// owner's rows; *not* pre-resolved to an index, because the member table
    /// is keyed by the owner and the name is the address.
    Member { base: DefId, member: Name },
    /// A name that resolved to nothing (H4).
    Unresolved(Name),
}

/// `name: value` in a record literal.
#[derive(Debug)]
pub struct HirFieldInit {
    pub hir_id: HirId,
    pub name: Name,
    pub value: HirExpr,
}

/// One `{…}` or literal segment of an interpolated string.
#[derive(Debug)]
pub enum HirInterpolationPart {
    Literal(Name),
    Expr(HirExpr),
}

/// The general conditional.
#[derive(Debug)]
pub struct HirMatch {
    pub scrutinee: HirExpr,
    /// In source order. Exhaustiveness is stage 4's check, against
    /// `Definitions`.
    pub arms: Vec<HirMatchArm>,
}

#[derive(Debug)]
pub struct HirMatchArm {
    pub hir_id: HirId,
    pub pattern: HirPattern,
    pub value: HirExpr,
}

/// An arm's pattern.
///
/// Only what today's desugarings produce: `Ternary`, statement `if` and UI `if`
/// all lower to boolean-literal arms. The surface `match` grammar adds case and
/// binding patterns **additively** when it lands (`directions.md` §9's
/// sequencing) — designing those now would be vocabulary with no constructor,
/// which A9 deletes.
#[derive(Debug)]
pub enum HirPattern {
    Bool(bool),
    /// An integer-literal arm — the surface `match`'s form (.yelir subset).
    Int(i64),
    /// A pattern the lowering does not understand yet; its diagnostic exists
    /// (H5), and stage 4 will skip the arm.
    Error,
}

/// A statement block: the body of a branch, loop, function or closure.
#[derive(Debug)]
pub struct HirBlock {
    pub stmts: Vec<HirStmt>,
    /// The block's value — a final expression with no `;`. `None` for a
    /// statement-position block.
    pub tail: Option<HirExpr>,
}

/// One statement.
#[derive(Debug)]
pub enum HirStmt {
    Let {
        hir_id: HirId,
        local: LocalId,
        value: HirExpr,
    },
    /// `target = value`. Compound forms (`+=`) arrive desugared, the rhs a
    /// synthesized `Binary` whose map origin is the assignment.
    Assign {
        hir_id: HirId,
        target: HirExpr,
        value: HirExpr,
    },
    Expr(HirExpr),
    Return {
        hir_id: HirId,
        value: Option<HirExpr>,
    },
    /// Statement-position `for`. A loop, not a conditional — `Match` does not
    /// cover it. The binder is the binder exception again.
    For {
        hir_id: HirId,
        binder: LocalId,
        iterable: HirExpr,
        body: HirBlock,
    },
    /// A recovery statement (H5).
    Error {
        hir_id: HirId,
    },
}

/// A UI element / component instantiation, as a builder call.
#[derive(Debug)]
pub struct HirInstantiate {
    /// What is instantiated. [`HirCallee::Def`] for a component, element or
    /// extern component; [`HirCallee::Unresolved`] for a name with no
    /// definition — which today includes every builtin element, since the
    /// builtin inventory arrives from Yel source later
    /// (`yelc-sema/src/stdlib.rs`).
    pub target: HirCallee,
    /// One uniform prop list (D1), in **merged source order**: same-named
    /// entries fold into one (F13), first occurrence's position. Handler /
    /// binding classification is *not* stored — it is a `Definitions` lookup
    /// (B3: derived, not carried).
    pub props: Vec<HirProp>,
    pub children: Vec<HirExpr>,
}

/// One prop after the F13 merge: getter and setter are two halves of one
/// entity.
///
/// `bind value: x` arrives as getter `x` plus an empty setter block; a bare
/// `value: x` as getter only; `set value: { … }` as setter only. A setter with
/// no getter is the frozen tree's `InvalidValueBinding`, reported by the
/// lowering.
#[derive(Debug)]
pub struct HirProp {
    pub hir_id: HirId,
    pub name: Name,
    pub getter: Option<HirExpr>,
    /// The setter. `Some` with an empty block for `bind` — the empty setter is
    /// what enables DOM→signal auto-sync. Shares the build body's arena like
    /// any closure.
    pub setter: Option<HirClosure>,
}

/// A dynamic region of the UI tree: the three template forms whose content
/// changes after first render. The name is the tree-shape concept — the
/// anchor at which subtrees mount, unmount and reconcile — and each boundary
/// is `signalck`'s natural dependency unit: the state its condition or
/// iterable reads decides when the region re-evaluates.
///
/// One boundary per surface construct: a whole `if`/`else if`/`else` chain is
/// **one** [`Conditional`](HirBoundary::Conditional) — the chain nests as
/// plain [`Match`](HirExprKind::Match) expressions inside it, because the
/// chain occupies one anchor in the tree, not one per branch.
#[derive(Debug)]
pub enum HirBoundary {
    /// A UI `if` chain. The conditional itself is still a [`HirMatch`] — the
    /// boundary wrapper carries the region identity, not the branching.
    Conditional(HirMatch),
    /// A UI `for`: children repeated per item, reconciled by `key`.
    Repeat(HirRepeat),
    /// `@children` — the mount point where a parent inserts its children.
    Children,
}

/// A UI `for` region — the payload of [`HirBoundary::Repeat`].
#[derive(Debug)]
pub struct HirRepeat {
    /// The loop binder — a local of the enclosing build body's scope. Untyped
    /// until stage 4 reads the iterable's element type.
    pub binder: LocalId,
    pub iterable: HirExpr,
    /// `key(expr)` for list reconciliation.
    pub key: Option<HirExpr>,
    pub children: Vec<HirExpr>,
}

/// Binary operators. Redeclared rather than imported from the frozen tree —
/// the frozen crate is reference, not a dependency.
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
    BitAnd,
    BitOr,
    Shl,
    Shr,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

/// Literals. `String` and `Unit` suffixes are interned names — no `String`
/// type survives into HIR (S4).
#[derive(Debug)]
pub enum HirLiteral {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(Name),
    /// `8px` — the value and its interned suffix.
    Unit(f64, Name),
}

/// One local: a parameter, a `let` binding, a loop or arm binder.
#[derive(Debug)]
pub struct HirLocal {
    pub hir_id: HirId,
    pub name: Name,
    /// The written annotation, by reference to the syntax. `None` when none was
    /// written — a binder, an unannotated `let`. Stage 4's inference fills the
    /// side table, never this.
    pub ty: Option<TypeId>,
}
