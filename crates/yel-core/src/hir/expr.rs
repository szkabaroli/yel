//! HIR expression types.

use crate::ids::{DefId, LocalId};
use crate::source::Span;
use crate::types::Ty;
use serde::Serialize;

/// A HIR expression.
#[derive(Debug, Clone, Serialize)]
pub struct HirExpr {
    pub kind: HirExprKind,
    pub span: Span,
}

impl HirExpr {
    pub fn new(kind: HirExprKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn error(span: Span) -> Self {
        Self {
            kind: HirExprKind::Error,
            span,
        }
    }
}

/// Kind of HIR expression.
#[derive(Debug, Clone, Serialize)]
pub enum HirExprKind {
    /// Local variable reference.
    Local(LocalId),
    /// Top-level definition reference (component, function, property).
    Def(DefId),
    /// Literal value.
    Literal(HirLiteral),
    /// Binary operation.
    Binary {
        op: BinOp,
        lhs: Box<HirExpr>,
        rhs: Box<HirExpr>,
    },
    /// Unary operation.
    Unary { op: UnaryOp, operand: Box<HirExpr> },
    /// Field access (field name not yet resolved to index).
    Field { base: Box<HirExpr>, field: String },
    /// Optional field access.
    OptionalField { base: Box<HirExpr>, field: String },
    /// Index access.
    Index {
        base: Box<HirExpr>,
        index: Box<HirExpr>,
    },
    /// Function call.
    Call { func: String, args: Vec<HirExpr> },
    /// Path call (e.g., `Type.case(args)` for variant constructors).
    PathCall {
        base: String,
        member: String,
        args: Vec<HirExpr>,
    },
    /// Method call (e.g., `list.filter(closure)` or `string.starts-with(prefix)`).
    MethodCall {
        receiver: Box<HirExpr>,
        method: String,
        args: Vec<HirExpr>,
    },
    /// Range expression.
    Range {
        start: Box<HirExpr>,
        end: Box<HirExpr>,
        inclusive: bool,
    },
    /// Ternary/conditional expression.
    Ternary {
        condition: Box<HirExpr>,
        then_expr: Box<HirExpr>,
        else_expr: Box<HirExpr>,
    },
    /// Closure expression.
    Closure {
        params: Vec<(String, Ty)>,
        body: Vec<HirStatement>,
    },
    /// String interpolation.
    Interpolation(Vec<HirInterpolationPart>),
    /// Path expression (e.g., `Enum.case` or `Type.field`).
    Path {
        /// Path segments.
        segments: Vec<String>,
    },
    /// Error recovery.
    Error,
}

/// HIR literal values.
#[derive(Debug, Clone, Serialize)]
pub enum HirLiteral {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    /// Value with unit: (value, unit) e.g., (8.0, "px")
    Unit(f64, String),
    /// List literal.
    List(Vec<HirExpr>),
    /// Tuple literal.
    Tuple(Vec<HirExpr>),
    /// Record literal with field expressions.
    Record {
        fields: Vec<(String, HirExpr)>,
    },
}

// Operator types live in `crate::ops` — they're not HIR-specific.
// Re-exported here so existing call sites (`crate::hir::expr::BinOp`)
// keep compiling, while LIR / flow-core / future frontends read
// `crate::ops::{BinOp, UnaryOp}` directly without depending on HIR.
pub use crate::ops::{BinOp, UnaryOp};

/// HIR statement (in closures/handlers).
#[derive(Debug, Clone, Serialize)]
pub enum HirStatement {
    /// Expression statement.
    Expr(HirExpr),
    /// Assignment: target = value.
    /// Note: Compound assignments (+=, -=, etc.) are desugared to Assign in HIR lowering.
    Assign { target: HirExpr, value: HirExpr },
    /// If statement.
    If {
        condition: HirExpr,
        then_branch: Vec<HirStatement>,
        else_branch: Option<Vec<HirStatement>>,
    },
    /// Let binding: `let name: type = value;`
    Let {
        name: String,
        ty: Option<Ty>,
        value: HirExpr,
    },
}

/// Part of a string interpolation.
#[derive(Debug, Clone, Serialize)]
pub enum HirInterpolationPart {
    /// Literal string segment.
    Literal(String),
    /// Interpolated expression.
    Expr(HirExpr),
}
