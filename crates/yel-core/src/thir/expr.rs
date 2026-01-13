//! THIR expression types with full type information.

use crate::ids::{DefId, ExprId, FieldIdx, LocalId, VariantIdx};
use crate::source::Span;
use crate::types::Ty;

use crate::hir::expr::{BinOp, HirLiteral, UnaryOp};

/// A typed expression.
#[derive(Debug, Clone)]
pub struct ThirExpr {
    /// Unique expression ID within the body.
    pub id: ExprId,
    /// The kind of expression.
    pub kind: ThirExprKind,
    /// The type of this expression.
    pub ty: Ty,
    /// Source location.
    pub span: Span,
}

impl ThirExpr {
    pub fn new(id: ExprId, kind: ThirExprKind, ty: Ty, span: Span) -> Self {
        Self { id, kind, ty, span }
    }

    pub fn error(id: ExprId, span: Span) -> Self {
        Self {
            id,
            kind: ThirExprKind::Error,
            ty: Ty::ERROR,
            span,
        }
    }
}

/// Kind of THIR expression.
#[derive(Debug, Clone)]
pub enum ThirExprKind {
    /// Local variable (parameter, let binding, loop var).
    Local(LocalId),
    /// Definition reference (property, function).
    Def(DefId),
    /// Literal value.
    Literal(HirLiteral),
    /// Binary operation.
    Binary {
        op: BinOp,
        lhs: Box<ThirExpr>,
        rhs: Box<ThirExpr>,
    },
    /// Unary operation.
    Unary { op: UnaryOp, operand: Box<ThirExpr> },
    /// Field access with resolved index.
    Field {
        base: Box<ThirExpr>,
        field_idx: FieldIdx,
        field_def: DefId,
    },
    /// Optional field access with resolved index.
    OptionalField {
        base: Box<ThirExpr>,
        field_idx: FieldIdx,
        field_def: DefId,
    },
    /// Index access.
    Index {
        base: Box<ThirExpr>,
        index: Box<ThirExpr>,
    },
    /// Function call.
    Call { func: DefId, args: Vec<ThirExpr> },
    /// Range expression.
    Range {
        start: Box<ThirExpr>,
        end: Box<ThirExpr>,
        inclusive: bool,
    },
    /// Ternary/conditional expression.
    Ternary {
        condition: Box<ThirExpr>,
        then_expr: Box<ThirExpr>,
        else_expr: Box<ThirExpr>,
    },
    /// Closure expression.
    Closure {
        params: Vec<(LocalId, Ty)>,
        body: Vec<ThirStatement>,
        captures: Vec<LocalId>,
    },
    /// String interpolation.
    Interpolation(Vec<ThirInterpolationPart>),
    /// Enum case reference (e.g., `Status.case`).
    EnumCase {
        /// Enum type DefId.
        ty_def: DefId,
        /// Case index (discriminant).
        case_idx: VariantIdx,
    },
    /// Variant constructor (e.g., `Message.error`).
    VariantCtor {
        /// Variant type DefId.
        ty_def: DefId,
        /// Case index.
        case_idx: VariantIdx,
        /// Payload (if any).
        payload: Option<Box<ThirExpr>>,
    },

    // ========================================================================
    // List and Record Literals (Part 2 of list-construct-plan.md)
    // ========================================================================

    /// List literal: [a, b, c]
    /// Element type is inferred from context or first element.
    ListLiteral {
        /// Element expressions.
        elements: Vec<ThirExpr>,
        /// Element type (all elements must have this type).
        element_ty: Ty,
    },

    /// Record literal: Person { name: "Alice", age: 30 }
    /// Fields are reordered to match definition order.
    RecordLiteral {
        /// Record type DefId.
        record_def: DefId,
        /// Field expressions in definition order.
        fields: Vec<ThirExpr>,
    },

    /// Tuple literal: (a, b, c)
    TupleLiteral {
        /// Element expressions.
        elements: Vec<ThirExpr>,
    },

    /// Error recovery.
    Error,
}

/// THIR statement (in closures/handlers).
#[derive(Debug, Clone)]
pub enum ThirStatement {
    /// Expression statement.
    Expr(ThirExpr),
    /// Assignment: target = value.
    Assign { target: ThirExpr, value: ThirExpr },
    /// If statement.
    If {
        condition: ThirExpr,
        then_branch: Vec<ThirStatement>,
        else_branch: Option<Vec<ThirStatement>>,
    },
}

/// Part of a string interpolation.
#[derive(Debug, Clone)]
pub enum ThirInterpolationPart {
    /// Literal string segment.
    Literal(String),
    /// Interpolated expression.
    Expr(ThirExpr),
}

/// A pattern for matching.
#[derive(Debug, Clone)]
pub enum ThirPattern {
    /// Wildcard: _
    Wildcard,
    /// Binding: name
    Binding(LocalId),
    /// Some(inner)
    Some(Box<ThirPattern>),
    /// None
    None,
    /// Variant case: Enum::Case or Variant::Case(binding)
    Variant {
        ty_def: DefId,
        case_idx: VariantIdx,
        binding: Option<LocalId>,
    },
    /// Literal
    Literal(HirLiteral),
}

/// A match arm.
#[derive(Debug, Clone)]
pub struct ThirMatchArm {
    pub pattern: ThirPattern,
    pub guard: Option<ThirExpr>,
    pub body: ThirExpr,
}
