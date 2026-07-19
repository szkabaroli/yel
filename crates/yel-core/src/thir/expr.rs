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
    /// Closure expression. Boxed: the three vectors make this the largest
    /// `ThirExprKind` variant, while closures are comparatively rare.
    Closure(Box<ThirClosure>),
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

    /// Read a property from a global singleton: `MailStore.items`.
    /// Codegen lowers this to a read of the host-pushed backing slot.
    GlobalRead {
        /// Global DefId.
        global: DefId,
        /// Field index within the global's properties.
        field: FieldIdx,
        /// The field's DefId (for type/name lookup).
        prop: DefId,
    },

    /// Call a function on a global singleton: `Global.fn-to-call(id)`.
    /// `function` is a `DefKind::Function` with `is_export` set by direction
    /// (false = callback/host-implements, true = public func/component-implements).
    GlobalCall {
        /// Global DefId.
        global: DefId,
        /// Function DefId.
        function: DefId,
        /// Arguments (already type-checked).
        args: Vec<ThirExpr>,
    },

    /// Error recovery.
    Error,
}

/// Payload of [`ThirExprKind::Closure`], boxed to keep `ThirExprKind` small.
#[derive(Debug, Clone)]
pub struct ThirClosure {
    /// Closure parameters (local id + type).
    pub params: Vec<(LocalId, Ty)>,
    /// Closure body statements.
    pub body: Vec<ThirStatement>,
    /// Captured locals.
    pub captures: Vec<LocalId>,
}

// `ThirExprKind` is stored per expression; guard against a variant re-bloating
// it (the `Closure` payload is boxed for this reason). Bump deliberately.
const _: () = assert!(std::mem::size_of::<ThirExprKind>() <= 40);

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
    /// Let binding: `let name: type = value;`
    Let {
        local_id: LocalId,
        name: String,
        ty: Ty,
        value: ThirExpr,
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
