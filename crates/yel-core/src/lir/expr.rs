//! LIR expression types.

use serde::{Serialize, Deserialize};

use crate::hir::expr::{BinOp, UnaryOp};
use crate::ids::{DefId, FieldIdx, LocalId};
use crate::types::Ty;

/// LIR literal values (primitives only - compound types use dedicated constructs).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LirLiteral {
    // Signed integers
    S8(i8),
    S16(i16),
    S32(i32),
    S64(i64),
    // Unsigned integers
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    // Floats
    F32(f32),
    F64(f64),
    // Other primitives
    Bool(bool),
    Char(char),
    String(String),
}

/// A LIR expression (optimized for codegen).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirExpr {
    pub kind: LirExprKind,
    pub ty: Ty,
}

impl LirExpr {
    pub fn new(kind: LirExprKind, ty: Ty) -> Self {
        Self { kind, ty }
    }
}

/// Kind of LIR expression.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LirExprKind {
    /// Local variable.
    Local(LocalId),
    /// Definition reference.
    Def(DefId),
    /// Literal value (primitives only).
    Literal(LirLiteral),
    /// Binary operation.
    Binary {
        op: BinOp,
        lhs: Box<LirExpr>,
        rhs: Box<LirExpr>,
    },
    /// Unary operation.
    Unary { op: UnaryOp, operand: Box<LirExpr> },
    /// Field access (by index).
    Field { base: Box<LirExpr>, field_idx: FieldIdx },
    /// Index access.
    Index {
        base: Box<LirExpr>,
        index: Box<LirExpr>,
    },
    /// Function call.
    Call { func: DefId, args: Vec<LirExpr> },
    /// Signal read (component-local or global property).
    SignalRead(DefId),
    /// Call a function on a global singleton (e.g. a host-implemented function).
    GlobalCall { function: DefId, args: Vec<LirExpr> },
    /// Ternary expression.
    Ternary {
        condition: Box<LirExpr>,
        then_expr: Box<LirExpr>,
        else_expr: Box<LirExpr>,
    },
    /// Enum case reference (pre-computed discriminant).
    EnumCase {
        /// Enum type DefId.
        ty_def: DefId,
        /// Pre-computed discriminant value.
        discriminant: u32,
    },
    /// Variant constructor.
    VariantCtor {
        /// Variant type DefId.
        ty_def: DefId,
        /// Case index.
        case_idx: u32,
        /// Payload (if any).
        payload: Option<Box<LirExpr>>,
    },

    // ========================================================================
    // List and Record Constructs (Part 2 of list-construct-plan.md)
    // ========================================================================

    /// Static list (all elements constant, stored in data section).
    /// Result is (ptr, len) pair pointing to pre-initialized data.
    ListStatic {
        /// Offset in WASM data section where element data starts.
        data_offset: u32,
        /// Number of elements.
        len: u32,
        /// Size of each element in bytes.
        element_size: u32,
    },

    /// Dynamic list construction (runtime allocation).
    /// Allocates memory and stores each element.
    ListConstruct {
        /// Element expressions to evaluate and store.
        elements: Vec<LirExpr>,
        /// Size of each element in bytes.
        element_size: u32,
    },

    /// Record literal construction.
    /// Stores fields at their computed offsets.
    RecordConstruct {
        /// Record type DefId (for layout lookup).
        record_def: DefId,
        /// Field expressions in definition order.
        fields: Vec<LirExpr>,
        /// Total size of the record in bytes.
        total_size: u32,
    },

    /// Tuple literal construction.
    TupleConstruct {
        /// Element expressions.
        elements: Vec<LirExpr>,
        /// Total size of the tuple in bytes.
        total_size: u32,
    },

    /// Closure expression (for filter predicates, etc.).
    /// The closure body is stored as statements to be lowered to a block later.
    Closure {
        /// Parameter local IDs (assigned during lowering).
        params: Vec<(LocalId, Ty)>,
        /// Body statements.
        body: Vec<LirStatement>,
    },

    /// Range expression (for iteration).
    /// Represents start..end (exclusive) or start..=end (inclusive).
    Range {
        start: Box<LirExpr>,
        end: Box<LirExpr>,
        /// If true, the range is inclusive (start..=end).
        inclusive: bool,
    },
}

/// LIR statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LirStatement {
    /// Expression statement.
    Expr(LirExpr),
    /// Signal write (component-local or global property).
    SignalWrite { signal: DefId, value: LirExpr },
    /// If statement.
    If {
        condition: LirExpr,
        then_branch: Vec<LirStatement>,
        else_branch: Option<Vec<LirStatement>>,
    },
    /// Let binding: allocates a local variable.
    Let {
        local_id: LocalId,
        value: LirExpr,
    },
}
