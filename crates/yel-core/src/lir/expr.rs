//! LIR expression types.

use serde::{Serialize, Deserialize};

// LIR uses operator semantics from the neutral `crate::ops` module —
// not from `crate::hir::expr`, because LIR has multiple frontends
// (HIR/THIR for yel-lang components, the graph IR for the flow
// frontend) and shouldn't reach into any one of them.
use crate::ops::{BinOp, UnaryOp};
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

    /// Phase 5e.5: discriminant test on an `option<T>` / `result<T,E>` /
    /// `variant` value. Lowers to a Wasm-GC `ref.test (ref $<parent>_<case>)`
    /// when the parent is migrated to the subtype-hierarchy GC repr;
    /// otherwise lowers to the legacy "load discriminant slot, compare
    /// against case constant" pattern. Result type is always `bool`.
    ///
    /// Backed by `LirExprKind::IsCase`'s `case_idx` field; case-index
    /// conventions match `VariantCtor`:
    ///   - `option<T>`: 0 = None, 1 = Some.
    ///   - `result<T,E>`: 0 = Ok, 1 = Err.
    ///   - User variant: declaration order in `VariantDef::cases`.
    IsCase {
        /// The option/result/variant value being tested.
        base: Box<LirExpr>,
        /// Case index to test against.
        case_idx: u32,
    },

    /// Phase 5e.5: payload extraction from a known case of an option /
    /// result / variant value. Lowers to `ref.cast (ref $<parent>_<case>);
    /// struct.get $<parent>_<case> <field_idx>` for migrated parents;
    /// otherwise to the legacy multi-slot canonical-ABI flat-load.
    ///
    /// `case_idx` selects the case the caller has already discriminated
    /// to (typically via a guarding `IsCase` test). `field_idx` selects
    /// within the case's payload — for the W3C lowering each case has at
    /// most a single payload field at index 0, but the field is kept as
    /// a `u32` for forward compatibility with multi-field payloads.
    ///
    /// Codegen emits `struct.get_s` for signed packed payloads (`s8`,
    /// `s16`) and `struct.get_u` for unsigned packed payloads (`bool`,
    /// `u8`, `u16`); plain `struct.get` otherwise.
    VariantField {
        /// The option/result/variant value.
        base: Box<LirExpr>,
        /// Case index whose payload is being read (caller must have
        /// already discriminated; reading a field from a non-active case
        /// traps via `ref.cast`'s type-check).
        case_idx: u32,
        /// Index within the case's payload (always 0 in YEL today).
        field_idx: u32,
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
