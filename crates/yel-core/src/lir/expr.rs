//! LIR expression types.

use serde::{Serialize, Deserialize};

// LIR uses operator semantics from the neutral `crate::ops` module —
// not from `crate::hir::expr`, because LIR has multiple frontends
// (HIR/THIR for yel-lang components, the graph IR for the flow
// frontend) and shouldn't reach into any one of them.
use crate::ops::{BinOp, UnaryOp};
use crate::ids::{DefId, FieldIdx, LocalId};
use crate::source::Span;
use crate::types::Ty;
use super::block::LirExprId;

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
    /// Source span this expression was lowered from, when known. Carried
    /// through THIR→LIR so diagnostics raised during or after lowering can
    /// still point at the user's code (`ir-preserve-spans`). `None` for
    /// synthetic expressions with no source origin.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub span: Option<Span>,
}

impl LirExpr {
    pub fn new(kind: LirExprKind, ty: Ty) -> Self {
        Self { kind, ty, span: None }
    }

    /// Like [`LirExpr::new`], but records the source span the expression was
    /// lowered from so later phases can still produce located diagnostics.
    pub fn new_spanned(kind: LirExprKind, ty: Ty, span: Span) -> Self {
        Self { kind, ty, span: Some(span) }
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
        lhs: LirExprId,
        rhs: LirExprId,
    },
    /// Unary operation.
    Unary { op: UnaryOp, operand: LirExprId },
    /// Field access (by index).
    Field { base: LirExprId, field_idx: FieldIdx },
    /// Index access.
    Index {
        base: LirExprId,
        index: LirExprId,
    },
    /// Function call by callee `DefId`. The callee may be a host import
    /// (DOM function, component callback, or global callback) or a local
    /// function; codegen resolves it through the import registry /
    /// function table and decides whether to push a receiver handle from
    /// the callee's kind. Global-singleton calls (`Global.fn(..)`) lower
    /// to this same variant.
    Call { func: DefId, args: Vec<LirExprId> },
    /// Read a **component-local** reactive signal (a field of `$self`'s
    /// `$Comp` GC struct, or a filter-captured WASM param). Global-property
    /// reads are the distinct [`Self::GlobalRead`] — codegen resolves them
    /// through a different path (per-block globals struct / core wasm globals),
    /// so keeping them separate lets codegen stop disambiguating by
    /// `owning_global_block`.
    SignalRead(DefId),
    /// Read a **global-block property** (`GlobalStore.prop`) — reactive state
    /// backed by the module's per-block globals struct rather than a component
    /// instance. The `DefId` is the property. For dependency/reactivity/DOT
    /// purposes it behaves exactly like a [`Self::SignalRead`]; only the
    /// codegen emission differs (globals struct vs `$self`).
    GlobalRead(DefId),
    /// Ternary expression.
    Ternary {
        condition: LirExprId,
        then_expr: LirExprId,
        else_expr: LirExprId,
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
        payload: Option<LirExprId>,
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
        base: LirExprId,
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
        base: LirExprId,
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
        elements: Vec<LirExprId>,
        /// Size of each element in bytes.
        element_size: u32,
    },

    /// Record literal construction.
    /// Stores fields at their computed offsets.
    RecordConstruct {
        /// Record type DefId (for layout lookup).
        record_def: DefId,
        /// Field expressions in definition order.
        fields: Vec<LirExprId>,
        /// Total size of the record in bytes.
        total_size: u32,
    },

    /// Tuple literal construction.
    TupleConstruct {
        /// Element expressions.
        elements: Vec<LirExprId>,
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
        start: LirExprId,
        end: LirExprId,
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
