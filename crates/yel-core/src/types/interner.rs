//! Type interner for efficient type storage and comparison.

use crate::ids::DefId;
use crate::syntax::ast::TyKind as AstTyKind;
use serde::Serialize;
use std::collections::HashMap;
use std::fmt;

/// An interned type reference.
///
/// This is a lightweight handle (just a u32) that represents a type.
/// Equal types always have the same `Ty` value.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default, Serialize)]
pub struct Ty(pub u32);

impl Ty {
    /// The error type (for recovery).
    pub const ERROR: Ty = Ty(0);
    /// The unit type.
    pub const UNIT: Ty = Ty(1);
    /// The bool type.
    pub const BOOL: Ty = Ty(2);
    /// The s32 type.
    pub const S32: Ty = Ty(3);
    /// The string type.
    pub const STRING: Ty = Ty(4);

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Ty({})", self.0)
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ty#{}", self.0)
    }
}

/// Canonical type representation for interning.
///
/// This is similar to the existing TyKind but uses Ty for recursive types
/// instead of Box<Ty>, enabling structural sharing.
#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize)]
pub enum InternedTyKind {
    // Primitives
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

    // Compound (use Ty handles for recursion)
    List(Ty),
    Option(Ty),
    Result { ok: Option<Ty>, err: Option<Ty> },
    Tuple(Vec<Ty>),

    // UI-specific
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

    // User-defined (resolved to DefId)
    Adt(DefId),

    // Function type
    Func { params: Vec<Ty>, ret: Option<Ty> },

    // Special
    Error,
    Unknown,
    Unit,
}

impl fmt::Display for InternedTyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InternedTyKind::Bool => write!(f, "bool"),
            InternedTyKind::S8 => write!(f, "s8"),
            InternedTyKind::S16 => write!(f, "s16"),
            InternedTyKind::S32 => write!(f, "s32"),
            InternedTyKind::S64 => write!(f, "s64"),
            InternedTyKind::U8 => write!(f, "u8"),
            InternedTyKind::U16 => write!(f, "u16"),
            InternedTyKind::U32 => write!(f, "u32"),
            InternedTyKind::U64 => write!(f, "u64"),
            InternedTyKind::F32 => write!(f, "f32"),
            InternedTyKind::F64 => write!(f, "f64"),
            InternedTyKind::Char => write!(f, "char"),
            InternedTyKind::String => write!(f, "string"),
            InternedTyKind::List(inner) => write!(f, "list<{}>", inner),
            InternedTyKind::Option(inner) => write!(f, "option<{}>", inner),
            InternedTyKind::Result { ok, err } => {
                write!(f, "result<")?;
                if let Some(ok) = ok {
                    write!(f, "{}", ok)?;
                } else {
                    write!(f, "_")?;
                }
                write!(f, ", ")?;
                if let Some(err) = err {
                    write!(f, "{}", err)?;
                } else {
                    write!(f, "_")?;
                }
                write!(f, ">")
            }
            InternedTyKind::Tuple(elems) => {
                write!(f, "tuple<")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, ">")
            }
            InternedTyKind::Length => write!(f, "length"),
            InternedTyKind::PhysicalLength => write!(f, "physical-length"),
            InternedTyKind::Angle => write!(f, "angle"),
            InternedTyKind::Duration => write!(f, "duration"),
            InternedTyKind::Percent => write!(f, "percent"),
            InternedTyKind::RelativeFontSize => write!(f, "relative-font-size"),
            InternedTyKind::Color => write!(f, "color"),
            InternedTyKind::Brush => write!(f, "brush"),
            InternedTyKind::Image => write!(f, "image"),
            InternedTyKind::Easing => write!(f, "easing"),
            InternedTyKind::Adt(def_id) => write!(f, "adt({})", def_id),
            InternedTyKind::Func { params, ret } => {
                write!(f, "func(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ")")?;
                if let Some(ret) = ret {
                    write!(f, " -> {}", ret)?;
                }
                Ok(())
            }
            InternedTyKind::Error => write!(f, "error"),
            InternedTyKind::Unknown => write!(f, "unknown"),
            InternedTyKind::Unit => write!(f, "unit"),
        }
    }
}

/// Type interner that deduplicates types.
#[derive(Debug)]
pub struct TypeInterner {
    /// Map from type to its interned handle.
    cache: HashMap<InternedTyKind, Ty>,
    /// All interned types.
    types: Vec<InternedTyKind>,
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInterner {
    /// Create a new type interner with pre-interned common types.
    pub fn new() -> Self {
        let mut interner = Self {
            cache: HashMap::new(),
            types: Vec::new(),
        };

        // Pre-intern common types at known indices
        // These MUST match the Ty::* constants
        assert_eq!(interner.intern(InternedTyKind::Error), Ty::ERROR);
        assert_eq!(interner.intern(InternedTyKind::Unit), Ty::UNIT);
        assert_eq!(interner.intern(InternedTyKind::Bool), Ty::BOOL);
        assert_eq!(interner.intern(InternedTyKind::S32), Ty::S32);
        assert_eq!(interner.intern(InternedTyKind::String), Ty::STRING);

        interner
    }

    /// Intern a type, returning its handle.
    ///
    /// If the type was already interned, returns the existing handle.
    pub fn intern(&mut self, kind: InternedTyKind) -> Ty {
        if let Some(&ty) = self.cache.get(&kind) {
            return ty;
        }

        let ty = Ty(self.types.len() as u32);
        self.types.push(kind.clone());
        self.cache.insert(kind, ty);
        ty
    }

    /// Get the kind of an interned type.
    pub fn kind(&self, ty: Ty) -> &InternedTyKind {
        &self.types[ty.index()]
    }

    /// Intern a list type.
    pub fn intern_list(&mut self, elem: Ty) -> Ty {
        self.intern(InternedTyKind::List(elem))
    }

    /// Intern an option type.
    pub fn intern_option(&mut self, inner: Ty) -> Ty {
        self.intern(InternedTyKind::Option(inner))
    }

    /// Intern a tuple type.
    pub fn intern_tuple(&mut self, elems: Vec<Ty>) -> Ty {
        self.intern(InternedTyKind::Tuple(elems))
    }

    /// Intern a function type.
    pub fn intern_func(&mut self, params: Vec<Ty>, ret: Option<Ty>) -> Ty {
        self.intern(InternedTyKind::Func { params, ret })
    }

    /// Intern a user-defined type (record, enum, variant, component).
    pub fn intern_adt(&mut self, def_id: DefId) -> Ty {
        self.intern(InternedTyKind::Adt(def_id))
    }

    /// Check if a type is a primitive.
    pub fn is_primitive(&self, ty: Ty) -> bool {
        matches!(
            self.kind(ty),
            InternedTyKind::Bool
                | InternedTyKind::S8
                | InternedTyKind::S16
                | InternedTyKind::S32
                | InternedTyKind::S64
                | InternedTyKind::U8
                | InternedTyKind::U16
                | InternedTyKind::U32
                | InternedTyKind::U64
                | InternedTyKind::F32
                | InternedTyKind::F64
                | InternedTyKind::Char
                | InternedTyKind::String
        )
    }

    /// Convert an AST TyKind to an interned Ty.
    ///
    /// This is used during AST → HIR lowering.
    /// Named types remain unresolved (converted to Unknown) until name resolution.
    pub fn intern_ast_ty(&mut self, ast_ty: &AstTyKind) -> Ty {
        match ast_ty {
            AstTyKind::Bool => Ty::BOOL,
            AstTyKind::S8 => self.intern(InternedTyKind::S8),
            AstTyKind::S16 => self.intern(InternedTyKind::S16),
            AstTyKind::S32 => Ty::S32,
            AstTyKind::S64 => self.intern(InternedTyKind::S64),
            AstTyKind::U8 => self.intern(InternedTyKind::U8),
            AstTyKind::U16 => self.intern(InternedTyKind::U16),
            AstTyKind::U32 => self.intern(InternedTyKind::U32),
            AstTyKind::U64 => self.intern(InternedTyKind::U64),
            AstTyKind::F32 => self.intern(InternedTyKind::F32),
            AstTyKind::F64 => self.intern(InternedTyKind::F64),
            AstTyKind::Char => self.intern(InternedTyKind::Char),
            AstTyKind::String => Ty::STRING,

            AstTyKind::List(inner) => {
                let inner_ty = self.intern_ast_ty(&inner.kind);
                self.intern_list(inner_ty)
            }
            AstTyKind::Option(inner) => {
                let inner_ty = self.intern_ast_ty(&inner.kind);
                self.intern_option(inner_ty)
            }
            AstTyKind::Result { ok, err } => {
                let ok_ty = ok.as_ref().map(|t| self.intern_ast_ty(&t.kind));
                let err_ty = err.as_ref().map(|t| self.intern_ast_ty(&t.kind));
                self.intern(InternedTyKind::Result {
                    ok: ok_ty,
                    err: err_ty,
                })
            }
            AstTyKind::Tuple(elems) => {
                let elem_tys: Vec<_> = elems.iter().map(|t| self.intern_ast_ty(&t.kind)).collect();
                self.intern_tuple(elem_tys)
            }

            AstTyKind::Length => self.intern(InternedTyKind::Length),
            AstTyKind::PhysicalLength => self.intern(InternedTyKind::PhysicalLength),
            AstTyKind::Angle => self.intern(InternedTyKind::Angle),
            AstTyKind::Duration => self.intern(InternedTyKind::Duration),
            AstTyKind::Percent => self.intern(InternedTyKind::Percent),
            AstTyKind::RelativeFontSize => self.intern(InternedTyKind::RelativeFontSize),
            AstTyKind::Color => self.intern(InternedTyKind::Color),
            AstTyKind::Brush => self.intern(InternedTyKind::Brush),
            AstTyKind::Image => self.intern(InternedTyKind::Image),
            AstTyKind::Easing => self.intern(InternedTyKind::Easing),

            AstTyKind::Func {
                params,
                return_type,
            } => {
                let param_tys: Vec<_> = params.iter().map(|(_, t)| self.intern_ast_ty(&t.kind)).collect();
                let ret_ty = return_type.as_ref().map(|t| self.intern_ast_ty(&t.kind));
                self.intern_func(param_tys, ret_ty)
            }

            AstTyKind::Named(_) => {
                // Named types need resolution - return Unknown for now
                self.intern(InternedTyKind::Unknown)
            }

            AstTyKind::Unknown => self.intern(InternedTyKind::Unknown),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_interning_deduplication() {
        let mut interner = TypeInterner::new();

        let t1 = interner.intern(InternedTyKind::S32);
        let t2 = interner.intern(InternedTyKind::S32);
        let t3 = interner.intern(InternedTyKind::String);

        assert_eq!(t1, t2);
        assert_ne!(t1, t3);
    }

    #[test]
    fn test_pre_interned_types() {
        let interner = TypeInterner::new();

        assert!(matches!(interner.kind(Ty::ERROR), InternedTyKind::Error));
        assert!(matches!(interner.kind(Ty::BOOL), InternedTyKind::Bool));
        assert!(matches!(interner.kind(Ty::S32), InternedTyKind::S32));
        assert!(matches!(interner.kind(Ty::STRING), InternedTyKind::String));
    }

    #[test]
    fn test_compound_type_interning() {
        let mut interner = TypeInterner::new();

        let list_s32_1 = interner.intern_list(Ty::S32);
        let list_s32_2 = interner.intern_list(Ty::S32);
        let list_string = interner.intern_list(Ty::STRING);

        assert_eq!(list_s32_1, list_s32_2);
        assert_ne!(list_s32_1, list_string);
    }

    #[test]
    fn test_is_primitive() {
        let mut interner = TypeInterner::new();

        assert!(interner.is_primitive(Ty::BOOL));
        assert!(interner.is_primitive(Ty::S32));
        assert!(interner.is_primitive(Ty::STRING));

        let list_ty = interner.intern_list(Ty::S32);
        assert!(!interner.is_primitive(list_ty));
    }
}
