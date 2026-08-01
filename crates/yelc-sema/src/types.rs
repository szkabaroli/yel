//! The type interner.
//!
//! [`Ty`] is a handle. Equal types share a handle, so equality is an integer
//! compare and there is exactly one place a type's structure lives.

use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fmt;

use crate::ids::DefId;

/// A handle into the [`TypeInterner`]. Equal types have equal handles.
///
/// # This type deliberately does not implement `Serialize` (decision B1)
///
/// The frozen tree derives `Serialize`/`Deserialize` on its equivalent
/// (`yel-core/src/types/interner.rs:13`), which means every struct containing a
/// `Ty` silently writes an **interner index** — a number whose meaning depends
/// on the order types happened to be interned in. It compiles perfectly and is
/// wrong on load.
///
/// Serialized positions must write the type's *structure* and re-intern on load,
/// which is Swift's rule: *"types are always serialized with enough info to
/// regenerate them at load time."* Not deriving the trait is what makes the
/// wrong thing a **type error** rather than a review finding — the enforcement
/// has to survive a contributor who has not read this comment.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Ty(u32);

impl Ty {
    fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// The structure behind a [`Ty`].
///
/// Recursion goes through `Ty` handles rather than boxes, so a nested type is
/// one `u32` and structural sharing is automatic.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TyKind {
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

    List(Ty),
    Option(Ty),
    Result {
        ok: Option<Ty>,
        err: Option<Ty>,
    },
    Tuple(Vec<Ty>),

    /// A user-defined record, enum, variant or component.
    Adt(DefId),

    Func {
        params: Vec<Ty>,
        ret: Option<Ty>,
    },

    /// The `T` in a *declaration* — a placeholder in a template's stored
    /// signature (decision A3).
    ///
    /// **Legal only inside a template's signature**, and gone by substitution at
    /// instantiation. Its presence is what lets a generic body be checked
    /// **once, generically**, so an error inside `filter` is reported in the
    /// stdlib rather than at the user's call site — the C++ template-error
    /// problem, avoided by construction.
    Param(u32),

    /// An unknown being solved during checking (decision A4).
    ///
    /// **Distinct from [`TyKind::Param`]**, and conflating the two is the error
    /// that pair of decisions exists to prevent: a `Param` is a placeholder in a
    /// *declaration* with a substitution to come, an `Infer` is a hole in an
    /// *inference* with a solution to come. Must be gone by the end of checking.
    Infer(u32),

    /// Recovery. Never a silent fallback — a diagnostic was pushed alongside it.
    Error,
    Unit,
}

/// Interns [`TyKind`]s to [`Ty`] handles.
///
/// Interior mutability so interning is available through a shared reference —
/// the same arrangement as `yelc_base::NameInterner`, and for the same reason: a
/// type is looked up far more often than the surrounding code holds `&mut`.
pub struct TypeInterner {
    inner: RefCell<Inner>,
}

struct Inner {
    kinds: Vec<TyKind>,
    lookup: FxHashMap<TyKind, Ty>,
}

macro_rules! primitives {
    ($($index:literal $konst:ident => $kind:ident),* $(,)?) => {
        impl Ty {
            $(
                #[doc = concat!("Pre-interned `", stringify!($kind), "`.")]
                pub const $konst: Ty = Ty($index);
            )*
        }

        impl Inner {
            /// Pre-intern the primitives so their handles are compile-time
            /// constants. The order here **is** the constant values above;
            /// the assertion below is what keeps them agreeing.
            fn with_primitives() -> Self {
                let mut inner = Inner {
                    kinds: Vec::new(),
                    lookup: FxHashMap::default(),
                };
                $(
                    let ty = inner.intern(TyKind::$kind);
                    assert_eq!(
                        ty, Ty::$konst,
                        "primitive constant disagrees with interning order",
                    );
                )*
                inner
            }
        }
    };
}

// The literal index must equal the position, because that is the order
// `with_primitives` interns them in. The generated `assert_eq!` is what makes a
// mismatch a startup failure rather than a wrong type days later.
primitives! {
    0  ERROR => Error,
    1  UNIT => Unit,
    2  BOOL => Bool,
    3  S8 => S8,
    4  S16 => S16,
    5  S32 => S32,
    6  S64 => S64,
    7  U8 => U8,
    8  U16 => U16,
    9  U32 => U32,
    10 U64 => U64,
    11 F32 => F32,
    12 F64 => F64,
    13 CHAR => Char,
    14 STRING => String,
}

impl Inner {
    fn intern(&mut self, kind: TyKind) -> Ty {
        if let Some(&existing) = self.lookup.get(&kind) {
            return existing;
        }
        let ty = Ty::new(self.kinds.len() as u32);
        self.kinds.push(kind.clone());
        self.lookup.insert(kind, ty);
        ty
    }
}

impl TypeInterner {
    pub fn new() -> Self {
        Self {
            inner: RefCell::new(Inner::with_primitives()),
        }
    }

    pub fn intern(&self, kind: TyKind) -> Ty {
        self.inner.borrow_mut().intern(kind)
    }

    pub fn kind(&self, ty: Ty) -> TyKind {
        self.inner.borrow().kinds[ty.index()].clone()
    }

    /// Number of distinct types interned. Test/diagnostic use.
    pub fn len(&self) -> usize {
        self.inner.borrow().kinds.len()
    }

    pub fn is_empty(&self) -> bool {
        false // the primitives are always present
    }
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for Ty {
    /// Prints the handle, not the type — rendering structure needs the interner.
    /// Use [`TypeInterner::kind`] and format that.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ty#{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn equal_types_share_a_handle() {
        let interner = TypeInterner::new();
        let a = interner.intern(TyKind::List(Ty::S32));
        let b = interner.intern(TyKind::List(Ty::S32));
        assert_eq!(a, b);
        assert_ne!(a, interner.intern(TyKind::List(Ty::STRING)));
    }

    #[test]
    fn primitive_constants_match_their_interned_kinds() {
        let interner = TypeInterner::new();
        for (konst, kind) in [
            (Ty::ERROR, TyKind::Error),
            (Ty::UNIT, TyKind::Unit),
            (Ty::BOOL, TyKind::Bool),
            (Ty::S32, TyKind::S32),
            (Ty::F64, TyKind::F64),
            (Ty::STRING, TyKind::String),
        ] {
            assert_eq!(interner.kind(konst), kind);
        }
    }

    /// `Param` and `Infer` are different holes; the interner must not conflate
    /// them just because both carry an index (decisions A3 / A4).
    #[test]
    fn param_and_infer_are_distinct_at_the_same_index() {
        let interner = TypeInterner::new();
        assert_ne!(
            interner.intern(TyKind::Param(0)),
            interner.intern(TyKind::Infer(0)),
        );
    }

    /// Interning is order-independent for the resulting *structure* — the same
    /// program must produce the same types whichever order they are reached in.
    #[test]
    fn interning_is_deterministic_across_orders() {
        let forward = TypeInterner::new();
        let a1 = forward.intern(TyKind::List(Ty::S32));
        let b1 = forward.intern(TyKind::Option(Ty::STRING));

        let backward = TypeInterner::new();
        let b2 = backward.intern(TyKind::Option(Ty::STRING));
        let a2 = backward.intern(TyKind::List(Ty::S32));

        assert_eq!(forward.kind(a1), backward.kind(a2));
        assert_eq!(forward.kind(b1), backward.kind(b2));
    }
}
