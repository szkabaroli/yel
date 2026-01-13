//! Central compiler context.

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;

use crate::definitions::{Definitions, Namespace};
use crate::diagnostic::Diagnostics;
use crate::ids::{BlockId, DefId};
use crate::interner::{Interner, Name};
use crate::known::KnownDefinitions;
use crate::source::SourceMap;
use crate::syntax::ast::TyKind as AstTyKind;
use crate::types::{InternedTyKind, Ty, TypeInterner};

/// Stores global compiler state.
///
/// This is the equivalent of rustc's TyCtxt - the single source of truth
/// for all compiler data.
pub struct CompilerContext {
    /// String interner (shared, thread-safe).
    pub interner: Arc<Interner>,
    /// Type interner.
    pub types: TypeInterner,
    /// All definitions.
    pub defs: Definitions,
    /// Known builtin definitions.
    pub known: KnownDefinitions,
    /// Source file management.
    pub source_map: SourceMap,
    /// Accumulated diagnostics.
    pub diagnostics: Diagnostics,
    /// Debug names for LIR blocks: (component DefId, BlockId) -> name
    /// Uses RefCell for interior mutability since blocks are named during lowering.
    block_names: RefCell<HashMap<(DefId, BlockId), String>>,
}

impl Default for CompilerContext {
    fn default() -> Self {
        Self::new()
    }
}

impl CompilerContext {
    /// Create a new compiler context.
    pub fn new() -> Self {
        Self {
            interner: Arc::new(Interner::new()),
            types: TypeInterner::new(),
            defs: Definitions::new(),
            known: KnownDefinitions::new(),
            source_map: SourceMap::new(),
            diagnostics: Diagnostics::new(),
            block_names: RefCell::new(HashMap::new()),
        }
    }

    /// Create with a shared interner.
    pub fn with_interner(interner: Arc<Interner>) -> Self {
        Self {
            interner,
            types: TypeInterner::new(),
            defs: Definitions::new(),
            known: KnownDefinitions::new(),
            source_map: SourceMap::new(),
            diagnostics: Diagnostics::new(),
            block_names: RefCell::new(HashMap::new()),
        }
    }

    // ========================================================================
    // String interning
    // ========================================================================

    /// Intern a string.
    pub fn intern(&self, s: &str) -> Name {
        self.interner.intern(s)
    }

    /// Get the string for an interned name.
    pub fn str(&self, name: Name) -> String {
        self.interner.str(name).to_string()
    }

    // ========================================================================
    // Type interning
    // ========================================================================

    /// Intern a type.
    pub fn intern_ty(&mut self, kind: InternedTyKind) -> Ty {
        self.types.intern(kind)
    }

    /// Get the kind of an interned type.
    pub fn ty_kind(&self, ty: Ty) -> &InternedTyKind {
        self.types.kind(ty)
    }

    /// Intern a list type.
    pub fn mk_list(&mut self, elem: Ty) -> Ty {
        self.types.intern_list(elem)
    }

    /// Intern an option type.
    pub fn mk_option(&mut self, inner: Ty) -> Ty {
        self.types.intern_option(inner)
    }

    /// Intern a result type.
    pub fn mk_result(&mut self, ok: Ty, err: Ty) -> Ty {
        self.types.intern(InternedTyKind::Result {
            ok: Some(ok),
            err: Some(err),
        })
    }

    /// Intern a tuple type.
    pub fn mk_tuple(&mut self, elems: Vec<Ty>) -> Ty {
        self.types.intern_tuple(elems)
    }

    /// Intern a user-defined type.
    pub fn mk_adt(&mut self, def_id: DefId) -> Ty {
        self.types.intern_adt(def_id)
    }

    /// Intern a function type.
    pub fn mk_func(&mut self, params: Vec<Ty>, ret: Option<Ty>) -> Ty {
        self.types.intern_func(params, ret)
    }

    /// Intern an AST type, resolving named types to their definitions.
    ///
    /// This is the preferred way to convert AST types to interned types,
    /// as it properly resolves named types (records, enums, variants) to
    /// ADT types with their DefId.
    pub fn intern_ast_ty(&mut self, ast_ty: &AstTyKind) -> Ty {
        match ast_ty {
            AstTyKind::Bool => Ty::BOOL,
            AstTyKind::S8 => self.types.intern(InternedTyKind::S8),
            AstTyKind::S16 => self.types.intern(InternedTyKind::S16),
            AstTyKind::S32 => Ty::S32,
            AstTyKind::S64 => self.types.intern(InternedTyKind::S64),
            AstTyKind::U8 => self.types.intern(InternedTyKind::U8),
            AstTyKind::U16 => self.types.intern(InternedTyKind::U16),
            AstTyKind::U32 => self.types.intern(InternedTyKind::U32),
            AstTyKind::U64 => self.types.intern(InternedTyKind::U64),
            AstTyKind::F32 => self.types.intern(InternedTyKind::F32),
            AstTyKind::F64 => self.types.intern(InternedTyKind::F64),
            AstTyKind::Char => self.types.intern(InternedTyKind::Char),
            AstTyKind::String => Ty::STRING,

            AstTyKind::List(inner) => {
                let inner_ty = self.intern_ast_ty(&inner.kind);
                self.types.intern_list(inner_ty)
            }
            AstTyKind::Option(inner) => {
                let inner_ty = self.intern_ast_ty(&inner.kind);
                self.types.intern_option(inner_ty)
            }
            AstTyKind::Result { ok, err } => {
                let ok_ty = ok.as_ref().map(|t| self.intern_ast_ty(&t.kind));
                let err_ty = err.as_ref().map(|t| self.intern_ast_ty(&t.kind));
                self.types.intern(InternedTyKind::Result { ok: ok_ty, err: err_ty })
            }
            AstTyKind::Tuple(elems) => {
                let elem_tys: Vec<_> = elems.iter().map(|t| self.intern_ast_ty(&t.kind)).collect();
                self.types.intern_tuple(elem_tys)
            }

            AstTyKind::Length => self.types.intern(InternedTyKind::Length),
            AstTyKind::PhysicalLength => self.types.intern(InternedTyKind::PhysicalLength),
            AstTyKind::Angle => self.types.intern(InternedTyKind::Angle),
            AstTyKind::Duration => self.types.intern(InternedTyKind::Duration),
            AstTyKind::Percent => self.types.intern(InternedTyKind::Percent),
            AstTyKind::RelativeFontSize => self.types.intern(InternedTyKind::RelativeFontSize),
            AstTyKind::Color => self.types.intern(InternedTyKind::Color),
            AstTyKind::Brush => self.types.intern(InternedTyKind::Brush),
            AstTyKind::Image => self.types.intern(InternedTyKind::Image),
            AstTyKind::Easing => self.types.intern(InternedTyKind::Easing),

            AstTyKind::Func { params, return_type } => {
                let param_tys: Vec<_> = params.iter().map(|(_, t)| self.intern_ast_ty(&t.kind)).collect();
                let ret_ty = return_type.as_ref().map(|t| self.intern_ast_ty(&t.kind));
                self.types.intern_func(param_tys, ret_ty)
            }

            AstTyKind::Named(name) => {
                // Look up the named type in definitions
                let name_interned = self.interner.intern(name);
                if let Some(def_id) = self.defs.lookup(name_interned, Namespace::Type) {
                    // Create an ADT type with the definition's DefId
                    self.types.intern_adt(def_id)
                } else {
                    // Type not found - return Unknown and let type checker report error
                    self.types.intern(InternedTyKind::Unknown)
                }
            }

            AstTyKind::Unknown => self.types.intern(InternedTyKind::Unknown),
        }
    }

    // ========================================================================
    // Definition access
    // ========================================================================

    /// Get the type of a definition.
    pub fn type_of(&self, def_id: DefId) -> Option<Ty> {
        self.defs.type_of(def_id)
    }

    /// Look up a type definition by name.
    pub fn lookup_type(&self, name: &str) -> Option<DefId> {
        let n = self.interner.intern(name);
        self.defs.lookup(n, Namespace::Type)
    }

    /// Look up a component by name.
    pub fn lookup_component(&self, name: &str) -> Option<DefId> {
        let n = self.interner.intern(name);
        self.defs.lookup(n, Namespace::Component)
    }

    /// Look up a value (function, etc.) by name.
    pub fn lookup_value(&self, name: &str) -> Option<DefId> {
        let n = self.interner.intern(name);
        self.defs.lookup(n, Namespace::Value)
    }

    // ========================================================================
    // Diagnostics
    // ========================================================================

    /// Check if there are any errors.
    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }

    /// Get the error count.
    pub fn error_count(&self) -> usize {
        self.diagnostics.error_count()
    }

    /// Render all diagnostics.
    pub fn render_diagnostics(&self) -> String {
        self.diagnostics.render(&self.source_map)
    }

    // ========================================================================
    // Block debug names
    // ========================================================================

    /// Register a debug name for a block.
    /// Uses interior mutability to allow naming blocks during lowering.
    pub fn set_block_name(&self, comp_def_id: DefId, block_id: BlockId, name: String) {
        self.block_names.borrow_mut().insert((comp_def_id, block_id), name);
    }

    /// Get the debug name for a block.
    /// Returns a cloned string since we can't return a reference to RefCell contents.
    pub fn get_block_name(&self, comp_def_id: DefId, block_id: BlockId) -> Option<String> {
        self.block_names.borrow().get(&(comp_def_id, block_id)).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_basic() {
        let ctx = CompilerContext::new();

        let name = ctx.intern("test");
        assert_eq!(ctx.str(name), "test");
    }

    #[test]
    fn test_context_types() {
        let mut ctx = CompilerContext::new();

        let list_s32 = ctx.mk_list(Ty::S32);
        assert!(matches!(
            ctx.ty_kind(list_s32),
            InternedTyKind::List(Ty::S32)
        ));
    }

    #[test]
    fn test_context_shared_interner() {
        let interner = Arc::new(Interner::new());
        let name1 = interner.intern("shared");

        let ctx = CompilerContext::with_interner(interner.clone());
        let name2 = ctx.intern("shared");

        assert_eq!(name1, name2);
    }
}
