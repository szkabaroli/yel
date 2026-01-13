//! Local scope for tracking variables within a body.

use crate::ids::{DefId, LocalId};
use crate::index_vec::IndexVec;
use crate::interner::Name;
use crate::source::Span;
use crate::types::Ty;
use std::collections::HashMap;

/// Information about a local variable.
#[derive(Debug, Clone)]
pub struct LocalInfo {
    pub name: Name,
    pub ty: Ty,
    pub span: Span,
    /// Optional DefId if this local corresponds to a property/signal.
    pub def_id: Option<DefId>,
}

/// Scoped local variable tracker.
#[derive(Debug, Clone)]
pub struct LocalScope {
    /// All locals in this body.
    locals: IndexVec<LocalId, LocalInfo>,
    /// Current scope: name → LocalId.
    current: HashMap<Name, LocalId>,
    /// Parent scopes (for nested blocks).
    stack: Vec<HashMap<Name, LocalId>>,
}

impl Default for LocalScope {
    fn default() -> Self {
        Self::new()
    }
}

impl LocalScope {
    pub fn new() -> Self {
        Self {
            locals: IndexVec::new(),
            current: HashMap::new(),
            stack: Vec::new(),
        }
    }

    /// Define a new local variable.
    pub fn define(&mut self, name: Name, ty: Ty, span: Span) -> LocalId {
        self.define_with_def_id(name, ty, span, None)
    }

    /// Define a new local variable that corresponds to a property/signal.
    pub fn define_with_def_id(&mut self, name: Name, ty: Ty, span: Span, def_id: Option<DefId>) -> LocalId {
        let id = self.locals.push(LocalInfo { name, ty, span, def_id });
        self.current.insert(name, id);
        id
    }

    /// Look up a local by name.
    pub fn lookup(&self, name: Name) -> Option<LocalId> {
        if let Some(&id) = self.current.get(&name) {
            return Some(id);
        }
        for scope in self.stack.iter().rev() {
            if let Some(&id) = scope.get(&name) {
                return Some(id);
            }
        }
        None
    }

    /// Get info about a local.
    pub fn get(&self, id: LocalId) -> &LocalInfo {
        &self.locals[id]
    }

    /// Push a new nested scope.
    pub fn push_scope(&mut self) {
        self.stack.push(std::mem::take(&mut self.current));
    }

    /// Pop a nested scope.
    pub fn pop_scope(&mut self) {
        self.current = self.stack.pop().unwrap_or_default();
    }

    /// Iterate all locals.
    pub fn iter(&self) -> impl Iterator<Item = (LocalId, &LocalInfo)> {
        self.locals.iter_enumerated()
    }

    /// Number of locals.
    pub fn len(&self) -> usize {
        self.locals.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.locals.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interner::Interner;
    use crate::source::SourceId;

    fn dummy_span() -> Span {
        Span::new(SourceId(0), 0, 0)
    }

    #[test]
    fn test_local_scope_define_and_lookup() {
        let interner = Interner::new();
        let mut scope = LocalScope::new();

        let x_name = interner.intern("x");
        let x_id = scope.define(x_name, Ty::S32, dummy_span());

        assert_eq!(scope.lookup(x_name), Some(x_id));
        assert_eq!(scope.get(x_id).ty, Ty::S32);
    }

    #[test]
    fn test_local_scope_nested() {
        let interner = Interner::new();
        let mut scope = LocalScope::new();

        let x_name = interner.intern("x");
        let outer_x = scope.define(x_name, Ty::S32, dummy_span());

        scope.push_scope();

        let y_name = interner.intern("y");
        let y_id = scope.define(y_name, Ty::BOOL, dummy_span());

        // Inner scope can see outer
        assert_eq!(scope.lookup(x_name), Some(outer_x));
        assert_eq!(scope.lookup(y_name), Some(y_id));

        // Shadow x in inner scope
        let inner_x = scope.define(x_name, Ty::STRING, dummy_span());
        assert_eq!(scope.lookup(x_name), Some(inner_x));
        assert_ne!(inner_x, outer_x);

        scope.pop_scope();

        // Back to outer scope - x is original
        assert_eq!(scope.lookup(x_name), Some(outer_x));
        // y is no longer visible
        assert_eq!(scope.lookup(y_name), None);
    }
}
