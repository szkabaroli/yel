//! Ported from ark's `arkc-frontend/src/sym.rs` — the file, not the idea.
//!
//! Same types, same names, same lookup order: [`ModuleSymTable::get`] walks
//! block `levels` innermost-first, then `outer` (the module's declarations),
//! then the `prelude` — ark carries `dependencies` and `prelude` as
//! commented-out slots; here they are live, because yel already has both
//! (included modules live in the outer table; intrinsics and lang items are
//! the prelude).
//!
//! Deviations, each forced and nothing else:
//!
//! - **`SymbolKind` payloads are yel's ids.** Ark stores `Struct(HirId)` /
//!   `FnDecl(HirId)` because its `SymTable` is the primary store; yel's
//!   primary store is [`Definitions`], so declaration symbols carry `DefId`
//!   at that table's granularity, plus the body-local kinds ark adds during
//!   checking (`Var`) and yel's own (`Prop`, `Intrinsic`).
//! - **`outer` and `prelude` are consulted through a parameter.** Ark holds
//!   `Rc<SymTable>`; yel's outer table is `Definitions` on the
//!   `CompilerContext`, which the lowering also mutates, so `get` takes the
//!   context instead of owning a borrow. Interface otherwise unchanged.

use rustc_hash::FxHashMap as HashMap;
use yelc_base::Name;
use yelc_sema::{CompilerContext, DefId, ModuleId, Sym};

use crate::ids::LocalId;

/// What a name resolves to. Ark's enum with yel's payloads.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SymbolKind {
    /// A declared type — record, enum, variant (`Definitions` granularity).
    Type(DefId),
    /// A value — a top-level function or constant.
    Value(DefId),
    /// A component, element, or extern component.
    Component(DefId),
    /// A global singleton.
    Global(DefId),
    /// A module bound by an `include`.
    Module(ModuleId),
    /// A body-local variable — parameter, `let`, binder. Ark inserts these
    /// during checking (`add_local`); yel inserts them during lowering.
    Var(LocalId),
    /// A property or member function of the enclosing item, pushed as a
    /// level when a body opens (yel's addition — components put their
    /// members in scope; ark's structs do not).
    Prop { owner: DefId, member: Name },
    /// An intrinsic overload set — the prelude's contribution.
    Intrinsic(Name),
}

impl SymbolKind {
    pub fn is_module(&self) -> bool {
        matches!(self, SymbolKind::Module(_))
    }

    pub fn to_module(&self) -> Option<ModuleId> {
        match self {
            SymbolKind::Module(id) => Some(*id),
            _ => None,
        }
    }

    pub fn is_var(&self) -> bool {
        matches!(self, SymbolKind::Var(_))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Symbol {
    kind: SymbolKind,
}

impl Symbol {
    pub fn kind(&self) -> &SymbolKind {
        &self.kind
    }
}

/// A flat `name → Symbol` map. Ark's `SymTable`, verbatim.
#[derive(Clone, Debug, Default)]
pub struct SymTable {
    table: HashMap<Name, Symbol>,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            table: HashMap::default(),
        }
    }

    pub fn get(&self, name: Name) -> Option<SymbolKind> {
        self.table.get(&name).map(|sym| sym.kind)
    }

    pub fn get_sym(&self, name: Name) -> Option<&Symbol> {
        self.table.get(&name)
    }

    /// Insert, returning the shadowed symbol if the name was taken — ark's
    /// shadow-reporting contract.
    pub fn insert(&mut self, name: Name, kind: SymbolKind) -> Option<Symbol> {
        self.table.insert(name, Symbol { kind })
    }
}

/// The composed resolution view: block levels over the module's declarations
/// over the prelude. Ark's `ModuleSymTable`.
pub struct ModuleSymTable {
    levels: Vec<SymTable>,
}

impl Default for ModuleSymTable {
    fn default() -> ModuleSymTable {
        ModuleSymTable::new()
    }
}

impl ModuleSymTable {
    pub fn new() -> ModuleSymTable {
        ModuleSymTable { levels: Vec::new() }
    }

    pub fn push_level(&mut self) {
        self.levels.push(SymTable::new());
    }

    pub fn pop_level(&mut self) {
        assert!(!self.levels.is_empty());
        self.levels.pop();
    }

    pub fn levels(&mut self) -> usize {
        self.levels.len()
    }

    /// Ark's walk, with the commented-out slots live: levels innermost-first,
    /// then `outer` (= [`Definitions`], which already holds included modules —
    /// the `dependencies` slot), then the `prelude` (= the intrinsic table).
    pub fn get(&self, sema: &CompilerContext, name: Name) -> Option<SymbolKind> {
        for level in self.levels.iter().rev() {
            if let Some(val) = level.get(name) {
                return Some(val);
            }
        }

        if let Some(sym) = sema.defs.lookup(name).first() {
            return Some(match sym {
                Sym::Type(id) => SymbolKind::Type(*id),
                Sym::Value(id) => SymbolKind::Value(*id),
                Sym::Component(id) => SymbolKind::Component(*id),
                Sym::Global(id) => SymbolKind::Global(*id),
                Sym::Module(id) => SymbolKind::Module(*id),
            });
        }

        if !sema.intrinsics.overloads(name).is_empty() {
            return Some(SymbolKind::Intrinsic(name));
        }

        None
    }

    pub fn get_string(&self, sema: &CompilerContext, name: &str) -> Option<SymbolKind> {
        let interned_name = sema.names.intern(name);
        self.get(sema, interned_name)
    }

    /// Insert into the innermost level. Panics with no level open, like ark's
    /// `last_mut().unwrap()`.
    pub fn insert(&mut self, name: Name, kind: SymbolKind) -> Option<Symbol> {
        self.levels.last_mut().unwrap().insert(name, kind)
    }
}
