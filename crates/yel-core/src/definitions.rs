//! Central storage for all definitions in the program.

use crate::ids::{DefId, FieldIdx, VariantIdx};
use crate::index_vec::IndexVec;
use crate::interner::Name;
use crate::source::Span;
use crate::types::Ty;
use std::collections::HashMap;

/// All definitions in the compilation unit.
pub struct Definitions {
    /// All definition items.
    items: IndexVec<DefId, DefItem>,
    /// Type of each definition (for fields, params, etc.).
    types: HashMap<DefId, Ty>,
    /// Name to DefId lookup (by namespace).
    names: HashMap<(Name, Namespace), DefId>,
}

/// Metadata for a definition.
#[derive(Debug, Clone)]
pub struct DefItem {
    /// The interned name.
    pub name: Name,
    /// What kind of definition this is.
    pub kind: DefKind,
    /// Source location.
    pub span: Span,
}

/// The kind of a definition.
#[derive(Debug, Clone)]
pub enum DefKind {
    /// A UI component.
    Component(ComponentDef),
    /// A record type.
    Record(RecordDef),
    /// An enum type (simple cases).
    Enum(EnumDef),
    /// A variant type (cases with payloads).
    Variant(VariantDef),
    /// A function.
    Function(FunctionDef),
    /// A field of a record.
    Field(FieldDef),
    /// A signal (reactive property) of a component.
    Signal(SignalDef),
    /// A case of an enum/variant.
    VariantCase(VariantCaseDef),
    /// A parameter of a function.
    Parameter(ParameterDef),
    /// A local variable.
    Local,
}

/// Namespace for name resolution.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Namespace {
    /// Types (records, enums, variants).
    Type,
    /// Values (functions, constants).
    Value,
    /// UI Components.
    Component,
}

/// Component definition.
#[derive(Clone, Debug)]
pub struct ComponentDef {
    /// DefId of this component.
    pub def_id: DefId,
    /// Component name.
    pub name: Name,
    /// Property DefIds (in order).
    pub properties: Vec<DefId>,
    /// Callback/function DefIds.
    pub callbacks: Vec<DefId>,
    /// Whether exported.
    pub is_export: bool,
}

/// Record definition.
#[derive(Clone, Debug)]
pub struct RecordDef {
    pub def_id: DefId,
    pub name: Name,
    /// Field DefIds (in order, index = FieldIdx).
    pub fields: Vec<DefId>,
}

/// Enum definition.
#[derive(Clone, Debug)]
pub struct EnumDef {
    pub def_id: DefId,
    pub name: Name,
    /// Case DefIds (in order, index = VariantIdx).
    pub cases: Vec<DefId>,
}

/// Variant definition.
#[derive(Clone, Debug)]
pub struct VariantDef {
    pub def_id: DefId,
    pub name: Name,
    /// Case DefIds (in order, index = VariantIdx).
    pub cases: Vec<DefId>,
}

/// Function definition.
#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub def_id: DefId,
    pub name: Name,
    /// Parameter DefIds.
    pub params: Vec<DefId>,
    /// Return type.
    pub ret_ty: Ty,
    /// Whether exported.
    pub is_export: bool,
}

/// Field definition (for records).
#[derive(Clone, Debug)]
pub struct FieldDef {
    /// DefId of the owning type.
    pub owner: DefId,
    /// Field name.
    pub name: Name,
    /// Field type.
    pub ty: Ty,
    /// Index within owner.
    pub idx: FieldIdx,
}

/// Signal definition (reactive property of a component).
#[derive(Clone, Debug)]
pub struct SignalDef {
    /// DefId of the owning component.
    pub owner: DefId,
    /// Signal name.
    pub name: Name,
    /// Signal type.
    pub ty: Ty,
    /// Index within owner.
    pub idx: FieldIdx,
    /// Default/initial value expression (from HIR, to be type-checked).
    pub default: Option<crate::hir::HirExpr>,
}

/// Variant case definition.
#[derive(Clone, Debug)]
pub struct VariantCaseDef {
    /// DefId of the owning variant/enum.
    pub owner: DefId,
    /// Case name.
    pub name: Name,
    /// Optional payload type (None for enums and unit variants).
    pub payload: Option<Ty>,
    /// Index within owner.
    pub idx: VariantIdx,
}

/// Parameter definition.
#[derive(Clone, Debug)]
pub struct ParameterDef {
    /// DefId of the owning function.
    pub owner: DefId,
    /// Parameter name.
    pub name: Name,
    /// Parameter type.
    pub ty: Ty,
    /// Index within function parameters.
    pub idx: u32,
}

impl Default for Definitions {
    fn default() -> Self {
        Self::new()
    }
}

impl Definitions {
    pub fn new() -> Self {
        Self {
            items: IndexVec::new(),
            types: HashMap::new(),
            names: HashMap::new(),
        }
    }

    /// Allocate a new definition.
    pub fn alloc(&mut self, name: Name, kind: DefKind, span: Span) -> DefId {
        self.items.push(DefItem { name, kind, span })
    }

    /// Register a name in a namespace.
    pub fn register_name(&mut self, name: Name, ns: Namespace, def_id: DefId) {
        self.names.insert((name, ns), def_id);
    }

    /// Look up a definition by name and namespace.
    pub fn lookup(&self, name: Name, ns: Namespace) -> Option<DefId> {
        self.names.get(&(name, ns)).copied()
    }

    /// Get a definition by ID.
    pub fn get(&self, def_id: DefId) -> &DefItem {
        &self.items[def_id]
    }

    /// Get a mutable reference to a definition by ID.
    pub fn get_mut(&mut self, def_id: DefId) -> &mut DefItem {
        &mut self.items[def_id]
    }

    /// Get the kind of a definition.
    pub fn kind(&self, def_id: DefId) -> &DefKind {
        &self.items[def_id].kind
    }

    /// Get the name of a definition.
    pub fn name(&self, def_id: DefId) -> Name {
        self.items[def_id].name
    }

    /// Get the span of a definition.
    pub fn span(&self, def_id: DefId) -> Span {
        self.items[def_id].span
    }

    /// Set the type for a definition.
    pub fn set_type(&mut self, def_id: DefId, ty: Ty) {
        self.types.insert(def_id, ty);
    }

    /// Get the type of a definition.
    pub fn type_of(&self, def_id: DefId) -> Option<Ty> {
        self.types.get(&def_id).copied()
    }

    /// Get as a component.
    pub fn as_component(&self, def_id: DefId) -> Option<&ComponentDef> {
        match &self.items[def_id].kind {
            DefKind::Component(c) => Some(c),
            _ => None,
        }
    }

    /// Get as a mutable component.
    pub fn as_component_mut(&mut self, def_id: DefId) -> Option<&mut ComponentDef> {
        match &mut self.items[def_id].kind {
            DefKind::Component(c) => Some(c),
            _ => None,
        }
    }

    /// Get as a record.
    pub fn as_record(&self, def_id: DefId) -> Option<&RecordDef> {
        match &self.items[def_id].kind {
            DefKind::Record(r) => Some(r),
            _ => None,
        }
    }

    /// Get as a mutable record.
    pub fn as_record_mut(&mut self, def_id: DefId) -> Option<&mut RecordDef> {
        match &mut self.items[def_id].kind {
            DefKind::Record(r) => Some(r),
            _ => None,
        }
    }

    /// Get as an enum.
    pub fn as_enum(&self, def_id: DefId) -> Option<&EnumDef> {
        match &self.items[def_id].kind {
            DefKind::Enum(e) => Some(e),
            _ => None,
        }
    }

    /// Get as a mutable enum.
    pub fn as_enum_mut(&mut self, def_id: DefId) -> Option<&mut EnumDef> {
        match &mut self.items[def_id].kind {
            DefKind::Enum(e) => Some(e),
            _ => None,
        }
    }

    /// Get as a variant.
    pub fn as_variant(&self, def_id: DefId) -> Option<&VariantDef> {
        match &self.items[def_id].kind {
            DefKind::Variant(v) => Some(v),
            _ => None,
        }
    }

    /// Get as a mutable variant.
    pub fn as_variant_mut(&mut self, def_id: DefId) -> Option<&mut VariantDef> {
        match &mut self.items[def_id].kind {
            DefKind::Variant(v) => Some(v),
            _ => None,
        }
    }

    /// Get as a function.
    pub fn as_function(&self, def_id: DefId) -> Option<&FunctionDef> {
        match &self.items[def_id].kind {
            DefKind::Function(f) => Some(f),
            _ => None,
        }
    }

    /// Get as a mutable function.
    pub fn as_function_mut(&mut self, def_id: DefId) -> Option<&mut FunctionDef> {
        match &mut self.items[def_id].kind {
            DefKind::Function(f) => Some(f),
            _ => None,
        }
    }

    /// Get as a field.
    pub fn as_field(&self, def_id: DefId) -> Option<&FieldDef> {
        match &self.items[def_id].kind {
            DefKind::Field(f) => Some(f),
            _ => None,
        }
    }

    /// Get as a signal.
    pub fn as_signal(&self, def_id: DefId) -> Option<&SignalDef> {
        match &self.items[def_id].kind {
            DefKind::Signal(s) => Some(s),
            _ => None,
        }
    }

    /// Get as a mutable signal.
    pub fn as_signal_mut(&mut self, def_id: DefId) -> Option<&mut SignalDef> {
        match &mut self.items[def_id].kind {
            DefKind::Signal(s) => Some(s),
            _ => None,
        }
    }

    /// Check if a definition is a signal (reactive property).
    pub fn is_signal(&self, def_id: DefId) -> bool {
        matches!(&self.items[def_id].kind, DefKind::Signal(_))
    }

    /// Find field by name within a record.
    pub fn find_field(&self, owner: DefId, field_name: Name) -> Option<(FieldIdx, DefId)> {
        let fields = match self.kind(owner) {
            DefKind::Record(r) => &r.fields,
            _ => return None,
        };

        for (idx, &field_def_id) in fields.iter().enumerate() {
            if self.items[field_def_id].name == field_name {
                return Some((FieldIdx::new(idx as u32), field_def_id));
            }
        }
        None
    }

    /// Find signal by name within a component.
    pub fn find_signal(&self, owner: DefId, signal_name: Name) -> Option<(FieldIdx, DefId)> {
        let signals = match self.kind(owner) {
            DefKind::Component(c) => &c.properties,
            _ => return None,
        };

        for (idx, &signal_def_id) in signals.iter().enumerate() {
            if self.items[signal_def_id].name == signal_name {
                return Some((FieldIdx::new(idx as u32), signal_def_id));
            }
        }
        None
    }

    /// Find field or signal by name (works for both records and components).
    pub fn find_member(&self, owner: DefId, name: Name) -> Option<(FieldIdx, DefId)> {
        self.find_field(owner, name).or_else(|| self.find_signal(owner, name))
    }

    /// Get all component DefIds.
    pub fn components(&self) -> impl Iterator<Item = DefId> + '_ {
        self.items
            .iter_enumerated()
            .filter_map(|(id, item)| match &item.kind {
                DefKind::Component(_) => Some(id),
                _ => None,
            })
    }

    /// Get all record DefIds.
    pub fn records(&self) -> impl Iterator<Item = DefId> + '_ {
        self.items
            .iter_enumerated()
            .filter_map(|(id, item)| match &item.kind {
                DefKind::Record(_) => Some(id),
                _ => None,
            })
    }

    /// Get all enum DefIds.
    pub fn enums(&self) -> impl Iterator<Item = DefId> + '_ {
        self.items
            .iter_enumerated()
            .filter_map(|(id, item)| match &item.kind {
                DefKind::Enum(_) => Some(id),
                _ => None,
            })
    }

    /// Get all variant DefIds.
    pub fn variants(&self) -> impl Iterator<Item = DefId> + '_ {
        self.items
            .iter_enumerated()
            .filter_map(|(id, item)| match &item.kind {
                DefKind::Variant(_) => Some(id),
                _ => None,
            })
    }

    /// Iterate over all definitions.
    pub fn iter(&self) -> impl Iterator<Item = (DefId, &DefItem)> {
        self.items.iter_enumerated()
    }

    /// Number of definitions.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
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
    fn test_definitions_basic() {
        let interner = Interner::new();
        let mut defs = Definitions::new();

        let name = interner.intern("Person");
        let def_id = defs.alloc(
            name,
            DefKind::Record(RecordDef {
                def_id: DefId(0), // Will be set correctly
                name,
                fields: vec![],
            }),
            dummy_span(),
        );

        defs.register_name(name, Namespace::Type, def_id);

        assert_eq!(defs.lookup(name, Namespace::Type), Some(def_id));
        assert!(defs.as_record(def_id).is_some());
    }

    #[test]
    fn test_field_lookup() {
        let interner = Interner::new();
        let mut defs = Definitions::new();

        // Create record
        let record_name = interner.intern("Point");
        let record_id = defs.alloc(
            record_name,
            DefKind::Record(RecordDef {
                def_id: DefId(0),
                name: record_name,
                fields: vec![],
            }),
            dummy_span(),
        );

        // Create fields
        let x_name = interner.intern("x");
        let x_id = defs.alloc(
            x_name,
            DefKind::Field(FieldDef {
                owner: record_id,
                name: x_name,
                ty: Ty::S32,
                idx: FieldIdx::new(0),
            }),
            dummy_span(),
        );

        let y_name = interner.intern("y");
        let y_id = defs.alloc(
            y_name,
            DefKind::Field(FieldDef {
                owner: record_id,
                name: y_name,
                ty: Ty::S32,
                idx: FieldIdx::new(1),
            }),
            dummy_span(),
        );

        // Update record to include fields
        if let Some(record) = defs.as_record_mut(record_id) {
            record.fields = vec![x_id, y_id];
        }

        // Look up fields
        let (idx, found_id) = defs.find_field(record_id, x_name).unwrap();
        assert_eq!(idx, FieldIdx::new(0));
        assert_eq!(found_id, x_id);

        let (idx, found_id) = defs.find_field(record_id, y_name).unwrap();
        assert_eq!(idx, FieldIdx::new(1));
        assert_eq!(found_id, y_id);

        // Non-existent field
        let z_name = interner.intern("z");
        assert!(defs.find_field(record_id, z_name).is_none());
    }
}
