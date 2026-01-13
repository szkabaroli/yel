//! Unique identifiers for compiler definitions.

use serde::Serialize;
use std::fmt;

/// A unique identifier for any named definition in the program.
///
/// DefId is the universal identifier for:
/// - Components
/// - Records, Enums, Variants
/// - Functions
/// - Fields (within their owner)
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, Serialize)]
pub struct DefId(pub u32);

impl DefId {
    pub const INVALID: DefId = DefId(u32::MAX);

    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }

    pub fn is_valid(self) -> bool {
        self.0 != u32::MAX
    }
}

impl fmt::Display for DefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "def#{}", self.0)
    }
}

/// Index into fields of a record or component.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, Serialize)]
pub struct FieldIdx(pub u32);

impl FieldIdx {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for FieldIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "field#{}", self.0)
    }
}

/// Index into cases of an enum or variant.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, Serialize)]
pub struct VariantIdx(pub u32);

impl VariantIdx {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for VariantIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "variant#{}", self.0)
    }
}

/// Local variable identifier within a body.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, Serialize)]
pub struct LocalId(pub u32);

impl LocalId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for LocalId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "local#{}", self.0)
    }
}

/// Expression identifier within a THIR body.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, Serialize)]
pub struct ExprId(pub u32);

impl ExprId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for ExprId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "expr#{}", self.0)
    }
}

/// Node identifier for UI nodes.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, Serialize)]
pub struct NodeId(pub u32);

impl NodeId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "node#{}", self.0)
    }
}

/// Block identifier for LIR blocks within a component.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Default, Serialize)]
pub struct BlockId(pub u32);

impl BlockId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "block#{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_def_id() {
        let id = DefId::new(42);
        assert_eq!(id.index(), 42);
        assert!(id.is_valid());
        assert!(!DefId::INVALID.is_valid());
    }

    #[test]
    fn test_field_idx() {
        let idx = FieldIdx::new(3);
        assert_eq!(idx.index(), 3);
    }

    #[test]
    fn test_local_id() {
        let id = LocalId::new(5);
        assert_eq!(id.index(), 5);
    }
}
