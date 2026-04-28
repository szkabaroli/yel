
use std::sync::atomic::{AtomicU32, Ordering};

use serde::{Deserialize, Serialize};

/// Unique identifier for AST nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub struct NodeId(pub u32);

impl NodeId {
    /// Create a new unique node ID.
    pub fn new() -> Self {
        static NEXT_ID: AtomicU32 = AtomicU32::new(0);
        NodeId(NEXT_ID.fetch_add(1, Ordering::Relaxed))
    }

    /// Create a dummy node ID (for testing).
    pub fn dummy() -> Self {
        NodeId(u32::MAX)
    }
}