//! For-loop diff strategies.
//!
//! When a signal that drives a `for` loop's iterable changes, the
//! update_block runs. The naïve approach — unmount everything then
//! re-mount everything — is correct but wasteful: surviving iterations
//! lose their DOM nodes (and any nested state) and get re-created.
//!
//! Instead we emit a **diff** that reuses surviving iterations and only
//! touches the changed tail / reordered entries. The strategy is
//! pluggable so we can grow from an index-based unkeyed match (ship
//! today — handles the checkerboard / growing-list case) to a
//! key-indexed match (future — handles list reorder, prepend).
//!
//! This module intentionally stays data-only: it describes the *shape*
//! of the diff without generating WASM. The codegen side (see
//! `block_lower::create_for_update_block_reactive`) reads the strategy
//! to decide which loops to emit.
//!
//! ## Strategies
//!
//! - [`DiffStrategy::Unkeyed`] — iterations match by index. Survivors
//!   are `[0, min(old_len, new_len))`. Tail is mounted / unmounted.
//!   No hashing, no per-item equality. The only strategy wired in
//!   initially; matches `for i in 0..n` (checkerboard) exactly.
//!
//! - [`DiffStrategy::Keyed`] — placeholder for `for x in items key(x.id)`.
//!   A future implementation will emit a lookup from the new iterable's
//!   keys into the old tracking array's key column; entries whose keys
//!   survived get reused in-place (possibly with a DOM reorder), new
//!   keys mount, missing keys unmount. The *lookup* step behind that
//!   strategy is further swappable — linear scan for small N, or a
//!   hash-table for large N — but the caller-visible diff shape stays
//!   the same.
//!
//! The `key` LIR expression is already captured on `LirNodeKind::For`
//! (see `lir/node.rs`), so switching from Unkeyed to Keyed later is a
//! single match on whether `key.is_some()` at lowering time.

/// Compile-time diff strategy for a for-loop's update block.
#[derive(Debug, Clone, Copy)]
pub enum LirDiffStrategy {
    /// Match iterations by a user-provided key expression. Not yet
    /// wired into codegen — chosen only when the `for` has a
    /// `key(expr)` clause. Falls back to `Unkeyed` until the keyed
    /// diff loop is implemented.
    Keyed,
    /// Match iterations by index. Survivors are `[0, min(old, new))`.
    Unkeyed,
}

impl LirDiffStrategy {
    /// Pick the strategy for a for-loop based on whether it has a key
    /// clause. Keeps the choice point in one place so we can change
    /// the policy (e.g. require keys for lists of records) without
    /// chasing call sites.
    pub fn for_for_loop(has_key: bool) -> Self {
        if has_key {
            Self::Keyed
        } else {
            Self::Unkeyed
        }
    }
}
