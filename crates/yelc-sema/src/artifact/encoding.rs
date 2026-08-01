//! The codec, behind one boundary.
//!
//! Two functions, imported by nothing else, so the choice below is reversible in
//! an afternoon ([`directions.md` §6](../../../../plans/rewrite/directions.md)).
//!
//! # Why postcard
//!
//! Criteria, in the order `plans/modules.md` §6.6 states them.
//!
//! 1. **Schema stability across crate versions.** postcard publishes its wire
//!    format as a *separate specification*, stable since 1.0.0, and states that
//!    changing it requires a 2.0 with an updated spec. So an artifact is
//!    invalidated when *our* schema moves and not when a dependency bumps —
//!    which is precisely the property [`Stamp::format`](super::Stamp::format)
//!    exists to track, and it can only track one thing at a time. bincode has no
//!    such document: its format is defined by its implementation, it changed
//!    across 1.x → 2.0, and as of RUSTSEC-2025-0141 (2025-12-16) it is
//!    permanently unmaintained, with its own advisory recommending postcard.
//! 2. **Compactness.** Every integer above eight bits is a varint, and this
//!    format is almost entirely small integers: type-table indices, definition
//!    indices, sequence lengths. Each costs one byte.
//! 3. **No self-describing overhead.** From the specification: *"As `struct`s
//!    have a known number of elements with known names, their length and field
//!    names are not encoded on the wire."* Enum variant names are likewise
//!    absent. Both sides know the schema; nothing describes it twice.
//!
//! # What that buys, and what it costs
//!
//! Field names being absent is the reason [`Stamp::FORMAT`](super::Stamp::FORMAT)
//! must be bumped for *any* shape change in [`super::wire`]: a reordered field
//! or an inserted enum variant is not detectable from the bytes. That is the
//! trade taken deliberately — the alternative is paying for a schema on every
//! artifact to catch a mistake a version integer catches for four bytes.

use super::{Artifact, LoadError};

/// Encode an artifact.
///
/// # Panics
///
/// Only on a serializer error, which for plain owned data is a compiler bug
/// rather than a condition a caller can handle.
pub fn encode(artifact: &Artifact) -> Vec<u8> {
    postcard::to_allocvec(artifact).expect("artifact encoding is infallible for plain data")
}

/// Decode an artifact. Does **not** check the stamp — [`Artifact::load`] does,
/// before it touches any table.
pub fn decode(bytes: &[u8]) -> Result<Artifact, LoadError> {
    postcard::from_bytes(bytes).map_err(|error| LoadError::Decode(error.to_string()))
}
