//! `yelc-syntax` — lexer, lossless green tree, and recursive-descent parser.
//!
//! Replaces the frozen `yel-core/src/syntax/` (pest grammar + 3.3k-line wrapper).
//! Design ported from [`szkabaroli/ark`](https://github.com/szkabaroli/ark)
//! (`compiler/arkc-parser`); see `plans/rewrite/stage-1-syntax.md`.
//!
//! # SEAM: source → AST. Frozen for stage 2.
//!
//! The types and invariants in this file are the contract between stage 1 and
//! stage 2. They landed on `main` before stage 1 was implemented. **Do not
//! change them** — if a change is genuinely needed, stop and file a request in
//! `plans/rewrite/seam-changes.md`.
//!
//! Everything *inside* the modules below is stage 1's to design freely.
//!
//! # Why a green tree and not `Result<Ast, Vec<Error>>`
//!
//! Pest gives you a parse or a failure. A green tree gives you a complete tree
//! for **broken** input, which is the state a file is in most of the time in an
//! editor. That is what makes error recovery, incremental reparse, and a serious
//! LSP possible. A parser that discards the tree on failure is a regression even
//! though it is smaller — see `plans/rewrite/anti-spec.md` B1.

pub mod ast;
pub mod green;
pub mod lexer;
pub mod parser;
pub mod token;

use yelc_base::{Diagnostics, NameInterner, SourceId};

pub use green::GreenNode;

/// Identifier for an AST node, unique **within one parsed file**.
///
/// A distinct index space from `yelc_base::ids` (which holds general compiler
/// ids) and from stage 2's `HirId`. Stage 2 owns the `HirId ↔ NodeId` map.
///
/// # Allocation is per-file and starts at zero
///
/// Deliberately **not** the frozen tree's design, which allocated from a
/// process-global `AtomicU32` (`yel-core/src/syntax/ids.rs`). That made a node's
/// id depend on how many files had been parsed earlier in the process — a
/// determinism hazard of exactly the flavour anti-spec A6 forbids, and one that
/// would make a golden containing node ids unstable across runs.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub struct NodeId(pub u32);

impl NodeId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// The result of parsing one file. **Always produced** — there is no failure
/// return; see invariant S6.
pub struct ParsedFile {
    pub source: SourceId,
    /// Lossless concrete syntax: every byte of the input, trivia included.
    /// Present and complete even when the file does not parse.
    pub green: GreenNode,
    /// Typed view over the same text. May contain `Error` nodes at every
    /// recovery point.
    pub ast: ast::File,
}

/// Parse one source file.
///
/// Errors are pushed into `diags` and parsing **continues** — this function
/// never early-returns on the first error and never panics on malformed input.
/// See `plans/rewrite/keep-list.md` §6.
///
/// Note the signature takes `&mut Diagnostics` + `&NameInterner` rather than a whole
/// `CompilerContext`: the parser must stay usable by the LSP without dragging in
/// type-checking state.
pub fn parse(
    source: SourceId,
    content: &str,
    interner: &NameInterner,
    diags: &mut Diagnostics,
) -> ParsedFile {
    parser::Parser::new(source, content, interner, diags).parse()
}

// ---------------------------------------------------------------------------
// Invariants
// ---------------------------------------------------------------------------
//
// Invariants stage 1 ESTABLISHES (stage 2 may assume all of them):
//
//   S1. The green tree reconstructs the source BYTE-FOR-BYTE:
//       `parsed.green.text() == content`. This holds for every input,
//       including inputs that produce diagnostics. Asserted over the whole
//       2000-program corpus, not just fixtures.
//   S2. `parsed.green.len() as usize == content.len()`.
//   S3. Every AST node carries a `Span` that maps into the `SourceMap`, and a
//       `NodeId` unique within the file.
//   S4. Names are interned to `Name`. No `String` survives parsing.
//   S5. Ill-formed input produces a diagnostic AND an `Error` node — never a
//       panic, never a silently-dropped subtree.
//   S6. Parsing always terminates and always returns a `ParsedFile`.
//
// Invariants stage 2 MUST NOT rely on (explicitly out of contract):
//
//   - No ordering guarantee among sibling items beyond source order.
//   - `NodeId` values are NOT stable across reparses of an edited file.
//     They are stable across repeated parses of identical input (S3 + per-file
//     allocation), which is what determinism requires; incremental identity is
//     a different property and does not exist yet.
//   - The green tree is not incremental. S1/S2 make it POSSIBLE. Do not build
//     on a subtree-reuse API that has not been written.
//   - Trivia attachment — which comment "belongs to" which item — is NOT
//     decided here. Doc comments are a stage-2 concern, read off the green tree.
//   - Whether a given construct is representable in the AST says nothing about
//     whether it type-checks. The parser accepts the grammar, not the language.
