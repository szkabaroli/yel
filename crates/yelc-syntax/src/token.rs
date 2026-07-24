//! Token and node kinds, plus the `const fn` bitset used for prediction and
//! recovery.
//!
//! # Contract
//!
//! One enum holds **both** token kinds and node kinds, split by `EOF`:
//! everything `< EOF` is a real token the lexer can emit; everything `> EOF` is
//! a node kind the green-tree builder can close. This is ark's arrangement and
//! it is what lets `GreenTreeBuilder::{token, finish_node}` share one kind type.
//!
//! # Capacity — resolved before stage 1 started
//!
//! [`TokenSet`] is a `u128`, so `1 << (kind as u8)` is only valid for kinds
//! below 128. The kind budget was counted against `yel-core/src/syntax/grammar.pest`
//! before this contract landed:
//!
//! | Group | Count |
//! |---|---|
//! | trivia (whitespace, line comment, block comment) | 3 |
//! | literals + identifier (incl. string/template segments) | 10 |
//! | keywords | ~23 |
//! | delimiters, punctuation, operators, compound assignment | ~35 |
//! | `UNKNOWN`, `EOF` | 2 |
//! | **total tokens** | **~73** |
//!
//! ~73 < 128, so `u128` is sufficient and the seam does **not** need
//! `TokenSet([u64; N])`. Node kinds (~65) live above `EOF` and are never members
//! of a `TokenSet`, so they do not consume set capacity — but they do consume
//! the `u8` discriminant space, and ~138 total is comfortably under 256.
//!
//! The primitive type names (`bool`, `s32`, `string`, `color`, `list`, `option`,
//! …) are **NOT** keywords. They cannot be: the grammar permits `color` as an
//! attribute name (`Text { color: #ff0000 }`) and as an identifier generally.
//! They lex as `IDENTIFIER` and are recognised contextually by the type parser.
//! Reserving them would be a silent language change — see
//! `plans/rewrite/scope.md`.
//!
//! **The assertion below is not optional.** Without it, adding the 129th token
//! kind silently shifts a bit out of range and corrupts every recovery set at
//! runtime instead of failing the build.

/// A `const fn` bitset over token kinds.
///
/// FIRST sets and recovery sets are `const` and drive **both** prediction and
/// recovery — one declaration, two uses. A recovery set that exists but is never
/// consulted is a reference-fidelity failure, not a port.
#[derive(Copy, Clone)]
pub struct TokenSet(u128);

impl TokenSet {
    /// Fold kinds into the bitset at compile time.
    ///
    /// Implemented here rather than left to stage 1 on purpose: the bit
    /// arithmetic is the mechanism every recovery set depends on, and getting
    /// `1 << kind` subtly wrong corrupts prediction and recovery silently
    /// rather than loudly.
    pub const fn new(kinds: &[TokenKind]) -> TokenSet {
        let mut value: u128 = 0;
        let mut i = 0;
        while i < kinds.len() {
            debug_assert!((kinds[i] as u8) < 128);
            value |= 1u128 << (kinds[i] as u8);
            i += 1;
        }
        TokenSet(value)
    }

    pub const fn union(&self, other: TokenSet) -> TokenSet {
        TokenSet(self.0 | other.0)
    }

    pub const fn contains(&self, kind: TokenKind) -> bool {
        self.0 & (1u128 << (kind as u8)) != 0
    }
}

/// The empty set. Used as the recovery set where a caller genuinely has no
/// synchronising tokens to offer.
pub const EMPTY: TokenSet = TokenSet::new(&[]);

/// Token kinds (`< EOF`) and node kinds (`> EOF`) in one `u8`-discriminant enum.
///
/// Stage 1 fills this in from the frozen grammar. `EOF` must remain the
/// boundary, and `TOKEN_KIND_CAPACITY_CHECK` below must keep compiling.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum TokenKind {
    // --- tokens (< EOF) --------------------------------------------------
    UNKNOWN,
    // stage 1: trivia, literals, identifier, keywords, punctuation, operators
    /// Boundary. Everything below is a token; everything above is a node kind.
    EOF,
    // --- node kinds (> EOF) ----------------------------------------------
    /// Root node covering the whole file.
    SOURCE_FILE,
    /// Recovery node. Present wherever the parser could not match the grammar.
    ERROR,
    // stage 1: the remaining node kinds
}

impl TokenKind {
    /// Whitespace and comments. Trivia is skipped by the parser's `advance`
    /// but still pushed into the green tree by `raw_advance`.
    pub fn is_trivia(self) -> bool {
        todo!("stage 1")
    }
}

/// Compile-time guard for [`TokenSet`]'s `u128` capacity.
///
/// If this stops compiling, the token half of [`TokenKind`] has outgrown the
/// bitset: change `TokenSet` to `[u64; N]` behind the same `const fn` API and
/// record the change in `plans/rewrite/seam-changes.md` (pre-granted there).
const _: () = assert!(
    (TokenKind::EOF as u8) < 128,
    "TokenSet is a u128; token kinds must stay below 128"
);
