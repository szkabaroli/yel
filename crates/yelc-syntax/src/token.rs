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
//! - trivia (whitespace, line comment, block comment): 3
//! - literals + identifier (incl. string/template segments): 10
//! - keywords: 24
//! - delimiters, punctuation, operators, compound assignment: 35
//! - `UNKNOWN`, `EOF`: 2
//! - **total tokens: 74** (`EOF` has discriminant 73)
//!
//! 74 < 128, so `u128` is sufficient and the seam does **not** need
//! `TokenSet([u64; N])`. Node kinds (84 as of 2026-07-29; the count and every
//! change to it are recorded in `tests::token_kind_counts`) live above `EOF`
//! and are never members
//! of a `TokenSet`, so they do not consume set capacity — but they do consume
//! the `u8` discriminant space, and 150 total is comfortably under 256.
//!
//! The primitive type names (`bool`, `s32`, `string`, `color`, `list`, `option`,
//! …) are **NOT** keywords. They cannot be: the grammar permits `color` as an
//! attribute name (`Text { color: #ff0000 }`) and as an identifier generally.
//! They lex as `IDENTIFIER` and are recognised contextually by the type parser.
//! Reserving them would be a silent language change — see
//! `plans/rewrite/scope.md`.
//!
//! # Every keyword here is *contextual*
//!
//! The frozen grammar's `identifier` rule accepts `component`, `if`, `for`, …
//! verbatim — pest matches keywords as bare string literals inside the rules
//! that need them and never reserves them globally. Lexing them as distinct
//! kinds is what makes `const` FIRST sets possible, so the parser compensates
//! at every *name* position via [`NAME_FIRST`] / `Parser::is_name`: a keyword
//! token is accepted wherever the grammar wanted an `identifier`.
//!
//! **The assertion below is not optional.** Without it, adding the 129th token
//! kind silently shifts a bit out of range and corrupts every recovery set at
//! runtime instead of failing the build.

use TokenKind::*;

/// A `const fn` bitset over token kinds.
///
/// FIRST sets and recovery sets are `const`. Two of them genuinely serve both
/// roles: [`ITEM_FIRST`] gates `parse_item`'s dispatch *and* is
/// [`ITEM_RECOVERY`], and [`MEMBER_FIRST`] gates `parse_component_member`'s
/// dispatch *and* feeds [`RESYNC_MEMBER`]. The rest are single-role by nature —
/// a FIRST set used only to predict, or a recovery set used only to
/// resynchronise. What is *not* allowed is a set that is declared and never
/// consulted at all; that is a reference-fidelity failure, not a port, and
/// `EMPTY` was deleted for exactly that reason.
///
/// The gate is a prediction, never an assertion: `parse_item` and
/// `parse_component_member` fall through to a reporting recovery path rather
/// than `unreachable!()`, because a set that drifts out of sync with its
/// `match` must produce a diagnostic, not a panic (invariant S6).
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

/// Every keyword token. The grammar reserves none of them, so all of them are
/// legal in a *name* position; see [`NAME_FIRST`].
pub const KEYWORD_FIRST: TokenSet = TokenSet::new(&[
    COMPONENT_KW,
    GLOBAL_KW,
    RECORD_KW,
    ENUM_KW,
    VARIANT_KW,
    ELEMENT_KW,
    EXTERN_KW,
    PACKAGE_KW,
    EXPORT_KW,
    FUNC_KW,
    CALLBACK_KW,
    LET_KW,
    IF_KW,
    ELSE_KW,
    FOR_KW,
    IN_KW,
    OUT_KW,
    IN_OUT_KW,
    KEY_KW,
    SET_KW,
    BIND_KW,
    CHILDREN_KW,
    TRUE_KW,
    FALSE_KW,
]);

/// Anything the frozen grammar's `identifier` rule would have matched.
///
/// Keywords are contextual, so every keyword token is also a legal name.
pub const NAME_FIRST: TokenSet = TokenSet::new(&[IDENTIFIER]).union(KEYWORD_FIRST);

/// FIRST set of `top_level_item` (plus `package`, which is only legal first).
pub const ITEM_FIRST: TokenSet = TokenSet::new(&[
    RECORD_KW,
    ENUM_KW,
    VARIANT_KW,
    ELEMENT_KW,
    EXTERN_KW,
    GLOBAL_KW,
    COMPONENT_KW,
    EXPORT_KW,
    PACKAGE_KW,
]);

/// Synchronising set for anything that lives inside a top-level item: bail out
/// of a broken member list rather than swallow the next declaration.
pub const ITEM_RECOVERY: TokenSet = ITEM_FIRST;

/// FIRST set of `expr` — `prefix* ~ primary`.
pub const EXPRESSION_FIRST: TokenSet = TokenSet::new(&[
    INT_LITERAL,
    FLOAT_LITERAL,
    UNIT_LITERAL,
    COLOR_LITERAL,
    CHAR_LITERAL,
    STRING_LITERAL,
    TEMPLATE_LITERAL,
    L_BRACE,
    L_PAREN,
    L_BRACKET,
    SUB,
    NOT,
])
.union(NAME_FIRST);

/// FIRST set of `statement` inside a closure body.
pub const STATEMENT_FIRST: TokenSet = TokenSet::new(&[LET_KW, IF_KW]).union(EXPRESSION_FIRST);

/// FIRST set of `node` — the UI-tree productions.
pub const NODE_FIRST: TokenSet =
    TokenSet::new(&[IF_KW, FOR_KW, AT, STRING_LITERAL, TEMPLATE_LITERAL]).union(NAME_FIRST);

/// FIRST set of `element_item` — `named_prop | node | string_expr`.
///
/// **Equal to [`NODE_FIRST`], and that is not an accident.** `set` and `bind`
/// start a `named_prop`, and both are keywords, so both are already in
/// `KEYWORD_FIRST ⊆ NAME_FIRST ⊆ NODE_FIRST` — because *every* keyword is a
/// legal identifier in this grammar. Writing `NODE_FIRST.union(&[SET_KW,
/// BIND_KW])` read as intent the set did not carry: it looked like the two kinds
/// were being added, and they were already there. The alias stays because the
/// call site is asking a different question; the union does not.
pub const ELEMENT_ITEM_FIRST: TokenSet = NODE_FIRST;

/// FIRST set of a `component_member` / `global_member` / `element_property`.
///
/// Also equal to [`NODE_FIRST`], for the same reason: `export`, `callback`,
/// `in`, `out` and `in-out` are keywords, and keywords are names.
pub const MEMBER_FIRST: TokenSet = NODE_FIRST;

/// Recovery *for a list element*: stop at the closing brace or at the start of
/// the next top-level item. Deliberately narrow — a bad element should be
/// skipped, not end the list.
pub const MEMBER_RECOVERY: TokenSet = TokenSet::new(&[R_BRACE]).union(ITEM_RECOVERY);

/// Recovery *after a broken member*: resynchronise at anything that could start
/// the next member, so one bad line does not swallow the rest of the body.
pub const RESYNC_MEMBER: TokenSet = MEMBER_FIRST.union(MEMBER_RECOVERY);

/// Recovery inside a parenthesised/bracketed expression list.
pub const EXPR_LIST_RECOVERY: TokenSet =
    TokenSet::new(&[R_PAREN, R_BRACKET, R_BRACE, SEMICOLON]).union(ITEM_RECOVERY);

/// Recovery inside a type argument list.
pub const TYPE_LIST_RECOVERY: TokenSet =
    TokenSet::new(&[GT, R_PAREN, SEMICOLON, R_BRACE]).union(ITEM_RECOVERY);

/// FIRST set of `type_annotation`.
pub const TYPE_FIRST: TokenSet = TokenSet::new(&[FUNC_KW]).union(NAME_FIRST);

/// Token kinds (`< EOF`) and node kinds (`> EOF`) in one `u8`-discriminant enum.
///
/// `EOF` is the boundary and the capacity assertion below must keep compiling.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum TokenKind {
    // --- tokens (< EOF) --------------------------------------------------
    UNKNOWN,

    // trivia
    WHITESPACE,
    LINE_COMMENT,
    MULTILINE_COMMENT,

    // literals
    INT_LITERAL,
    FLOAT_LITERAL,
    /// `8px`, `100ms`, `45deg`, `50%`, `1.5rem`
    UNIT_LITERAL,
    /// `#rgb` … `#rrggbbaa`
    COLOR_LITERAL,
    CHAR_LITERAL,
    /// A complete `"…"` with no interpolation.
    STRING_LITERAL,
    /// `"…{` — opens an interpolated string.
    TEMPLATE_LITERAL,
    /// `}…{` — between two interpolations.
    TEMPLATE_MIDDLE_LITERAL,
    /// `}…"` — closes an interpolated string.
    TEMPLATE_END_LITERAL,

    IDENTIFIER,

    // keywords (all contextual — see module docs)
    COMPONENT_KW,
    GLOBAL_KW,
    RECORD_KW,
    ENUM_KW,
    VARIANT_KW,
    ELEMENT_KW,
    EXTERN_KW,
    PACKAGE_KW,
    EXPORT_KW,
    FUNC_KW,
    CALLBACK_KW,
    LET_KW,
    IF_KW,
    ELSE_KW,
    FOR_KW,
    IN_KW,
    OUT_KW,
    /// `in-out` — one identifier-shaped token, because `-` is an identifier
    /// character in this grammar.
    IN_OUT_KW,
    KEY_KW,
    SET_KW,
    BIND_KW,
    CHILDREN_KW,
    TRUE_KW,
    FALSE_KW,

    // brackets
    L_PAREN,
    R_PAREN,
    L_BRACKET,
    R_BRACKET,
    L_BRACE,
    R_BRACE,

    // punctuation
    COMMA,
    SEMICOLON,
    COLON,
    DOT,
    DOT_DOT,
    DOT_DOT_EQ,
    AT,
    ARROW,
    QUESTION,
    QUESTION_DOT,

    // operators — comparison / assignment
    EQ,
    EQ_EQ,
    NOT_EQ,
    LT,
    LE,
    GT,
    GE,

    // operators — logic
    AND_AND,
    OR_OR,
    NOT,

    // operators — arithmetic
    ADD,
    SUB,
    MUL,
    DIV,
    MODULO,

    // compound assignment
    ADD_EQ,
    SUB_EQ,
    MUL_EQ,
    DIV_EQ,

    /// Boundary. Everything below is a token; everything above is a node kind.
    EOF,

    // --- node kinds (> EOF) ----------------------------------------------
    /// Root node covering the whole file.
    SOURCE_FILE,
    /// Recovery node. Present wherever the parser could not match the grammar.
    ERROR,

    PACKAGE_DECL,
    PACKAGE_ID,

    RECORD_DECL,
    RECORD_FIELD_LIST,
    RECORD_FIELD,

    ENUM_DECL,
    ENUM_CASE_LIST,
    ENUM_CASE,

    VARIANT_DECL,
    VARIANT_CASE_LIST,
    VARIANT_CASE,

    ELEMENT_DECL,
    ELEMENT_PROPERTY,

    IMPORT_COMPONENT,
    IMPORT_PROPERTY,
    IMPORT_METHOD,

    GLOBAL_DECL,
    GLOBAL_PROPERTY,
    GLOBAL_CALLBACK,

    COMPONENT_DECL,
    PROPERTY_DECL,
    FUNCTION_DECL,

    MEMBER_LIST,
    MODIFIER,
    NAME,

    ATTRIBUTE_LIST,
    ATTRIBUTE,
    ATTRIBUTE_ARG_LIST,
    ATTRIBUTE_ARG,

    FUNC_TYPE,
    FUNC_BODY,
    FUNC_PARAM_LIST,
    TYPE_PARAM_LIST,
    TYPE_PARAM,
    FUNC_PARAM,
    FUNC_RETURN,

    PRIMITIVE_TYPE,
    NAMED_TYPE,
    LIST_TYPE,
    OPTION_TYPE,
    RESULT_TYPE,
    TUPLE_TYPE,
    TYPE_ARG_LIST,

    ELEMENT_NODE,
    ELEMENT_CONTENT,
    NAMED_PROP,
    TEXT_NODE,
    CHILDREN_NODE,

    IF_NODE,
    ELSE_IF_BRANCH,
    ELSE_BRANCH,
    NODE_BODY,

    FOR_NODE,
    KEY_CLAUSE,

    LET_STMT,
    IF_STMT,
    FOR_STMT,
    ASSIGN_STMT,
    EXPR_STMT,
    STMT_BLOCK,

    CLOSURE_EXPR,
    CLOSURE_PARAM_LIST,
    CLOSURE_PARAM,
    CLOSURE_BODY,

    RECORD_LITERAL,
    RECORD_LITERAL_FIELD_LIST,
    RECORD_LITERAL_FIELD,

    TUPLE_EXPR,
    PAREN_EXPR,
    LIST_EXPR,
    LITERAL_EXPR,
    IDENT_EXPR,
    STRING_EXPR,
    INTERPOLATION,

    UNARY_EXPR,
    BINARY_EXPR,
    TERNARY_EXPR,
    RANGE_EXPR,

    CALL_EXPR,
    ARG_LIST,
    MEMBER_EXPR,
    INDEX_EXPR,
}

impl TokenKind {
    /// Whitespace and comments. Trivia is skipped by the parser's `advance`
    /// but still pushed into the green tree by `raw_advance`.
    pub fn is_trivia(self) -> bool {
        matches!(self, WHITESPACE | LINE_COMMENT | MULTILINE_COMMENT)
    }

    /// Human-readable spelling, used by `Parser::expect`.
    pub fn spelling(self) -> &'static str {
        match self {
            UNKNOWN => "unknown character",
            WHITESPACE => "whitespace",
            LINE_COMMENT | MULTILINE_COMMENT => "comment",
            INT_LITERAL => "integer literal",
            FLOAT_LITERAL => "float literal",
            UNIT_LITERAL => "unit literal",
            COLOR_LITERAL => "color literal",
            CHAR_LITERAL => "character literal",
            STRING_LITERAL | TEMPLATE_LITERAL | TEMPLATE_MIDDLE_LITERAL | TEMPLATE_END_LITERAL => {
                "string literal"
            }
            IDENTIFIER => "identifier",
            COMPONENT_KW => "component",
            GLOBAL_KW => "global",
            RECORD_KW => "record",
            ENUM_KW => "enum",
            VARIANT_KW => "variant",
            ELEMENT_KW => "element",
            EXTERN_KW => "extern",
            PACKAGE_KW => "package",
            EXPORT_KW => "export",
            FUNC_KW => "func",
            CALLBACK_KW => "callback",
            LET_KW => "let",
            IF_KW => "if",
            ELSE_KW => "else",
            FOR_KW => "for",
            IN_KW => "in",
            OUT_KW => "out",
            IN_OUT_KW => "in-out",
            KEY_KW => "key",
            SET_KW => "set",
            BIND_KW => "bind",
            CHILDREN_KW => "children",
            TRUE_KW => "true",
            FALSE_KW => "false",
            L_PAREN => "(",
            R_PAREN => ")",
            L_BRACKET => "[",
            R_BRACKET => "]",
            L_BRACE => "{",
            R_BRACE => "}",
            COMMA => ",",
            SEMICOLON => ";",
            COLON => ":",
            DOT => ".",
            DOT_DOT => "..",
            DOT_DOT_EQ => "..=",
            AT => "@",
            ARROW => "->",
            QUESTION => "?",
            QUESTION_DOT => "?.",
            EQ => "=",
            EQ_EQ => "==",
            NOT_EQ => "!=",
            LT => "<",
            LE => "<=",
            GT => ">",
            GE => ">=",
            AND_AND => "&&",
            OR_OR => "||",
            NOT => "!",
            ADD => "+",
            SUB => "-",
            MUL => "*",
            DIV => "/",
            MODULO => "%",
            ADD_EQ => "+=",
            SUB_EQ => "-=",
            MUL_EQ => "*=",
            DIV_EQ => "/=",
            EOF => "end of file",
            _ => "syntax node",
        }
    }
}

/// Map an identifier-shaped word to its contextual keyword kind.
///
/// A `match` rather than a map: no allocation, no hashing, and no exposure to
/// the std-`HashMap` determinism lint.
pub fn keyword_kind(word: &str) -> Option<TokenKind> {
    Some(match word {
        "component" => COMPONENT_KW,
        "global" => GLOBAL_KW,
        "record" => RECORD_KW,
        "enum" => ENUM_KW,
        "variant" => VARIANT_KW,
        "element" => ELEMENT_KW,
        "extern" => EXTERN_KW,
        "package" => PACKAGE_KW,
        "export" => EXPORT_KW,
        "func" => FUNC_KW,
        "callback" => CALLBACK_KW,
        "let" => LET_KW,
        "if" => IF_KW,
        "else" => ELSE_KW,
        "for" => FOR_KW,
        "in" => IN_KW,
        "out" => OUT_KW,
        "in-out" => IN_OUT_KW,
        "key" => KEY_KW,
        "set" => SET_KW,
        "bind" => BIND_KW,
        "children" => CHILDREN_KW,
        "true" => TRUE_KW,
        "false" => FALSE_KW,
        _ => return None,
    })
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_kind_counts() {
        // Reported in the stage file; a change here is a change to the budget.
        assert_eq!(TokenKind::EOF as u8, 73, "token kind count changed");
        // 76 → 78 on 2026-07-29: TYPE_PARAM_LIST and TYPE_PARAM, for
        // `func<T>(…)` (LANGUAGE.md § Type Parameters). Purely additive — no
        // existing kind moved, which is what keeps the corpus comparable.
        //
        // 78 → 82 on 2026-07-29: ATTRIBUTE_LIST, ATTRIBUTE, ATTRIBUTE_ARG_LIST
        // and ATTRIBUTE_ARG, for `@name(key = value)` before a declaration
        // (`plans/rewrite/scope.md` § attributes on items, and `@unsafe`).
        // Four rather than three because the argument list is a `parse_list`
        // production and `parse_list` closes a green node of its own — the same
        // reason `FUNC_PARAM_LIST` sits beside `FUNC_PARAM`. Additive: no token
        // kind moved and no `TokenSet` changed, so every FIRST and recovery set
        // is bit-for-bit what it was.
        //
        // 82 → 84 on 2026-07-29: FUNC_BODY and FOR_STMT, for function bodies
        // and `for` in statement position (`plans/rewrite/scope.md`
        // § *function bodies, sharing `Block` with closures* and § *`for` as a
        // statement*). Two rather than four: the *block* inside both reuses the
        // existing STMT_BLOCK / CLOSURE_BODY kinds, because `ast::Block` is one
        // construct and giving each of its four owners a green kind of its own
        // would be a distinction the AST no longer draws.
        //
        // Still no token kind and no `TokenSet`: `for` was already reachable in
        // statement position through `NAME_FIRST` ⊆ `EXPRESSION_FIRST` ⊆
        // `STATEMENT_FIRST` — every keyword is a legal identifier here — so
        // STATEMENT_FIRST is bit-for-bit what it was.
        //
        // ⚠️ **This assertion measures `INDEX_EXPR`'s position, not the length
        // of the list.** It catches every kind *inserted* before `INDEX_EXPR` —
        // which is every change so far, and the change that matters, since an
        // insertion shifts existing discriminants. A kind **appended after
        // `INDEX_EXPR`** is invisible to it: verified by mutation on
        // 2026-07-29, a `SPURIOUS_KIND` added at the end of the enum left this
        // test green. So `INDEX_EXPR` must stay the last variant, and moving it
        // is a change to this test as well as to the enum. Named here rather
        // than fixed, because the mechanical fix needs a variant count Rust
        // does not give on stable, and a sentinel variant would be a fake kind
        // in an enum whose members are all real.
        assert_eq!(
            TokenKind::INDEX_EXPR as u8 - TokenKind::EOF as u8,
            84,
            "node kind count changed"
        );
    }

    #[test]
    fn token_sets_are_consistent() {
        assert!(NAME_FIRST.contains(IDENTIFIER));
        assert!(NAME_FIRST.contains(COMPONENT_KW));
        assert!(EXPRESSION_FIRST.contains(TRUE_KW));
        assert!(!EXPRESSION_FIRST.contains(SEMICOLON));
        assert!(ITEM_FIRST.contains(RECORD_KW));
        assert!(MEMBER_RECOVERY.contains(R_BRACE));
    }

    /// Every kind `parse_item` has a dispatch arm for must be in
    /// [`ITEM_FIRST`], because `parse_item` *gates* on that set before it
    /// reaches the `match`. A kind in the `match` but not the set is dead code;
    /// a kind in the set but not the `match` used to fall into the `_` arm and
    /// report "expected a top-level declaration" on the word `package`.
    #[test]
    fn item_first_covers_every_dispatched_kind() {
        // Mirrors `parser::items::parse_item`'s arms, in order.
        let dispatched = [
            RECORD_KW,
            ENUM_KW,
            VARIANT_KW,
            ELEMENT_KW,
            EXTERN_KW,
            GLOBAL_KW,
            COMPONENT_KW,
            EXPORT_KW,
            PACKAGE_KW,
        ];
        for kind in dispatched {
            assert!(
                ITEM_FIRST.contains(kind),
                "{kind:?} is dispatched by parse_item but not in ITEM_FIRST"
            );
        }
    }

    /// The same check for the member dispatch: `parse_component_member` gates on
    /// [`MEMBER_FIRST`] before predicting.
    #[test]
    fn member_first_covers_every_dispatched_kind() {
        assert!(MEMBER_FIRST.contains(EXPORT_KW), "export function_decl");
        for kind in [IDENTIFIER, COMPONENT_KW, IF_KW, FOR_KW, TRUE_KW] {
            assert!(MEMBER_FIRST.contains(kind), "{kind:?} starts a property");
        }
        for kind in [AT, STRING_LITERAL, TEMPLATE_LITERAL] {
            assert!(MEMBER_FIRST.contains(kind), "{kind:?} starts a node");
        }
        // NODE_FIRST is a subset, so the node dispatch can never escape the gate.
        for kind in [
            IF_KW,
            FOR_KW,
            AT,
            STRING_LITERAL,
            TEMPLATE_LITERAL,
            IDENTIFIER,
        ] {
            assert!(NODE_FIRST.contains(kind) <= MEMBER_FIRST.contains(kind));
        }
    }

    /// The kinds the removed unions used to name are in the sets anyway,
    /// because every keyword is a legal identifier. If one of these ever stops
    /// being a keyword, this fails and the aliases need real unions again.
    #[test]
    fn member_and_element_sets_need_no_extra_keywords() {
        for kind in [
            SET_KW,
            BIND_KW,
            EXPORT_KW,
            CALLBACK_KW,
            IN_KW,
            OUT_KW,
            IN_OUT_KW,
        ] {
            assert!(KEYWORD_FIRST.contains(kind), "{kind:?} is not a keyword");
            assert!(NODE_FIRST.contains(kind), "{kind:?} escaped NODE_FIRST");
            assert!(MEMBER_FIRST.contains(kind));
            assert!(ELEMENT_ITEM_FIRST.contains(kind));
        }
    }

    #[test]
    fn trivia_classification() {
        assert!(WHITESPACE.is_trivia());
        assert!(LINE_COMMENT.is_trivia());
        assert!(MULTILINE_COMMENT.is_trivia());
        assert!(!IDENTIFIER.is_trivia());
    }
}
