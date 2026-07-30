//! The lexer.
//!
//! # Contract
//!
//! - Returns **parallel arrays** of kinds and byte widths, not a `Vec<Token>`.
//! - **No absolute offsets in tokens.** Widths only; the parser accumulates
//!   `offset`. This is what keeps token data position-independent.
//! - Trivia (whitespace, `//` line comments, `/* */` block comments) are real
//!   tokens, emitted like any other.
//! - Errors go to `Diagnostics` and lexing **continues**: an unknown character
//!   becomes an `UNKNOWN` token, never a bail.
//!
//! # Behaviours inherited from the frozen grammar
//!
//! * `-` is an **identifier character, but only between name characters**.
//!   `identifier` is `(ALPHA|"_") ~ (ALNUM|"_"|("-" ~ &(ALNUM|"_")))*`, so
//!   `count-1` and `selected-id` are single identifiers while `count - 1` is a
//!   subtraction, `count-=1` is a compound assignment, and `s32->p` is a type
//!   followed by an arrow.
//!
//!   The one-character lookahead is what lets a *tokenizing* lexer track a
//!   *scannerless* PEG here. pest matches `primitive_type`'s bare `"s32"`
//!   literal and stops after three characters; unconditional maximal munch
//!   produced `s32-` instead, so `->` never formed and `{ p: s32->p }` was read
//!   as a record where pest read a closure. Recorded and then fixed — see
//!   `plans/rewrite/goldens-changed.md`.
//! * Strings have **no escape sequences** (`string_text` is
//!   `(!("\"" | "{") ~ ANY)+`), so a backslash is an ordinary character and the
//!   first unescaped `"` ends the string. Character literals *do* have escapes.
//! * `{` inside a string opens an interpolation. Nesting is tracked with an
//!   `open_braces` stack so `"{ ["a","b"][1] }"` — a string inside an
//!   interpolation inside a string — lexes correctly.
//! * A unit suffix is matched as an **ordered prefix** of whatever follows the
//!   digits (`px|pt|in|mm|cm|phx|deg|rad|turn|ms|s|rem|%`), exactly as pest's
//!   ordered choice does: `10ms` is `10ms`, and `10second` is `10s` followed by
//!   the identifier `econd`.

use crate::token::{TokenKind, TokenKind::*, keyword_kind};
use yelc_base::{Diagnostics, ErrorCode, SourceId, Span};

pub struct LexerResult {
    pub tokens: Vec<TokenKind>,
    /// `widths[i]` is the byte width of `tokens[i]`. Sums to `content.len()`.
    pub widths: Vec<u32>,
}

/// Unit suffixes, in the frozen grammar's ordered-choice order. Order is
/// load-bearing: `ms` must be tried before `s`.
const UNIT_SUFFIXES: &[&str] = &[
    "px", "pt", "in", "mm", "cm", "phx", "deg", "rad", "turn", "ms", "s", "rem", "%",
];

pub fn lex(source: SourceId, content: &str, diags: &mut Diagnostics) -> LexerResult {
    let mut lexer = Lexer::new(source, content, diags);
    let mut tokens = Vec::new();
    let mut widths = Vec::new();

    while !lexer.is_eof() {
        let start = lexer.offset();
        let token = lexer.read_token();
        debug_assert!(token < EOF, "lexer produced a node kind");
        let end = lexer.offset();
        debug_assert!(end > start, "lexer failed to make progress");
        tokens.push(token);
        widths.push(end - start);
    }

    LexerResult { tokens, widths }
}

struct Lexer<'a> {
    source: SourceId,
    content: &'a str,
    offset: usize,
    diags: &'a mut Diagnostics,
    /// Depth counter per *open interpolation*. `{` inside an interpolation
    /// pushes the depth up; the `}` that brings it back to zero resumes string
    /// scanning rather than emitting `R_BRACE`.
    open_braces: Vec<usize>,
}

impl<'a> Lexer<'a> {
    fn new(source: SourceId, content: &'a str, diags: &'a mut Diagnostics) -> Lexer<'a> {
        Lexer {
            source,
            content,
            offset: 0,
            diags,
            open_braces: Vec::new(),
        }
    }

    fn read_token(&mut self) -> TokenKind {
        let ch = self.curr().expect("end of file reached");

        if is_whitespace(ch) {
            self.read_whitespace()
        } else if self.is_line_comment() {
            self.read_line_comment()
        } else if self.is_multiline_comment() {
            self.read_multiline_comment()
        } else if ch.is_ascii_digit() {
            self.read_number()
        } else if is_identifier_start(ch) {
            self.read_identifier()
        } else if ch == '"' {
            self.read_string(false)
        } else if ch == '\'' {
            self.read_char_literal()
        } else if ch == '#' {
            self.read_color_literal()
        } else if is_operator(ch) {
            self.read_operator()
        } else {
            self.read_unknown_char()
        }
    }

    // -- trivia ------------------------------------------------------------

    fn read_whitespace(&mut self) -> TokenKind {
        while self.curr().is_some_and(is_whitespace) {
            self.eat_char();
        }
        WHITESPACE
    }

    fn read_line_comment(&mut self) -> TokenKind {
        while self.curr().is_some_and(|c| c != '\n') {
            self.eat_char();
        }
        LINE_COMMENT
    }

    fn read_multiline_comment(&mut self) -> TokenKind {
        let start = self.offset;
        self.eat_char();
        self.eat_char();

        while self.curr().is_some() && !self.is_multiline_comment_end() {
            self.eat_char();
        }

        if self.curr().is_none() {
            self.report(start, "unterminated block comment");
        } else {
            self.eat_char();
            self.eat_char();
        }

        MULTILINE_COMMENT
    }

    // -- identifiers -------------------------------------------------------

    /// `identifier = @{ (ALPHA | "_") ~ (ALPHANUMERIC | "_" | ("-" ~ &(ALPHANUMERIC | "_")))* }`
    ///
    /// A `-` joins the identifier **only when another identifier character
    /// follows it**. That one character of lookahead is what separates
    /// `selected-id` (one name) from `s32->p` (`s32`, then an arrow), and it is
    /// the whole reason `->`, `-=` and a trailing `-` reach the parser as
    /// operators at all.
    ///
    /// The lookahead keeps the lexer **context-free**: it is a fact about the
    /// next character, not about where the parser happens to be. That is the
    /// property that makes a hand-written lexer able to track a scannerless PEG
    /// here — pest matches `primitive_type`'s bare `"s32"` literal and stops,
    /// and this stops in the same place for the same local reason.
    fn read_identifier(&mut self) -> TokenKind {
        let start = self.offset;
        self.eat_char();
        while let Some(ch) = self.curr() {
            if ch == '-' {
                // The grammar's lookahead is `&(ALPHANUMERIC | "_")`, which
                // **excludes** a second `-`. So `item--7` is `item` followed by
                // `--7`, not one identifier: the first hyphen has no name
                // character after it. Using `is_identifier_continue` here
                // instead — which admits `-` — made `item--7` a single name and
                // was a widening the random-mutation sweep caught.
                if !self.lookahead().is_some_and(is_name_char) {
                    break;
                }
            } else if !is_identifier_continue(ch) {
                break;
            }
            self.eat_char();
        }
        let word = &self.content[start..self.offset];
        keyword_kind(word).unwrap_or(IDENTIFIER)
    }

    // -- numbers -----------------------------------------------------------

    fn read_number(&mut self) -> TokenKind {
        self.read_digits();

        let mut is_float = false;
        if self.curr() == Some('.') && self.lookahead().is_some_and(|c| c.is_ascii_digit()) {
            is_float = true;
            self.eat_char();
            self.read_digits();
        }

        if let Some(suffix) = self.match_unit_suffix() {
            self.offset += suffix.len();
            return UNIT_LITERAL;
        }

        if is_float { FLOAT_LITERAL } else { INT_LITERAL }
    }

    fn read_digits(&mut self) {
        while self.curr().is_some_and(|c| c.is_ascii_digit()) {
            self.eat_char();
        }
    }

    /// The frozen grammar's `unit_suffix` is an ordered choice of plain string
    /// literals inside an atomic rule, so it matches a *prefix* of the trailing
    /// text and does not require a word boundary.
    fn match_unit_suffix(&self) -> Option<&'static str> {
        let rest = &self.content[self.offset..];
        UNIT_SUFFIXES
            .iter()
            .copied()
            .find(|suffix| rest.starts_with(suffix))
    }

    // -- strings -----------------------------------------------------------

    /// Read a string body. `continuation` means the `}` that closed an
    /// interpolation has already been consumed by [`Lexer::read_operator`].
    fn read_string(&mut self, continuation: bool) -> TokenKind {
        let start = if continuation {
            // `}` was already consumed by read_operator.
            self.offset - '}'.len_utf8()
        } else {
            debug_assert_eq!(self.curr(), Some('"'));
            let start = self.offset;
            self.eat_char();
            start
        };

        while let Some(ch) = self.curr() {
            if ch == '"' {
                self.eat_char();
                return if continuation {
                    TEMPLATE_END_LITERAL
                } else {
                    STRING_LITERAL
                };
            }
            if ch == '{' {
                self.eat_char();
                self.open_braces.push(1);
                return if continuation {
                    TEMPLATE_MIDDLE_LITERAL
                } else {
                    TEMPLATE_LITERAL
                };
            }
            // No escape sequences: `string_text` is `(!("\"" | "{") ~ ANY)+`.
            self.eat_char();
        }

        self.report(start, "unterminated string literal");
        if continuation {
            TEMPLATE_END_LITERAL
        } else {
            STRING_LITERAL
        }
    }

    fn read_char_literal(&mut self) -> TokenKind {
        let start = self.offset;
        self.eat_char();

        while self.curr().is_some_and(|c| c != '\'') {
            if self.curr() == Some('\\') {
                self.eat_char();
            }
            self.eat_char();
        }

        if self.curr() == Some('\'') {
            self.eat_char();
        } else {
            self.report(start, "unterminated character literal");
        }

        CHAR_LITERAL
    }

    fn read_color_literal(&mut self) -> TokenKind {
        let start = self.offset;
        self.eat_char(); // '#'

        let mut digits = 0;
        while digits < 8 && self.curr().is_some_and(|c| c.is_ascii_hexdigit()) {
            self.eat_char();
            digits += 1;
        }

        if digits < 3 {
            // `color_literal` is `"#" ~ HEX{3,8}`; fewer than three hex digits
            // does not match any production at all.
            self.offset = start + 1;
            self.report(start, "expected 3 to 8 hex digits after `#`");
            return UNKNOWN;
        }

        COLOR_LITERAL
    }

    // -- operators ---------------------------------------------------------

    fn read_operator(&mut self) -> TokenKind {
        let ch = self.curr().expect("missing operator char");
        self.eat_char();
        let next = self.curr();
        let after = self.lookahead();

        match ch {
            '(' => L_PAREN,
            ')' => R_PAREN,
            '[' => L_BRACKET,
            ']' => R_BRACKET,
            '{' => {
                if let Some(depth) = self.open_braces.last_mut() {
                    *depth += 1;
                }
                L_BRACE
            }
            '}' => {
                if let Some(depth) = self.open_braces.last_mut() {
                    *depth -= 1;
                    if *depth == 0 {
                        self.open_braces.pop();
                        return self.read_string(true);
                    }
                }
                R_BRACE
            }
            ',' => COMMA,
            ';' => SEMICOLON,
            ':' => COLON,
            '@' => AT,
            '.' => {
                if next == Some('.') {
                    self.eat_char();
                    if self.curr() == Some('=') {
                        self.eat_char();
                        DOT_DOT_EQ
                    } else {
                        DOT_DOT
                    }
                } else {
                    DOT
                }
            }
            '?' => {
                if next == Some('.') {
                    self.eat_char();
                    QUESTION_DOT
                } else {
                    QUESTION
                }
            }
            '=' => {
                if next == Some('=') {
                    self.eat_char();
                    EQ_EQ
                } else {
                    EQ
                }
            }
            '!' => {
                if next == Some('=') {
                    self.eat_char();
                    NOT_EQ
                } else {
                    NOT
                }
            }
            '<' => {
                if next == Some('=') {
                    self.eat_char();
                    LE
                } else {
                    LT
                }
            }
            '>' => {
                if next == Some('=') {
                    self.eat_char();
                    GE
                } else {
                    GT
                }
            }
            '&' => {
                if next == Some('&') {
                    self.eat_char();
                    AND_AND
                } else {
                    self.report(self.offset - 1, "expected `&&`");
                    UNKNOWN
                }
            }
            '|' => {
                if next == Some('|') {
                    self.eat_char();
                    OR_OR
                } else {
                    self.report(self.offset - 1, "expected `||`");
                    UNKNOWN
                }
            }
            '+' => {
                if next == Some('=') {
                    self.eat_char();
                    ADD_EQ
                } else {
                    ADD
                }
            }
            '-' => {
                if next == Some('=') {
                    self.eat_char();
                    SUB_EQ
                } else if next == Some('>') {
                    self.eat_char();
                    ARROW
                } else {
                    SUB
                }
            }
            '*' => {
                if next == Some('=') {
                    self.eat_char();
                    MUL_EQ
                } else {
                    MUL
                }
            }
            '/' => {
                if next == Some('=') {
                    self.eat_char();
                    DIV_EQ
                } else {
                    DIV
                }
            }
            '%' => MODULO,
            _ => {
                let _ = after;
                unreachable!("is_operator admitted {ch:?}")
            }
        }
    }

    fn read_unknown_char(&mut self) -> TokenKind {
        let start = self.offset;
        let ch = self.curr().expect("missing char");
        self.eat_char();
        self.report(start, format!("unexpected character `{ch}`"));
        UNKNOWN
    }

    // -- cursor ------------------------------------------------------------

    fn report(&mut self, start: usize, message: impl Into<String>) {
        let span = Span::new(self.source, start, self.offset);
        self.diags.error(span, ErrorCode::SyntaxError, message);
    }

    fn offset(&self) -> u32 {
        self.offset.try_into().expect("source exceeds 4 GiB")
    }

    fn eat_char(&mut self) -> Option<char> {
        let curr = self.curr();
        if let Some(ch) = curr {
            self.offset += ch.len_utf8();
        }
        curr
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.content.len()
    }

    fn curr(&self) -> Option<char> {
        self.content[self.offset..].chars().next()
    }

    fn lookahead(&self) -> Option<char> {
        let mut it = self.content[self.offset..].chars();
        it.next();
        it.next()
    }

    fn is_line_comment(&self) -> bool {
        self.curr() == Some('/') && self.lookahead() == Some('/')
    }

    fn is_multiline_comment(&self) -> bool {
        self.curr() == Some('/') && self.lookahead() == Some('*')
    }

    fn is_multiline_comment_end(&self) -> bool {
        self.curr() == Some('*') && self.lookahead() == Some('/')
    }
}

fn is_whitespace(ch: char) -> bool {
    // The frozen grammar's WHITESPACE rule: " " | "\t" | "\r" | "\n".
    matches!(ch, ' ' | '\t' | '\r' | '\n')
}

fn is_identifier_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

/// The grammar's kebab lookahead alphabet: `&(ASCII_ALPHANUMERIC | "_")`.
///
/// Deliberately **excludes** `-`, so `item--7` is `item` followed by `--7`
/// rather than one name. Distinct from [`is_identifier_continue`], which admits
/// `-` because a hyphen *already joined* is an identifier character.
fn is_name_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

fn is_identifier_continue(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_' || ch == '-'
}

fn is_operator(ch: char) -> bool {
    "+-*/%&|,=!;:.()[]{}<>@?".contains(ch)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kinds(source: &str) -> Vec<TokenKind> {
        let mut diags = Diagnostics::new();
        let result = lex(SourceId(0), source, &mut diags);
        result.tokens
    }

    fn kinds_no_trivia(source: &str) -> Vec<TokenKind> {
        kinds(source)
            .into_iter()
            .filter(|k| !k.is_trivia())
            .collect()
    }

    /// Every lex must partition the input exactly. This is the lexer half of
    /// invariant S1.
    fn assert_covers(source: &str) {
        let mut diags = Diagnostics::new();
        let result = lex(SourceId(0), source, &mut diags);
        let total: u32 = result.widths.iter().sum();
        assert_eq!(
            total as usize,
            source.len(),
            "lexer lost bytes in {source:?}"
        );
    }

    #[test]
    fn lexes_keywords_and_identifiers() {
        assert_eq!(
            kinds_no_trivia("component Foo record enum"),
            vec![COMPONENT_KW, IDENTIFIER, RECORD_KW, ENUM_KW]
        );
    }

    #[test]
    fn hyphen_joins_an_identifier_only_when_a_name_character_follows() {
        // Kebab names are unaffected — a name character follows the hyphen.
        assert_eq!(kinds_no_trivia("count-1"), vec![IDENTIFIER]);
        assert_eq!(kinds_no_trivia("selected-id"), vec![IDENTIFIER]);
        assert_eq!(kinds_no_trivia("in-out"), vec![IN_OUT_KW]);
        assert_eq!(
            kinds_no_trivia("count - 1"),
            vec![IDENTIFIER, SUB, INT_LITERAL]
        );

        // A hyphen with no name character after it is an operator, not the last
        // byte of a name. `a-` used to lex as a single IDENTIFIER; that is what
        // made `a-=1` an assignment to a variable called `a-`, and `s32->p` a
        // record.
        assert_eq!(kinds_no_trivia("a-"), vec![IDENTIFIER, SUB]);
        assert_eq!(
            kinds_no_trivia("count-=1"),
            vec![IDENTIFIER, SUB_EQ, INT_LITERAL]
        );
    }

    /// The rule this file exists to get right.
    ///
    /// `identifier` admits `-`, and `->` is an operator, so maximal munch had to
    /// pick one. It used to pick the identifier unconditionally, which meant
    /// `->` glued to a name never produced an `ARROW` at all — and `s32->p`
    /// parsed as a *record* here while pest read a *closure*, because pest is
    /// scannerless and matched `primitive_type`'s bare `"s32"` literal, stopping
    /// after three characters.
    ///
    /// One character of lookahead reproduces that without making the lexer
    /// context-aware: join the hyphen only when a name character follows.
    #[test]
    fn arrow_versus_hyphen_identifier() {
        assert_eq!(kinds_no_trivia("p->x"), vec![IDENTIFIER, ARROW, IDENTIFIER]);
        assert_eq!(
            kinds_no_trivia("p -> x"),
            vec![IDENTIFIER, ARROW, IDENTIFIER]
        );
        assert_eq!(
            kinds_no_trivia("s32->p"),
            vec![IDENTIFIER, ARROW, IDENTIFIER]
        );
        // …and a hyphen between two name characters still joins.
        assert_eq!(
            kinds_no_trivia("a-b->c"),
            vec![IDENTIFIER, ARROW, IDENTIFIER]
        );
    }

    #[test]
    fn numbers_and_units() {
        assert_eq!(kinds_no_trivia("42"), vec![INT_LITERAL]);
        assert_eq!(kinds_no_trivia("1.5"), vec![FLOAT_LITERAL]);
        assert_eq!(kinds_no_trivia("8px"), vec![UNIT_LITERAL]);
        assert_eq!(kinds_no_trivia("100ms"), vec![UNIT_LITERAL]);
        assert_eq!(kinds_no_trivia("45deg"), vec![UNIT_LITERAL]);
        assert_eq!(kinds_no_trivia("50%"), vec![UNIT_LITERAL]);
        assert_eq!(kinds_no_trivia("1.5rem"), vec![UNIT_LITERAL]);
        // Ordered-choice prefix match, matching pest.
        assert_eq!(kinds_no_trivia("10second"), vec![UNIT_LITERAL, IDENTIFIER]);
        assert_eq!(
            kinds_no_trivia("50 % 3"),
            vec![INT_LITERAL, MODULO, INT_LITERAL]
        );
    }

    #[test]
    fn range_is_not_a_float() {
        assert_eq!(
            kinds_no_trivia("0..5"),
            vec![INT_LITERAL, DOT_DOT, INT_LITERAL]
        );
        assert_eq!(
            kinds_no_trivia("0..=5"),
            vec![INT_LITERAL, DOT_DOT_EQ, INT_LITERAL]
        );
    }

    #[test]
    fn strings_have_no_escapes() {
        assert_eq!(kinds_no_trivia(r#""hello""#), vec![STRING_LITERAL]);
        // The backslash is an ordinary character; the next `"` ends the string.
        assert_eq!(
            kinds_no_trivia(r#""a\" b""#),
            vec![STRING_LITERAL, IDENTIFIER, STRING_LITERAL]
        );
    }

    #[test]
    fn interpolation_uses_a_brace_stack() {
        assert_eq!(
            kinds_no_trivia(r#""Hello {name}!""#),
            vec![TEMPLATE_LITERAL, IDENTIFIER, TEMPLATE_END_LITERAL]
        );
        assert_eq!(
            kinds_no_trivia(r#""{a}-{b}""#),
            vec![
                TEMPLATE_LITERAL,
                IDENTIFIER,
                TEMPLATE_MIDDLE_LITERAL,
                IDENTIFIER,
                TEMPLATE_END_LITERAL
            ]
        );
    }

    #[test]
    fn nested_string_inside_interpolation() {
        let src = r#""{ ["a","b"][1] }""#;
        assert_eq!(
            kinds_no_trivia(src),
            vec![
                TEMPLATE_LITERAL,
                L_BRACKET,
                STRING_LITERAL,
                COMMA,
                STRING_LITERAL,
                R_BRACKET,
                L_BRACKET,
                INT_LITERAL,
                R_BRACKET,
                TEMPLATE_END_LITERAL
            ]
        );
        assert_covers(src);
    }

    #[test]
    fn record_literal_inside_interpolation() {
        // The inner braces must not terminate the interpolation.
        let src = r#""{ {a: 1}.a }""#;
        assert_eq!(
            kinds_no_trivia(src),
            vec![
                TEMPLATE_LITERAL,
                L_BRACE,
                IDENTIFIER,
                COLON,
                INT_LITERAL,
                R_BRACE,
                DOT,
                IDENTIFIER,
                TEMPLATE_END_LITERAL
            ]
        );
        assert_covers(src);
    }

    #[test]
    fn closing_brace_inside_plain_string_is_text() {
        assert_eq!(kinds_no_trivia(r#""a}b""#), vec![STRING_LITERAL]);
    }

    #[test]
    fn comments_are_trivia_tokens() {
        assert_eq!(
            kinds("// hi\n/* x */"),
            vec![LINE_COMMENT, WHITESPACE, MULTILINE_COMMENT]
        );
    }

    #[test]
    fn colors_and_unknown_chars() {
        assert_eq!(kinds_no_trivia("#ff0000"), vec![COLOR_LITERAL]);
        assert_eq!(kinds_no_trivia("#abc"), vec![COLOR_LITERAL]);
        // Nine hex digits: eight are consumed, the ninth lexes on its own.
        assert_eq!(
            kinds_no_trivia("#123456789"),
            vec![COLOR_LITERAL, INT_LITERAL]
        );
        assert_eq!(kinds_no_trivia("#a"), vec![UNKNOWN, IDENTIFIER]);
        assert_eq!(kinds_no_trivia("$"), vec![UNKNOWN]);
    }

    #[test]
    fn compound_assignment() {
        assert_eq!(
            kinds_no_trivia("a += 1; b -= 2; c *= 3; d /= 4;"),
            vec![
                IDENTIFIER,
                ADD_EQ,
                INT_LITERAL,
                SEMICOLON,
                IDENTIFIER,
                SUB_EQ,
                INT_LITERAL,
                SEMICOLON,
                IDENTIFIER,
                MUL_EQ,
                INT_LITERAL,
                SEMICOLON,
                IDENTIFIER,
                DIV_EQ,
                INT_LITERAL,
                SEMICOLON
            ]
        );
    }

    #[test]
    fn every_input_is_fully_covered() {
        for src in [
            "",
            "component A {}",
            r#""unterminated"#,
            "/* unterminated",
            "'x",
            "#",
            "\u{1F600}",
            r#""{"#,
            "}",
        ] {
            assert_covers(src);
        }
    }
}
