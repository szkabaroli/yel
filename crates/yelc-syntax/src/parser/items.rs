//! Top-level items and their members.
//!
//! Ordering here mirrors the frozen grammar's PEG alternatives exactly, because
//! PEG order *is* the disambiguation rule. Two places where that matters and
//! where the two look identical to the eye:
//!
//! * In a **component**, `property_decl` precedes `function_decl`, and
//!   `type_annotation` includes `func_type` — so `on-click: func();` is a
//!   *property* whose type is a function, and only `export on-click: func();`
//!   is a `function_decl`.
//! * In a **global**, `function_decl` precedes `global_property` — so the same
//!   `on-click: func();` is a *callback*. The grammar really does invert.
//!
//! # One member list per declaration
//!
//! Every bodied declaration collects **one** `Vec` of members in source order,
//! each with an `Error` variant. The split views (`properties()`, `functions()`,
//! `body()`) are accessors on the AST. A parser that sorted members into three
//! `Vec`s as it went had nowhere to put a recovery node, which is how
//! `global G { 42 }` and `component A { 42; }` produced a diagnostic and no
//! `Error` node anywhere in the tree.

use super::{Parser, TrailingSep, is_kebab_lower};
use crate::ast;
use crate::token::{
    ITEM_FIRST, ITEM_RECOVERY, MEMBER_FIRST, MEMBER_RECOVERY, NODE_FIRST, RESYNC_MEMBER, TokenKind,
    TokenKind::*, TokenSet,
};
use yelc_base::{Name, Span};

/// `package_version = @{ "@" ~ ASCII_DIGIT+ ~ ("." ~ ASCII_DIGIT+)* }`, minus
/// the `@` this is called with.
fn is_package_version(text: &str) -> bool {
    !text.is_empty()
        && text
            .split('.')
            .all(|run| !run.is_empty() && run.bytes().all(|byte| byte.is_ascii_digit()))
}

/// The outcome of the possessive `property_direction?` — see
/// `Parser::global_property_direction`.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum DirectionMatch {
    /// No direction spelling at the head of this member.
    Absent,
    /// A direction matched and a name follows it.
    Present,
    /// A direction matched possessively and what is left cannot be an
    /// `identifier`; the whole `global_property` alternative dies.
    Dead,
}

/// Recovery set for a parameter list: the closers that mean "this list is over".
const PARAM_RECOVERY: TokenSet = TokenSet::new(&[R_PAREN, SEMICOLON, R_BRACE]).union(ITEM_RECOVERY);

impl<'a> Parser<'a> {
    pub(super) fn parse_file(&mut self) -> ast::File {
        let id = self.new_node_id();

        // The green root covers leading trivia; the AST span starts after it.
        self.builder.start_node();
        self.skip_trivia();
        let start_token = self.token_idx;
        let start_offset = self.offset;

        let mut items = Vec::new();

        // `file = SOI ~ package_decl? ~ (top_level_item | CATCH_ALL)* ~ EOI` —
        // the package declaration is only legal in first position.
        if self.is(PACKAGE_KW) {
            items.push(ast::ItemKind::Package(self.parse_package_decl()));
        }

        while !self.is_eof() {
            let before = self.position();
            items.push(self.parse_item());
            assert!(self.position() > before, "parse_item consumed nothing");
        }

        let green = self.builder.finish_node(SOURCE_FILE);
        let span = self.span_between(start_token, start_offset);

        ast::File {
            id,
            source: self.source,
            span,
            green,
            items,
            recovery_marks: self.take_recovery_marks(),
        }
    }

    /// `top_level_item`, gated by [`ITEM_FIRST`] before the `match` so the FIRST
    /// set drives prediction as well as recovery.
    ///
    /// The gate is deliberately *not* an assertion: a set that drifts out of
    /// sync with the `match` must fall through to the reporting `_` arm, because
    /// `unreachable!()` here would break invariant S6.
    fn parse_item(&mut self) -> ast::ItemKind {
        if !self.is_set(ITEM_FIRST) {
            return self.parse_error_item("expected a top-level declaration");
        }

        match self.current() {
            RECORD_KW => self.parse_record_decl(),
            ENUM_KW => self.parse_enum_decl(),
            VARIANT_KW => self.parse_variant_decl(),
            ELEMENT_KW => self.parse_element_decl(),
            EXTERN_KW => self.parse_extern_component(),
            GLOBAL_KW => self.parse_global_decl(),
            COMPONENT_KW => self.parse_component_decl(),
            // `package` is in ITEM_FIRST but only legal in first position, which
            // `parse_file` has already consumed by the time control reaches here.
            PACKAGE_KW => {
                self.parse_error_item("`package` must be the first declaration in a file")
            }
            EXPORT_KW => match self.after_export() {
                GLOBAL_KW => self.parse_global_decl(),
                COMPONENT_KW => self.parse_component_decl(),
                _ => self.parse_error_item("expected `component` or `global` after `export`"),
            },
            _ => self.parse_error_item("expected a top-level declaration"),
        }
    }

    /// What follows `export` — `global`, `component`, or neither.
    fn after_export(&self) -> TokenKind {
        self.nth_non_trivia(1)
    }

    /// The `CATCH_ALL` equivalent: report once, consume at least one token, then
    /// resynchronise on [`ITEM_RECOVERY`] — the same `const` set that drives
    /// prediction in `parse_item`.
    fn parse_error_item(&mut self, message: &str) -> ast::ItemKind {
        self.start_node();
        self.error_here(message);
        if !self.is_eof() {
            self.advance();
        }
        self.recover_to(ITEM_RECOVERY);
        let span = self.finish_node(ERROR);
        ast::ItemKind::Error {
            id: self.new_node_id(),
            span,
        }
    }

    /// Close the item currently being parsed as an `ERROR` node, after
    /// resynchronising on [`ITEM_RECOVERY`].
    fn finish_error_item(&mut self) -> ast::ItemKind {
        self.recover_to(ITEM_RECOVERY);
        let span = self.finish_node(ERROR);
        ast::ItemKind::Error {
            id: self.new_node_id(),
            span,
        }
    }

    /// A zero-width hole at the current token, for a member the parser could
    /// not read. Carries a span and nothing else.
    fn member_hole<R: ast::Recovery>(&mut self, message: &str) -> R {
        self.start_node();
        self.error_here(message);
        if !self.recover_to(RESYNC_MEMBER) && !self.is_eof() {
            self.advance();
        }
        let span = self.finish_node(ERROR);
        R::recovery(self.new_node_id(), span)
    }

    // -- package -----------------------------------------------------------

    fn parse_package_decl(&mut self) -> ast::PackageDecl {
        self.start_node();
        self.assert(PACKAGE_KW);

        self.start_node();
        let namespace = self.expect_name();
        self.expect(COLON);
        let name = self.expect_name();
        let (version, version_span) = self.parse_package_version();
        self.finish_node(PACKAGE_ID);

        self.expect(SEMICOLON);
        let span = self.finish_node(PACKAGE_DECL);

        ast::PackageDecl {
            id: self.new_node_id(),
            span,
            namespace,
            name,
            version,
            version_span,
        }
    }

    /// `package_version = @{ "@" ~ DIGIT+ ~ ("." ~ DIGIT+)* }`.
    ///
    /// The rule is atomic, so the version is whatever run of digits and dots
    /// follows `@` with no intervening space. The lexer splits `1.0.0` into
    /// `FLOAT_LITERAL DOT INT_LITERAL`, so this stitches adjacent tokens back
    /// together rather than teaching the lexer about versions.
    fn parse_package_version(&mut self) -> (Option<Name>, Option<Span>) {
        if !self.is(AT) {
            return (None, None);
        }
        let at_span = self.current_span();
        self.assert(AT);

        let start = self.offset as usize;
        if start != at_span.end || !matches!(self.current(), INT_LITERAL | FLOAT_LITERAL) {
            self.error_at(at_span, "expected a version number after `@`");
            // `PackageDecl::version` is an `Option`, so `None` here is
            // indistinguishable from `package a:b;` — the version is a *token*
            // run and has no node slot. `package a:b@;` used to report and mark
            // nothing at all, while `package a:b@ 1;` marked it via the gap
            // check below, which made the S5 hole version-specific.
            let at = self.zero_width_error_node();
            self.record_recovery_mark(at);
            return (None, None);
        }

        let mut end = start;
        while matches!(self.current(), INT_LITERAL | FLOAT_LITERAL | DOT) {
            // Atomic rule: stop at the first gap between tokens.
            if self.offset as usize != end {
                break;
            }
            end = self.current_span().end;
            self.advance();
        }

        let span = Span::new(self.source, start, end);
        let text = self.text(span);
        if !is_package_version(text) {
            // `("." ~ ASCII_DIGIT+)*` — every dot needs a digit run after it, so
            // `package a:b@1.;` is not a version at all and the frozen parser
            // rejects it. The lexer hands `1.` over as one `FLOAT_LITERAL`, so
            // stitching tokens back together is not enough; the stitched text
            // has to be checked against the rule.
            self.error_at(span, format!("`{text}` is not a package version"));
            let at = self.zero_width_error_node();
            self.record_recovery_mark(at);
        }
        (Some(self.intern(text)), Some(span))
    }

    // -- record / enum / variant -------------------------------------------

    fn parse_record_decl(&mut self) -> ast::ItemKind {
        self.start_node();
        self.assert(RECORD_KW);
        let name = self.expect_name();
        if !self.is(L_BRACE) {
            self.expect(L_BRACE);
            return self.finish_error_item();
        }

        let fields = self.parse_list(
            L_BRACE,
            COMMA,
            R_BRACE,
            MEMBER_RECOVERY,
            RECORD_FIELD_LIST,
            TrailingSep::Allowed,
            |p| p.parse_record_field().map(ast::Recovered::Present),
        );

        let span = self.finish_node(RECORD_DECL);
        ast::ItemKind::Record(ast::RecordDecl {
            id: self.new_node_id(),
            span,
            name,
            fields,
        })
    }

    fn parse_record_field(&mut self) -> Option<ast::RecordField> {
        if !self.is_name() {
            return None;
        }
        self.start_node();
        let name = self.expect_name();
        self.expect(COLON);
        let ty = self.parse_type();
        let span = self.finish_node(RECORD_FIELD);
        Some(ast::RecordField {
            id: self.new_node_id(),
            span,
            name,
            ty,
        })
    }

    fn parse_enum_decl(&mut self) -> ast::ItemKind {
        self.start_node();
        self.assert(ENUM_KW);
        let name = self.expect_name();
        if !self.is(L_BRACE) {
            self.expect(L_BRACE);
            return self.finish_error_item();
        }

        let cases = self.parse_list(
            L_BRACE,
            COMMA,
            R_BRACE,
            MEMBER_RECOVERY,
            ENUM_CASE_LIST,
            TrailingSep::Allowed,
            |p| p.parse_case_name(ENUM_CASE),
        );

        let span = self.finish_node(ENUM_DECL);
        ast::ItemKind::Enum(ast::EnumDecl {
            id: self.new_node_id(),
            span,
            name,
            cases,
        })
    }

    /// `enum_case` / `variant_case_name` — lowercase kebab only. A name the
    /// lexer produced as one `IDENTIFIER` but pest would have split (`Foo`,
    /// `my_case`) is rejected here, exactly as it is today.
    fn parse_case_name(&mut self, node: TokenKind) -> Option<ast::MaybeIdent> {
        if !self.is_name() || !self.at_kebab_case_name() {
            return None;
        }
        self.start_node();
        let ident = self.expect_name();
        self.finish_node(node);
        Some(ident)
    }

    /// Report a case name the frozen `enum_case` shape would not have matched.
    /// Returns whether the current token is a usable case name.
    fn at_kebab_case_name(&mut self) -> bool {
        if is_kebab_lower(self.current_text()) {
            return true;
        }
        let span = self.current_span();
        let text = self.current_text().to_string();
        self.error_at(
            span,
            format!("`{text}` is not a lowercase kebab-case case name"),
        );
        false
    }

    fn parse_variant_decl(&mut self) -> ast::ItemKind {
        self.start_node();
        self.assert(VARIANT_KW);
        let name = self.expect_name();
        if !self.is(L_BRACE) {
            self.expect(L_BRACE);
            return self.finish_error_item();
        }

        let cases = self.parse_list(
            L_BRACE,
            COMMA,
            R_BRACE,
            MEMBER_RECOVERY,
            VARIANT_CASE_LIST,
            TrailingSep::Allowed,
            |p| p.parse_variant_case().map(ast::Recovered::Present),
        );

        let span = self.finish_node(VARIANT_DECL);
        ast::ItemKind::Variant(ast::VariantDecl {
            id: self.new_node_id(),
            span,
            name,
            cases,
        })
    }

    fn parse_variant_case(&mut self) -> Option<ast::VariantCase> {
        if !self.is_name() || !self.at_kebab_case_name() {
            return None;
        }
        self.start_node();
        let name = self.expect_name();
        let payload = if self.is(L_PAREN) {
            self.assert(L_PAREN);
            let ty = self.parse_type();
            self.expect(R_PAREN);
            Some(ty)
        } else {
            None
        };
        let span = self.finish_node(VARIANT_CASE);
        Some(ast::VariantCase {
            id: self.new_node_id(),
            span,
            name,
            payload,
        })
    }

    // -- element -----------------------------------------------------------

    fn parse_element_decl(&mut self) -> ast::ItemKind {
        self.start_node();
        self.assert(ELEMENT_KW);
        let name = self.expect_name();
        if !self.expect(L_BRACE) {
            return self.finish_error_item();
        }

        let mut members: Vec<ast::Recovered<ast::PropertyDecl>> = Vec::new();
        self.builder.start_node();
        while !self.is(R_BRACE) && !self.is_eof() {
            let before = self.position();
            if self.is_name() && self.nth_non_trivia(1) == COLON {
                // `element_property = identifier ~ ":" ~ type_annotation ~ ";"`
                // — no default value, unlike a component property.
                members.push(ast::Recovered::Present(self.parse_property_decl(false)));
            } else {
                members.push(self.member_hole("expected a property declaration"));
            }
            assert!(self.position() > before, "element member consumed nothing");
        }
        self.builder.finish_node(MEMBER_LIST);
        self.expect(R_BRACE);

        let span = self.finish_node(ELEMENT_DECL);
        ast::ItemKind::Element(ast::ElementDecl {
            id: self.new_node_id(),
            span,
            name,
            members,
        })
    }

    // -- extern component --------------------------------------------------

    fn parse_extern_component(&mut self) -> ast::ItemKind {
        self.start_node();
        self.assert(EXTERN_KW);
        if !self.expect(COMPONENT_KW) {
            return self.finish_error_item();
        }
        let name = self.expect_name();
        if !self.expect(L_BRACE) {
            return self.finish_error_item();
        }

        let mut members: Vec<ast::ExternMember> = Vec::new();

        self.builder.start_node();
        while !self.is(R_BRACE) && !self.is_eof() {
            let before = self.position();
            if self.is(FUNC_KW) {
                members.push(ast::ExternMember::Method(self.parse_extern_method()));
            } else if self.at_children_marker() {
                let span = self.parse_children_marker();
                members.push(ast::ExternMember::Children {
                    id: self.new_node_id(),
                    span,
                });
            } else if self.is_name() && self.nth_non_trivia(1) == COLON {
                members.push(ast::ExternMember::Property(self.parse_property_decl(false)));
            } else {
                members.push(self.member_hole("expected a property, `func`, or `@children`"));
            }
            assert!(self.position() > before, "import member consumed nothing");
        }
        self.builder.finish_node(MEMBER_LIST);
        self.expect(R_BRACE);

        let span = self.finish_node(IMPORT_COMPONENT);
        ast::ItemKind::ExternComponent(ast::ExternComponentDecl {
            id: self.new_node_id(),
            span,
            name,
            members,
        })
    }

    /// `import_method = "func" ~ identifier ~ "(" ~ func_params? ~ ")" ~ func_return? ~ ";"`
    fn parse_extern_method(&mut self) -> ast::FunctionDecl {
        self.start_node();
        self.assert(FUNC_KW);
        let name = self.expect_name();
        let signature = self.parse_func_signature();
        self.expect(SEMICOLON);
        let span = self.finish_node(IMPORT_METHOD);
        ast::FunctionDecl {
            id: self.new_node_id(),
            span,
            name,
            is_export: false,
            signature: ast::Recovered::Present(signature),
        }
    }

    // -- global ------------------------------------------------------------

    fn parse_global_decl(&mut self) -> ast::ItemKind {
        self.start_node();
        let is_export = self.parse_export_modifier();
        self.assert(GLOBAL_KW);
        let name = self.expect_name();
        if !self.expect(L_BRACE) {
            return self.finish_error_item();
        }

        let mut members: Vec<ast::GlobalMember> = Vec::new();

        self.builder.start_node();
        while !self.is(R_BRACE) && !self.is_eof() {
            let before = self.position();
            members.push(self.parse_global_member());
            assert!(self.position() > before, "global member consumed nothing");
        }
        self.builder.finish_node(MEMBER_LIST);
        self.expect(R_BRACE);

        let span = self.finish_node(GLOBAL_DECL);
        ast::ItemKind::Global(ast::GlobalDecl {
            id: self.new_node_id(),
            span,
            name,
            is_export,
            members,
        })
    }

    /// `global_member = function_decl | global_callback | global_property`.
    ///
    /// `function_decl` comes first, so `name: func(..);` is a callback here
    /// even though the identical text is a *property* inside a component.
    fn parse_global_member(&mut self) -> ast::GlobalMember {
        // `export? identifier ":" func_type ";"` — the **first** alternative, so
        // it is tried first. `callback` is not a reserved word, so
        // `callback: func();` is a function declaration *named* `callback`;
        // committing on the keyword ahead of this made that one input the only
        // name in the language that could not be used here.
        let func_offset = usize::from(self.is(EXPORT_KW));
        if self.nth_is_name(func_offset)
            && self.nth_non_trivia(func_offset + 1) == COLON
            && self.nth_non_trivia(func_offset + 2) == FUNC_KW
            // `func_type` needs the `(`. Without it this is not a `function_decl`
            // at all: `global G { f: func; }` is a *property* whose type is the
            // named type `func`, which the frozen parser accepts.
            && self.nth_non_trivia(func_offset + 3) == L_PAREN
        {
            return ast::GlobalMember::Callback(self.parse_function_decl());
        }

        // `global_callback = "callback" ~ identifier ~ "(" ~ …`. `callback` is a
        // word now, but it is still not *reserved*: `callbacks: s32;` is a
        // property, and `callback: func();` was already taken by the
        // `function_decl` alternative above.
        if self.is(CALLBACK_KW) && (self.nth_non_trivia(1) == L_PAREN || self.nth_is_name(1)) {
            return ast::GlobalMember::Callback(self.parse_global_callback());
        }

        // `property_direction? identifier ":" type_annotation ("=" expr)? ";"`
        match self.global_property_direction() {
            DirectionMatch::Dead => {
                return self.member_hole(
                    "expected a name after `in`, `out` or `in-out` in this global property",
                );
            }
            DirectionMatch::Present => {
                return ast::GlobalMember::Property(self.parse_global_property(true));
            }
            DirectionMatch::Absent => {}
        }
        if self.nth_is_name(0) && self.nth_non_trivia(1) == COLON {
            return ast::GlobalMember::Property(self.parse_global_property(false));
        }

        self.member_hole("expected a global property or callback declaration")
    }

    /// What `property_direction?` does to the `global_property` starting here.
    ///
    /// `property_direction` is `(!GLUED_IN_OUT ~ "in-out") | (!GLUED_IN ~ "in")
    /// | (!GLUED_OUT ~ "out")` and PEG's `?` is **possessive**: once it matches,
    /// pest never backtracks out of it. So one case still rejects input a "does
    /// a direction token start this member?" test happily accepts — the
    /// spelling is the *whole* token but no name follows, `identifier` is
    /// matched against `:`, fails, and the whole alternative dies.
    /// `global G { in: s32; }` is **rejected**, not a property called `in`.
    /// (`global G { in: func(); }` is still fine — `function_decl` is an earlier
    /// alternative and matched before this one was tried.)
    ///
    /// The spellings are words now, so a longer identifier that merely *starts*
    /// with one — `input`, `outer`, `in-outer` — is an ordinary property name
    /// and never reaches the possessive `?` at all. Before the boundary,
    /// `input:` was direction `in` on a property called `put`.
    ///
    /// The literals are tried in the grammar's order, so `in-out` wins over `in`.
    fn global_property_direction(&self) -> DirectionMatch {
        if matches!(self.current(), IN_KW | OUT_KW | IN_OUT_KW) {
            return if self.nth_is_name(1) && self.nth_non_trivia(2) == COLON {
                DirectionMatch::Present
            } else {
                DirectionMatch::Dead
            };
        }
        DirectionMatch::Absent
    }

    fn parse_global_property(&mut self, has_direction: bool) -> ast::GlobalProperty {
        self.start_node();
        // `has_direction` is the caller's prediction; the `match` is the only
        // thing that can confirm it. A `todo!()` here used to make the two
        // disagreeing a **panic**, on a function whose contract is that parsing
        // always returns (invariant S6) — the same shape as an `unreachable!()`
        // behind a FIRST-set gate, which round 1 removed everywhere else.
        let direction = match (has_direction, self.current()) {
            (true, IN_KW) => Some(ast::PropertyDirection::In),
            (true, OUT_KW) => Some(ast::PropertyDirection::Out),
            (true, IN_OUT_KW) => Some(ast::PropertyDirection::InOut),
            (true, _) => {
                self.error_here(format!(
                    "expected `in`, `out` or `in-out`, found `{}`",
                    self.current().spelling()
                ));
                let at = self.zero_width_error_node();
                self.record_recovery_mark(at);
                None
            }
            (false, _) => None,
        };
        if direction.is_some() {
            self.start_node();
            self.advance();
            self.finish_node(MODIFIER);
        }

        let name = self.expect_name();
        self.expect(COLON);
        let ty = self.parse_type();
        let default = if self.eat(EQ) {
            Some(self.parse_expr())
        } else {
            None
        };
        self.expect(SEMICOLON);
        let span = self.finish_node(GLOBAL_PROPERTY);

        ast::GlobalProperty {
            id: self.new_node_id(),
            span,
            direction,
            name,
            ty,
            default,
        }
    }

    /// `global_callback = "callback" ~ identifier ~ "(" ~ params? ~ ")" ~ ret? ~ ";"`
    fn parse_global_callback(&mut self) -> ast::FunctionDecl {
        self.start_node();
        self.assert(CALLBACK_KW);
        let name = self.expect_name();
        let signature = self.parse_func_signature();
        self.expect(SEMICOLON);
        let span = self.finish_node(GLOBAL_CALLBACK);
        ast::FunctionDecl {
            id: self.new_node_id(),
            span,
            name,
            is_export: false,
            signature: ast::Recovered::Present(signature),
        }
    }

    // -- component ---------------------------------------------------------

    fn parse_component_decl(&mut self) -> ast::ItemKind {
        self.start_node();
        let is_export = self.parse_export_modifier();
        self.assert(COMPONENT_KW);
        let name = self.expect_name();
        if !self.expect(L_BRACE) {
            return self.finish_error_item();
        }

        let mut members: Vec<ast::ComponentMember> = Vec::new();

        self.builder.start_node();
        while !self.is(R_BRACE) && !self.is_eof() {
            let before = self.position();
            members.push(self.parse_component_member());
            assert!(
                self.position() > before,
                "component member consumed nothing"
            );
        }
        self.builder.finish_node(MEMBER_LIST);
        self.expect(R_BRACE);

        let span = self.finish_node(COMPONENT_DECL);
        ast::ItemKind::Component(ast::ComponentDecl {
            id: self.new_node_id(),
            span,
            name,
            is_export,
            members,
        })
    }

    /// `component_member = property_decl | function_decl | node | CATCH_ALL`,
    /// gated by [`MEMBER_FIRST`] so that set drives prediction and not only
    /// resynchronisation. As in `parse_item`, the gate reports rather than
    /// asserts.
    fn parse_component_member(&mut self) -> ast::ComponentMember {
        if !self.is_set(MEMBER_FIRST) {
            return self.member_hole("expected a property, function, or node");
        }

        // `export identifier ":" func_type ";"` — the only `function_decl` form
        // reachable here, because `property_decl` shadows the unexported one.
        // `export` is not reserved, so `export: s32 = 0;` is a property *called*
        // `export`, which `property_decl` matches one alternative earlier.
        if self.is(EXPORT_KW) && self.nth_is_name(1) && self.nth_non_trivia(2) == COLON {
            return ast::ComponentMember::Function(self.parse_function_decl());
        }

        if self.is_name() && self.nth_non_trivia(1) == COLON {
            return ast::ComponentMember::Property(self.parse_property_decl(true));
        }

        if self.is_set(NODE_FIRST) {
            return ast::ComponentMember::Node(self.parse_ui_node());
        }

        self.member_hole("expected a property, function, or node")
    }

    /// `property_decl = identifier ~ ":" ~ type_annotation ~ ("=" ~ expr)? ~ ";"`
    ///
    /// `allow_default` is false for `element_property` / `import_property`,
    /// which the grammar writes without the `= expr` tail.
    fn parse_property_decl(&mut self, allow_default: bool) -> ast::PropertyDecl {
        self.start_node();
        let name = self.expect_name();
        self.expect(COLON);
        let ty = self.parse_type();
        let default = if self.is(EQ) {
            if !allow_default {
                self.error_here("a default value is not allowed on this declaration");
                // …and mark it. `PropertyDecl` has a `default` field, so the
                // value itself has somewhere to go; what has no slot is the
                // fact that writing one here is not grammatical, and reporting
                // that without marking it left `element E { a: s32 = 1; }` and
                // `extern component D { a: s32 = 1; }` with a diagnostic and
                // zero recovery nodes (invariant S5).
                let at = self.zero_width_error_node();
                self.record_recovery_mark(at);
            }
            self.assert(EQ);
            Some(self.parse_expr())
        } else {
            None
        };
        self.expect(SEMICOLON);
        let span = self.finish_node(PROPERTY_DECL);

        ast::PropertyDecl {
            id: self.new_node_id(),
            span,
            name,
            ty,
            default,
        }
    }

    /// `function_decl = export_modifier? ~ identifier ~ ":" ~ func_type ~ ";"`
    ///
    /// A missing `func` keyword makes the whole signature a hole. It is not an
    /// empty parameter list: `component A { export x: s32; }` used to yield a
    /// `FunctionDecl` named `x` with zero parameters, with the `s32` the user
    /// actually wrote silently orphaned.
    fn parse_function_decl(&mut self) -> ast::FunctionDecl {
        self.start_node();
        let is_export = self.parse_export_modifier();
        let name = self.expect_name();
        self.expect(COLON);

        // `func_type = "func" ~ "(" ~ func_params? ~ ")" ~ func_return?`
        self.start_node();
        let signature = if self.is(FUNC_KW) {
            self.assert(FUNC_KW);
            ast::Recovered::Present(self.parse_func_signature())
        } else {
            let at = self.current_span();
            self.error_here("expected `func`");
            ast::Recovered::Missing {
                id: self.new_node_id(),
                span: Span::point(at.source, at.start),
            }
        };
        self.finish_node(FUNC_TYPE);

        self.expect(SEMICOLON);
        let span = self.finish_node(FUNCTION_DECL);

        ast::FunctionDecl {
            id: self.new_node_id(),
            span,
            name,
            is_export,
            signature,
        }
    }

    /// `"(" ~ func_params? ~ ")" ~ func_return?` — shared by every callable
    /// shape. `func_params` forbids a trailing comma.
    ///
    /// An absent `(` makes `params` a hole rather than an empty `Vec`: no
    /// parameter list was read at all, which is not the same as reading one with
    /// nothing in it. This is reachable only where the grammar has already
    /// committed to a callable — `export f: func;`, `callback c;`,
    /// `extern component C { func m; }`. A bare `f: func;` never gets here:
    /// `func_type` needs the `(`, so that is a property whose type is the
    /// *named* type `func`, which the frozen parser accepts.
    fn parse_type_param(&mut self) -> Option<ast::TypeParam> {
        if !self.is_name() {
            return None;
        }
        self.start_node();
        let name = self.expect_name();
        let span = self.finish_node(TYPE_PARAM);
        Some(ast::TypeParam {
            id: self.new_node_id(),
            span,
            name,
        })
    }

    pub(super) fn parse_func_signature(&mut self) -> ast::FuncSignature {
        let mark = self.mark();

        // `<T, U>` — optional, and its absence is not a recovery position.
        let type_params = if self.is(LT) {
            self.parse_list(
                LT,
                COMMA,
                GT,
                PARAM_RECOVERY,
                TYPE_PARAM_LIST,
                TrailingSep::Forbidden,
                |p| p.parse_type_param().map(ast::Recovered::Present),
            )
        } else {
            Vec::new()
        };

        let params = if self.is(L_PAREN) {
            ast::Recovered::Present(self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                PARAM_RECOVERY,
                FUNC_PARAM_LIST,
                TrailingSep::Forbidden,
                |p| p.parse_func_param().map(ast::Recovered::Present),
            ))
        } else {
            let at = self.current_span();
            self.expect(L_PAREN);
            ast::Recovered::Missing {
                id: self.new_node_id(),
                span: Span::point(at.source, at.start),
            }
        };

        let return_type = if self.is(ARROW) {
            self.start_node();
            self.assert(ARROW);
            let ty = self.parse_type();
            self.finish_node(FUNC_RETURN);
            Some(ty)
        } else {
            None
        };

        let span = self.span_from(&mark);
        ast::FuncSignature {
            id: self.new_node_id(),
            span,
            type_params,
            params,
            return_type,
        }
    }

    fn parse_func_param(&mut self) -> Option<ast::FuncParam> {
        if !self.is_name() {
            return None;
        }
        self.start_node();
        let name = self.expect_name();
        self.expect(COLON);
        let ty = self.parse_type();
        let span = self.finish_node(FUNC_PARAM);
        Some(ast::FuncParam {
            id: self.new_node_id(),
            span,
            name,
            ty,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::{parse_err, parse_ok};
    use crate::ast;

    #[test]
    fn parse_package_id_holes_point_at_the_missing_name() {
        let p = parse_err("package ;");
        let ast::ItemKind::Package(pkg) = &p.items()[0] else {
            panic!("expected a package declaration")
        };
        // Not a fabricated `Name`: a hole, at the token that is missing.
        let ast::Recovered::Missing { span, .. } = &pkg.namespace else {
            panic!("the namespace should be a hole")
        };
        assert_eq!(span.start, 8, "the hole is at `;`, not at the `package`");
        assert!(pkg.name.is_missing());
    }

    #[test]
    fn parse_record_with_a_garbage_field_keeps_a_recovery_element() {
        let p = parse_err("record R { a: s32, 42 }");
        let ast::ItemKind::Record(record) = &p.items()[0] else {
            panic!("expected a record")
        };
        assert_eq!(record.fields.len(), 2);
        assert!(record.fields[1].is_missing());
        assert_eq!(record.present_fields().count(), 1);
    }

    #[test]
    fn parse_component_member_error_is_a_member() {
        let p = parse_err("component A { 42; div { \"x\" } }");
        let component = p.component(0);
        assert!(
            component
                .members
                .iter()
                .any(|m| matches!(m, ast::ComponentMember::Error { .. })),
            "the unreadable member must be in the member list"
        );
        // Recovery still finds the node that follows it.
        assert_eq!(component.body().count(), 1);
    }

    #[test]
    fn parse_global_member_error_is_a_member() {
        let p = parse_err("global G { 42 }");
        let ast::ItemKind::Global(global) = &p.items()[0] else {
            panic!("expected a global")
        };
        assert!(
            global
                .members
                .iter()
                .any(|m| matches!(m, ast::GlobalMember::Error { .. }))
        );
    }

    #[test]
    fn parse_export_without_func_leaves_the_signature_a_hole() {
        // The written `s32` is not silently orphaned into an empty parameter
        // list; the signature is missing outright.
        let p = parse_err("component A { export x: s32; }");
        let component = p.component(0);
        let function = component.functions().next().expect("a function decl");
        assert!(function.signature.is_missing());
        assert!(function.is_export);
    }

    #[test]
    fn parse_package_after_an_item_is_reported_as_misplaced() {
        let p = parse_err("component A {}\npackage a:b;");
        assert!(matches!(p.items()[1], ast::ItemKind::Error { .. }));
    }

    #[test]
    fn parse_element_and_extern_members_stay_in_source_order() {
        let p = parse_ok(
            "extern component Dialog { name: string; func show(a: s32) -> bool; @children }",
        );
        let ast::ItemKind::ExternComponent(ec) = &p.items()[0] else {
            panic!("expected an external component")
        };
        assert!(matches!(ec.members[0], ast::ExternMember::Property(_)));
        assert!(matches!(ec.members[1], ast::ExternMember::Method(_)));
        assert!(matches!(ec.members[2], ast::ExternMember::Children { .. }));
    }
}
