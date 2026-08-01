//! `return` as a statement — `plans/rewrite/scope.md` § *`return`, reversing the
//! decision two entries above* (2026-07-29).
//!
//! # Why these are node-shape assertions, and why some of them read the oracle
//!
//! Same reason `tests/blocks.rs` gives: a misparse round-trips byte-for-byte, so
//! invariant S1 holds and nothing fails. Every case below names the construct the
//! parser must build.
//!
//! `return` needs one thing `blocks.rs` did not. The other surface additions were
//! **purely additive** — every text they claimed was a syntax error on both
//! parsers beforehand — so "the frozen parser has no oracle for this" was the
//! whole story. `return` is the first one that is not additive: the frozen
//! grammar has no `return` production, so every `return` it sees is an ordinary
//! *name*, and it already accepts `return;`, `return -1;`, `return(x);` and
//! `return = 1;` in statement position. Committing on the keyword takes those
//! texts away.
//!
//! That is a real accept/reject and construct-identity change, and **`parity.rs`
//! and `identity.rs` are blind to it**: not because their sweeps are weak, but
//! because the word `return` does not occur outside a comment in any of the 2118
//! checked-in `.yel` files, and neither mutation generator can introduce a word
//! that is not already in the text. So the delta is measured here, against the
//! frozen parser directly, in
//! [`the_frozen_parser_accepts_every_shape_this_change_narrows`] and
//! [`the_frozen_parser_rejects_every_shape_this_change_adds`]. Those two tests
//! are the whole boundary, enumerated in both directions.

use yelc_base::{Diagnostics, NameInterner, SourceId};
use yelc_syntax::ast;
use yelc_syntax::ast::visit::{self, ErrorNodeCounter, Visitor};

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

struct Parsed {
    interner: NameInterner,
    file: ast::File,
    diagnostics: usize,
    error_nodes: usize,
}

/// Parse, and assert invariants S1 and S2 on the way through.
fn parse(source: &str) -> Parsed {
    let interner = NameInterner::new();
    let mut diags = Diagnostics::new();
    let parsed = yelc_syntax::parse(SourceId(0), source, &interner, &mut diags);
    assert_eq!(
        parsed.green.text(),
        source,
        "S1: the green tree must reconstruct the source byte-for-byte"
    );
    assert_eq!(
        parsed.green.len() as usize,
        source.len(),
        "S2: green length must equal source length"
    );
    let error_nodes = ErrorNodeCounter::run(&parsed.ast).count;
    Parsed {
        interner,
        file: parsed.ast,
        diagnostics: diags.error_count(),
        error_nodes,
    }
}

fn parse_ok(source: &str) -> Parsed {
    let parsed = parse(source);
    assert_eq!(
        (parsed.diagnostics, parsed.error_nodes),
        (0, 0),
        "expected {source:?} to parse cleanly"
    );
    parsed
}

/// Ill-formed input: a diagnostic **and** a recovery node, never one alone
/// (invariant S5).
fn parse_err(source: &str) -> Parsed {
    let parsed = parse(source);
    assert!(
        parsed.diagnostics > 0,
        "expected a diagnostic for {source:?}"
    );
    assert!(
        parsed.error_nodes > 0,
        "S5: {source:?} produced {} diagnostic(s) and no recovery node",
        parsed.diagnostics
    );
    parsed
}

impl Parsed {
    fn text(&self, name: yelc_base::Name) -> String {
        self.interner.str(name).to_string()
    }

    fn ident(&self, ident: &ast::MaybeIdent) -> String {
        self.text(ident.present().expect("a present identifier").name)
    }

    fn component(&self, index: usize) -> &ast::ComponentDecl {
        self.file
            .items
            .iter()
            .filter_map(|item| match item {
                ast::ItemKind::Component(component) => Some(component),
                _ => None,
            })
            .nth(index)
            .expect("a component declaration")
    }

    fn global(&self, index: usize) -> &ast::GlobalDecl {
        self.file
            .items
            .iter()
            .filter_map(|item| match item {
                ast::ItemKind::Global(global) => Some(global),
                _ => None,
            })
            .nth(index)
            .expect("a global declaration")
    }

    /// The body of the `index`-th function declared in the first global.
    fn function_body(&self, index: usize) -> &ast::Block {
        self.global(0)
            .callbacks()
            .nth(index)
            .expect("a function declaration")
            .body
            .as_ref()
            .expect("a body")
    }

    /// The closure most cases below write as the first prop of the first element
    /// of the first component — the shortest route to a statement block.
    fn first_closure(&self) -> &ast::ClosureExpr {
        let ast::UiNode::Element(element) = self.component(0).body().next().expect("a body node")
        else {
            panic!("expected an element")
        };
        let ast::ExprKind::Closure(closure) = &element.props[0].value.kind else {
            panic!("expected a closure")
        };
        closure
    }
}

/// The `ReturnStmt` at `stmts[index]`, or a panic naming what was there instead.
fn return_at(block: &ast::Block, index: usize) -> &ast::ReturnStmt {
    match block.stmts.get(index) {
        Some(ast::Stmt::Return(stmt)) => stmt,
        other => panic!("expected a `return` statement at {index}, found {other:?}"),
    }
}

/// An `if`-statement's then-branch block.
fn then_block(stmt: &ast::Stmt) -> &ast::Block {
    let ast::Stmt::If(node) = stmt else {
        panic!("expected an `if` statement, found {stmt:?}")
    };
    node.then_branch.present().expect("an opened `{`")
}

/// A `for` statement's body block.
fn for_block(stmt: &ast::Stmt) -> &ast::Block {
    let ast::Stmt::For(node) = stmt else {
        panic!("expected a `for` statement, found {stmt:?}")
    };
    match &node.body {
        ast::ForBody::Statements(block) => block.present().expect("an opened `{`"),
        ast::ForBody::Nodes(_) => panic!("expected a statement body, found UI nodes"),
    }
}

/// Every source this file parses, so the S1 sweep below covers all of them
/// rather than the handful whose test happens to be looked at.
const EVERY_SOURCE: &[&str] = &[
    RETURN_A_VALUE,
    RETURN_NOTHING,
    STARTS_WITH,
    RETURN_AND_A_TAIL,
    RETURN_IN_A_CLOSURE_INSIDE_A_FUNCTION,
    RETURN_IN_EVERY_BLOCK_POSITION,
    RETURN_LAST,
    RETURN_WITHOUT_A_SEMICOLON,
    RETURN_OF_GARBAGE,
    RETURN_AS_A_NAME,
    WALKED,
];

#[test]
fn every_source_in_this_file_round_trips() {
    // S1 again, as a sweep: `parse` asserts it per call, and this is the check
    // that no constant here is reachable only from a test that was deleted.
    for source in EVERY_SOURCE {
        let interner = NameInterner::new();
        let mut diags = Diagnostics::new();
        let parsed = yelc_syntax::parse(SourceId(0), source, &interner, &mut diags);
        assert_eq!(parsed.green.text(), *source, "S1 failed for {source:?}");
    }
    assert_eq!(EVERY_SOURCE.len(), 11, "a source was added without a test");
}

/// The text of every `RETURN_STMT` node in the green tree, in source order,
/// with trailing trivia trimmed (a green node covers the trivia after its last
/// token; the AST span does not — `parser.rs`, `finish_node`).
fn green_return_stmts(source: &str) -> Vec<String> {
    fn walk(node: &yelc_syntax::green::GreenNode, out: &mut Vec<String>) {
        if node.kind() == yelc_syntax::token::TokenKind::RETURN_STMT {
            out.push(node.text().trim_end().to_string());
        }
        for child in node.children() {
            if let Some(inner) = child.to_node() {
                walk(&inner, out);
            }
        }
    }
    let interner = NameInterner::new();
    let mut diags = Diagnostics::new();
    let parsed = yelc_syntax::parse(SourceId(0), source, &interner, &mut diags);
    let mut out = Vec::new();
    walk(&parsed.green, &mut out);
    out
}

/// # The typed AST is not the only tree that can misparse
///
/// Every other assertion in this file reads `ast::Stmt::Return`. The **green**
/// tree is a second, independent record of the same decision, and it is the one
/// the LSP and `yelc2 --emit=green` read. Closing this node as `EXPR_STMT` —
/// which is what the code did before `RETURN_STMT` existed, and what a
/// copy-pasted `finish_node` would still do — leaves every AST assertion green,
/// leaves S1 green because the bytes are unchanged, and silently mislabels the
/// construct for every consumer of the concrete tree.
#[test]
fn the_green_tree_records_a_return_stmt_node() {
    assert_eq!(green_return_stmts(RETURN_A_VALUE), ["return 1;"]);
    assert_eq!(green_return_stmts(RETURN_NOTHING), ["return;"]);
    assert_eq!(
        green_return_stmts(STARTS_WITH),
        ["return false;", "return false;"],
        "both early exits, and nothing else"
    );
    assert!(
        green_return_stmts(RETURN_AS_A_NAME).is_empty(),
        "a `return` used as a name must not produce a RETURN_STMT node"
    );
}

// ---------------------------------------------------------------------------
// The two forms
// ---------------------------------------------------------------------------

const RETURN_A_VALUE: &str = "component A { div { on: { return 1; } } }";

#[test]
fn return_with_a_value_is_a_statement() {
    let p = parse_ok(RETURN_A_VALUE);
    let body = &p.first_closure().body;
    assert_eq!(body.stmts.len(), 1);
    let value = return_at(body, 0).value.as_ref().expect("a returned value");
    assert!(
        matches!(value.kind, ast::ExprKind::Int(_)),
        "the value is the expression that was written, not a hole"
    );
    assert!(
        body.tail.is_none(),
        "`return 1;` is a statement — it is not the block's tail"
    );
}

const RETURN_NOTHING: &str = "component A { div { on: { return; } } }";

#[test]
fn a_bare_return_has_no_value() {
    let p = parse_ok(RETURN_NOTHING);
    let body = &p.first_closure().body;
    assert_eq!(body.stmts.len(), 1);
    assert!(
        return_at(body, 0).value.is_none(),
        "`return;` returns nothing; no value may be invented for it"
    );
}

// ---------------------------------------------------------------------------
// The motivating case
// ---------------------------------------------------------------------------

/// The function `stdlib/string.yel` has commented out, and the reason `return`
/// exists (`scope.md`, 2026-07-29): a `return` inside an `if` inside a `for`,
/// with a tail expression after the loop. Nothing else expresses "stop iterating
/// and answer now".
const STARTS_WITH: &str = "export global Str {\n\
    \x20   starts-with: func(text: string, prefix: string) -> bool {\n\
    \x20       if bytes-len(prefix) > bytes-len(text) { return false; }\n\
    \n\
    \x20       for i in 0..bytes-len(prefix) {\n\
    \x20           if byte-at(text, i) != byte-at(prefix, i) { return false; }\n\
    \x20       }\n\
    \n\
    \x20       true\n\
    \x20   }\n\
    }\n";

#[test]
fn the_starts_with_shape_parses() {
    let p = parse_ok(STARTS_WITH);
    let body = p.function_body(0);

    assert_eq!(body.stmts.len(), 2, "the guard `if`, then the `for`");

    // `if … { return false; }` — the guard.
    let guard = then_block(&body.stmts[0]);
    assert_eq!(guard.stmts.len(), 1);
    let returned = return_at(guard, 0).value.as_ref().expect("`false`");
    assert!(matches!(returned.kind, ast::ExprKind::Bool(false)));

    // `for i in 0..bytes-len(prefix) { if … { return false; } }` — the early
    // exit from a *loop*, which is the case `match` does not subsume.
    let ast::Stmt::For(loop_node) = &body.stmts[1] else {
        panic!("expected the `for` statement")
    };
    assert_eq!(p.ident(&loop_node.item), "i");
    assert!(
        matches!(
            loop_node.iterable.kind,
            ast::ExprKind::Range {
                inclusive: false,
                ..
            }
        ),
        "the iterable is the whole range, not just its start"
    );
    let loop_body = for_block(&body.stmts[1]);
    assert_eq!(loop_body.stmts.len(), 1);
    let inner = then_block(&loop_body.stmts[0]);
    assert_eq!(inner.stmts.len(), 1);
    assert!(return_at(inner, 0).value.is_some());

    // …and the tail after the loop is still the function's value.
    let tail = p
        .function_body(0)
        .tail
        .as_ref()
        .expect("`true` is the function's value");
    assert!(matches!(tail.kind, ast::ExprKind::Bool(true)));
}

const RETURN_AND_A_TAIL: &str =
    "global G {\n  f: func(n: s32) -> s32 { if n < 0 { return 0; } n * 2 }\n}\n";

#[test]
fn a_function_may_use_both_return_and_a_tail() {
    // `LANGUAGE.md` § Return: "A function's last expression is still its value —
    // `return` is for leaving *before* the end, not for producing the result."
    // Both in one body, and neither swallows the other.
    let p = parse_ok(RETURN_AND_A_TAIL);
    let body = p.function_body(0);
    assert_eq!(body.stmts.len(), 1, "the guard `if` is the only statement");
    assert!(return_at(then_block(&body.stmts[0]), 0).value.is_some());
    let tail = body.tail.as_ref().expect("`n * 2` is still the tail");
    assert!(matches!(tail.kind, ast::ExprKind::Binary { .. }));
}

// ---------------------------------------------------------------------------
// Position
// ---------------------------------------------------------------------------

const RETURN_IN_A_CLOSURE_INSIDE_A_FUNCTION: &str = "global G {\n\
    \x20 f: func() -> s32 { let g = { return 1; }; return 2; }\n\
    }\n";

#[test]
fn a_return_inside_a_closure_lands_in_the_closure() {
    // `return` inside a closure exits the *closure*. The parser does not enforce
    // that — which body a `return` leaves is a later phase's question — but the
    // tree must not make it unanswerable: the inner `return` belongs to the
    // closure's `Block`, and the outer one to the function's, with no shared
    // list and nothing linking either to a declaration.
    let p = parse_ok(RETURN_IN_A_CLOSURE_INSIDE_A_FUNCTION);
    let body = p.function_body(0);
    assert_eq!(body.stmts.len(), 2, "the `let`, then the outer `return`");

    let ast::Stmt::Let(binding) = &body.stmts[0] else {
        panic!("expected the `let`")
    };
    let ast::ExprKind::Closure(closure) = &binding.value.kind else {
        panic!("expected the initialiser to be a closure")
    };
    assert_eq!(closure.body.stmts.len(), 1);
    let inner = return_at(&closure.body, 0)
        .value
        .as_ref()
        .expect("the closure returns `1`");
    assert!(matches!(inner.kind, ast::ExprKind::Int(_)));

    // …and the function's own `return` is a sibling of the `let`, not a child of
    // the closure.
    assert!(return_at(body, 1).value.is_some());
}

const RETURN_IN_EVERY_BLOCK_POSITION: &str = "global G {\n\
    \x20 f: func() -> s32 { return 1; }\n\
    }\n\
    component A {\n\
    \x20 div { on: { return 2; } }\n\
    \x20 div { on: { if c { return 3; } } }\n\
    \x20 div { on: { for i in 0..3 { return 4; } } }\n\
    }\n";

/// Every statement-block owner, in one file.
///
/// "A statement, legal anywhere a statement is" is four positions, not one:
/// `ast::Block` has four owners (closure body, function body, `if`-statement
/// branch, `for`-statement body) and the dispatch lives in `parse_stmt_inner`,
/// which all four reach. Asserting one of them would leave the claim about the
/// other three untested.
#[test]
fn return_is_legal_in_every_statement_block() {
    let p = parse_ok(RETURN_IN_EVERY_BLOCK_POSITION);

    // function body
    assert!(return_at(p.function_body(0), 0).value.is_some());

    let mut closures = p.component(0).body().map(|node| {
        let ast::UiNode::Element(element) = node else {
            panic!("expected an element")
        };
        let ast::ExprKind::Closure(closure) = &element.props[0].value.kind else {
            panic!("expected a closure")
        };
        &closure.body
    });

    // closure body
    let plain = closures.next().expect("the first element");
    assert!(return_at(plain, 0).value.is_some());

    // `if`-statement branch
    let branch = closures.next().expect("the second element");
    assert!(return_at(then_block(&branch.stmts[0]), 0).value.is_some());

    // `for`-statement body
    let loop_body = closures.next().expect("the third element");
    assert!(return_at(for_block(&loop_body.stmts[0]), 0).value.is_some());

    assert!(closures.next().is_none(), "three elements, three positions");
}

const RETURN_LAST: &str = "component A { div { on: { g(); return 1; } } }";

#[test]
fn return_may_be_the_last_statement() {
    // A block that ends in `return …;` has **no tail**: the block's value comes
    // from a semicolon-*less* final expression, and `return 1;` has a semicolon.
    // The two mechanisms sit next to each other and must not be confused, which
    // is exactly what a `tail` holding `1` here would be.
    let p = parse_ok(RETURN_LAST);
    let body = &p.first_closure().body;
    assert_eq!(body.stmts.len(), 2);
    assert!(matches!(body.stmts[0], ast::Stmt::Expr(_)), "`g();`");
    assert!(return_at(body, 1).value.is_some());
    assert!(
        body.tail.is_none(),
        "`return 1;` is a statement; it does not become the block's tail"
    );
}

// ---------------------------------------------------------------------------
// Recovery (invariant S5)
// ---------------------------------------------------------------------------

const RETURN_WITHOUT_A_SEMICOLON: &str = "component A { div { on: { return 1 } } }";

#[test]
fn a_return_without_a_semicolon_reports_and_marks_it() {
    // `return_statement = "return" ~ expr? ~ ";"` — the `;` is not optional, and
    // a `return` is never the block's tail. The value the user wrote is kept.
    let p = parse_err(RETURN_WITHOUT_A_SEMICOLON);
    let body = &p.first_closure().body;
    assert_eq!(body.stmts.len(), 1);
    assert!(
        return_at(body, 0).value.is_some(),
        "the expression is reported against, not discarded"
    );
    assert!(body.tail.is_none());
}

const RETURN_OF_GARBAGE: &str = "component A { div { on: { return * 1; } } }";

#[test]
fn a_return_of_something_that_cannot_start_an_expression_reports_and_recovers() {
    // `*` is not a prefix operator, so it cannot start the optional value: the
    // `return` has no value, the missing `;` is reported where it is missing,
    // and the leftover `* 1;` becomes an `Error` statement rather than being
    // swallowed. Both halves of S5 — a diagnostic *and* a node — and no
    // invented expression standing in for text the user did not write.
    let p = parse_err(RETURN_OF_GARBAGE);
    let body = &p.first_closure().body;
    assert!(
        return_at(body, 0).value.is_none(),
        "no value may be fabricated from a token that cannot start one"
    );
    assert!(
        body.stmts
            .iter()
            .any(|s| matches!(s, ast::Stmt::Error { .. })),
        "the unparsed remainder must reach the tree as an `Error` statement, \
         found {:?}",
        body.stmts
    );
}

// ---------------------------------------------------------------------------
// `return` is still a name — everywhere a name is legal
// ---------------------------------------------------------------------------

const RETURN_AS_A_NAME: &str = "record R { return: s32 }\n\
    global G { return: s32 = 1; }\n\
    component A {\n\
    \x20 return: s32 = 0;\n\
    \x20 return { }\n\
    \x20 div { on: { let return = 1; g(return); h(x.return); } }\n\
    }\n";

#[test]
fn return_is_still_an_ordinary_name_outside_statement_position() {
    // `RETURN_KW` joins `KEYWORD_FIRST` ⊆ `NAME_FIRST`, so it is accepted at
    // every position that reads an `identifier` — exactly as `for`, `if` and
    // `let` are. The narrowing this change makes is confined to *statement*
    // position; this is the test that says how far it reaches.
    //
    // The frozen parser accepts this text too, and that is asserted rather than
    // assumed: if it did not, the file would prove nothing about narrowing.
    assert!(
        !frozen_rejects(RETURN_AS_A_NAME),
        "the premise of this test is that the frozen parser accepts it"
    );
    let p = parse_ok(RETURN_AS_A_NAME);

    let ast::ItemKind::Record(record) = &p.file.items[0] else {
        panic!("expected the record")
    };
    assert_eq!(
        p.ident(&record.fields[0].present().expect("a field").name),
        "return"
    );
    assert_eq!(
        p.ident(&p.global(0).properties().next().expect("a property").name),
        "return"
    );

    let component = p.component(0);
    assert_eq!(
        p.ident(&component.properties().next().expect("a property").name),
        "return"
    );
    let ast::UiNode::Element(element) = component.body().next().expect("a node") else {
        panic!("expected an element called `return`")
    };
    assert_eq!(p.ident(&element.name), "return");

    let body = &p.first_closure_named_return();
    let ast::Stmt::Let(binding) = &body.stmts[0] else {
        panic!("`let return = 1;` must bind a variable called `return`")
    };
    assert_eq!(p.ident(&binding.name), "return");
    assert!(matches!(body.stmts[1], ast::Stmt::Expr(_)), "`g(return);`");
    assert!(
        matches!(body.stmts[2], ast::Stmt::Expr(_)),
        "`h(x.return);`"
    );
}

impl Parsed {
    /// The closure in `RETURN_AS_A_NAME`, which is the *second* node of its
    /// component — the first is the element called `return`.
    fn first_closure_named_return(&self) -> &ast::Block {
        let ast::UiNode::Element(element) = self.component(0).body().nth(1).expect("a second node")
        else {
            panic!("expected the `div`")
        };
        let ast::ExprKind::Closure(closure) = &element.props[0].value.kind else {
            panic!("expected a closure")
        };
        &closure.body
    }
}

// ---------------------------------------------------------------------------
// The accept/reject boundary, measured against the frozen parser
// ---------------------------------------------------------------------------

/// The oracle `parity.rs` uses, on the same terms: a hard pest failure, or a
/// `CATCH_ALL` node it recovered from.
fn frozen_rejects(content: &str) -> bool {
    match yel_core::syntax::parser::parse_file_with_source_id(content, yel_core::SourceId(0)) {
        Ok(result) => !result.catched_errors.is_empty(),
        Err(_) => true,
    }
}

/// Wrap a statement-position body so both parsers see the same context.
fn in_a_closure(body: &str) -> String {
    format!("component A {{ div {{ on: {{ {body} }} }} }}")
}

/// Texts the frozen parser accepts as something about a **variable** called
/// `return`, and which this change re-reads as a `return` statement.
///
/// Nine texts, in two groups by what happens to them. This is the whole
/// narrowing, enumerated — not a sample.
const NARROWED_TO_A_RETURN: &[&str] = &[
    // an expression statement about a variable called `return`
    "return;",
    // binary subtraction with `return` as the left operand
    "return - 1;",
    // a call whose callee is a variable called `return`
    "return(x);",
    // an index into a variable called `return`
    "return [0];",
];

const NARROWED_TO_A_SYNTAX_ERROR: &[&str] = &[
    // assignment to a variable called `return`
    "return = 1;",
    "return += 1;",
    // member access on a variable called `return`
    "return.x = 1;",
    "return?.x;",
    // a bare `return` as the block's trailing expression
    "g(); return",
];

#[test]
fn the_frozen_parser_accepts_every_shape_this_change_narrows() {
    // # This is the test that makes the narrowing a measurement
    //
    // `scope.md`'s `return` entry does not mention that `return` must become a
    // keyword, and the four surface additions before it were all purely
    // additive — every text they claimed was already a syntax error. `return` is
    // the first that is not. Rather than assert that in prose, each text below
    // is run through the frozen parser: the *premise* (it used to parse) and the
    // *consequence* (it now parses as a `return`, or not at all) are both
    // checked, so neither half can rot into a claim.
    for body in NARROWED_TO_A_RETURN {
        let source = in_a_closure(body);
        assert!(
            !frozen_rejects(&source),
            "{body:?} is listed as narrowed, but the frozen parser rejects it"
        );
        let p = parse_ok(&source);
        let block = &p.first_closure().body;
        assert!(
            block
                .stmts
                .iter()
                .any(|s| matches!(s, ast::Stmt::Return(_))),
            "{body:?} must now read as a `return` statement, found {:?}",
            block.stmts
        );
    }

    for body in NARROWED_TO_A_SYNTAX_ERROR {
        let source = in_a_closure(body);
        assert!(
            !frozen_rejects(&source),
            "{body:?} is listed as narrowed, but the frozen parser rejects it"
        );
        parse_err(&source);
    }

    assert_eq!(
        NARROWED_TO_A_RETURN.len() + NARROWED_TO_A_SYNTAX_ERROR.len(),
        9,
        "the narrowing is enumerated, not sampled; adding to it is a diff to \
         this line and to plans/rewrite/seam-changes.md"
    );
}

/// The other direction: what the change buys. Every one of these is a syntax
/// error on the frozen parser, because `return` there is a name and a name
/// followed by another expression is not an expression.
const WIDENED: &[&str] = &[
    "return x;",
    "return 1;",
    "return false;",
    "return \"s\";",
    "return !x;",
];

#[test]
fn the_frozen_parser_rejects_every_shape_this_change_adds() {
    for body in WIDENED {
        let source = in_a_closure(body);
        assert!(
            frozen_rejects(&source),
            "{body:?} is listed as new, but the frozen parser already accepts it"
        );
        let p = parse_ok(&source);
        assert!(
            return_at(&p.first_closure().body, 0).value.is_some(),
            "{body:?} must read as a `return` with a value"
        );
    }
}

// ---------------------------------------------------------------------------
// The walk
// ---------------------------------------------------------------------------

/// Every name the visitor reaches, however it is spelled — the same probe
/// `blocks.rs` uses, for the same reason.
#[derive(Default)]
struct NamesSeen(Vec<yelc_base::Name>);

impl Visitor for NamesSeen {
    fn visit_expr(&mut self, node: &ast::Expr) {
        if let ast::ExprKind::Ident(name) = node.kind {
            self.0.push(name);
        }
        visit::walk_expr(self, node);
    }

    fn visit_ident(&mut self, node: &ast::Ident) {
        self.0.push(node.name);
    }
}

const WALKED: &str = "component A {\n\
    \x20 div { on: { return reached-return-value; } }\n\
    \x20 div { on: { return; } }\n\
    }\n";

/// # The check the compiler cannot make for you
///
/// `visit.rs`'s exhaustive `match` on `Stmt` turned `Stmt::Return` into a
/// compile error there, which is the guarantee working. `ReturnStmt::value` is a
/// **field**, and no such error exists for one: a `walk_return_stmt` with an
/// empty body compiles, and the returned expression — the only thing the node
/// carries — silently disappears from every pass built on the visitor.
#[test]
fn the_walker_reaches_the_returned_expression() {
    let p = parse_ok(WALKED);
    let mut seen = NamesSeen::default();
    seen.visit_file(&p.file);
    let names: Vec<String> = seen.0.iter().map(|name| p.text(*name)).collect();
    assert!(
        names.iter().any(|name| name == "reached-return-value"),
        "the walk never reached the returned expression; it saw {names:?}"
    );
}
