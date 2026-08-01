//! Function bodies, the shared `Block`, and `for` in statement position —
//! `plans/rewrite/scope.md` § *function bodies, sharing `Block` with closures*
//! and § *`for` as a statement (GAP 2), and no `return` (GAP 1)* (2026-07-29).
//!
//! # Why these are node-shape assertions and not accept/reject checks
//!
//! Same reason `tests/attributes.rs` gives, and it bites harder here. `for` is
//! now legal in **two** positions with identical syntax, and `for` is not a
//! reserved word, so it is also a legal *variable*. Getting either dispatch
//! wrong is a silent misparse: the file still round-trips byte-for-byte, so
//! invariant S1 holds, `parity.rs`'s one accept/reject bit does not move, and
//! nothing fails. Every case below names the construct the parser must build.
//!
//! The frozen parser accepts none of this, so there is no oracle — see
//! `scope.md` § *The freeze now carries four breaks*. What holds these honest is
//! that each assertion names a tree, and that the mutation log in the stage
//! report records the deliberate breakage each one was seen to catch.
//!
//! The harness below is a third copy of the one in `attributes.rs` and
//! `generics.rs`. Deliberate: each test binary asserts on a different slice of
//! the AST and carries the accessors for it. What is *not* duplicated is the
//! S1/S2/S5 checking, which every copy performs identically because it is the
//! part that must not drift.

mod support;

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

    /// The `index`-th function declared in the `index0`-th global.
    fn global_function(&self, global: usize, index: usize) -> &ast::FunctionDecl {
        self.global(global)
            .callbacks()
            .nth(index)
            .expect("a function declaration")
    }

    /// The closure that most cases below write as the first prop of the first
    /// element of the first component — the shortest route to a statement
    /// block.
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

/// A `for` body's statements, or a panic naming what it held instead.
fn for_statements(node: &ast::ForNode) -> &ast::Block {
    match &node.body {
        ast::ForBody::Statements(block) => block.present().expect("an opened `{`"),
        ast::ForBody::Nodes(_) => panic!("expected a statement body, found UI nodes"),
    }
}

/// A `for` body's UI nodes, or a panic naming what it held instead.
fn for_nodes(node: &ast::ForNode) -> &[ast::UiNode] {
    match &node.body {
        ast::ForBody::Nodes(nodes) => nodes.present().expect("an opened `{`"),
        ast::ForBody::Statements(_) => panic!("expected UI nodes, found a statement body"),
    }
}

/// Every source this file parses, so the S1 sweep below covers all of them
/// rather than the handful whose test happens to be looked at.
const EVERY_SOURCE: &[&str] = &[
    BODIED_FUNCTION,
    BODYLESS_FUNCTION,
    EXPORTED_COMPONENT_FUNCTION,
    FUNC_TYPED_PROPERTY,
    EXTERN_METHOD_AND_CALLBACK,
    BODY_WITH_STATEMENTS_AND_TAIL,
    BODY_WITHOUT_A_TAIL,
    CLOSURE_WITH_PARAMS,
    FOR_OVER_A_LIST,
    FOR_OVER_A_RANGE,
    FOR_IN_A_TEMPLATE,
    BOTH_FOR_POSITIONS,
    NESTED_FOR_STATEMENTS,
    FOR_STATEMENT_WITH_A_KEY,
    FOR_AS_A_NAME,
    FOR_AS_AN_ELEMENT_NAME,
    MALFORMED_BODY,
    BODY_WITH_AN_EQUALS,
    BODY_WITH_A_TRAILING_SEMICOLON,
    FOR_STATEMENT_WITHOUT_A_BODY,
    FOR_BODY_FORBIDS_A_TAIL,
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
    assert_eq!(EVERY_SOURCE.len(), 22, "a source was added without a test");
}

// ---------------------------------------------------------------------------
// Function bodies
// ---------------------------------------------------------------------------

const BODIED_FUNCTION: &str = "export global Math {\n  double: func(n: s32) -> s32 { n * 2 }\n}\n";

#[test]
fn a_function_declaration_may_carry_a_block_body() {
    let p = parse_ok(BODIED_FUNCTION);
    let function = p.global_function(0, 0);
    assert_eq!(p.ident(&function.name), "double");
    let signature = function.signature.present().expect("a signature");
    assert_eq!(signature.present_params().count(), 1);
    let body = function.body.as_ref().expect("a body");
    assert!(
        body.stmts.is_empty(),
        "`n * 2` is the tail, not a statement"
    );
    let tail = body.tail.as_ref().expect("a tail");
    assert!(matches!(tail.kind, ast::ExprKind::Binary { .. }));
}

const BODYLESS_FUNCTION: &str = "export global Clock {\n  now: func() -> s64;\n}\n";

#[test]
fn a_declaration_without_a_body_is_still_a_declaration() {
    // The only form that existed before 2026-07-29. `None` means someone else
    // implements it — a host callback here — and nothing about it moved.
    let p = parse_ok(BODYLESS_FUNCTION);
    let function = p.global_function(0, 0);
    assert_eq!(p.ident(&function.name), "now");
    assert!(function.signature.present().is_some(), "signature is read");
    assert!(
        function.body.is_none(),
        "a bodyless declaration must not acquire an empty body"
    );
}

const EXPORTED_COMPONENT_FUNCTION: &str =
    "component A {\n  export twice: func(n: s32) -> s32 { n + n }\n  div { }\n}\n";

#[test]
fn an_exported_component_function_may_carry_a_body() {
    let p = parse_ok(EXPORTED_COMPONENT_FUNCTION);
    let function = p
        .component(0)
        .functions()
        .next()
        .expect("a function member");
    assert!(function.is_export);
    assert!(function.body.is_some());
    // …and the component still has its node, so the body did not swallow it.
    assert_eq!(p.component(0).body().count(), 1);
}

const FUNC_TYPED_PROPERTY: &str = "component A {\n  on-click: func(a: s32);\n}\n";

#[test]
fn a_func_typed_component_property_is_still_a_property() {
    // `property_decl` shadows `function_decl` inside a component (only the
    // `export`-prefixed form is a function there). Bodies must not have moved
    // that line: this text is a *property* whose type is a function, and the
    // 90 positive fixtures depend on it.
    let p = parse_ok(FUNC_TYPED_PROPERTY);
    assert_eq!(p.component(0).properties().count(), 1);
    assert_eq!(p.component(0).functions().count(), 0);
}

const EXTERN_METHOD_AND_CALLBACK: &str =
    "extern component C {\n  func m(a: s32);\n}\nglobal G {\n  callback c(a: s32);\n}\n";

#[test]
fn extern_methods_and_callbacks_stay_bodyless() {
    // Both are `FunctionDecl`s and both gained the field; neither gained the
    // syntax. `LANGUAGE.md` gives a body to the `name: func(…)` spelling only.
    let p = parse_ok(EXTERN_METHOD_AND_CALLBACK);
    let ast::ItemKind::ExternComponent(extern_component) = &p.file.items[0] else {
        panic!("expected an extern component")
    };
    assert!(
        extern_component
            .methods()
            .all(|method| method.body.is_none())
    );
    assert!(p.global(0).callbacks().all(|c| c.body.is_none()));
}

// No trailing `;`, deliberately. With one, this input is rejected because of
// the semicolon whether or not the `=` is accepted, and the test passes for the
// wrong reason — verified: a mutation that accepted `= {` left the `;` version
// green. The `=` has to be the only thing wrong with the text.
const BODY_WITH_AN_EQUALS: &str = "global G {\n  f: func() -> s32 = { 1 }\n}\n";

#[test]
fn a_function_body_is_not_written_with_an_equals() {
    // "block directly after the signature, no `=`" — the `=` form is a
    // property-style default, and there is no such thing on a function.
    parse_err(BODY_WITH_AN_EQUALS);
}

const BODY_WITH_A_TRAILING_SEMICOLON: &str = "global G {\n  f: func() -> s32 { 1 };\n}\n";

#[test]
fn a_function_body_takes_no_trailing_semicolon() {
    // …and the `;` is not silently eaten either: it is reported where it is
    // written, as a member that is not a declaration.
    parse_err(BODY_WITH_A_TRAILING_SEMICOLON);
}

// `let ;` would *not* do: `let` is not reserved and `let_statement` needs a
// name after it, so that text is the expression statement `let;` and parses
// cleanly. A missing initialiser is the shortest thing that is genuinely
// ungrammatical inside a block.
const MALFORMED_BODY: &str = "global G {\n  f: func() -> s32 { let x = ; }\n}\n";

#[test]
fn a_malformed_body_reports_and_leaves_a_recovery_node() {
    // Invariant S5: a diagnostic *and* an `Error` node, which `parse_err`
    // asserts. The body is still `Some`, because the `{` was read — a hole in
    // the block is not the same as no block.
    let p = parse_err(MALFORMED_BODY);
    let function = p.global_function(0, 0);
    let body = function.body.as_ref().expect("the `{` was read");
    assert!(
        !body.stmts.is_empty(),
        "the broken statement must be present in the tree, not dropped"
    );
}

// ---------------------------------------------------------------------------
// `Block` and its tail
// ---------------------------------------------------------------------------

const BODY_WITH_STATEMENTS_AND_TAIL: &str = "component A { div { on: { let x = 1; g(); x + 1 } } }";

#[test]
fn a_block_with_a_final_expression_has_a_tail() {
    let p = parse_ok(BODY_WITH_STATEMENTS_AND_TAIL);
    let body = &p.first_closure().body;
    assert_eq!(body.stmts.len(), 2, "`let` and `g();`");
    let tail = body.tail.as_ref().expect("`x + 1` is the block's value");
    assert!(matches!(tail.kind, ast::ExprKind::Binary { .. }));
}

const BODY_WITHOUT_A_TAIL: &str = "component A { div { on: { g(); } } }";

#[test]
fn a_block_ending_in_a_semicolon_has_no_tail() {
    let p = parse_ok(BODY_WITHOUT_A_TAIL);
    let body = &p.first_closure().body;
    assert_eq!(body.stmts.len(), 1);
    assert!(
        body.tail.is_none(),
        "`g();` is a statement; the block produces nothing"
    );
    assert!(matches!(body.stmts[0], ast::Stmt::Expr(_)));
}

const CLOSURE_WITH_PARAMS: &str = "component A { div { on: { p: s32 -> p + 1 } } }";

#[test]
fn a_closure_body_parses_unchanged() {
    // The seam change is `ClosureExpr::body`'s *type*; the closure grammar did
    // not move. Parameters, the `->`, and the tail all read as before.
    let p = parse_ok(CLOSURE_WITH_PARAMS);
    let closure = p.first_closure();
    assert_eq!(closure.params.len(), 1);
    assert!(closure.params[0].present().expect("a param").ty.is_some());
    assert!(closure.body.stmts.is_empty());
    assert!(matches!(
        closure.body.tail.as_ref().expect("a tail").kind,
        ast::ExprKind::Binary { .. }
    ));
}

// ---------------------------------------------------------------------------
// `for` as a statement
// ---------------------------------------------------------------------------

const FOR_OVER_A_LIST: &str =
    "component A { div { on: { for item in items { total = total + item; } } } }";

#[test]
fn for_is_a_statement_over_a_list() {
    let p = parse_ok(FOR_OVER_A_LIST);
    let ast::Stmt::For(node) = &p.first_closure().body.stmts[0] else {
        panic!("expected a `for` statement")
    };
    assert_eq!(p.ident(&node.item), "item");
    assert!(matches!(node.iterable.kind, ast::ExprKind::Ident(_)));
    assert!(node.key.is_none());
    let block = for_statements(node);
    assert_eq!(block.stmts.len(), 1);
    assert!(matches!(block.stmts[0], ast::Stmt::Assign(_)));
    assert!(block.tail.is_none());
}

const FOR_OVER_A_RANGE: &str = "component A { div { on: { for i in 0..count { total += i; } } } }";

#[test]
fn for_is_a_statement_over_a_range() {
    let p = parse_ok(FOR_OVER_A_RANGE);
    let ast::Stmt::For(node) = &p.first_closure().body.stmts[0] else {
        panic!("expected a `for` statement")
    };
    assert_eq!(p.ident(&node.item), "i");
    assert!(
        matches!(
            node.iterable.kind,
            ast::ExprKind::Range {
                inclusive: false,
                ..
            }
        ),
        "the iterable is the range, not just its start"
    );
    assert_eq!(for_statements(node).stmts.len(), 1);
}

const FOR_IN_A_TEMPLATE: &str = "component A { for row in rows key(row.id) { \"x\" } }";

#[test]
fn for_in_a_template_still_parses() {
    let p = parse_ok(FOR_IN_A_TEMPLATE);
    let ast::UiNode::For(node) = p.component(0).body().next().expect("a node") else {
        panic!("expected a `for` node")
    };
    assert_eq!(p.ident(&node.item), "row");
    assert!(node.key.is_some(), "`key(…)` is a template clause");
    let body = for_nodes(node);
    assert_eq!(body.len(), 1);
    assert!(matches!(body[0], ast::UiNode::Text(_)));
}

const BOTH_FOR_POSITIONS: &str = "component A {\n\
     for row in rows { \"x\" }\n\
     div { on: { for i in 0..3 { total += i; } } }\n\
     }\n";

#[test]
fn both_for_positions_in_one_file_read_as_different_constructs() {
    // The trap this file exists for. `for` is now legal in a component body and
    // in a statement block, with identical syntax, and which one is built must
    // be decided by *which body the parser is in* — never by looking at what
    // follows the `for`. One file, both positions, both asserted.
    let p = parse_ok(BOTH_FOR_POSITIONS);
    let mut members = p.component(0).members.iter();

    let Some(ast::ComponentMember::Node(ast::UiNode::For(template))) = members.next() else {
        panic!("the component-body `for` must be a UI node")
    };
    assert_eq!(p.ident(&template.item), "row");
    assert!(matches!(for_nodes(template)[0], ast::UiNode::Text(_)));

    let Some(ast::ComponentMember::Node(ast::UiNode::Element(element))) = members.next() else {
        panic!("expected the element")
    };
    let ast::ExprKind::Closure(closure) = &element.props[0].value.kind else {
        panic!("expected a closure")
    };
    let ast::Stmt::For(statement) = &closure.body.stmts[0] else {
        panic!("the closure-body `for` must be a statement")
    };
    assert_eq!(p.ident(&statement.item), "i");
    assert!(matches!(
        for_statements(statement).stmts[0],
        ast::Stmt::Assign(_)
    ));
}

const NESTED_FOR_STATEMENTS: &str =
    "component A { div { on: { for i in 0..3 { for j in 0..3 { g(i, j); } } } } }";

#[test]
fn for_statements_nest() {
    let p = parse_ok(NESTED_FOR_STATEMENTS);
    let ast::Stmt::For(outer) = &p.first_closure().body.stmts[0] else {
        panic!("expected the outer `for`")
    };
    let ast::Stmt::For(inner) = &for_statements(outer).stmts[0] else {
        panic!("expected the inner `for`")
    };
    assert_eq!(p.ident(&inner.item), "j");
    assert_eq!(for_statements(inner).stmts.len(), 1);
}

const FOR_STATEMENT_WITH_A_KEY: &str =
    "component A { div { on: { for x in xs key(x.id) { g(x); } } } }";

#[test]
fn a_key_clause_in_statement_position_is_read_and_kept() {
    // One `for` parser means the whole `for_node` shape is grammatical in both
    // positions. `key(…)` is only *meaningful* for template reconciliation, but
    // the parser accepts the grammar rather than the language, and dropping
    // what the user wrote is what invariant S5 forbids. Rejecting it is a later
    // phase's call, and it has the node to reject.
    let p = parse_ok(FOR_STATEMENT_WITH_A_KEY);
    let ast::Stmt::For(node) = &p.first_closure().body.stmts[0] else {
        panic!("expected a `for` statement")
    };
    assert!(node.key.is_some());
}

const FOR_AS_A_NAME: &str = "component A { div { on: { for = 1; g(for); for } } }";

#[test]
fn for_is_still_an_ordinary_name_in_statement_position() {
    // `for` is not reserved, and in a *statement* block it is a legal
    // expression — unlike in a node body. So the statement guard asks for the
    // whole `for <name> in` head and nothing looser: a guard copied from
    // `parse_ui_node` (`the next token is not `{``) would steal every line
    // here.
    let p = parse_ok(FOR_AS_A_NAME);
    let body = &p.first_closure().body;
    assert_eq!(body.stmts.len(), 2);
    let ast::Stmt::Assign(assign) = &body.stmts[0] else {
        panic!("`for = 1;` is an assignment to a variable called `for`")
    };
    let ast::ExprKind::Ident(name) = assign.target.kind else {
        panic!("expected a plain name as the assignment target")
    };
    assert_eq!(p.text(name), "for");
    assert!(matches!(body.stmts[1], ast::Stmt::Expr(_)), "`g(for);`");
    assert!(
        matches!(
            body.tail.as_ref().expect("a tail").kind,
            ast::ExprKind::Ident(_)
        ),
        "a bare `for` is the block's trailing expression"
    );
}

const FOR_AS_AN_ELEMENT_NAME: &str = "component A { for { span { \"x\" } } }";

#[test]
fn for_followed_by_a_brace_is_still_an_element_name() {
    // The template side of the same question, unchanged: `for_node` wants
    // `ident in expr`, so a `for` followed directly by `{` falls through to
    // `element_node`. This row is in `identity.rs`'s hand-written table.
    let p = parse_ok(FOR_AS_AN_ELEMENT_NAME);
    let ast::UiNode::Element(element) = p.component(0).body().next().expect("a node") else {
        panic!("expected an element called `for`")
    };
    assert_eq!(p.ident(&element.name), "for");
}

const FOR_STATEMENT_WITHOUT_A_BODY: &str = "component A { div { on: { for x in xs } } }";

#[test]
fn a_for_statement_without_a_block_reports_and_marks_it() {
    // `Recovered::Missing`, not an empty block: the body was never opened.
    let p = parse_err(FOR_STATEMENT_WITHOUT_A_BODY);
    let ast::Stmt::For(node) = &p.first_closure().body.stmts[0] else {
        panic!("expected a `for` statement")
    };
    let ast::ForBody::Statements(block) = &node.body else {
        panic!("expected a statement body")
    };
    assert!(block.is_missing());
}

const FOR_BODY_FORBIDS_A_TAIL: &str = "component A { div { on: { for x in xs { g(x) } } } }";

#[test]
fn a_for_body_forbids_a_trailing_expression() {
    // A `for` produces nothing, so its body is `statement*` — the same rule an
    // `if`-statement branch follows. The expression is reported *and* kept in
    // `tail`, because dropping the subtree the user wrote is what S5 forbids.
    let p = parse_err(FOR_BODY_FORBIDS_A_TAIL);
    let ast::Stmt::For(node) = &p.first_closure().body.stmts[0] else {
        panic!("expected a `for` statement")
    };
    let block = for_statements(node);
    assert!(block.stmts.is_empty());
    assert!(
        block.tail.is_some(),
        "the expression is reported, not discarded"
    );
}

// ---------------------------------------------------------------------------
// The walk
// ---------------------------------------------------------------------------

/// Every name the visitor reaches, however it is spelled: an `ExprKind::Ident`
/// carries a bare `Name` and never becomes an `Ident` node, while a call's
/// callee does, so both routes are recorded.
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

const WALKED: &str = "global G {\n\
     f: func() -> s32 { reached-function-body-tail }\n\
     }\n\
     component A {\n\
     div { on: { reached-closure-statement(); reached-closure-tail } }\n\
     div { on: { if c { reached-if-branch(); } } }\n\
     div { on: { for y in ys { reached-for-statement-body(); } } }\n\
     for z in zs { \"x\" }\n\
     }\n";

/// # This is the check the compiler cannot make for you
///
/// `visit.rs`'s exhaustive matches turn a new AST **variant** into a compile
/// error. A new **field** is not: `Block::tail` and `FunctionDecl::body` were
/// wired into the walk by hand, and forgetting either would silently skip a
/// subtree with everything still compiling — the same gap `attributes` hit and
/// `plans/rewrite/seam-changes.md` records.
///
/// So the walk is asserted rather than trusted. Each name below is reachable
/// through exactly one of the wired lines.
#[test]
fn the_walker_reaches_every_block_and_every_tail() {
    let p = parse_ok(WALKED);
    let mut seen = NamesSeen::default();
    seen.visit_file(&p.file);
    let names: Vec<String> = seen.0.iter().map(|name| p.text(*name)).collect();

    for expected in [
        // FunctionDecl::body, then Block::tail inside it
        "reached-function-body-tail",
        // ClosureExpr::body -> Block::stmts
        "reached-closure-statement",
        // ClosureExpr::body -> Block::tail
        "reached-closure-tail",
        // IfStmt branches, which are Blocks now
        "reached-if-branch",
        // Stmt::For -> ForBody::Statements -> Block::stmts
        "reached-for-statement-body",
        // UiNode::For -> ForBody::Nodes, the position that already worked
        "zs",
    ] {
        assert!(
            names.iter().any(|name| name == expected),
            "the walk never reached `{expected}`; it saw {names:?}"
        );
    }
}
