//! `@name` / `@name(key = value)` on declarations — `plans/rewrite/scope.md`
//! § *attributes on items, and `unsafe`* (2026-07-29).
//!
//! # Why these are node-shape assertions and not accept/reject checks
//!
//! `@` was already taken: `@children` is a UI node, it sits in `NODE_FIRST`, and
//! in a component body it occupies the same position an attributed member does.
//! Getting that tiebreak wrong is a **silent misparse** — the file still
//! round-trips byte-for-byte, so invariant S1 is satisfied, `parity.rs`'s one
//! accept/reject bit does not move, and nothing fails. That is exactly the shape
//! `func<T>` fell into (`plans/rewrite/seam-changes.md`, 2026-07-29), and
//! `tests/generics.rs` is its regression cover. This file is the same thing for
//! attributes: every case asserts **which construct** the parser built.
//!
//! The frozen parser accepts none of this, so there is no oracle — see
//! `scope.md` § *The freeze now carries four breaks*. What holds these honest is
//! that each assertion names a tree, and that the mutation log in the stage
//! report records the deliberate breakage each one was seen to catch.

mod support;

use yelc_base::{Diagnostics, Interner, SourceId};
use yelc_syntax::ast;
use yelc_syntax::ast::visit::ErrorNodeCounter;

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

struct Parsed {
    interner: Interner,
    file: ast::File,
    diagnostics: usize,
    error_nodes: usize,
}

/// Parse, and assert invariants S1 and S2 on the way through.
fn parse(source: &str) -> Parsed {
    let interner = Interner::new();
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

    /// The names of the attributes in `list`, in source order.
    fn attribute_names(&self, list: &Option<ast::AttributeList>) -> Vec<String> {
        list.as_ref()
            .expect("an attribute list")
            .present()
            .map(|attribute| self.ident(&attribute.name))
            .collect()
    }

    /// `(key, rendered value)` for one attribute's arguments.
    fn args(&self, attribute: &ast::Attribute) -> Vec<(String, String)> {
        attribute
            .present_args()
            .map(|arg| {
                let value = match &arg.value.kind {
                    ast::ExprKind::String(text) => format!("{:?}", self.text(*text)),
                    ast::ExprKind::Int(value) => value.to_string(),
                    ast::ExprKind::Bool(value) => value.to_string(),
                    ast::ExprKind::Ident(name) => self.text(*name),
                    other => format!("{other:?}"),
                };
                (self.ident(&arg.name), value)
            })
            .collect()
    }
}

/// Every source this file parses, so the S1 sweep below covers all of them
/// rather than the handful whose test happens to be looked at.
const EVERY_SOURCE: &[&str] = &[
    BARE_ON_A_MEMBER,
    BARE_ON_AN_ITEM,
    WITH_ARGS,
    SEVERAL,
    UNKNOWN,
    CHILDREN_EVERYWHERE,
    ATTRIBUTE_BESIDE_CHILDREN,
    BARE_AT,
    NO_DECLARATION,
    ON_A_UI_NODE,
    POSITIONAL_ARG,
    ARG_WITHOUT_A_VALUE,
    ARG_WITHOUT_EQUALS,
    UNCLOSED_ARGS,
    ON_A_GLOBAL_CALLBACK,
    ON_A_GLOBAL_PROPERTY,
    SPACED_AT_CHILDREN,
    EVERY_OWNER,
];

// ---------------------------------------------------------------------------
// The shapes that must parse
// ---------------------------------------------------------------------------

const BARE_ON_A_MEMBER: &str =
    "package a:b@0.1.0;\ncomponent App {\n  @unsafe\n  raw: s32 = 0;\n  Text { \"x\" }\n}\n";

#[test]
fn a_bare_attribute_attaches_to_the_member_that_follows_it() {
    let p = parse_ok(BARE_ON_A_MEMBER);
    let component = p.component(0);
    // The member is a *property*, not a UI node and not a recovery hole.
    let property = component.properties().next().expect("a property");
    assert_eq!(p.attribute_names(&property.attributes), ["unsafe"]);
    assert_eq!(p.ident(&property.name), "raw");
    // …and the attribute did not eat the node that follows.
    assert_eq!(component.body().count(), 1);
    assert_eq!(component.members.len(), 2);
}

const BARE_ON_AN_ITEM: &str = "package a:b@0.1.0;\n@unsafe\nrecord R { a: s32 }\n@unsafe\nexport component App { Text { \"x\" } }\n";

#[test]
fn a_bare_attribute_attaches_to_the_top_level_item_that_follows_it() {
    let p = parse_ok(BARE_ON_AN_ITEM);
    let ast::ItemKind::Record(record) = &p.file.items[1] else {
        panic!("expected a record declaration, got {:?}", p.file.items[1]);
    };
    assert_eq!(p.attribute_names(&record.attributes), ["unsafe"]);
    assert_eq!(p.ident(&record.name), "R");

    let component = p.component(0);
    assert_eq!(p.attribute_names(&component.attributes), ["unsafe"]);
    // The `export` modifier still belongs to the component, not to the attribute.
    assert!(component.is_export);
}

const WITH_ARGS: &str = "package a:b@0.1.0;\nexport global S {\n  @primitive(op = \"@wasm.ref_array_any_get\")\n  array-any-get: func(a: s32, i: s32) -> s32;\n}\ncomponent App { Text { \"x\" } }\n";

#[test]
fn attribute_arguments_are_named_pairs() {
    let p = parse_ok(WITH_ARGS);
    let callback = p.global(0).callbacks().next().expect("a callback");
    let list = callback.attributes.as_ref().expect("an attribute list");
    let attribute = list.present().next().expect("an attribute");
    assert_eq!(p.ident(&attribute.name), "primitive");
    assert_eq!(
        p.args(attribute),
        [("op".to_string(), "\"@wasm.ref_array_any_get\"".to_string())],
        "the argument must be read as `key = value`, not as a positional string"
    );
}

const SEVERAL: &str = "package a:b@0.1.0;\nexport global S {\n  @unsafe\n  @primitive(op = \"@wasm.ref_array_any_get\", arity = 2)\n  array-any-get: func(a: s32, i: s32) -> s32;\n}\ncomponent App { Text { \"x\" } }\n";

#[test]
fn several_attributes_attach_to_one_declaration_in_source_order() {
    let p = parse_ok(SEVERAL);
    let callback = p.global(0).callbacks().next().expect("a callback");
    assert_eq!(
        p.attribute_names(&callback.attributes),
        ["unsafe", "primitive"]
    );
    let list = callback.attributes.as_ref().unwrap();
    let primitive = list.present().nth(1).unwrap();
    assert_eq!(
        p.args(primitive),
        [
            ("op".to_string(), "\"@wasm.ref_array_any_get\"".to_string()),
            ("arity".to_string(), "2".to_string()),
        ]
    );
    // The bare attribute has no argument list; empty and absent are the same
    // thing, so this is a `Vec` and not a `Recovered`.
    assert_eq!(list.present().next().unwrap().args.len(), 0);
}

const ON_A_GLOBAL_CALLBACK: &str = "package a:b@0.1.0;\nglobal S { @unsafe callback c(a: s32); }\ncomponent App { Text { \"x\" } }\n";

const ON_A_GLOBAL_PROPERTY: &str = "package a:b@0.1.0;\nglobal S { @unsafe in-out count: s32; }\ncomponent App { Text { \"x\" } }\n";

#[test]
fn attributes_reach_both_global_member_forms() {
    let p = parse_ok(ON_A_GLOBAL_CALLBACK);
    let callback = p.global(0).callbacks().next().expect("a callback");
    assert_eq!(p.attribute_names(&callback.attributes), ["unsafe"]);
    assert_eq!(p.ident(&callback.name), "c");

    let p = parse_ok(ON_A_GLOBAL_PROPERTY);
    let property = p.global(0).properties().next().expect("a property");
    assert_eq!(p.attribute_names(&property.attributes), ["unsafe"]);
    // The possessive `property_direction?` still ran, on the right token.
    assert_eq!(property.direction, Some(ast::PropertyDirection::InOut));
    assert_eq!(p.ident(&property.name), "count");
}

// ---------------------------------------------------------------------------
// `@children` — unchanged, everywhere it was legal before
// ---------------------------------------------------------------------------

const CHILDREN_EVERYWHERE: &str = "package a:b@0.1.0;\nextern component Dialog { title: string; @children }\nexport component App {\n  show: bool = true;\n  @children\n  VStack { @children }\n  if show { @children }\n  for i in 0..2 { @children }\n}\n";

#[test]
fn children_still_parses_as_a_ui_node_everywhere_it_did_before() {
    let p = parse_ok(CHILDREN_EVERYWHERE);

    let ast::ItemKind::ExternComponent(extern_component) = &p.file.items[1] else {
        panic!("expected an extern component");
    };
    assert!(
        extern_component.children_slot().is_some(),
        "an `extern component` body must still declare a `@children` slot"
    );

    let component = p.component(0);
    // 1. directly as a component member
    let mut body = component.body();
    assert!(
        matches!(body.next(), Some(ast::UiNode::Children { .. })),
        "`@children` as a component member must still be a node, not an attribute"
    );
    // 2. inside an element body
    let Some(ast::UiNode::Element(stack)) = body.next() else {
        panic!("expected an element")
    };
    assert!(matches!(stack.children[0], ast::UiNode::Children { .. }));
    // 3. inside an `if` body
    let Some(ast::UiNode::If(if_node)) = body.next() else {
        panic!("expected an if node")
    };
    assert!(matches!(
        if_node.then_branch.present().unwrap()[0],
        ast::UiNode::Children { .. }
    ));
    // 4. inside a `for` body
    let Some(ast::UiNode::For(for_node)) = body.next() else {
        panic!("expected a for node")
    };
    assert!(matches!(
        for_node.body.present().unwrap()[0],
        ast::UiNode::Children { .. }
    ));
    assert!(body.next().is_none());

    // …and no member of that component acquired an attribute list on the way.
    assert!(
        component
            .properties()
            .all(|property| property.attributes.is_none())
    );
}

const ATTRIBUTE_BESIDE_CHILDREN: &str = "package a:b@0.1.0;\n@unsafe\nexport component Panel {\n  @unsafe\n  raw: s32 = 0;\n  @children\n}\n";

#[test]
fn an_attributed_item_can_still_contain_a_children_node() {
    let p = parse_ok(ATTRIBUTE_BESIDE_CHILDREN);
    let component = p.component(0);
    assert_eq!(p.attribute_names(&component.attributes), ["unsafe"]);

    let property = component.properties().next().expect("a property");
    assert_eq!(p.attribute_names(&property.attributes), ["unsafe"]);

    let mut body = component.body();
    assert!(matches!(body.next(), Some(ast::UiNode::Children { .. })));
    assert!(body.next().is_none());
    // Three members: the attributed property, and the `@children` node. The
    // attribute did not become a member of its own, and `@children` did not
    // become an attribute on something.
    assert_eq!(component.members.len(), 2);
}

/// `children_node` is one atomic string literal in the frozen grammar, so
/// `@ children` was never the marker. It is an attribute now — an unknown one,
/// so it is still rejected, which is what keeps `parity.rs` unmoved.
const SPACED_AT_CHILDREN: &str = "package a:b@0.1.0;\ncomponent A { @ children }\n";

#[test]
fn a_spaced_at_children_is_not_the_marker() {
    let p = parse_err(SPACED_AT_CHILDREN);
    assert_eq!(
        p.component(0).body().count(),
        0,
        "`@ children` must not become a `Children` node"
    );
}

// ---------------------------------------------------------------------------
// Errors — a diagnostic AND a recovery node, per invariant S5
// ---------------------------------------------------------------------------

const UNKNOWN: &str =
    "package a:b@0.1.0;\ncomponent App {\n  @unsfae\n  raw: s32 = 0;\n  Text { \"x\" }\n}\n";

#[test]
fn an_unknown_attribute_is_an_error_and_is_not_dropped() {
    let p = parse_err(UNKNOWN);
    // Reported…
    assert_eq!(p.diagnostics, 1);
    // …and still present in the tree, attached to the declaration it was
    // written on. A dropped attribute is the `_ => {}` shape with a friendlier
    // face: working code with the gate the user thought they applied absent.
    let property = p.component(0).properties().next().expect("a property");
    assert_eq!(p.attribute_names(&property.attributes), ["unsfae"]);
}

const BARE_AT: &str = "package a:b@0.1.0;\ncomponent A { @ }\n";

#[test]
fn an_at_with_no_name_is_a_hole_in_the_attribute_list() {
    // The `Recovered::Missing` arm of `AttributeList::attributes` is reachable;
    // a list whose hole arm cannot occur is shape-only (anti-spec A9).
    parse_err(BARE_AT);
}

const NO_DECLARATION: &str = "package a:b@0.1.0;\ncomponent A { @unsafe }\n";

#[test]
fn an_attribute_with_no_declaration_after_it_is_reported() {
    let p = parse_err(NO_DECLARATION);
    let component = p.component(0);
    assert!(
        matches!(component.members[0], ast::ComponentMember::Error { .. }),
        "the orphaned attribute must leave a recovery member"
    );
    // The recovery span covers the attribute text, so it is still attributable.
    let ast::ComponentMember::Error { span, .. } = component.members[0] else {
        unreachable!()
    };
    assert_eq!(
        &NO_DECLARATION[span.start..span.start + "@unsafe".len()],
        "@unsafe"
    );
}

const ON_A_UI_NODE: &str = "package a:b@0.1.0;\ncomponent A { @unsafe VStack { \"x\" } }\n";

#[test]
fn an_attribute_cannot_be_written_on_a_ui_node() {
    // An attribute precedes a *declaration*. `ElementNode` has nowhere to hold
    // one, so attaching it silently would be a dropped subtree.
    let p = parse_err(ON_A_UI_NODE);
    let component = p.component(0);
    assert!(
        matches!(component.members[0], ast::ComponentMember::Error { .. }),
        "the attribute must be reported as a recovery member"
    );
    // …and the node itself is still read as a node, exactly once. The orphaned
    // attribute consumes nothing, so recovery does not eat the `VStack` and
    // then re-report its `{` and its `"x"` as two further broken members.
    assert_eq!(component.members.len(), 2);
    let Some(ast::UiNode::Element(element)) = component.body().next() else {
        panic!("expected the element to survive")
    };
    assert_eq!(p.ident(&element.name), "VStack");
}

const POSITIONAL_ARG: &str = "package a:b@0.1.0;\ncomponent A { @primitive(\"op\") x: s32 = 0; }\n";

#[test]
fn a_positional_argument_is_rejected() {
    // WIT's own gates are `@since(version = …)` / `@unstable(feature = …)`, so
    // arguments are named pairs and a bare value is not an argument at all.
    parse_err(POSITIONAL_ARG);
}

const ARG_WITHOUT_A_VALUE: &str =
    "package a:b@0.1.0;\ncomponent A { @primitive(op) x: s32 = 0; }\n";

/// A key and a value with no `=` between them.
///
/// This is the case that separates `expect(EQ)` from `eat(EQ)`: both of the two
/// rows above are rejected either way — one by the name check, the other by the
/// expression parser running out of input — so neither of them pins the `=` as
/// **mandatory**. This one does.
const ARG_WITHOUT_EQUALS: &str =
    "package a:b@0.1.0;\ncomponent A { @primitive(op 1) x: s32 = 0; }\n";

const UNCLOSED_ARGS: &str = "package a:b@0.1.0;\ncomponent A { @primitive(op = 1 x: s32 = 0; }\n";

#[test]
fn a_malformed_argument_list_reports_and_recovers() {
    parse_err(ARG_WITHOUT_A_VALUE);
    parse_err(ARG_WITHOUT_EQUALS);
    parse_err(UNCLOSED_ARGS);
}

// ---------------------------------------------------------------------------
// The walker
// ---------------------------------------------------------------------------

/// One attribute on **every** declaration that can carry one.
///
/// Eleven attribute lists across the ten struct types that own the field:
/// `FunctionDecl` appears twice, once as a `global` callback and once as a
/// component's exported function, because those are two different dispatch
/// paths into the same struct.
const EVERY_OWNER: &str = "package a:b@0.1.0;\n\
@unsafe record R { a: s32 }\n\
@unsafe enum E { c }\n\
@unsafe variant V { c(s32) }\n\
@unsafe element El { a: s32; }\n\
@unsafe extern component C { a: string; }\n\
@unsafe global S { @unsafe in x: s32; @unsafe callback c(a: s32); }\n\
@unsafe export component App { @unsafe raw: s32 = 0; @unsafe export f: func(a: s32); Text { \"x\" } }\n";

/// Every `attributes` field is reached by [`ast::visit`].
///
/// # Why this test exists at all
///
/// `walk_*` is exhaustive with no `_` arm, so a new AST **variant** is a compile
/// error in exactly one file (anti-spec A3). A new **field** is not. Ten
/// `walk_attributes(v, &node.attributes)` lines had to be written by hand, and
/// omitting any one of them would silently skip every attribute on that
/// declaration — with the crate still compiling, every other test in this file
/// still passing (they read the fields directly), and `ErrorNodeCounter` still
/// reporting the same numbers. The gap is documented in
/// `plans/rewrite/seam-changes.md`; this is the assertion that covers it here.
#[test]
fn the_walker_reaches_the_attributes_on_every_declaration_that_can_carry_them() {
    use yelc_syntax::ast::visit::{self, Visitor};

    #[derive(Default)]
    struct Count {
        lists: usize,
        attributes: usize,
        args: usize,
    }
    impl Visitor for Count {
        fn visit_attribute_list(&mut self, node: &ast::AttributeList) {
            self.lists += 1;
            visit::walk_attribute_list(self, node);
        }
        fn visit_attribute(&mut self, node: &ast::Attribute) {
            self.attributes += 1;
            visit::walk_attribute(self, node);
        }
        fn visit_attribute_arg(&mut self, node: &ast::AttributeArg) {
            self.args += 1;
            visit::walk_attribute_arg(self, node);
        }
    }

    let p = parse_ok(EVERY_OWNER);
    let mut count = Count::default();
    count.visit_file(&p.file);
    assert_eq!(
        count.lists, 11,
        "one of the ten `walk_attributes` call sites is missing"
    );
    assert_eq!(count.attributes, 11);

    // …and the argument arm is reached too, which the source above deliberately
    // does not exercise, so it gets its own input rather than a zero that would
    // pass whether or not `walk_attribute` descends.
    let p = parse_ok(SEVERAL);
    let mut count = Count::default();
    count.visit_file(&p.file);
    assert_eq!(
        count.args, 2,
        "`walk_attribute` does not reach its arguments"
    );
}

// ---------------------------------------------------------------------------
// Invariants over every source above
// ---------------------------------------------------------------------------

#[test]
fn every_source_in_this_file_round_trips() {
    // Exact, so a source cannot be added to the file and left out of the sweep.
    assert_eq!(EVERY_SOURCE.len(), 18);
    for source in EVERY_SOURCE {
        // `parse` asserts S1 and S2 on the way through.
        let _ = parse(source);
    }
}

/// Truncating an attributed program at every byte must never panic, must always
/// return, and must never report without leaving a node behind (S5/S6).
///
/// The generator ships, not its instances (anti-spec A13): every prefix, not a
/// hand-picked few.
#[test]
fn every_truncation_of_every_source_terminates_and_marks() {
    let mut checked = 0usize;
    for source in EVERY_SOURCE {
        for cut in 0..=source.len() {
            if !source.is_char_boundary(cut) {
                continue;
            }
            let prefix = &source[..cut];
            let parsed = parse(prefix);
            checked += 1;
            if parsed.diagnostics > 0 {
                assert!(
                    parsed.error_nodes > 0,
                    "S5: {prefix:?} produced {} diagnostic(s) and no recovery node",
                    parsed.diagnostics
                );
            }
        }
    }
    assert!(
        checked > 1_000,
        "the truncation sweep shrank to {checked} inputs"
    );
}

/// Inserting an `@` at every position of a real program must not panic and must
/// not silently produce a `@children` node where none was written.
#[test]
fn an_at_inserted_anywhere_never_invents_a_children_node() {
    let base = support::read(&support::positive_fixtures()[0]);
    let children_before = children_nodes(&base);
    let mut checked = 0usize;
    for at in 0..=base.len() {
        if !base.is_char_boundary(at) {
            continue;
        }
        let mut mutated = String::with_capacity(base.len() + 1);
        mutated.push_str(&base[..at]);
        mutated.push('@');
        mutated.push_str(&base[at..]);
        assert!(
            children_nodes(&mutated) <= children_before,
            "inserting `@` at {at} created a `@children` node"
        );
        checked += 1;
    }
    assert!(checked > 100, "the sweep shrank to {checked} inputs");
}

/// How many `UiNode::Children` the file holds, at any depth.
fn children_nodes(source: &str) -> usize {
    use yelc_syntax::ast::visit::{self, Visitor};

    #[derive(Default)]
    struct Count(usize);
    impl Visitor for Count {
        fn visit_ui_node(&mut self, node: &ast::UiNode) {
            if matches!(node, ast::UiNode::Children { .. }) {
                self.0 += 1;
            }
            visit::walk_ui_node(self, node);
        }
    }

    let interner = Interner::new();
    let mut diags = Diagnostics::new();
    let parsed = yelc_syntax::parse(SourceId(0), source, &interner, &mut diags);
    let mut count = Count::default();
    count.visit_file(&parsed.ast);
    count.0
}
