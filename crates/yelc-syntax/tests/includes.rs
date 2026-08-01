//! `from "list" include List;` — the module import statement (user-approved
//! surface addition, 2026-07-31; refines `plans/modules.md` §4.1 with a
//! plain-name string locator, `std:` reserved for compiler-shipped modules).
//!
//! **Purely additive**, like `<T>` and attributes and unlike `return`: the
//! frozen grammar has no `include` production and `include` occurs in none of
//! the checked-in `.yel` files, so every text below was a syntax error on both
//! parsers before this change and there is no accept/reject boundary to
//! enumerate against the oracle. Node-shape assertions are the whole test, plus
//! S1/S5 through the shared harness.

use yelc_base::{Diagnostics, NameInterner, SourceId};
use yelc_syntax::ast;
use yelc_syntax::ast::visit::ErrorNodeCounter;

struct Parsed {
    interner: NameInterner,
    file: ast::File,
    diagnostics: usize,
    error_nodes: usize,
}

fn parse(source: &str) -> Parsed {
    let interner = NameInterner::new();
    let mut diags = Diagnostics::new();
    let parsed = yelc_syntax::parse(SourceId(0), source, &interner, &mut diags);
    assert_eq!(
        parsed.green.text(),
        source,
        "S1: the green tree must reconstruct the source byte-for-byte"
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

fn the_include(parsed: &Parsed) -> &ast::IncludeDecl {
    let mut includes = parsed.file.items.iter().filter_map(|item| match item {
        ast::ItemKind::Include(decl) => Some(decl),
        _ => None,
    });
    let decl = includes.next().expect("an include was parsed");
    assert!(includes.next().is_none(), "expected exactly one include");
    decl
}

#[test]
fn a_bare_specifier_includes_a_module() {
    let parsed = parse_ok("package a:b;\nfrom \"list\" include List;\n");
    let decl = the_include(&parsed);
    assert_eq!(
        &*parsed.interner.str(decl.specifier.expect("specifier")),
        "list"
    );
    let name = decl.name.present().expect("name present");
    assert_eq!(&*parsed.interner.str(name.name), "List");
}

/// `std:` lives *inside* the string — the grammar carries no opinion about
/// it; distinguishing it is the resolver's job.
#[test]
fn a_std_specifier_is_just_a_string_to_the_grammar() {
    let parsed = parse_ok("package a:b;\nfrom \"std:list\" include List;\n");
    let decl = the_include(&parsed);
    assert_eq!(
        &*parsed.interner.str(decl.specifier.expect("specifier")),
        "std:list"
    );
}

/// `from` and `include` are contextual: legal names everywhere a name is.
#[test]
fn from_and_include_stay_legal_names() {
    parse_ok(
        "package a:b;\ncomponent App { from: s32 = 1; include: s32 = 2; Text { text: \"{from} {include}\" } }\n",
    );
    parse_ok("package a:b;\nrecord R { from: s32, include: s32 }\n");
}

/// A hole where the specifier should be: reported, recovered, the bound name
/// still read (S5 — a diagnostic AND recovery, never a silent drop).
#[test]
fn a_missing_specifier_is_reported_and_recovered() {
    let parsed = parse("package a:b;\nfrom include List;\nrecord R { x: s32 }\n");
    assert!(parsed.diagnostics > 0, "the hole is reported");
    let decl = the_include(&parsed);
    assert!(decl.specifier.is_none());
    let name = decl.name.present().expect("the written name survives");
    assert_eq!(&*parsed.interner.str(name.name), "List");
    assert!(
        parsed
            .file
            .items
            .iter()
            .any(|item| matches!(item, ast::ItemKind::Record(_))),
        "recovery resumes at the next item"
    );
}

/// An interpolated specifier is a computed locator, which this declaration
/// deliberately is not.
#[test]
fn a_template_specifier_is_rejected() {
    let parsed = parse("package a:b;\nfrom \"std:{x}\" include List;\n");
    assert!(parsed.diagnostics > 0);
    let decl = the_include(&parsed);
    assert!(decl.specifier.is_none());
}

#[test]
fn a_missing_bound_name_is_reported() {
    let parsed = parse("package a:b;\nfrom \"list\" include ;\n");
    assert!(parsed.diagnostics > 0);
    let decl = the_include(&parsed);
    assert!(decl.name.is_missing());
    assert_eq!(
        &*parsed.interner.str(decl.specifier.expect("specifier kept")),
        "list"
    );
}

#[test]
fn a_missing_semicolon_is_reported() {
    let parsed = parse("package a:b;\nfrom \"list\" include List\nrecord R { x: s32 }\n");
    assert!(parsed.diagnostics > 0);
    the_include(&parsed);
}
