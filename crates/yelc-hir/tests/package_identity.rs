//! The rule that a directory of files is one package.
//!
//! `plans/modules.md` — "Decided 2026-07-30 — every file declares, and
//! disagreement is an error".
//!
//! Every test here asserts on the **error code**, never on message text. The
//! codes are the stable surface; the prose is not, and a test that pins prose
//! is a test that gets weakened the first time the wording improves.

use yelc_base::ErrorCode;
use yelc_hir::{PackageIdentity, check_package_identity};
use yelc_sema::{CompilerContext, PackageId};
use yelc_syntax::ParsedFile;

/// Parse `sources` in order into a fresh context, then run the check.
///
/// Order is the argument order, standing in for the driver's sorted file list —
/// which file is "first" is what the mismatch diagnostic is written against.
fn check(sources: &[&str]) -> (Option<PackageIdentity>, Vec<ErrorCode>, CompilerContext) {
    let mut ctx = CompilerContext::with_builtins(PackageId::LOCAL);
    let mut parsed: Vec<ParsedFile> = Vec::new();
    for content in sources {
        let source = ctx.sources.add_inline(*content);
        parsed.push(yelc_syntax::parse(
            source,
            content,
            &ctx.names,
            &mut ctx.diagnostics,
        ));
    }
    // Parse errors would pollute the code list, so this fixture asserts none
    // were produced unless a test opts in by expecting SyntaxError.
    let identity = check_package_identity(&parsed, &mut ctx);
    let codes = ctx.diagnostics.iter().filter_map(|d| d.code).collect();
    (identity, codes, ctx)
}

const A: &str = "package my:app@0.1.0;\n\nglobal Alpha {\n}\n";

#[test]
fn agreeing_files_are_one_package() {
    let (identity, codes, ctx) = check(&[A, "package my:app@0.1.0;\n\nglobal Beta {\n}\n"]);
    assert_eq!(codes, vec![]);
    let identity = identity.expect("two agreeing files establish an identity");
    assert_eq!(&*ctx.names.str(identity.namespace), "my");
    assert_eq!(&*ctx.names.str(identity.name), "app");
    assert_eq!(identity.version.map(|v| ctx.names.str(v).to_string()).as_deref(), Some("0.1.0"));
}

#[test]
fn a_differing_name_is_rejected() {
    let (_, codes, _) = check(&[A, "package other:thing@0.1.0;\n"]);
    assert_eq!(codes, vec![ErrorCode::PackageNameMismatch]);
}

/// The version is part of the identifier — `wit-parser` compares the whole name
/// too. When the version is removed from the language this test is the one that
/// should be deleted, deliberately, rather than quietly starting to pass.
#[test]
fn a_differing_version_is_rejected() {
    let (_, codes, _) = check(&[A, "package my:app@0.2.0;\n"]);
    assert_eq!(codes, vec![ErrorCode::PackageNameMismatch]);
}

/// A version on one file and none on the other is still a disagreement: absent
/// does not mean "matches anything".
#[test]
fn a_missing_version_does_not_match_a_present_one() {
    let (_, codes, _) = check(&[A, "package my:app;\n"]);
    assert_eq!(codes, vec![ErrorCode::PackageNameMismatch]);
}

/// Three files naming three packages is three disagreements, not one. The
/// check accumulates rather than stopping at the first
/// (`plans/rewrite/keep-list.md`).
#[test]
fn every_mismatching_file_is_reported() {
    let (_, codes, _) = check(&[A, "package my:app@0.2.0;\n", "package other:thing;\n"]);
    assert_eq!(
        codes,
        vec![
            ErrorCode::PackageNameMismatch,
            ErrorCode::PackageNameMismatch
        ]
    );
}

#[test]
fn a_file_with_no_clause_is_rejected() {
    let (identity, codes, _) = check(&[A, "global Beta {\n}\n"]);
    assert_eq!(codes, vec![ErrorCode::MissingPackageDecl]);
    // The package still has an identity — one bad file does not erase the
    // others', which is what lets lowering continue and report more.
    assert!(identity.is_some());
}

/// Absence is reported per file and needs no sibling to point at, so a one-file
/// package with no clause is just as wrong as a five-file one.
#[test]
fn a_lone_file_with_no_clause_is_rejected() {
    let (identity, codes, _) = check(&["global Alpha {\n}\n"]);
    assert_eq!(codes, vec![ErrorCode::MissingPackageDecl]);
    assert_eq!(identity, None);
}

#[test]
fn every_file_with_no_clause_is_reported() {
    let (identity, codes, _) = check(&["global Alpha {\n}\n", "global Beta {\n}\n"]);
    assert_eq!(
        codes,
        vec![
            ErrorCode::MissingPackageDecl,
            ErrorCode::MissingPackageDecl
        ]
    );
    assert_eq!(identity, None);
}

/// A hole in the identifier is already a syntax error. This check must not turn
/// one typo into two errors, so it neither establishes an identity from the
/// broken file nor reports it as a mismatch.
#[test]
fn a_malformed_clause_is_not_reported_twice() {
    let (identity, codes, _) = check(&[A, "package my:;\n"]);
    assert_eq!(
        codes,
        vec![ErrorCode::SyntaxError],
        "the parser's error, and only the parser's"
    );
    assert!(identity.is_some(), "the good file still names the package");
}

/// The first *usable* clause establishes the identity, so a broken first file
/// does not make every later file mismatch against a hole.
#[test]
fn a_malformed_first_file_does_not_poison_the_rest() {
    let (identity, codes, ctx) = check(&["package my:;\n", A, "package my:app@0.1.0;\n"]);
    assert_eq!(codes, vec![ErrorCode::SyntaxError]);
    let identity = identity.expect("the two good files agree");
    assert_eq!(&*ctx.names.str(identity.name), "app");
}
