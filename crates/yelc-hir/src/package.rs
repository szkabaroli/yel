//! Package identity: the check that a directory of files is **one** package.
//!
//! `plans/modules.md` — "Decided 2026-07-30 — every file declares, and
//! disagreement is an error". Go's rule: every file in a package directory
//! carries the `package` clause, and all of them must be identical.
//!
//! # Why this lives in `yelc-hir` and not lower
//!
//! It is a rule about a **set** of files, and [`yelc_syntax`] is per-file — a
//! parser that could enforce this would be a parser holding state across
//! parses. It cannot live in [`yelc_sema`] either, which sits below
//! [`yelc_syntax`] and so cannot name [`ast::PackageDecl`]. This crate is the
//! first one that can see a `&[ParsedFile]` and a `Diagnostics` at once, which
//! makes it the first one that can state the rule at all.
//!
//! It still runs **before** [`crate::lower_files`], not inside it: it reads the
//! `package` clause of each file and nothing else, needs no symbol table, and
//! reports without one.

use yelc_base::{Diagnostic, ErrorCode, Name, Span};
use yelc_sema::CompilerContext;
use yelc_syntax::{ParsedFile, ast};

/// What a `package ns:name@version;` clause names.
///
/// **Compared whole**, version included — `wit-parser` does the same
/// (`ast/resolve.rs`, `cur_name != *prev`), and a directory holding two
/// versions is a directory that is two packages.
///
/// The version is [slated for removal](../../../plans/modules.md); nothing here
/// treats it as load-bearing, and dropping it degrades this to comparing the
/// bare name with no other change.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PackageIdentity {
    pub namespace: Name,
    pub name: Name,
    pub version: Option<Name>,
}

/// Establish the package identity shared by every file, reporting each file
/// that breaks the rule.
///
/// Returns the identity the package settled on, or `None` when no file declared
/// a usable one. A `None` return is **not** "no errors" — it means every file
/// was missing or malformed, and each was reported individually.
///
/// Both failures accumulate rather than early-returning: a directory where
/// three files each name a different package should say so three times, not
/// once (`plans/rewrite/keep-list.md`, accumulate-and-continue).
pub fn check_package_identity(
    parsed: &[ParsedFile],
    ctx: &mut CompilerContext,
) -> Option<PackageIdentity> {
    // The first file to declare a usable identity establishes it; every later
    // file is compared against that one. Source order is the driver's — sorted
    // by file name — so which file is "first" is stable across runs
    // (`plans/rewrite/anti-spec.md` A6).
    let mut established: Option<(PackageIdentity, Span)> = None;

    for file in parsed {
        let Some(decl) = package_decl(&file.ast) else {
            ctx.diagnostics.push(
                Diagnostic::error("this file has no `package` declaration")
                    .with_span(file.ast.span)
                    .with_code(ErrorCode::MissingPackageDecl)
                    .with_note(
                        "every file in a package directory must declare it; the package is \
                         inferred neither from a sibling file nor from the directory name",
                    ),
            );
            continue;
        };

        // A hole in the identifier is already a reported syntax error. Such a
        // file neither establishes an identity nor mismatches one — reporting
        // it again here would turn one typo into two errors.
        let (Some(namespace), Some(name)) = (decl.namespace.present(), decl.name.present()) else {
            continue;
        };

        let identity = PackageIdentity {
            namespace: namespace.name,
            name: name.name,
            version: decl.version,
        };

        match established {
            None => established = Some((identity, decl.span)),
            Some((first, _)) if first == identity => {}
            Some((first, first_span)) => {
                // Built before the mutable borrow, and naming the *other* file:
                // the error is a disagreement, so a message that names one side
                // asks the reader to go find the other.
                let message = format!(
                    "package `{}` does not match `{}` declared earlier in this directory",
                    render(&identity, ctx),
                    render(&first, ctx),
                );
                let note = match ctx.sources.get(first_span.source) {
                    Some(source) => format!("`{}` is declared in {}", render(&first, ctx), source.name()),
                    None => format!("`{}` is declared in another file", render(&first, ctx)),
                };
                ctx.diagnostics.push(
                    Diagnostic::error(message)
                        .with_span(decl.span)
                        .with_code(ErrorCode::PackageNameMismatch)
                        .with_note(note),
                );
            }
        }
    }

    established.map(|(identity, _)| identity)
}

/// The file's `package` clause, if it has one.
///
/// Scans all items rather than checking only the first: the parser permits the
/// clause only in first position and reports a later one itself, so a second
/// clause reaching here is already-diagnosed input that should not also crash
/// or silently change which identity wins.
fn package_decl(file: &ast::File) -> Option<&ast::PackageDecl> {
    file.items.iter().find_map(|item| match item {
        ast::ItemKind::Package(decl) => Some(decl),
        _ => None,
    })
}

fn render(identity: &PackageIdentity, ctx: &CompilerContext) -> String {
    let mut out = format!(
        "{}:{}",
        &*ctx.names.str(identity.namespace),
        &*ctx.names.str(identity.name)
    );
    if let Some(version) = identity.version {
        out.push('@');
        out.push_str(&ctx.names.str(version));
    }
    out
}
