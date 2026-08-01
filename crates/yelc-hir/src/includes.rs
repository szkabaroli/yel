//! Resolving `from "…" include Name;` — the consumer half of the module
//! artifact, and the only I/O between parsing and lowering.
//!
//! # Why this is frontend, not driver
//!
//! It decides what a specifier *means*, which modules exist, and how a loaded
//! package joins the symbol table — language semantics, every line of it. It
//! sat in `yelc-driver` for a day and violated that crate's own charter ("it
//! formats and routes; it decides nothing"); ark draws the same line —
//! `ProgramParser`, with its stdlib and dependency packages, lives in
//! `arkc-frontend`, and the driver calls one function.
//!
//! What stays with the driver is **data**: the embedded std registry is
//! `build.rs` output — binary packaging — and arrives here as the
//! `std_modules` parameter, the same shape as ark's `SemaArgs.packages`.
//!
//! The specifier is a **locator**: a bare `list` is searched as `list.yelmod`
//! through the `--include` directories in the order given; `std:` resolves
//! from the **embedded stdlib** — `stdlib/*.yel` compiled by `build.rs` into
//! this binary — and is never searched in the directories. A fallback would
//! quietly give user files the `std:` namespace, which is the one thing the
//! prefix exists to prevent.
//!
//! Runs after parsing and **before** `lower_files`: binding a module into the
//! root scope is registration, and H1 wants every name registered before any
//! body resolves against the table.
//!
//! # Ark's split, kept
//!
//! Loading lives in [`crate::program`]'s ark-named functions
//! (`add_std_package`, `add_dependency_package`); this pass **scans and
//! binds** — the `useck` half's `include` bite. A module's members are *not*
//! copied into a second scope: resolution of `Geo.thing` reads the loaded
//! package's **own** table (`CompilerContext::module_member`), which is
//! exactly ark's `table_for_module` — a module is looked *into*, never
//! flattened out.

use std::path::{Path, PathBuf};

use yelc_base::{Diagnostic, ErrorCode};
use yelc_sema::{CompilerContext, PackageId};
use yelc_syntax::{ParsedFile, ast};

pub fn resolve_includes(
    parsed: &[ParsedFile],
    directories: &[PathBuf],
    std_modules: &[(&str, &[u8])],
    context: &mut CompilerContext,
) {
    for file in parsed {
        for item in &file.ast.items {
            let ast::ItemKind::Include(decl) = item else {
                continue;
            };
            // A hole in either half was reported by the parser; there is
            // nothing to resolve (H5's diagnostic arm).
            let Some(bound) = decl.name.present() else {
                continue;
            };
            let Some(specifier) = decl.specifier else {
                continue;
            };
            let span = decl.specifier_span.unwrap_or(decl.span);

            let text = context.names.str(specifier).to_string();
            // Two spellings, one registry: `std:num` is the shorthand the
            // registry keys; `yel:std/num@0.1.0` is the WIT-style full name
            // plans/modules.md designs and the desugar artifact writes. The
            // version is dropped in the normalization — the embedded stdlib
            // has exactly one, the compiler's own.
            let std_lookup = text.strip_prefix("std:").map(str::to_string).or_else(|| {
                let full = text.strip_prefix("yel:std/")?;
                let name = full.split('@').next()?;
                Some(name.to_string())
            });
            if let Some(std_name) = std_lookup.as_deref() {
                // The embedded stdlib, compiled by build.rs from
                // `stdlib/*.yel` and stamped by the same yelc-sema build —
                // never the --include directories, which is the point of the
                // prefix.
                match std_modules.iter().find(|(name, _)| *name == std_name) {
                    Some((_, bytes)) => {
                        match crate::program::add_std_package(context, std_name, bytes, bound.name)
                        {
                            Ok(package) => bind_module(context, package, bound),
                            Err(error) => {
                                report_load_error(context, &text, &error, "<embedded stdlib>", span)
                            }
                        }
                    }
                    None => {
                        let available = std_modules
                            .iter()
                            .map(|(name, _)| *name)
                            .collect::<Vec<&str>>();

                        context.diagnostics.push(
                            Diagnostic::error(format!("no std module named `{std_name}`"))
                                .with_span(span)
                                .with_code(ErrorCode::UnresolvedName)
                                .with_note(format!("the compiler ships: {}", available.join(", "))),
                        );
                    }
                }
                continue;
            }

            let Some(path) = locate(&text, directories) else {
                let searched = if directories.is_empty() {
                    "no --include directories were given".to_string()
                } else {
                    let paths: Vec<String> = directories
                        .iter()
                        .map(|directory| {
                            directory
                                .join(format!("{text}.yelmod"))
                                .display()
                                .to_string()
                        })
                        .collect();
                    format!("searched: {}", paths.join(", "))
                };
                context.diagnostics.push(
                    Diagnostic::error(format!("cannot find module `{text}`"))
                        .with_span(span)
                        .with_code(ErrorCode::UnresolvedName)
                        .with_note(searched),
                );
                continue;
            };

            let bytes = match std::fs::read(&path) {
                Ok(bytes) => bytes,
                Err(error) => {
                    context.diagnostics.push(
                        Diagnostic::error(format!("cannot read module `{text}`: {error}"))
                            .with_span(span)
                            .with_code(ErrorCode::UnresolvedName)
                            .with_note(format!("at {}", path.display())),
                    );
                    continue;
                }
            };
            let provenance = path.display().to_string();
            match crate::program::add_dependency_package(context, specifier, &bytes, bound.name) {
                Ok(package) => bind_module(context, package, bound),
                Err(error) => report_load_error(context, &text, &error, &provenance, span),
            }
        }
    }
}

/// The `useck` half for `include`: bind the module name in the program's
/// scope. Items stay in the loaded package's own table — `use X.{…}` is the
/// (future, useck-ported) pass that pulls *items* across.
fn bind_module(context: &mut CompilerContext, package: PackageId, bound: &ast::Ident) {
    if let Err(collision) = context
        .defs
        .register_module(bound.name, package, bound.span)
    {
        crate::lower::report_duplicate_collision(context, &collision);
    }
}

fn report_load_error(
    context: &mut CompilerContext,
    text: &str,
    error: &str,
    provenance: &str,
    span: yelc_base::Span,
) {
    context.diagnostics.push(
        Diagnostic::error(format!("cannot load module `{text}`: {error}"))
            .with_span(span)
            .with_code(ErrorCode::UnresolvedName)
            .with_note(format!("at {provenance}")),
    );
}

/// First hit through the directories, in the order given.
fn locate(specifier: &str, directories: &[PathBuf]) -> Option<PathBuf> {
    directories
        .iter()
        .map(|directory| directory.join(format!("{specifier}.yelmod")))
        .find(|candidate| Path::is_file(candidate))
}
