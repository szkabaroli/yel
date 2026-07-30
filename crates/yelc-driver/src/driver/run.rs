//! The pipeline, in a straight line.
//!
//! Phases run in order; each `--emit-*` prints between two of them. As stages
//! land, this function grows lines in the middle — it does not grow branches.

use yelc_sema::{CompilerContext, PackageId};
use yelc_syntax::ParsedFile;

use super::{emit, package};
use crate::Args;

/// Exit code: 0 clean, 1 the program has errors, 2 the driver could not run.
pub fn run(args: Args) -> i32 {
    // Stage 3, phase 1 — the language's own definitions, before a byte of the
    // input is read. Built unconditionally rather than behind `--emit-builtins`:
    // it is the sequence `new → register_builtins → resolve_known`, and a
    // sequence that only runs under a flag is a sequence nobody runs.
    //
    // It also owns the `SourceMap` and the `Diagnostics`. The driver used to
    // keep its own of each beside the context's, which meant two id spaces that
    // nothing checked agreed — spans minted against one and rendered against
    // the other resolve to the wrong file, or to none
    // (`plans/rewrite/anti-spec.md` F12).
    let mut context = CompilerContext::with_builtins(PackageId::LOCAL);
    if args.emit_builtins {
        print!("{}", emit::builtins(&context));
    }

    // The package is a directory of files, not a file (`plans/modules.md` §4).
    let paths = match package::collect(&args.path) {
        Ok(paths) => paths,
        Err(message) => {
            eprintln!("yelc2: {message}");
            return 2;
        }
    };

    // `--emit-green-text` is the instrument for invariant S1, which is a claim
    // about **one** file's bytes. Concatenating two files' texts would produce
    // output that no comparison against any single input can be made from, and
    // a harness diffing it would report a pass it never measured. Refuse
    // instead of emitting something misleading.
    if args.emit_green_text && paths.len() > 1 {
        eprintln!(
            "yelc2: --emit-green-text needs one file, but {} is a package of {} \
             files; name the file directly",
            args.path.display(),
            paths.len()
        );
        return 2;
    }

    let mut parsed: Vec<ParsedFile> = Vec::with_capacity(paths.len());
    for path in &paths {
        let content = match std::fs::read_to_string(path) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("yelc2: cannot read {}: {err}", path.display());
                return 2;
            }
        };
        let source = context.sources.add_file(path, content.clone());
        // Stage 1 — source → AST. Never fails; see yelc-syntax invariant S6.
        parsed.push(yelc_syntax::parse(
            source,
            &content,
            &context.names,
            &mut context.diagnostics,
        ));
    }

    // Emission is unconditional on diagnostics. A tree that only prints for
    // input that parses is useless for the case it was built to serve: the
    // parser is being reviewed precisely on what it does with broken input.
    //
    // Names are prefixed only when there is more than one file, as `grep` and
    // `head` do — a single-file invocation keeps the output it had.
    let label = paths.len() > 1;
    for (path, file) in paths.iter().zip(&parsed) {
        if args.emit_green {
            if label {
                println!("==> {} <==", path.display());
            }
            print!("{}", emit::green_tree(&file.green));
        }
        if args.emit_green_text {
            print!("{}", file.green.text());
        }
        if let Some(filter) = &args.emit_ast {
            if label {
                println!("==> {} <==", path.display());
            }
            let filter = (!filter.is_empty()).then_some(filter.as_str());
            print!(
                "{}",
                emit::ast(
                    &file.ast,
                    &context.names,
                    filter,
                    args.identified,
                    args.spans
                )
            );
        }
    }

    // The files are one package, and this is where that stops being an
    // assumption. It runs over every file at once, which is the whole reason it
    // could not have run inside the parse loop above.
    yelc_hir::check_package_identity(&parsed, &mut context);

    // Stage 2+ emit points land here as their crates arrive.

    if !context.diagnostics.is_empty() {
        eprint!("{}", context.diagnostics.render(&context.sources));
    }

    i32::from(context.diagnostics.has_errors())
}
