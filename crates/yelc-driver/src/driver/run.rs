//! The pipeline, in a straight line.
//!
//! Phases run in order; each `--emit-*` prints between two of them. As stages
//! land, this function grows lines in the middle — it does not grow branches.

use yelc_base::{Diagnostics, Interner, SourceMap};
use yelc_sema::{CompilerContext, PackageId};

use super::emit;
use crate::Args;

/// Exit code: 0 clean, 1 the program has errors, 2 the driver could not run.
pub fn run(args: Args) -> i32 {
    let content = match std::fs::read_to_string(&args.file) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("yelc2: cannot read {}: {err}", args.file.display());
            return 2;
        }
    };

    let mut source_map = SourceMap::new();
    let source = source_map.add_file(&args.file, content.clone());
    let interner = Interner::new();
    let mut diagnostics = Diagnostics::new();

    // Stage 3, phase 1 — the language's own definitions, before a byte of the
    // file is read. Built unconditionally rather than behind `--emit-builtins`:
    // it is the sequence `new → register_builtins → resolve_known`, and a
    // sequence that only runs under a flag is a sequence nobody runs.
    let context = CompilerContext::with_builtins(PackageId::LOCAL);
    if args.emit_builtins {
        print!("{}", emit::builtins(&context));
    }

    // Stage 1 — source → AST. Never fails; see yelc-syntax invariant S6.
    let parsed = yelc_syntax::parse(source, &content, &interner, &mut diagnostics);

    // Emission is unconditional on diagnostics. A tree that only prints for
    // input that parses is useless for the case it was built to serve: the
    // parser is being reviewed precisely on what it does with broken input.
    if args.emit_green {
        print!("{}", emit::green_tree(&parsed.green));
    }
    if args.emit_green_text {
        print!("{}", parsed.green.text());
    }
    if let Some(filter) = &args.emit_ast {
        let filter = (!filter.is_empty()).then_some(filter.as_str());
        print!(
            "{}",
            emit::ast(&parsed.ast, &interner, filter, args.identified, args.spans)
        );
    }

    // Stage 2+ emit points land here as their crates arrive.

    if !diagnostics.is_empty() {
        eprint!("{}", diagnostics.render(&source_map));
    }

    i32::from(diagnostics.has_errors())
}
