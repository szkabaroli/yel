//! `yelc-hir` — the frontend: package checking, AST → HIR lowering, and the
//! HIR itself, the desugared and name-resolved IR that type checking runs on.
//!
//! The pipeline entry is [`check_package`]: collect the package's files, parse
//! them, check they declare one package ([`packageck`]), bind included modules
//! into the root scope ([`includes`]), then lower ([`lower_files`]) — register
//! every item, collect every declared type, lower bodies and desugar the UI
//! tree to builder expressions, with state-dependency analysis ([`signalck`])
//! at the tail. Type checking is a later phase over the **same** nodes —
//! there is no second IR after this one.
//!
//! Two decisions this crate's shape encodes:
//!
//! - analysis results live **beside** nodes, never on them — [`NodeMap`];
//! - a HIR node points back at **which file's** AST node — [`SourceNodeId`],
//!   because `yelc-syntax` allocates node ids per file from zero, so a bare
//!   node id cannot key a package-wide map.

pub mod emit;
pub mod emit_hir;
pub mod expr;
pub mod ids;
pub mod includes;
pub mod lower;
pub mod map;
pub mod module;
pub mod packageck;
pub mod program;
pub mod signalck;
pub mod sym;
pub mod visit;

pub use expr::{
    BinaryOp, HirBlock, HirBoundary, HirCallee, HirExpr, HirExprKind, HirFieldInit, HirInstantiate,
    HirInterpolationPart, HirLiteral, HirLocal, HirMatch, HirMatchArm, HirPattern, HirProp,
    HirRepeat, HirStmt, UnaryOp,
};
pub use ids::{BodyId, HirId, HirItemId, LocalId, SourceNodeId, TypeId};
pub use includes::resolve_includes;
pub use map::HirMap;
pub use module::{HirBody, HirComponent, HirDefault, HirFunction, HirGlobal, HirItem, HirModule};
pub use node_map::NodeMap;
pub use packageck::{PackageIdentity, check_package_identity};
pub use signalck::{BodyDependencies, StateRef};

mod node_map;

use std::path::{Path, PathBuf};

use yelc_sema::CompilerContext;
use yelc_syntax::ParsedFile;

/// Build the HIR for one package.
///
/// # Three phases, each sweeping every file before the next begins
///
/// Invariant H1 (`plans/rewrite/stage-3-hir-build.md`). This is why the whole
/// file set arrives at once rather than one file at a time:
///
/// | phase | does, across all files | may not |
/// |---|---|---|
/// | 1 · register | a `DefId` + name for every item | resolve a type — no name is guaranteed to exist yet |
/// | 2 · collect | resolve every **declared** type into the definition tables | look at any body |
/// | 3 · lower | lower bodies, desugar the UI tree | register new items |
///
/// A body may reference any item regardless of source order, **and so may a
/// declared type, and so may either across file boundaries.** The frozen driver
/// merges fully-lowered files inside a loop, so cross-file references resolve in
/// one direction only; phase-major sweeping is what fixes that.
///
/// # Postcondition
///
/// [`HirModule::types`] is **empty**. Declared types are in
/// [`CompilerContext::defs`]; expression types are stage 4's.
///
/// Errors accumulate in [`CompilerContext::diagnostics`] and lowering continues —
/// there is no `Result` here on purpose
/// (`plans/rewrite/keep-list.md`, accumulate-and-continue).
pub fn lower_files(parsed: &[ParsedFile], ctx: &mut CompilerContext) -> HirModule {
    lower::lower_files(parsed, ctx)
}

// ---------------------------------------------------------------------------
// The pipeline — ark keeps `check_program` in `arkc-frontend/src/lib.rs`, and
// this crate keeps its equivalent here for the same reason: open the crate,
// see the pipeline. The machinery lives in the modules; the *order* lives at
// the front door.
// ---------------------------------------------------------------------------

/// Everything the frontend produced for one package: the per-file parses (the
/// driver's emit instruments need them) and the lowered module.
pub struct CheckedPackage {
    pub paths: Vec<PathBuf>,
    pub parsed: Vec<ParsedFile>,
    /// The identity the package settled on; `None` when no file declared a
    /// usable one (each was reported individually).
    pub identity: Option<packageck::PackageIdentity>,
    pub module: HirModule,
}

/// The frontend, end to end.
///
/// Diagnostics accumulate on `context`; the driver reads `has_errors()` for
/// its exit code. `Err` is reserved for the environment failing (unreadable
/// path), which no diagnostic can represent honestly — there is no source
/// span to attach it to.
pub fn check_package(
    path: &Path,
    include_directories: &[PathBuf],
    std_modules: &[(&str, &[u8])],
    context: &mut CompilerContext,
) -> Result<CheckedPackage, String> {
    check_package_with_overlay(path, None, include_directories, std_modules, context)
}

/// [`check_package`] with one file's content supplied from memory — the LSP's
/// entry: the editor's unsaved buffer stands in for the file on disk, and
/// everything else about the package still comes from the filesystem.
pub fn check_package_with_overlay(
    path: &Path,
    overlay: Option<(&Path, &str)>,
    include_directories: &[PathBuf],
    std_modules: &[(&str, &[u8])],
    context: &mut CompilerContext,
) -> Result<CheckedPackage, String> {
    let paths = program::collect(path)?;

    // The program package and its top-level module — dora's
    // `add_program_package`, the first row of the compilation's structure
    // (`plans/rewrite/definition-arenas.md` step 1).
    let program_module = context.compilation.add_package(
        yelc_sema::PackageId::LOCAL,
        yelc_sema::PackageRole::Program,
        None,
    );

    let mut parsed: Vec<ParsedFile> = Vec::with_capacity(paths.len());
    for path in &paths {
        let content = match overlay {
            Some((overlay_path, text)) if overlay_path == path => text.to_string(),
            _ => std::fs::read_to_string(path)
                .map_err(|error| format!("cannot read {}: {error}", path.display()))?,
        };
        let source = context.sources.add_file(path, content.clone());
        context
            .compilation
            .assign_file(source, yelc_sema::PackageId::LOCAL, program_module);
        // Stage 1 — source → AST. Never fails; see yelc-syntax invariant S6.
        parsed.push(yelc_syntax::parse(
            source,
            &content,
            &context.names,
            &mut context.diagnostics,
        ));
    }

    // The files are one package, and this is where that stops being an
    // assumption (E0071/E0072).
    let identity = packageck::check_package_identity(&parsed, context);

    // Includes bind modules into the root scope before lowering: a module
    // binding is registration, and H1 wants every name registered before any
    // body resolves.
    includes::resolve_includes(&parsed, include_directories, std_modules, context);

    // Stage 3 — register, collect, lower; signalck runs at its tail.
    let module = lower::lower_files(&parsed, context);

    Ok(CheckedPackage {
        paths,
        parsed,
        identity,
        module,
    })
}

/// What one invocation asked for — clap-free: the driver parses whatever
/// surface it likes and converts into this.
#[derive(Default)]
pub struct RunArgs {
    pub path: PathBuf,
    pub include_directories: Vec<PathBuf>,
    /// Emit the typed AST, optionally filtered to one item by name (`Some("")`
    /// = unfiltered).
    pub emit_ast: Option<String>,
    pub emit_green: bool,
    pub emit_green_text: bool,
    pub emit_hir: bool,
    pub emit_module: Option<PathBuf>,
    pub identified: bool,
    pub spans: bool,
    pub debug_ast: bool,
    pub debug_defs: bool,
    pub debug_hir: bool,
}

/// The whole invocation: frontend behind [`check_package`], every `--emit-*`
/// and `--debug-*` view, exit code out. The driver parses flags into
/// [`RunArgs`], supplies the embedded std bytes, and exits with what this
/// returns.
///
/// Exit code: 0 clean, 1 the program has errors, 2 the environment failed.
pub fn run(args: &RunArgs, std_modules: &[(&str, &[u8])]) -> i32 {
    // The language's own definitions, before a byte of the input is read —
    // and the context owns the SourceMap and Diagnostics (one id space each).
    let mut context = CompilerContext::with_intrinsics(yelc_sema::PackageId::LOCAL);
    // `--debug-*` dumps print `Name("count")` instead of `Name(16)` — the
    // Debug impl resolves through this thread-local installation.
    context.names.install_for_debug();

    // The frontend, end to end.
    let checked = match check_package(
        &args.path,
        &args.include_directories,
        std_modules,
        &mut context,
    ) {
        Ok(checked) => checked,
        Err(message) => {
            eprintln!("yelc2: {message}");
            return 2;
        }
    };

    // `--emit-green-text` is the instrument for invariant S1, a claim about
    // **one** file's bytes; a concatenation of files can be diffed against
    // nothing. Refuse instead of emitting something misleading.
    if args.emit_green_text && checked.paths.len() > 1 {
        eprintln!(
            "yelc2: --emit-green-text needs one file, but {} is a package of {} \
             files; name the file directly",
            args.path.display(),
            checked.paths.len()
        );
        return 2;
    }

    // Emission is unconditional on diagnostics: the parser is reviewed
    // precisely on what it does with broken input. Names are prefixed only
    // when there is more than one file, as `grep` and `head` do.
    let label = checked.paths.len() > 1;
    for (path, file) in checked.paths.iter().zip(&checked.parsed) {
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
        if args.debug_ast {
            if label {
                println!("==> {} <==", path.display());
            }
            println!("{:#?}", file.ast);
        }
    }

    if args.emit_hir {
        print!("{}", emit_hir::hir(&checked.module, &context));
    }
    if args.debug_defs {
        println!("{:#?}", context.defs);
        for table in &context.imported {
            println!("{table:#?}");
        }
    }
    if args.debug_hir {
        println!("{:#?}", checked.module);
    }

    // The serialized module — last, because it must see every diagnostic: a
    // module emitted from a failed compilation would be a corrupt cache
    // wearing a stamp.
    if let Some(path) = &args.emit_module {
        if context.diagnostics.has_errors() {
            eprintln!(
                "yelc2: refusing to write {}: the compilation has errors",
                path.display()
            );
        } else if let Some(identity) = checked.identity {
            let package = yelc_sema::PackageName::new(
                context.names.str(identity.namespace).to_string(),
                context.names.str(identity.name).to_string(),
                // Slated for removal and not load-bearing (plans/modules.md);
                // absent is written as empty rather than invented.
                identity
                    .version
                    .map(|version| context.names.str(version).to_string())
                    .unwrap_or_default(),
            );
            let artifact = yelc_sema::Artifact::from_context(package, &context);
            let bytes = yelc_sema::artifact::encode(&artifact);
            if let Err(error) = std::fs::write(path, &bytes) {
                eprintln!("yelc2: cannot write {}: {error}", path.display());
                return 2;
            }
        } else {
            eprintln!(
                "yelc2: refusing to write {}: the package has no identity",
                path.display()
            );
        }
    }

    if !context.diagnostics.is_empty() {
        eprint!("{}", context.diagnostics.render(&context.sources));
    }

    i32::from(context.diagnostics.has_errors())
}
