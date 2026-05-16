//! Yel DSL Compiler CLI

use anyhow::{Context, Result};
use clap::{Parser, Subcommand, ValueEnum};
use std::fs;
use std::path::PathBuf;
use yel_core::{Compiler, syntax::ast::PackageId};
use yel_wasm_codegen as codegen;

// Build info from shadow-rs
shadow_rs::shadow!(build);

/// Get long version string with git info
fn long_version() -> &'static str {
    shadow_rs::formatcp!(
        "{} ({} {})\nbuilt: {}\nrustc: {}",
        build::PKG_VERSION,
        build::SHORT_COMMIT,
        build::COMMIT_DATE,
        build::BUILD_TIME,
        build::RUST_VERSION
    )
}

#[derive(Parser)]
#[command(name = "yelc")]
#[command(author, version = build::PKG_VERSION, long_version = long_version(), about = "Yel Compiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile Yel source files
    Compile {
        /// Input file(s) to compile
        #[arg(required = true)]
        files: Vec<PathBuf>,

        /// Output format
        #[arg(short, long, value_enum, default_value = "wasm")]
        output: OutputFormat,

        /// Package name for generated code
        #[arg(short, long, default_value = "yel_app")]
        package: String,

        /// Run Binaryen's `wasm-opt` on the core module before
        /// component-encoding. All trailing args after `--` are
        /// forwarded verbatim to `wasm-opt` (e.g.
        /// `yelc compile foo.yel -- -O3 --enable-gc --type-merging`).
        /// Requires `wasm-opt` on PATH.
        #[arg(long = "opt", default_value_t = false)]
        opt: bool,

        /// Release build: validated wasm-opt pass stack +
        /// strip component-level custom sections (name, producers).
        /// Equivalent to `--opt -- --type-ssa --type-merging --gufa
        /// -O3 --gufa -Oz --converge --closed-world --enable-gc
        /// --enable-reference-types --enable-multivalue
        /// --enable-bulk-memory --enable-bulk-memory-opt`, followed by
        /// stripping non-essential custom sections. Requires
        /// `wasm-opt` and `wasm-tools` on PATH. Mutually exclusive
        /// with `--opt`.
        #[arg(long = "release", default_value_t = false, conflicts_with = "opt")]
        release: bool,

        /// Trailing args forwarded to `wasm-opt` (only used with
        /// `--opt`). Pass via `-- -O3 --enable-gc ...` so clap stops
        /// flag parsing first.
        #[arg(last = true, allow_hyphen_values = true, num_args = 0..)]
        wasm_opt_args: Vec<String>,
    },

    /// Parse and dump AST
    Ast {
        /// Input file to parse
        file: PathBuf,

        /// Pretty print
        #[arg(short, long)]
        pretty: bool,

        /// Output as JSON
        #[arg(short, long)]
        json: bool,
    },

    /// Dump intermediate representation (LIR)
    Ir {
        /// Input file to analyze
        file: PathBuf,

        /// Pretty print
        #[arg(short, long)]
        pretty: bool,

        /// Output as JSON
        #[arg(short, long)]
        json: bool,
    },

    /// Check source files for errors
    Check {
        /// Input file(s) to check
        ///
        #[arg(required = true)]
        files: Vec<PathBuf>,
    },
}

#[derive(Clone, ValueEnum)]
enum OutputFormat {
    /// Generated Rust source code
    Rust,
    /// Generated WIT interface
    Wit,
    /// WebAssembly component
    Wasm,
    /// Graphviz DOT: signal/effect dependency graph
    Dot,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Compile {
            files,
            output,
            package,
            opt,
            release,
            wasm_opt_args,
        } => compile(files, output, package, opt, release, wasm_opt_args),
        Commands::Ast { file, pretty, json } => dump_ast(file, pretty, json),
        Commands::Ir { file, pretty, json } => dump_lir(file, pretty, json),
        Commands::Check { files } => check(files),
    }
}

/// Validated wasm-opt incantation for `--release`. Each arg sent
/// individually so wasm-opt sees them as discrete tokens. Pass order
/// matters: type-ssa specializes, type-merging dedupes, gufa
/// propagates, -O3/-Oz prune, --converge re-runs to fixpoint.
const RELEASE_WASM_OPT_ARGS: &[&str] = &[
    "--type-ssa",
    "--type-merging",
    "--gufa",
    "-O3",
    "--gufa",
    "-Oz",
    "--converge",
    "--closed-world",
    "--enable-gc",
    "--enable-reference-types",
    "--enable-multivalue",
    "--enable-bulk-memory",
    "--enable-bulk-memory-opt",
];

/// Strip non-essential custom sections from a component to shed
/// debug names. Shells out to `wasm-tools strip` which understands
/// component nesting (wasm-opt doesn't see the outer component).
fn strip_custom_sections(input: &[u8]) -> Result<Vec<u8>> {
    use std::io::Write;
    use std::process::Command;
    let pid = std::process::id();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let tmp_dir = std::env::temp_dir();
    let in_path = tmp_dir.join(format!("yelc-strip-in-{}-{}.wasm", pid, nanos));
    let out_path = tmp_dir.join(format!("yelc-strip-out-{}-{}.wasm", pid, nanos));
    {
        let mut f = std::fs::File::create(&in_path).context("create strip input tempfile")?;
        f.write_all(input).context("write strip input tempfile")?;
    }
    let output = Command::new("wasm-tools")
        .arg("strip")
        .arg("--all")
        .arg(&in_path)
        .arg("-o")
        .arg(&out_path)
        .output();
    let result = match output {
        Ok(out) if out.status.success() => std::fs::read(&out_path).context("read strip output"),
        Ok(out) => Err(anyhow::anyhow!(
            "wasm-tools strip failed (status {}): {}",
            out.status,
            String::from_utf8_lossy(&out.stderr)
        )),
        Err(e) => Err(anyhow::anyhow!(
            "wasm-tools strip: failed to spawn (is it on PATH?): {}",
            e
        )),
    };
    let _ = std::fs::remove_file(&in_path);
    let _ = std::fs::remove_file(&out_path);
    result
}

fn compile(
    files: Vec<PathBuf>,
    output: OutputFormat,
    _package: String,
    opt: bool,
    release: bool,
    wasm_opt_args: Vec<String>,
) -> Result<()> {
    let mut compiler = Compiler::new();

    // Collect all LIR components and package info
    let mut lir_components = Vec::new();
    let mut package_info: Option<PackageId> = None;

    for file in &files {
        let source = fs::read_to_string(file)
            .with_context(|| format!("Failed to read file: {}", file.display()))?;

        let parsed = compiler
            .parse(&source)
            .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;

        // Extract package info from first file that has it
        if package_info.is_none() {
            package_info = parsed.package.clone();
        }

        let hir_components = compiler.lower_to_hir(&parsed);

        if compiler.has_errors() {
            eprintln!("{}", compiler.render_diagnostics());
            return Err(anyhow::anyhow!("Compilation failed"));
        }

        for hir in &hir_components {
            let thir = compiler.type_check(hir);

            if compiler.has_errors() {
                eprintln!("{}", compiler.render_diagnostics());
                return Err(anyhow::anyhow!("Type checking failed"));
            }

            let lir = compiler.lower_to_lir(&thir);
            lir_components.push(lir);
        }
    }

    // Type-check and lower global-singleton property defaults once, after all
    // components. The module start function seeds these slots at load time.
    let thir_globals = compiler.type_check_globals();
    if compiler.has_errors() {
        eprintln!("{}", compiler.render_diagnostics());
        return Err(anyhow::anyhow!("Type checking failed"));
    }
    let lir_globals = compiler.lower_globals_to_lir(&thir_globals);

    // Assemble the module — one compilation unit holding every component and
    // module-scope artifact (global defaults, package header).
    let module = yel_core::lir::LirModule {
        components: lir_components.clone(),
        global_defaults: lir_globals.clone(),
        package: package_info.clone(),
    };

    // Generate output for each component
    let ctx = compiler.context();

    // Build WitOptions from package info
    let wit_options = if let Some(ref pkg) = package_info {
        codegen::WitOptions {
            namespace: pkg.namespace.clone(),
            name: pkg.name.clone(),
            version: pkg.version.clone().unwrap_or_else(|| "0.1.0".to_string()),
            include_dom_interface: true,
        }
    } else {
        codegen::WitOptions {
            namespace: "yel".to_string(),
            name: "app".to_string(),
            version: "0.1.0".to_string(),
            include_dom_interface: true,
        }
    };

    match output {
        OutputFormat::Rust => {
            // Rust codegen temporarily disabled - needs update for block-based LIR
            println!("// Rust codegen not available - use WASM output instead");
        }
        OutputFormat::Wit => {
            // Unified path: library files (no exports) get a well-formed
            // package + library world from the same builder.
            let wit_code = codegen::generate_wit(&lir_components, ctx, &wit_options)
                .map_err(|e| anyhow::anyhow!("WIT generation error: {}", e))?;
            println!("{}", wit_code);
        }
        OutputFormat::Wasm => {
            use std::io::Write;

            let effective_opt_args: Option<Vec<String>> = if release {
                Some(RELEASE_WASM_OPT_ARGS.iter().map(|s| s.to_string()).collect())
            } else if opt {
                Some(wasm_opt_args.clone())
            } else {
                None
            };
            let wasm_options = codegen::WasmWithWitOptions {
                namespace: wit_options.namespace.clone(),
                name: wit_options.name.clone(),
                version: wit_options.version.clone(),
                global_defaults: lir_globals.clone(),
                wasm_opt_args: effective_opt_args,
            };
            let wasm_bytes = codegen::generate_wasm_module(&module, ctx, &wasm_options)
                .map_err(|e| anyhow::anyhow!("WASM generation error: {}", e))?;

            let final_bytes = if release {
                strip_custom_sections(&wasm_bytes)?
            } else {
                wasm_bytes
            };

            std::io::stdout()
                .write_all(&final_bytes)
                .context("Failed to write WASM output")?;
        }
        OutputFormat::Dot => {
            let dot = codegen::generate_dot(&lir_components, ctx, &codegen::DotOptions::new())
                .map_err(|e| anyhow::anyhow!("DOT generation error: {}", e))?;
            print!("{}", dot);
        }
    }

    Ok(())
}

fn dump_ast(file: PathBuf, pretty: bool, json: bool) -> Result<()> {
    let source = fs::read_to_string(&file)
        .with_context(|| format!("Failed to read file: {}", file.display()))?;

    let ast = yel_core::parse(&source).map_err(|e| {
        eprintln!("Parse error: {}", e);
        anyhow::anyhow!("Failed to parse")
    })?;

    if json {
        if pretty {
            println!(
                "{}",
                serde_json::to_string_pretty(&ast).context("Failed to serialize AST to JSON")?
            );
        } else {
            println!(
                "{}",
                serde_json::to_string(&ast).context("Failed to serialize AST to JSON")?
            );
        }
    } else if pretty {
        println!("{:#?}", ast);
    } else {
        println!("{:?}", ast);
    }

    Ok(())
}

fn dump_lir(file: PathBuf, pretty: bool, json: bool) -> Result<()> {
    let source = fs::read_to_string(&file)
        .with_context(|| format!("Failed to read file: {}", file.display()))?;

    let mut compiler = Compiler::new();

    let parsed = compiler.parse(&source).map_err(|e| {
        eprintln!("Parse error: {}", e);
        anyhow::anyhow!("Failed to parse")
    })?;

    let hir_components = compiler.lower_to_hir(&parsed);

    if compiler.has_errors() {
        eprintln!("{}", compiler.render_diagnostics());
        return Err(anyhow::anyhow!("HIR lowering failed"));
    }

    // Collect all LIR components first
    let mut lir_components = Vec::new();
    for hir in &hir_components {
        let thir = compiler.type_check(hir);

        if compiler.has_errors() {
            eprintln!("{}", compiler.render_diagnostics());
            return Err(anyhow::anyhow!("Type checking failed"));
        }

        let lir = compiler.lower_to_lir(&thir);
        lir_components.push(lir);
    }

    if json {
        if pretty {
            println!(
                "{}",
                serde_json::to_string_pretty(&lir_components)
                    .context("Failed to serialize LIR to JSON")?
            );
        } else {
            println!(
                "{}",
                serde_json::to_string(&lir_components)
                    .context("Failed to serialize LIR to JSON")?
            );
        }
    } else {
        // Now we can borrow context for printing
        let ctx = compiler.context();

        for lir in &lir_components {
            // Print LIR summary
            let name = ctx.str(lir.name);
            println!("=== Component: {} ===\n", name);

            println!("Signals ({}):", lir.signals.len());
            for s in &lir.signals {
                let sig_name = ctx.str(ctx.defs.name(s.def_id));
                println!("  {} : {:?} = {:?}", sig_name, ctx.ty_kind(s.ty), s.default);
            }

            println!("\nEffects ({}):", lir.effects.len());
            for e in &lir.effects {
                println!(
                    "  update_block={:?} deps={:?}",
                    e.update_block, e.dependencies
                );
            }

            println!("\nBlocks ({}):", lir.blocks.len());
            for (i, block) in lir.blocks.iter().enumerate() {
                let mount_marker = if lir.mount_block.0 as usize == i {
                    " (mount)"
                } else {
                    ""
                };
                println!("  Block {:?}{}:", block.id, mount_marker);
                for op in &block.ops {
                    println!("    {:?}", op);
                }
            }

            println!("\nStrings ({}):", lir.strings.len());
            for (i, s) in lir.strings.iter().enumerate() {
                println!("  [{}] \"{}\"", i, s);
            }
        }
    }

    Ok(())
}

fn check(files: Vec<PathBuf>) -> Result<()> {
    let mut compiler = Compiler::new();
    let mut total_components = 0;

    for file in &files {
        let source = fs::read_to_string(file)
            .with_context(|| format!("Failed to read file: {}", file.display()))?;

        let parsed = match compiler.parse(&source) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("Parse error in {}: {}", file.display(), e);
                return Err(anyhow::anyhow!("Check failed"));
            }
        };

        let hir_components = compiler.lower_to_hir(&parsed);

        if compiler.has_errors() {
            eprintln!("{}", compiler.render_diagnostics());
            return Err(anyhow::anyhow!("Check failed"));
        }

        for hir in &hir_components {
            let _thir = compiler.type_check(hir);
            total_components += 1;
        }

        if compiler.has_errors() {
            eprintln!("{}", compiler.render_diagnostics());
            return Err(anyhow::anyhow!("Check failed with type errors"));
        }
    }

    eprintln!("OK: {} component(s) checked", total_components);
    Ok(())
}
