//! `yelc2` — the rewrite's driver.
//!
//! The observation instrument for `plans/rewrite/`. Stage 1 was reviewed
//! entirely through throwaway `#[cfg(test)]` probes, and two of the
//! integrator's own measurements were wrong because of it. This crate exists so
//! that "what does the new parser do with this file" is one command.
//!
//! # Thin is the requirement
//!
//! It formats and routes. It decides nothing about the language. The moment it
//! grows behaviour of its own it becomes a third implementation to keep in sync
//! with two others.

mod driver;

use std::path::PathBuf;

use clap::Parser;

/// Run the new pipeline over one file and emit the IRs asked for.
///
/// Flag shape follows ark's `arkc`: one `--emit-<ir>` per stage, each taking an
/// optional filter, all on a single invocation. That last part is the reason —
/// `--emit-ast --emit-green` dumps both views of **one** parse, so when they
/// disagree it cannot be because they came from different runs.
#[derive(Parser)]
#[command(name = "yelc2")]
#[command(about = "Yel compiler (rewrite) — parse a file and emit its IRs", long_about = None)]
pub struct Args {
    /// The package to compile: a **directory** of `.yel` files.
    ///
    /// A single file is also accepted and read as a one-file package, which
    /// keeps `--emit-green-text` — a per-file instrument — usable.
    pub path: PathBuf,

    /// Emit the typed AST. Optionally filter to one top-level item by name,
    /// written `--emit-ast=Counter`.
    ///
    /// `require_equals` is load-bearing, not style. With an optional value and
    /// no `=`, clap reads the **next token** as the filter — so
    /// `--emit-ast counter/` filtered for an item named `counter/` and then
    /// reported the package argument missing. The separator makes the value
    /// unambiguous and gives the positional back.
    #[arg(long, value_name = "ITEM", num_args = 0..=1, require_equals = true, default_missing_value = "")]
    pub emit_ast: Option<String>,

    /// Emit the green tree — kinds and widths, trivia included.
    #[arg(long)]
    pub emit_green: bool,

    /// Emit the builtin table and the resolved lang-items.
    ///
    /// Depends on no source input, which is what makes it comparable against
    /// the frozen compiler's tables *before* a file is parsed — the standalone
    /// checkpoint `yelc-sema::stdlib` owes.
    #[arg(long)]
    pub emit_builtins: bool,

    /// Emit `green.text()`: the source reconstructed from the tree.
    ///
    /// Invariant S1 says this equals the input byte for byte, for every input,
    /// including one that does not parse. This is how you look at it.
    #[arg(long)]
    pub emit_green_text: bool,

    /// Include `NodeId` on every AST node.
    ///
    /// Taken from rustc's `-Z unpretty=…,identified`. The capability is worth
    /// having — a diagnostic or an LSP request has to be pointed at a node. The
    /// flag name is not: rustc's `--unpretty` is named against a `--pretty`
    /// that was removed years ago.
    #[arg(long)]
    pub identified: bool,

    /// Include byte spans on every AST node.
    #[arg(long)]
    pub spans: bool,
}

fn main() {
    std::process::exit(driver::run(Args::parse()));
}
