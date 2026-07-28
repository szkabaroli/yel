//! Driver internals. `main.rs` parses arguments and nothing else.
//!
//! Shape ported from [`szkabaroli/ark`](https://github.com/szkabaroli/ark)'s
//! `compiler/arkc/src/main.rs`: a straight-line function that runs the phases in
//! order and emits at points along the way.
//!
//! Ark also carries a `src/driver/cmd.rs` — a 40-field docopt-era `Args` struct
//! with a hand-written `USAGE` string, inherited from dora. It is **not wired
//! into `main.rs`**; ark moved to clap derive and left it behind. It is
//! reference for what not to build, not a module to port.

pub mod emit;
pub mod run;

pub use self::run::run;
