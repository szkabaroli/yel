//! Zed extension for Yel.
//!
//! Thin launcher only: finds the `yelc-lsp` binary and hands Zed a command.
//! The server itself lives in `crates/yelc-lsp` and speaks LSP over stdio
//! with no arguments; everything language-shaped (diagnostics, symbols,
//! hover) is its job, not this crate's.

use zed_extension_api::{self as zed, settings::LspSettings, LanguageServerId, Result};

struct YelExtension;

impl zed::Extension for YelExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        // An explicit binary in Zed settings wins, so a checkout can point at
        // `target/debug/yelc-lsp` without putting it on PATH:
        //
        //   "lsp": { "yelc-lsp": { "binary": { "path": "/…/target/debug/yelc-lsp" } } }
        if let Some(binary) = LspSettings::for_worktree("yelc-lsp", worktree)
            .ok()
            .and_then(|settings| settings.binary)
        {
            if let Some(path) = binary.path {
                return Ok(zed::Command {
                    command: path,
                    args: binary.arguments.unwrap_or_default(),
                    env: Vec::new(),
                });
            }
        }

        let command = worktree.which("yelc-lsp").ok_or_else(|| {
            "yelc-lsp not found on PATH — build it with `cargo build -p yelc-lsp --release`, \
             then add it to PATH or set `lsp.yelc-lsp.binary.path` in Zed settings"
                .to_string()
        })?;

        Ok(zed::Command {
            command,
            args: Vec::new(),
            env: Vec::new(),
        })
    }
}

zed::register_extension!(YelExtension);
