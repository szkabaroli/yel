// @ts-check
const esbuild = require("esbuild");
const { execSync } = require("child_process");
const fs = require("fs");
const path = require("path");

const production = process.argv.includes("--production");
const watch = process.argv.includes("--watch");

// Build targets for LSP binary (used in production)
const targets = [
  { triple: "aarch64-apple-darwin", output: "yel-lsp-darwin-arm64" },
  { triple: "x86_64-apple-darwin", output: "yel-lsp-darwin-x64" },
  { triple: "x86_64-unknown-linux-gnu", output: "yel-lsp-linux-x64" },
  { triple: "aarch64-unknown-linux-gnu", output: "yel-lsp-linux-arm64" },
  { triple: "x86_64-pc-windows-msvc", output: "yel-lsp-win32-x64.exe" },
];

async function buildLspBinary() {
  const binDir = path.join(__dirname, "bin");
  if (!fs.existsSync(binDir)) {
    fs.mkdirSync(binDir, { recursive: true });
  }

  const workspaceRoot = path.join(__dirname, "..", "..");

  if (production) {
    // In production, build for all targets
    console.log("Building LSP binaries for all platforms...");
    for (const target of targets) {
      console.log(`  Building for ${target.triple}...`);
      try {
        execSync(`cargo build --release -p yel-lsp --target ${target.triple}`, {
          cwd: workspaceRoot,
          stdio: "inherit",
        });

        const srcName = target.output.endsWith(".exe")
          ? "yel-lsp.exe"
          : "yel-lsp";
        const source = path.join(
          workspaceRoot,
          "target",
          target.triple,
          "release",
          srcName,
        );
        const dest = path.join(binDir, target.output);

        if (fs.existsSync(source)) {
          fs.copyFileSync(source, dest);
          // Make executable on Unix
          if (!target.output.endsWith(".exe")) {
            fs.chmodSync(dest, 0o755);
          }
          console.log(`  Copied ${target.output}`);
        }
      } catch (e) {
        console.warn(`  Warning: Failed to build for ${target.triple}`);
      }
    }
  } else {
    // In development, just build for current platform
    console.log("Building LSP binary for current platform...");
    try {
      execSync("cargo build --release -p yel-lsp", {
        cwd: workspaceRoot,
        stdio: "inherit",
      });
      console.log("LSP binary built successfully");
    } catch (e) {
      console.error("Failed to build LSP binary:", e.message);
    }
  }
}

async function buildExtension() {
  const ctx = await esbuild.context({
    entryPoints: ["src/extension.ts"],
    bundle: true,
    outfile: "out/extension.js",
    external: ["vscode"],
    format: "cjs",
    platform: "node",
    target: "node18",
    sourcemap: !production,
    minify: production,
  });

  if (watch) {
    await ctx.watch();
    console.log("Watching for changes...");
  } else {
    await ctx.rebuild();
    await ctx.dispose();
    console.log("Extension built successfully");
  }
}

async function main() {
  try {
    await buildLspBinary();
    await buildExtension();
  } catch (e) {
    console.error("Build failed:", e);
    process.exit(1);
  }
}

main();
