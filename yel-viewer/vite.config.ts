import { defineConfig, type Plugin } from 'vite'
import { svelte } from '@sveltejs/vite-plugin-svelte'
import tailwindcss from '@tailwindcss/vite'
import path from 'path'

// Plugin to serve WASM files with correct MIME type
function wasmMimePlugin(): Plugin {
  return {
    name: 'wasm-mime-type',
    configureServer(server) {
      server.middlewares.use((req, res, next) => {
        if (req.url?.endsWith('.wasm')) {
          res.setHeader('Content-Type', 'application/wasm');
        }
        next();
      });
    },
  };
}

// https://vite.dev/config/
export default defineConfig({
  plugins: [wasmMimePlugin(), svelte(), tailwindcss()],
  resolve: {
    alias: {
      $lib: path.resolve('./src/lib'),
      // Pin @bytecodealliance/jco to the top-level 1.18.1 install —
      // the transitive `componentize-js@0.20.0` pulls in jco@1.15.4
      // inside its pnpm store, and vite would otherwise resolve to it
      // in some import paths. 1.15.4's bundled wasmparser rejects
      // WASM GC's `rec` groups, which our backend now emits.
      '@bytecodealliance/jco': path.resolve('./node_modules/@bytecodealliance/jco/src/browser.js'),
    },
  },
  optimizeDeps: {
    // Exclude jco from optimization to let it load its WASM files properly
    exclude: ['@bytecodealliance/jco'],
  },
  assetsInclude: ['**/*.wasm'],
  build: {
    // Enable source maps for debugging (optional)
    sourcemap: false,
    // Optimize chunk splitting
    rollupOptions: {
      output: {
        // Hash WASM files for cache busting
        assetFileNames: (assetInfo) => {
          if (assetInfo.name?.endsWith('.wasm')) {
            return 'assets/[name]-[hash][extname]';
          }
          return 'assets/[name]-[hash][extname]';
        },
      },
    },
  },
})
