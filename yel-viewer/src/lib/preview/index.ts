/**
 * Yel Preview Module
 *
 * This module provides the infrastructure for running compiled Yel components
 * in the browser preview pane.
 *
 * ## Architecture
 *
 * The preview system works as follows:
 *
 * 1. Yel source code is compiled to WebAssembly Component Model format
 * 2. The component is transpiled to JS + core WASM using jco
 * 3. The transpiled module is instantiated with DOM bindings
 * 4. The component is mounted to the preview container
 *
 * ## Usage
 *
 * ```typescript
 * import { YelPreviewHost } from './preview';
 *
 * const host = new YelPreviewHost();
 * host.init(previewContainer);
 *
 * // When WASM bytes are available from compilation:
 * await host.load(wasmBytes, {
 *   incremented: () => console.log('Counter incremented!'),
 * });
 *
 * host.mount();
 * ```
 */

export * from "./dom";
export * from "./host";

// Import all Svelte custom elements
// These register themselves as custom elements when imported
import './components/Button.svelte';
import './components/Text.svelte';