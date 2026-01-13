// Yel Compiler wrapper
// Uses the WASI component compiled with jco

import { compiler } from './compiler/yel_dsl_cli.js';

export interface Diagnostic {
  /** Plain error message for UIs and LSPs */
  message: string;
  /** Rendered error with source context (for terminal display) */
  rendered: string;
  line: number;
  column: number;
  /** Length of the span in characters */
  length: number;
  severity: string;
}

export interface CompileResult {
  rustCode: string;
  witCode: string;
  wasmBytes: Uint8Array;
  wastCode: string;
  hirCode: string;
  thirCode: string;
}

export type CompileOutcome =
  | { tag: 'success'; val: CompileResult }
  | { tag: 'failure'; val: Diagnostic[] };

export type OutputFormat = 'rust' | 'wit' | 'wasm' | 'wast' | 'hir' | 'thir';

/**
 * Check Yel source code for errors without compiling.
 */
export function check(filename: string, source: string): Diagnostic[] {
  return compiler.check(filename, source);
}

/**
 * Compile Yel source code to the specified output format.
 */
export function compile(
  filename: string,
  source: string,
  format: OutputFormat
): CompileOutcome {
  return compiler.compile(filename, source, format);
}

/**
 * Compile multiple Yel source files together.
 */
export function compileMulti(
  files: Array<[string, string]>,
  format: OutputFormat
): CompileOutcome {
  return compiler.compileMulti(files, format);
}

/**
 * Parse Yel source code and return AST as JSON string.
 * Returns the JSON string on success, or error message on failure.
 */
export function parseToJson(source: string): { ok: true; value: string } | { ok: false; error: string } {
  try {
    const result = compiler.parseToJson(source);
    // Handle WIT result type: { tag: 'ok'|'err', val: string }
    if (typeof result === 'object' && result !== null && 'tag' in result) {
      const witResult = result as { tag: 'ok' | 'err'; val: string };
      if (witResult.tag === 'ok') {
        return { ok: true, value: witResult.val };
      } else {
        return { ok: false, error: witResult.val || 'Unknown error' };
      }
    }
    // If it's a plain string, return it directly
    if (typeof result === 'string') {
      return { ok: true, value: result };
    }
    // If it's already a parsed object (AST), stringify it
    if (typeof result === 'object' && result !== null) {
      return { ok: true, value: JSON.stringify(result) };
    }
    return { ok: false, error: `Unexpected result type: ${typeof result}` };
  } catch (e) {
    return { ok: false, error: String(e) };
  }
}

/**
 * Compile to WIT and return the code, or null if compilation fails.
 */
export function compileToWit(filename: string, source: string): string | null {
  const result = compile(filename, source, 'wit');
  if (result.tag === 'success') {
    return result.val.witCode;
  }
  return null;
}

/**
 * Compile to WASM and return the bytes, or null if compilation fails.
 */
export function compileToWasm(filename: string, source: string): Uint8Array | null {
  const result = compile(filename, source, 'wasm');
  if (result.tag === 'success') {
    return result.val.wasmBytes;
  }
  return null;
}

/**
 * Compile to WAST (WebAssembly Text format) and return the code, or null if compilation fails.
 */
export function compileToWast(filename: string, source: string): string | null {
  const result = compile(filename, source, 'wast');
  if (result.tag === 'success') {
    return result.val.wastCode;
  }
  return null;
}

/**
 * Compile to HIR (High-level IR) and return the JSON, or null if compilation fails.
 */
export function compileToHir(filename: string, source: string): { ok: true; value: string } | { ok: false; error: string } {
  const result = compile(filename, source, 'hir');
  if (result.tag === 'success') {
    return { ok: true, value: result.val.hirCode };
  }
  const errorMsg = result.val.map(d => `${d.line}:${d.column}: ${d.message}`).join('\n');
  return { ok: false, error: errorMsg };
}

/**
 * Compile to THIR (Typed HIR) and return the JSON, or null if compilation fails.
 */
export function compileToThir(filename: string, source: string): { ok: true; value: string } | { ok: false; error: string } {
  const result = compile(filename, source, 'thir');
  if (result.tag === 'success') {
    return { ok: true, value: result.val.thirCode };
  }
  const errorMsg = result.val.map(d => `${d.line}:${d.column}: ${d.message}`).join('\n');
  return { ok: false, error: errorMsg };
}
