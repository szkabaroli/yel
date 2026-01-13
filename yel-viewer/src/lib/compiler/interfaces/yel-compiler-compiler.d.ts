/** @module Interface yel:compiler/compiler@0.1.0 **/
export function compile(filename: string, source: string, format: OutputFormat): CompileOutcome;
export function compileMulti(files: Array<[string, string]>, format: OutputFormat): CompileOutcome;
export function parseToJson(source: string): string;
export function check(filename: string, source: string): Array<Diagnostic>;
/**
 * # Variants
 * 
 * ## `"rust"`
 * 
 * ## `"wit"`
 * 
 * ## `"wasm"`
 * 
 * ## `"wast"`
 * 
 * ## `"hir"`
 * 
 * ## `"thir"`
 */
export type OutputFormat = 'rust' | 'wit' | 'wasm' | 'wast' | 'hir' | 'thir';
export interface CompileResult {
  rustCode: string,
  witCode: string,
  wasmBytes: Uint8Array,
  wastCode: string,
  hirCode: string,
  thirCode: string,
}
export interface Diagnostic {
  message: string,
  rendered: string,
  line: number,
  column: number,
  length: number,
  severity: string,
}
export type CompileOutcome = CompileOutcomeSuccess | CompileOutcomeFailure;
export interface CompileOutcomeSuccess {
  tag: 'success',
  val: CompileResult,
}
export interface CompileOutcomeFailure {
  tag: 'failure',
  val: Array<Diagnostic>,
}
