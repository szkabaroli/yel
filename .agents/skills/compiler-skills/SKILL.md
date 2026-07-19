---
name: compiler-skills
description: >
  Compiler-construction guidelines distilled from the yel compiler (a
  reactive-UI language that lowers AST → HIR → THIR → LIR → WASM). 54 rules
  across 12 categories covering IR design, interning, typed indices, the
  compilation context, pass/pipeline architecture, diagnostics & error
  recovery, type checking, name resolution, codegen/backend decoupling,
  testing compilers, compiler performance, and anti-patterns. Use when writing, reviewing, or refactoring a compiler,
  interpreter, or any multi-stage program transformation. Invoke with
  /compiler-skills.
license: Apache-2.0
metadata:
  author: yel
  version: "1.0.0"
  sources:
    - The yel compiler (crates/yel-core, crates/yel-wasm-codegen, crates/yelc)
    - rustc dev guide (TyCtxt, HIR/THIR/MIR, query system)
    - "Crafting Interpreters" — Robert Nystrom
    - "Engineering a Compiler" — Cooper & Torczon
    - Cranelift, Salsa, rust-analyzer idioms
---

# Compiler Construction Best Practices

Guidelines for building correct, maintainable, fast compilers and program
transformers. The 53 rules below are grounded in the **yel** compiler — a
reactive-UI language whose front-end lowers source through a stack of
intermediate representations (`AST → HIR → THIR → LIR`) and emits a
WebAssembly component. This is the house playbook for working in **this** repository — every rule reflects how yel is actually built and where it's heading, not general compiler theory.

## When to Apply

Reference these guidelines when:
- Designing or adding an intermediate representation (IR) layer
- Writing a lowering, desugaring, or normalization pass
- Implementing a type checker, name resolver, or semantic-analysis pass
- Building or extending the diagnostics / error-reporting subsystem
- Wiring up a compilation driver or pass pipeline
- Designing the backend / code generator and its IR↔target boundary
- Choosing how to identify, intern, and store program entities
- Writing tests for a compiler (snapshots, diagnostic fixtures, execution)

## Rule Categories by Priority

| Priority | Category | Impact | Prefix | Rules |
|----------|----------|--------|--------|-------|
| 1 | IR Design | CRITICAL | `ir-` | 7 |
| 2 | Identifiers & Indices | CRITICAL | `id-` | 4 |
| 3 | Interning | CRITICAL | `intern-` | 4 |
| 4 | Diagnostics & Error Recovery | CRITICAL | `diag-` | 7 |
| 5 | Compilation Context | HIGH | `ctx-` | 3 |
| 6 | Pass & Pipeline Architecture | HIGH | `pass-` | 6 |
| 7 | Type Checking | HIGH | `ty-` | 3 |
| 8 | Name Resolution | HIGH | `res-` | 3 |
| 9 | Codegen & Backend | MEDIUM | `cg-` | 7 |
| 10 | Testing Compilers | MEDIUM | `test-` | 5 |
| 11 | Performance | MEDIUM | `perf-` | 1 |
| 12 | Anti-patterns | REFERENCE | `anti-` | 4 |

---

## Quick Reference

### 1. IR Design (CRITICAL)

- [`ir-layered-lowering`](rules/ir-layered-lowering.md) - Use a distinct IR per abstraction level; each lowering adds information and removes ambiguity
- [`ir-kind-span-struct`](rules/ir-kind-span-struct.md) - Model a node as `{ kind: …Kind, span, … }` — separate the variant payload from per-node metadata
- [`ir-box-large-variant`](rules/ir-box-large-variant.md) - Box recursive children in tree-shaped IRs to keep enum size small
- [`ir-handles-over-boxes`](rules/ir-handles-over-boxes.md) - In flat/late IRs, reference children by id handles into a side table, not `Box`
- [`ir-preserve-spans`](rules/ir-preserve-spans.md) - Carry source spans through every IR so any later phase can still point at the user's code
- [`ir-side-tables`](rules/ir-side-tables.md) - Store analysis and derived results in side tables keyed by id, not by mutating IR nodes
- [`ir-lower-away-domain`](rules/ir-lower-away-domain.md) - Lower frontend/domain concepts (UI `Node`, `Signal`, dependency `Effect`s) out before the lowest IR — keep it target-generic

### 2. Identifiers & Indices (CRITICAL)

- [`id-newtype-index`](rules/id-newtype-index.md) - Wrap every entity index in a `u32` newtype (`DefId`, `ExprId`, …), never pass raw `usize`
- [`id-indexvec`](rules/id-indexvec.md) - Store entities in a typed `IndexVec<I, T>` keyed by their own id type
- [`id-invalid-sentinel`](rules/id-invalid-sentinel.md) - Reserve an explicit invalid sentinel and an `is_valid()` check instead of overloading `0`
- [`id-stable-across-passes`](rules/id-stable-across-passes.md) - Keep correlation ids stable across lowerings so later passes can tie output back to source constructs

### 3. Interning (CRITICAL)

- [`intern-strings`](rules/intern-strings.md) - Intern identifiers/strings to small handles for O(1) equality and shared storage
- [`intern-types`](rules/intern-types.md) - Intern types so structural equality collapses to an integer comparison
- [`intern-preintern-constants`](rules/intern-preintern-constants.md) - Pre-intern common types/symbols at fixed indices and assert the constants on startup
- [`intern-dedupe-tables`](rules/intern-dedupe-tables.md) - Back every interner with a cache map so equal inputs return the same handle

### 4. Diagnostics & Error Recovery (CRITICAL)

- [`diag-accumulate-continue`](rules/diag-accumulate-continue.md) - Collect diagnostics into a sink and keep going; don't abort on the first error
- [`diag-error-type-recovery`](rules/diag-error-type-recovery.md) - Poison failed nodes with an `Error` type/value and continue, suppressing cascade errors
- [`diag-no-silent-fallback`](rules/diag-no-silent-fallback.md) - Never emit placeholder/dummy IR for unimplemented paths — fail loudly with `todo!()`
- [`diag-exhaustive-match`](rules/diag-exhaustive-match.md) - When matching on an op/node/kind enum, don't let a catch-all arm swallow unhandled variants — make the gap explicit with `todo!()` or drop the wildcard
- [`diag-spans-everywhere`](rules/diag-spans-everywhere.md) - Attach a source span to every diagnostic; a message without a location is half a bug report
- [`diag-builder-messages`](rules/diag-builder-messages.md) - Build diagnostics fluently; write lowercase, punctuation-free messages with notes for detail
- [`diag-error-codes`](rules/diag-error-codes.md) - Give diagnostics stable error codes so users, docs, and tests can refer to them

### 5. Compilation Context (HIGH)

- [`ctx-central-context`](rules/ctx-central-context.md) - Put interners, definitions, source map, and diagnostics on one shared context (`TyCtxt`-style)
- [`ctx-thread-through-passes`](rules/ctx-thread-through-passes.md) - Thread `&ctx` / `&mut ctx` explicitly through every phase; avoid global mutable state
- [`ctx-interior-mutability`](rules/ctx-interior-mutability.md) - Use `RefCell`/`Cell` for monotonic side tables and id counters that grow during otherwise-shared passes

### 6. Pass & Pipeline Architecture (HIGH)

- [`pass-explicit-phases`](rules/pass-explicit-phases.md) - Make the pipeline a sequence of named phases, each consuming one IR and producing the next
- [`pass-register-then-lower`](rules/pass-register-then-lower.md) - Register all declarations before lowering any body, so forward references resolve
- [`pass-lowering-struct`](rules/pass-lowering-struct.md) - Encapsulate per-body lowering state (counters, scopes, output buffers) in a dedicated struct
- [`pass-visitor-recurse`](rules/pass-visitor-recurse.md) - Walk IR with recursive `match`-based visitors; keep analysis passes read-only
- [`pass-deferred-emission`](rules/pass-deferred-emission.md) - Pre-allocate ids and defer emitting bodies until their dependencies are known
- [`pass-postpass-dedupe`](rules/pass-postpass-dedupe.md) - Deduplicate structurally-identical output in a separate post-pass via normalized hashing

### 7. Type Checking (HIGH)

- [`ty-bidirectional`](rules/ty-bidirectional.md) - Use bidirectional checking: an `Infer` (synthesize) mode and a `Check(expected)` mode
- [`ty-record-typemap`](rules/ty-record-typemap.md) - Record a `span → type` map during checking to power IDE hover, completion, and tooling
- [`ty-literal-polymorphism`](rules/ty-literal-polymorphism.md) - Keep untyped literals polymorphic and resolve them against the expected type at the boundary

### 8. Name Resolution (HIGH)

- [`res-namespaced-defs`](rules/res-namespaced-defs.md) - Key the definition table by `(name, namespace)` so types, values, and components can share a name
- [`res-scope-stack`](rules/res-scope-stack.md) - Manage locals with a push/pop scope stack that supports shadowing
- [`res-builtin-registry`](rules/res-builtin-registry.md) - Register builtins/prelude into the same def table and cache their ids in a known-definitions struct

### 9. Codegen & Backend (MEDIUM)

- [`cg-late-binding-refs`](rules/cg-late-binding-refs.md) - Emit symbolic references in the IR and resolve them to concrete target indices at codegen time
- [`cg-arena-traits`](rules/cg-arena-traits.md) - Abstract IR storage behind read traits so multiple frontends can reuse one backend
- [`cg-lower-to-primitives`](rules/cg-lower-to-primitives.md) - Lower high-level constructs to generic target ops during lowering, not via a runtime library
- [`cg-repr-single-source`](rules/cg-repr-single-source.md) - Funnel "how is a value represented on the target" through one module, never per-emit-site
- [`cg-flatten-at-boundary`](rules/cg-flatten-at-boundary.md) - Keep the internal representation typed; flatten to the flat ABI only at FFI / ABI boundaries
- [`cg-debug-names`](rules/cg-debug-names.md) - Emit a name/debug section so generated artifacts stay inspectable
- [`cg-no-domain-vocabulary`](rules/cg-no-domain-vocabulary.md) - The backend must consume only generic ops/types — never branch on a source-domain concept

### 10. Testing Compilers (MEDIUM)

- [`test-snapshot-golden`](rules/test-snapshot-golden.md) - Snapshot deterministic textual outputs (IR dumps, generated code) so drift shows up as a diff
- [`test-diagnostic-fixtures`](rules/test-diagnostic-fixtures.md) - Pin error messages with `source + expected` fixture pairs
- [`test-execution-e2e`](rules/test-execution-e2e.md) - Execute the emitted artifact end-to-end to catch "valid output, wrong behaviour" bugs
- [`test-known-bugs-ignore`](rules/test-known-bugs-ignore.md) - Track known bugs with `#[ignore]` + a reference; never soften an assertion to match wrong output
- [`test-deterministic-output`](rules/test-deterministic-output.md) - Sort and dedup collections before emitting so output is byte-stable across runs

### 11. Performance (MEDIUM)

- [`perf-fxhash`](rules/perf-fxhash.md) - Use a fast non-cryptographic hasher (`rustc-hash` / `FxHashMap`) for internal compiler maps — they have no DoS surface

### 12. Anti-patterns (REFERENCE)

- [`anti-duplicate-walker`](rules/anti-duplicate-walker.md) - Don't re-implement the IR-traversal `match` in every pass — factor it into one shared walker
- [`anti-side-channel-ir`](rules/anti-side-channel-ir.md) - Don't make the backend read an out-of-band representation to interpret the IR — encode intent as explicit ops + typed types
- [`anti-god-pass`](rules/anti-god-pass.md) - Don't let one pass or lowering struct accrete dozens of fields and thousands of lines — split by concern
- [`anti-permanent-bridge`](rules/anti-permanent-bridge.md) - When migrating a representation, converge — a `legacy_*` shim or flag-gated second codepath only stays healthy while it's shrinking

---

## How to Use

This skill provides rule identifiers for quick reference. When generating or
reviewing compiler code:

1. **Identify the layer** you're working in (front-end IR, semantic analysis,
   backend, driver, or tests).
2. **Apply rules** with the matching prefix.
3. **Prioritize** CRITICAL > HIGH > MEDIUM.
4. **Read rule files** in `rules/` for detailed before/after examples.

### Rule Application by Task

| Task | Primary Categories |
|------|-------------------|
| New IR layer | `ir-`, `id-`, `intern-` |
| New lowering / desugaring pass | `pass-`, `ir-`, `diag-` |
| Type checker / inference | `ty-`, `diag-`, `intern-` |
| Name resolution / scoping | `res-`, `id-`, `ctx-` |
| Diagnostics work | `diag-`, `ir-` |
| Compiler driver / orchestration | `ctx-`, `pass-` |
| Backend / code generation | `cg-`, `ir-`, `id-` |
| Tests | `test-` |
| Performance tuning | `perf-`, `intern-`, `id-` |
| Code review / refactoring | `anti-`, `pass-`, `ir-` |

---

## Sources & Attribution

These rules are an original synthesis of patterns observed in the yel compiler
and well-known compiler-engineering literature. They are not affiliated with or
endorsed by any of the projects referenced.

- The yel compiler source (this repository)
- [Rust Compiler Development Guide](https://rustc-dev-guide.rust-lang.org/) — HIR/THIR/MIR, `TyCtxt`, interning, queries
- [Crafting Interpreters](https://craftinginterpreters.com/) — Robert Nystrom
- *Engineering a Compiler* — Keith Cooper & Linda Torczon
- [Cranelift](https://cranelift.dev/), [Salsa](https://github.com/salsa-rs/salsa), and [rust-analyzer](https://rust-analyzer.github.io/) architecture notes
