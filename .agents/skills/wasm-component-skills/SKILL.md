---
name: wasm-component-skills
description: >
  Guidelines for compiling a language to a WebAssembly Component Model
  artifact, distilled from the yel back-end (`yel-wasm-codegen`, which lowers
  LIR → a `wasm:component` with a WIT world, canonical-ABI lift/lower shims,
  WASM-GC typed state, and a linear-memory boundary). 32 rules across 8
  categories: component assembly, WIT interface design, the canonical ABI,
  linear memory & strings, WASM-GC types, the host contract, validation &
  tooling, and anti-patterns. Use when emitting a component, designing its WIT,
  flattening values across the ABI, or wiring host imports. Invoke with
  /wasm-component-skills.
license: Apache-2.0
metadata:
  author: yel
  version: "1.0.0"
  sources:
    - The yel back-end (crates/yel-wasm-codegen, crates/yelc)
    - WebAssembly Component Model spec & Canonical ABI (component-model/design)
    - The bytecodealliance `wasm-tools` / `wit-component` / `wit-encoder` crates
    - WASM-GC proposal (typed structs/arrays)
---

# Compiling to the WebAssembly Component Model

Guidelines for lowering a language to a **WebAssembly component** — a core
module wrapped with a typed WIT interface, talking to its host through the
canonical ABI. The 32 rules below are grounded in **yel**, whose back-end
(`yel-wasm-codegen`) compiles a generic block IR (LIR) into a component that is
GC + linear-memory hybrid and speaks `yel:ui/dom@0.1.0` to its host. Where a rule
turns on a canonical-ABI detail (despecialization, discriminant sizing, the
flattening join, lift-time traps), it is grounded directly in the Component
Model's Canonical ABI reference. This is the
house playbook for emitting components in **this** repository — every rule
reflects how the back-end is actually built, not component-model theory in the
abstract.

## When to Apply

Reference these guidelines when:
- Assembling a component from a core module (encoder, embedded WIT metadata)
- Designing or generating the component's WIT world, interfaces, and resources
- Crossing the canonical ABI — flattening aggregates, writing lift/lower shims
- Laying out strings, lists, and aggregates in linear memory
- Choosing the internal representation of a value (scalar / GC ref / memory)
- Importing host capability through a versioned WIT interface
- Validating the emitted artifact or snapshotting its interface

## Rule Categories by Priority

| Priority | Category | Impact | Prefix | Rules |
|----------|----------|--------|--------|-------|
| 1 | Component Assembly | CRITICAL | `comp-` | 4 |
| 2 | WIT Interface Design | CRITICAL | `wit-` | 4 |
| 3 | Canonical ABI | CRITICAL | `abi-` | 8 |
| 4 | Linear Memory & Strings | HIGH | `mem-` | 6 |
| 5 | WASM-GC Types | HIGH | `gc-` | 3 |
| 6 | Host Contract & Imports | MEDIUM | `host-` | 2 |
| 7 | Validation & Tooling | MEDIUM | `val-` | 2 |
| 8 | Anti-patterns | REFERENCE | `anti-` | 3 |

---

## Quick Reference

### 1. Component Assembly (CRITICAL)

- [`comp-encode-from-core`](rules/comp-encode-from-core.md) - Emit a core module, then wrap it into a component with an encoder + embedded WIT metadata; never hand-assemble component sections
- [`comp-real-module-for-state`](rules/comp-real-module-for-state.md) - Emit a real core module (memory, allocator, start) whenever there's state; only stub truly empty modules with a dummy
- [`comp-string-encoding-explicit`](rules/comp-string-encoding-explicit.md) - Pin the canonical string encoding (UTF-8) at the embed step and match it everywhere strings are lowered
- [`comp-validate-on-encode`](rules/comp-validate-on-encode.md) - Encode with validation enabled; a component that fails to validate is worse than no output

### 2. WIT Interface Design (CRITICAL)

- [`wit-world-as-contract`](rules/wit-world-as-contract.md) - The WIT world *is* the component's contract; generate it from the IR and always produce one well-formed package, even for empty/library modules
- [`wit-resource-for-handles`](rules/wit-resource-for-handles.md) - Model stateful exported entities as WIT `resource`s with a constructor + methods; the component holds the state, the host holds an opaque handle
- [`wit-own-vs-borrow`](rules/wit-own-vs-borrow.md) - Transfer ownership with `own<T>`, lend transient access with `borrow<T>`; getting it wrong leaks or double-frees handles
- [`wit-single-type-owner`](rules/wit-single-type-owner.md) - Each named type/resource is owned by exactly one interface; others `use`/alias it — redefining mints two distinct types

### 3. Canonical ABI (CRITICAL)

- [`abi-flatten-at-boundary`](rules/abi-flatten-at-boundary.md) - Keep values in their rich internal repr; flatten to canonical-ABI scalars only inside the export/import lift-lower shims
- [`abi-respect-flattening-limit`](rules/abi-respect-flattening-limit.md) - The canonical ABI flattens only up to a fixed count; past it, values spill through a memory return-area pointer — compute signatures accordingly
- [`abi-lift-lower-shims`](rules/abi-lift-lower-shims.md) - The WIT signature is not the core signature; generate explicit lift (host→guest) and lower (guest→host) shims per exported/imported function
- [`abi-despecialize-first`](rules/abi-despecialize-first.md) - Despecialize tuple/enum/option/result/map down to record and variant before computing layout or flattening
- [`abi-variant-discriminant`](rules/abi-variant-discriminant.md) - Size a variant's discriminant by its case count, then place the payload at the maximum case alignment
- [`abi-flatten-variant-join`](rules/abi-flatten-variant-join.md) - Flatten a variant to its discriminant plus the positional join of all case payloads, and reinterpret values into the joined slots
- [`abi-trap-on-invalid-lift`](rules/abi-trap-on-invalid-lift.md) - Lifting must validate and trap on invalid inputs — bad chars, malformed UTF, out-of-bounds or misaligned lists, unknown discriminants — never silently accept
- [`abi-resource-handle-table`](rules/abi-resource-handle-table.md) - Cross resources as i32 handle-table indices, not pointers; lift/lower through the per-instance table and honor own-vs-borrow lifetimes

### 4. Linear Memory & Strings (HIGH)

- [`mem-fat-pointer-strings`](rules/mem-fat-pointer-strings.md) - Represent strings/lists as a `(ptr, len)` fat pointer into linear memory; funnel reads/writes through store/load helpers, not ad-hoc per-site memory ops
- [`mem-cabi-realloc`](rules/mem-cabi-realloc.md) - Export the canonical-ABI `cabi_realloc` so the host can allocate into your linear memory when lowering lists/strings in
- [`mem-return-buffer-ownership`](rules/mem-return-buffer-ownership.md) - Free returned linear-memory buffers in the guest's exported `post-return`, after the host has lifted them — never inline at return time
- [`mem-canonical-alignment`](rules/mem-canonical-alignment.md) - Lay out aggregates in linear memory with the canonical ABI's size/alignment rules, or the host reads garbage
- [`mem-list-elem-count`](rules/mem-list-elem-count.md) - A list is (ptr, length) where length counts elements, not bytes; store elements contiguously at the element's size and alignment
- [`mem-string-tagged-units`](rules/mem-string-tagged-units.md) - Encode strings as (ptr, code-units) under the negotiated encoding; the byte length and unit depend on utf8 / utf16 / latin1+utf16

### 5. WASM-GC Types (HIGH)

- [`gc-typed-internal`](rules/gc-typed-internal.md) - Use WASM-GC typed struct/array refs for internal aggregates — one ref slot, not a tuple of loose scalars
- [`gc-classify-once`](rules/gc-classify-once.md) - Decide scalar vs GC-ref vs linear-memory in one classifier consulted everywhere, never re-derived per emit site
- [`gc-hybrid-gc-and-memory`](rules/gc-hybrid-gc-and-memory.md) - GC and linear memory coexist in one component; use GC for typed internal state and linear memory at the ABI boundary, and keep dual-backed values coherent

### 6. Host Contract & Imports (MEDIUM)

- [`host-versioned-imports`](rules/host-versioned-imports.md) - Import host capability from a versioned WIT interface (`pkg:iface@x.y.z`) so host and guest can evolve compatibly
- [`host-import-roundtrip`](rules/host-import-roundtrip.md) - Give each host import a stable internal id and resolve it to a concrete wasm import index at codegen; never hardcode import indices

### 7. Validation & Tooling (MEDIUM)

- [`val-validate-component`](rules/val-validate-component.md) - Validate the artifact with a component-model-aware validator (`wasm-tools` / `Validator`), not a magic-number check
- [`val-snapshot-wit`](rules/val-snapshot-wit.md) - Snapshot the generated WIT (and any debug graph) so interface drift surfaces as a reviewable diff

### 8. Anti-patterns (REFERENCE)

- [`anti-hand-rolled-component`](rules/anti-hand-rolled-component.md) - Don't hand-emit component sections or canonical-function entries — drive it through the encoder
- [`anti-flatten-everywhere`](rules/anti-flatten-everywhere.md) - Don't thread flattened ABI scalars through internal calls; flatten once, at the boundary
- [`anti-placeholder-instructions`](rules/anti-placeholder-instructions.md) - Don't emit dummy core instructions for unimplemented paths — wrong stack shapes fail component validation and are untraceable from a hex dump

---

## How to Use

This skill provides rule identifiers for quick reference. When generating or
reviewing component-emitting code:

1. **Identify the layer** you're working in (component assembly, WIT, the ABI
   boundary, memory/GC representation, host imports, or validation).
2. **Apply rules** with the matching prefix.
3. **Prioritize** CRITICAL > HIGH > MEDIUM.
4. **Read rule files** in `rules/` for detailed before/after examples.

### Rule Application by Task

| Task | Primary Categories |
|------|-------------------|
| Wrapping a core module into a component | `comp-`, `val-` |
| Designing / generating the WIT | `wit-`, `host-` |
| Crossing the ABI (params, results, lists, strings) | `abi-`, `mem-` |
| Choosing a value's representation | `gc-`, `mem-`, `abi-` |
| Wiring host capability | `host-`, `wit-` |
| Verifying the output | `val-`, `comp-` |
| Code review / refactoring | `anti-`, `abi-`, `gc-` |

---

## Sources & Attribution

These rules are an original synthesis of patterns in the yel back-end and the
public Component Model specification and tooling. They are not affiliated with
or endorsed by the projects referenced.

- The yel back-end source (this repository — `crates/yel-wasm-codegen`)
- [The WebAssembly Component Model](https://component-model.bytecodealliance.org/) and the [Canonical ABI](https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md)
- [`wasm-tools`](https://github.com/bytecodealliance/wasm-tools) — `wit-component`, `wit-encoder`, `wasm-encoder`, the `Validator`
- [The WASM-GC proposal](https://github.com/WebAssembly/gc) — typed structs and arrays
