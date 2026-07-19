# ctx-central-context

> Put interners, definitions, source map, and diagnostics on one shared context (`TyCtxt`-style)

## Why It Matters

A compiler has global state every phase needs: string and type interners, the definition table, the source map, and the diagnostics sink. Scattering these as separate parameters bloats signatures and invites them drifting out of sync. yel follows rustc's `TyCtxt` model: a single `CompilerContext` owns all global state, and convenience methods (`intern()`, `ty_kind()`, `alloc_block_id()`) hang off it so any phase interns, resolves, and reports through one handle.

## Bad

```rust
fn type_check(
    interner: &Interner,
    types: &TypeInterner,
    defs: &Definitions,
    source_map: &SourceMap,
    diagnostics: &mut Diagnostics,
    known: &KnownDefinitions,
) { /* six params, easy to misorder */ }
```

## Good

```rust
pub struct CompilerContext {
    pub interner: Arc<Interner>,
    pub types: TypeInterner,
    pub defs: Definitions,
    pub source_map: SourceMap,
    pub diagnostics: Diagnostics,
    pub dom_imports: Option<DomImports>,
    pub known: KnownDefinitions,
}

fn type_check(ctx: &mut CompilerContext) {
    let sym = ctx.intern("count");
    let kind = ctx.ty_kind(ty);
    // resolve, intern, report — all through one handle
}
```

## See Also

- [ctx-thread-through-passes](ctx-thread-through-passes.md) - Pass this context explicitly into every phase
- [intern-strings](intern-strings.md) - The interner that lives on the context
- [res-builtin-registry](res-builtin-registry.md) - Known/builtin definitions hang off the context
