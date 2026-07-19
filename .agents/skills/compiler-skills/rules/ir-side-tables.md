# ir-side-tables

> Store analysis and derived results in side tables keyed by id, not by mutating IR nodes

## Why It Matters

Once you add an `analysis_result` field to an IR node, every node pays for it, every constructor must fill it, and only one analysis can own it. Keeping derived data in **side tables** keyed by a node's id (or span) keeps nodes small and uniform, lets independent passes attach their own data, and makes results trivial to discard or serialize separately. yel records type-checking results in a `TypeMap` (`span → Ty`) and signal analysis in `SignalDependencies` (`signal → [EffectSource]`) rather than bolting fields onto every `ThirExpr`; the interned `Ty`/`Name` tables are themselves side tables keyed by handle.

## Bad

```rust
struct ThirExpr {
    kind: ThirExprKind,
    ty: Ty,
    // every pass that wants to annotate bloats the node and fights the others:
    inferred_effects: Option<Vec<DefId>>,
    is_const: Option<bool>,
    lints: Vec<Lint>,
}
```

## Good

```rust
struct ThirExpr { kind: ThirExprKind, ty: Ty, span: Span }

// produced alongside the IR, keyed by id/span — discardable, composable:
struct TypeCheckResult { thir: ThirComponent, type_map: HashMap<Span, Ty> }
type EffectIndex = IndexVec<ExprId, Vec<DefId>>;
```

## See Also

- [id-indexvec](id-indexvec.md) - The natural backing store for id-keyed side tables
- [ty-record-typemap](ty-record-typemap.md) - The `span → Ty` side table in practice
- [intern-types](intern-types.md) - Interned tables are side tables keyed by handle
