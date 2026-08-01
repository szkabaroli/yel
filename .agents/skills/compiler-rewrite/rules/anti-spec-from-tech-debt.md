# anti-spec-from-tech-debt

> Write down the shapes the rewrite may not reproduce, or agents will rediscover every one of them

## Why It Matters

A subagent handed "rewrite THIR→LIR lowering, make it clean" has no way to know
that the previous author also intended to make it clean. The 8.5k-line
`blocks.rs` was not written by someone aiming for an 8.5k-line file; it grew one
locally-reasonable decision at a time. Ask an agent to solve the same problem
under the same local pressures and it will make the same decisions.

The defence is an **anti-spec**: an explicit, per-stage list of the failure
shapes that produced the current code, handed to the agent alongside the
contract. This is the highest-value artifact in the whole rewrite, and the
repository already contains its raw material — `docs/TECH_DEBT.md` is a
360-line catalogue of exactly what went wrong and where.

The anti-spec must name *shapes*, not incidents. "Don't put 8.5k lines in
blocks.rs" is useless — the new file has a different name. "No pass may both
allocate identifiers and decide control flow; split allocation from lowering"
is a rule an agent can actually apply to code that does not exist yet.

## Bad

```markdown
<!-- Agent brief -->
Rewrite the THIR→LIR lowering. The current implementation is in
lower_to_lir/blocks.rs and is a mess — please make it clean and modular.
```

"Clean" is a matter of taste and the agent's taste is not informed by this
codebase's scar tissue. It will produce something readable that reintroduces the
side-channel (`tree_shape`), because the side-channel is the locally easy way to
get boundary information across a pass boundary.

## Good

```markdown
<!-- Agent brief: THIR→LIR, anti-spec (violating any of these fails review) -->

Derived from docs/TECH_DEBT.md §1, §2, §6.

1. NO side-channel IR. Everything a later stage needs is in the IR node or an
   explicit side table keyed by a typed id — never a parallel structure the
   consumer must know to consult. (`tree_shape` is the cautionary case.)
2. NO god pass. Lowering must not simultaneously allocate ids, resolve names,
   decide block structure, and emit ops. Split into passes with named inputs
   and outputs; no pass over ~800 lines without a written reason.
3. NO domain vocabulary below the frontend seam. Nothing named `mount`,
   `boundary`, `component`, or `dom` may appear in the LIR-facing code —
   see cg-no-domain-vocabulary.
4. NO duplicated walkers. One visitor owns IR recursion; passes override arms.
   Four hand-written copies of one descent is the current state (§6.1).
5. NO permanent bridge. If you need an adapter between old and new
   representations, it ships with a deletion commit in the same PR series.
6. NO silent fallback. Unimplemented path => todo!("…") or a typed error.
```

Keep the anti-spec versioned next to the plan and update it when review finds a
*new* failure shape — the list is the accumulated memory of the rewrite.

## See Also

- [contract-before-fanout](contract-before-fanout.md) - The positive half of the same brief
- [`anti-side-channel-ir`](../../compiler-skills/rules/anti-side-channel-ir.md), [`anti-god-pass`](../../compiler-skills/rules/anti-god-pass.md), [`anti-permanent-bridge`](../../compiler-skills/rules/anti-permanent-bridge.md), [`anti-duplicate-walker`](../../compiler-skills/rules/anti-duplicate-walker.md) - The four shapes, in detail
