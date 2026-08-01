# oracle-never-rebless

> `UPDATE_SNAPSHOTS=1` run against the new compiler converts your test suite into a tautology

## Why It Matters

This is the single most common way a compiler rewrite ships broken and nobody
notices. The mechanism is banal: a rewritten stage changes 40 WIT files, the
diff is large and boring, the change "looks intentional," someone runs
`INSTA_UPDATE=always` or `UPDATE_SNAPSHOTS=1`, and the suite goes green. From
that commit onward the golden files assert that the new compiler produces what
the new compiler produces. The oracle is gone and nothing in CI says so.

The rewrite has a specific property that makes re-blessing especially lethal:
**a rewrite is not supposed to change output at all.** A refactor that changes
40 goldens has, by definition, changed observable behaviour — which is either a
bug or a scope violation. Either way the correct response is to investigate all
40, not to accept them.

There are exactly two legitimate reasons a golden changes during the rewrite,
and both require the diff to be read line by line and justified in the PR body:
a known-bug fixture that now compiles (a win — graduate it to `positive/`), or
an agreed, written-down output change (a new WIT convention). "It was easier"
and "the new IR naturally orders it differently" are not on the list — ordering
differences are a determinism bug (`test-deterministic-output`), not an
aesthetic.

## Bad

```bash
# Stage 4 rewritten; 40 WIT snapshots differ.
UPDATE_SNAPSHOTS=1 cargo test -p yel-wasm-codegen
INSTA_UPDATE=always cargo test -p yelc --test snapshot
git commit -am "rewrite: lower_to_lir; update snapshots"
```

The commit is self-certifying. If two of those 40 encode a real miscompilation,
they are now the expected output and the execution tests that would have caught
them were probably "updated" in the same sweep.

## Good

```bash
# Snapshots differing is the FINDING, not a chore before the finding.
cargo test -p yel-wasm-codegen 2>&1 | grep -c 'WIT mismatch'   # 40

# Diff one, understand it, classify it, and only then decide.
diff <(git show HEAD:crates/yel-wasm-codegen/tests/fixtures/positive/callbacks.wit) \
     crates/yel-wasm-codegen/tests/fixtures/positive/callbacks.wit
```

Classify every changed golden into exactly one bucket, in the PR body:

| Bucket | Action |
|--------|--------|
| Bug in the new stage | Fix the stage. The golden does not change. |
| Known-bug fixture now compiles | Graduate the fixture to `positive/`, note it in the PR. |
| Agreed output change | Cite the decision. Re-bless *only these files*, individually. |
| Non-deterministic ordering | Determinism bug — sort at the source, don't accept the new order. |

If the bucket is unclear, it is bucket one. Re-blessing in bulk is never
correct; if you cannot enumerate the changed files in a sentence each, the stage
is not ready to land.

## See Also

- [oracle-freeze-behaviour](oracle-freeze-behaviour.md) - Building the corpus this rule protects
- [verify-ratchet-never-down](verify-ratchet-never-down.md) - The number that catches what goldens miss
- [`test-deterministic-output`](../../compiler-skills/rules/test-deterministic-output.md) - Why an "ordering-only" diff is still a bug
