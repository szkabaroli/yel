# fuzz-categorize-signatures

> Bucket failures by error signature; fix the biggest cluster, not the first seed

## Why It Matters

With 100+ failing seeds, picking seed #1 and diving in is a lottery — you might
spend an afternoon on a construct that appears once. One root-cause bug usually
manifests as *many* seeds sharing an error signature, so the highest-leverage
move is to histogram the failures and attack the tallest bar. The biggest jumps
come from spotting that an entire cluster of seeds — often dozens sharing one
signature like `values remaining on stack` — is a single root cause; fixing it
clears the whole bucket at once.

Extract a *normalized* signature: strip offsets and type ids so `found (ref
null $type)` and `found (ref (exact $type))` don't fragment the count.

## Bad

```bash
# grab the first failing seed and start bisecting it blind
./target/release/yel-smith --seed 1 > /tmp/f.yel
# ... 200 lines, could be any of a dozen unrelated bugs
```

## Good

```bash
for s in $FAILING; do
  ./target/release/yel-smith --seed $s > /tmp/f.yel 2>/dev/null
  err=$(./target/release/yelc compile -o wasm /tmp/f.yel 2>&1 >/tmp/f.wasm)
  [ -z "$err" ] && err=$(wasm-tools validate /tmp/f.wasm 2>&1 | head -1)
  echo "$err" | grep -oiE "found \(?ref[^)]*|found (f32|f64|i64|i32)|values remaining|not yet supported|invalid IR: [a-z_]*"
done | sed 's/[0-9]//g' | sort | uniq -c | sort -rn
#  49  found (ref (exact $type      <- a composite-ref cluster: one root cause
#  42  found (ref null $type
#   7  invalid IR: getter
#   5  invalid IR: <some fn>         <- a loud, self-labeling gap: cheap
```

Loud `invalid IR: <fn_name>` and `not yet supported` buckets are often the
cheapest wins — the compiler already told you the function and the gap, so start
there when the tallest bar is a hard one.

## See Also

- [fuzz-delta-minimize](fuzz-delta-minimize.md) - Shrink one representative from the chosen bucket
- [symptom-to-suspect](symptom-to-suspect.md) - One bug can span produce/consume/free with three different signatures
