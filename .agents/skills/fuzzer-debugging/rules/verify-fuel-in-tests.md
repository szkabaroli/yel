# verify-fuel-in-tests

> Meter execution tests with fuel so a non-terminating loop traps fast instead of hanging the suite

## Why It Matters

The worst failure mode is a codegen bug that emits valid WASM which never
terminates — a wrong stride that corrupts the allocator free-list, a garbage
length that drives a multi-billion-iteration copy. With no metering the test
binary just stalls: no failure, no location, no output, and it takes the whole
suite (and CI) down with it. Wasmtime **fuel** converts this into a fast,
localized trap: the run consumes fuel and dies with "all fuel consumed" in a
fraction of a second, pointing at the call that looped. A generous ceiling only
ever catches genuine non-termination — real round-trips finish orders of
magnitude under it.

## Good

```rust
fn engine() -> Engine {
    let mut cfg = Config::new();
    cfg.wasm_component_model(true);
    cfg.wasm_gc(true);
    cfg.wasm_function_references(true);
    cfg.consume_fuel(true);              // <- meter execution
    Engine::new(&cfg).expect("engine")
}

// generous — real round-trips run well under this; a runaway burns it in ~ms
const STORE_FUEL: u64 = 5_000_000_000;
// in instantiate():
store.set_fuel(STORE_FUEL).expect("set fuel");
```

For an *ad hoc* run of a possibly-hanging test that isn't fuel-metered yet, wrap
it in an OS-level watchdog that kills the process group — a bare `cargo test &`
+ `kill` leaves the orphaned test binary running, so target the test binary by
name:

```bash
"$@" & CMD=$!; ( sleep "$LIMIT"; pkill -9 -P $CMD; kill -9 $CMD; pkill -9 -f "execution-" ) &
wait $CMD
```

The lesson from this session: the getter and setter for `list<tuple>`
round-tripped correctly — the hang was entirely in the *cleanup* path, which no
assertion looked at. Fuel would have pointed straight at it. Turn hangs into
failures before you go looking.

## See Also

- [symptom-to-suspect](symptom-to-suspect.md) - The stride/cleanup shapes whose hangs fuel converts to fast failures
- [verify-roundtrip-not-validate](verify-roundtrip-not-validate.md) - The other half of proving a fix
