# Mail — Yel port of the Svelte mail panel

This example translates the Svelte `mail/+page.svelte` (the workspace mail
app) into the Yel DSL + a Rust backend.

## Layout

```
src/
├── yel/
│   ├── types.yel      # Records / enums / variants shared with WIT
│   ├── elements.yel   # Host widget schemas (Dialog, DropdownMenu, Icon, …)
│   └── mail_panel.yel # UI components + the main MailPanel export
└── rust/
    └── mail_backend.rs  # Implements the MailBackend import interface
```

## Separation of concerns

**Yel (UI only):**

- tree structure (heading, list, detail pane, compose dialog)
- reactive property bindings (`content: {...}`, `color: ...`)
- local UI state: `compose-open`, `reply-body`, `forward-to`, local
  composer buffers
- event handlers that dispatch to backend commands

**Rust (everything else):**

- JMAP session, cache, auth
- selection model (`BTreeSet<Selection>`), shift-click ranges, keyboard
  focus ring
- threading (`thread-id` grouping + sorting + header/member flattening)
- filtering and sorting pipeline (matches `visibleMessages` → `flatRows`
  derivation from the Svelte file)
- tint / rounding / divider pass so Yel has zero layout arithmetic
- date formatting (`Intl.DateTimeFormat` → `time::OffsetDateTime`)
- byte-size display, slugification, smart-card handle table
- label persistence, prompt wiring, optimistic update + server patch

The Yel component receives only primitives and typed records, so swapping
in a different backend (local SQLite, mock, different mail protocol) is a
matter of re-implementing `MailBackend`.

## Feature parity with the Svelte source

| Svelte feature                              | Status in Yel port                                     |
|---------------------------------------------|---------------------------------------------------------|
| Mailbox & label routing via `page.params`   | ✅ `boot()` reads host params; dispatches signals       |
| Visible message filtering                   | ✅ backend pipeline (`push_rows`)                       |
| Threading + expand/collapse                 | ✅ `toggle-thread-expanded`, pre-flattened `flat-rows`  |
| Search box                                  | ✅ `set-search` debounced on the host side              |
| Filter + sort menus                         | ✅ `FilterSortMenu` component                           |
| Selection (click / cmd-click / shift-click) | ✅ `select-only` / `toggle-select` / `extend-selection-to` |
| Keyboard shortcuts                          | ✅ `KeybindingAction` elements + `handle-action`        |
| Mark read/unread, flag, move                | ✅ `toggle-read`, `toggle-flag`, `bulk-move`            |
| Per-message label add/remove + create       | ✅ `update-message-labels`, `create-label-from-prompt`  |
| Reply / reply-all / forward composers       | ✅ `ReplyComposer` with local buffers                   |
| Compose-new dialog                          | ✅ `ComposeDialog`                                      |
| Smart-card carousel                         | ✅ `SmartCardCarousel`                                  |
| Multi-select stack (up to 5)                | ✅ `SelectionStack` with `selection-preview`            |
| Storage quota meter                         | ✅ `StorageFooter`                                      |

## Known gaps — things Yel cannot express today

These came up during the port and are documented here so the compiler work
can close them in order of impact:

1. **Named slots / snippets.** Svelte used `{#snippet heading}`,
   `{#snippet sidebarAction}`, `{#snippet detail}` to hand layout regions to
   a parent. Yel's element content is positional only. The port uses a
   `slot:` attribute convention the host recognises; this is a stopgap and
   will need a real slots feature in the grammar (see
   `crates/yel-core/src/syntax/grammar.pest`).
2. **Imperative DOM refs.** `bind:this={detailScrollEl}` was used to anchor
   the `ComposeBox` scroll container. `ScrollArea` now takes an
   `anchor-id`; the backend resolves it through the host.
3. **`prompt()` for label creation.** Yel has no synchronous host dialog
   primitive. The backend calls `host::prompt_for_label_name()` and the
   host re-enters `add-label` / `update-message-labels` with the result.
4. **`Set<T>` / `Map<K,V>` in Yel types.** The selection set is flattened
   to `list<string>` when pushed; ordering is maintained server-side.
5. **Intl formatters.** `Intl.DateTimeFormat` and `toLocaleString` are
   replaced by `format-full-date` on the backend; messages carry
   pre-formatted `date-display` / `date-full` strings.
6. **Dynamic class strings.** Svelte merged tailwind classes for tint,
   merge, rounding. Yel has no class-binding primitive — rounding and tint
   are passed as typed properties (`round-top`, `round-bottom`, `tint`) and
   the host component implements the visual mapping.
7. **Pattern matching.** The `selectedMailbox` → icon lookup is a chain of
   `if/else-if` today (`MailboxIcon`). A `match` or exhaustive `when`
   expression would collapse this.
8. **Array helpers on Yel collections.** `.filter(...)`, `.map(...)`,
   `.sort(...)` only partially exist (grammar has `filter` special-cased
   in codegen). The port keeps all list wrangling on the Rust side, which
   is arguably the right call regardless.

## Building

```
# WIT interface is emitted from types.yel + elements.yel
yelc compile -o wit src/yel/types.yel src/yel/elements.yel src/yel/mail_panel.yel > target/mail.wit

# WASM component
yelc compile -o wasm src/yel/types.yel src/yel/elements.yel src/yel/mail_panel.yel > target/mail_panel.wasm

# Rust backend
cargo component build --release -p mail-backend
```

The two components compose through the `MailBackend` world; the host
(browser or native wasmtime) wires them via `wit-component`.
