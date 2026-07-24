# Async UI and WASI 0.3 Proposal

## Status

**Draft.** This proposal introduces native async functions and a small set of
UI-owned async primitives for Yel. It is designed to target the WebAssembly
Component Model's WASI 0.3 async ABI while keeping ordinary UI code declarative
and synchronous.

## Goals

- Map host-bound async operations directly to WASI 0.3 `async func`.
- Let component and in-tree global code perform ordinary async work.
- Make loading, error, cancellation, and stale-result handling safe by default
  for UIs.
- Support ongoing host event feeds through WASI 0.3 `stream<T>`.
- Add as little parser surface as possible.

## Non-goals

- Making template expressions suspend.
- Exposing Component Model continuations or raw task scheduling to ordinary UI
  code.
- Replacing existing reactive state, globals, or host-bound properties.
- Defining an implicit, unbounded application cache.

## Design overview

Yel has three different layers of asynchronous values:

| Layer                                   | Meaning                                  | Typical use                            |
| --------------------------------------- | ---------------------------------------- | -------------------------------------- |
| `future<T>`                             | One computation that will resolve to `T` | A call to an `async func`              |
| `stream<T>`                             | Zero or more `T` values over time        | A host event feed or byte body         |
| `resource<T, E>` / `subscription<T, E>` | UI-owned lifecycle wrappers              | Loading UI data or listening to a feed |

`future<T>` and `stream<T>` are Component Model/WASI boundary concepts.
`resource`, `action`, and `subscription` are Yel prelude constructs backed by
the runtime; they are never part of an exported WIT interface.

The only new reserved words proposed by this document are `async` and `await`.
`resource`, `action`, and `subscription` are prelude names, not keywords.

## Async functions

Async functions are declared by prefixing the existing function form with
`async`:

```yel
async func load-profile(id: string) -> result<Profile, ApiError> {
    await Api.profile(id)
}
```

Host-bound functions use the same syntax and lower to WIT/WASI 0.3 async
functions:

```yel
export global Api {
    profile: async func(id: string) -> result<Profile, ApiError>;
    save: async func(draft: Draft) -> result<(), SaveError>;
}
```

### Typing rules

Given an async function:

```text
async func(P) -> T
```

calling it has the operational type `future<T>`:

```text
Api.profile(user-id)        : future<result<Profile, ApiError>>
await Api.profile(user-id)  : result<Profile, ApiError>
```

`await` is valid only inside an `async func` or an `async { ... }` block.
An async function's declared result is its resolved result; `future` is not
written in the function declaration.

```yel
async func save-draft(draft: Draft) -> result<(), SaveError> {
    let prepared = prepare(draft);
    await Api.save(prepared)
}
```

The runtime lowers suspension to the Component Model async ABI. It must not
block the render thread or emulate async by polling synchronously.

## Rendering remains synchronous

Templates, interpolation, conditional rendering, list rendering, and computed
property initializers must remain synchronous. An `await` in any of those
contexts is a type error.

Async work instead updates reactive state through a `resource`, `action`,
`subscription`, or an explicit async event handler. This keeps each render
deterministic and prevents a slow host call from pausing the UI tree.

## Resources: one eventual UI result

A resource owns a restartable async operation and exposes its state to the UI.
It is a special runtime-backed type, not an ordinary record and not a future.

```yel
profile: resource<Profile, ApiError> = resource(async {
    await Api.profile(user-id)
})
```

The async block resolves to `result<Profile, ApiError>`. `resource(...)` stores
a restartable async thunk, not a single already-started future. The resulting
property has type `resource<Profile, ApiError>`.

The resource exposes:

```text
profile.pending : bool
profile.value   : option<Profile>
profile.error   : option<ApiError>
profile.start   : func()
profile.reload  : func()
profile.cancel  : func()
```

Example UI:

```yel
if let some(value) = profile.value {
    ProfileView { profile: value }
}

if profile.pending {
    Spinner {}
}

if let some(error) = profile.error {
    ErrorBanner {
        text: "{error}"
        clicked: { profile.reload(); }
    }
}
```

`if let` is shown here as a companion pattern-matching proposal; until it
exists, users can branch on a resource state value instead.

### Resource policies

Policies are option records, not language syntax:

```yel
profile: resource<Profile, ApiError> = resource(
    async { await Api.profile(user-id) },
    {
        key: some(user-id),
        start: ResourceStart.eager,
        retain-previous: true,
        retry: some({
            max-attempts: 3,
            initial-delay: 250ms,
            max-delay: 5s,
        }),
    },
)
```

The initial standard policy is intentionally small:

| Option            | Default               | Meaning                                                                               |
| ----------------- | --------------------- | ------------------------------------------------------------------------------------- |
| `key`             | `none`                | A new key invalidates the prior operation and starts a new one.                       |
| `start`           | `ResourceStart.eager` | Start after the owner mounts; `manual` starts only through `.start()` or `.reload()`. |
| `retain-previous` | `true`                | Keep the last successful value while a refresh is pending.                            |
| `retry`           | `none`                | Retry only explicitly configured, bounded, transient failures.                        |

Resources use **latest-wins** semantics. When their key changes, the runtime
cancels the old computation where possible and always invalidates its result.
A late completion must never update the new resource state.

Cancellation and unmounting are not errors. They move the resource to `idle`
or invalidate it according to its owner; they must not populate `.error`.

## Actions: user-initiated async work

An action is a resource-like operation that only runs when explicitly invoked.
It is intended for mutation and command UI such as save, send, and delete.

```yel
save: action<(), SaveError> = action(
    async { await Api.save(draft) },
    { concurrency: ActionConcurrency.drop },
)

Button {
    "Save"
    disabled: save.pending
    clicked: { save.run(); }
}
```

`drop` ignores repeated invocations while pending. Future versions may add
`restart` and `queue`; mutation actions must not retry automatically by
default because repeating a command can duplicate its effects.

## Streams and subscriptions

A `stream<T>` is a raw Component Model sequence of values. Creating a stream
need not itself be async:

```yel
export global Chat {
    events: func(room-id: string) -> stream<ChatEvent>;
}
```

The stream's next value may suspend. A `subscription` owns that consumption
for the UI:

```yel
chat-events: subscription<ChatEvent> = subscription(
    Chat.events(room-id),
    {
        key: some(room-id),
        on-item: { event ->
            if event.kind == ChatEvent.message {
                messages = [...messages, event.message];
            }
        },
    },
)
```

When opening a stream itself can fail or suspend, the source may instead be an
async function returning `result<stream<T>, E>`:

```yel
export global Chat {
    connect: async func(room-id: string)
        -> result<stream<ChatEvent>, StreamError>;
}

chat-events: subscription<ChatEvent, StreamError> = subscription(async {
    await Chat.connect(room-id)
})
```

Subscriptions expose at least `pending`, `active`, `error`, `stop()`, and
`restart()`. On a key change or owner teardown, the runtime closes/drops the
old stream and ignores late delivery.

Subscription delivery policies are also option values:

```yel
cursor: subscription<CursorPosition> = subscription(
    Editor.events(),
    { delivery: Delivery.latest }
)

logs: subscription<LogEntry> = subscription(
    System.logs(),
    { delivery: Delivery.batch(16ms), buffer: 1000 }
)
```

`latest` is appropriate for fast-changing state such as cursor position.
Bounded buffering or batching is appropriate for logs and high-volume feeds.
An implementation must never create an unbounded UI queue by default.

## Ownership and scope

Resources, actions, and subscriptions are not serializable data and cannot
appear in records, variants, component boundary APIs, or WIT-bound members.

They may be declared:

- in a component, where each mounted instance owns its own work; and
- in an in-tree global, where the application runtime owns one shared instance.

They may not be `in`, `out`, `in-out`, or host-bound function members of a
global. For example, this is valid application-lifetime state:

```yel
global Session {
    user: resource<User, ApiError> = resource(async {
        await AccountApi.current-user()
    })
}
```

An in-tree global subscription deliberately lasts for the application lifetime.
It is suitable for a shared session, notification feed, or shared connection;
application shutdown is responsible for final teardown.

## Error model

Expected application failures use the existing `result<T, E>` type. A resource
turns its resolved `ok(T)` or `err(E)` into UI-visible value/error state.

Runtime traps and ABI/protocol failures must either be mapped by the host into
the declared `E` or reported as a distinct runtime failure; they must not be
silently converted to `none`.

## Implementation stages

1. Add `async`, `await`, async function types, and WASI 0.3 async lowering.
2. Add component-scoped `resource` with eager/manual start, latest-wins
   invalidation, pending/value/error, and cancellation on unmount.
3. Add `action` for event-driven work with single-flight (`drop`) behavior.
4. Add `stream<T>` boundary support and `subscription` with bounded delivery.
5. Add global ownership, retry/backoff options, and higher-level loading
   boundaries only when applications demonstrate the need.

This order keeps the initial feature set small while establishing the core
invariant: native async work can suspend without ever blocking UI rendering.
