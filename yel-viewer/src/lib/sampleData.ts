export interface CodeExample {
  id: string;
  name: string;
  code: string;
}

export const examples: CodeExample[] = [
  {
    id: "temp-converter",
    name: "Temperature Converter (7GUIs)",
    code: `// The classic 7GUIs temperature converter. Typing in either
// Input fires its \`set value:\` closure, which writes the other
// side via the formula. Programmatic writes don't fire the closure
// (matches Svelte \`oninput\` semantics exactly).
package yel:temperature@1.0.0;

export component TempConverter {
    celsius: f32 = 0.0;
    fahrenheit: f32 = 32.0;

    HStack {
        FloatInput {
            value: celsius
            set value: {
                fahrenheit = 32.0 + (9.0 / 5.0) * celsius;
            }
        }
        Text { "°C = " }
        FloatInput {
            value: fahrenheit
            set value: {
                celsius = (5.0 / 9.0) * (fahrenheit - 32.0);
            }
        }
        Text { "°F" }
    }
}`,
  },
  {
    id: "counter",
    name: "Counter",
    code: `package yel:counter@1.0.0;

record Person {
    name: string,
    age: u32,
}

enum status { pending, active, completed }

export component Counter {
    count: s32 = 0;
    label: string = "Count";
    items: list<Person> = [{ name: "Alice", age: 30 }];
    numbers: list<u32> = [1, 2, 3];

    incremented: func();

    VStack {
        Text { "{label}: {count}" }

        HStack {
            Button {
                "-"
                clicked: { count -= 1; }
            }
            Button {
                "+"
                clicked: { count += 1; incremented(); }
            }
        }

        if count > 10 {
            Text { "High count!" }
        } else if count < 0 {
            Text { "Negative!" }
        }

        Text { "Items: {items.len()} {items[0].name}" }

        for item in items key(item.name) {
            Text { "{item.name}" }

            for n in numbers key(n) {
                Text { "{n}" }
            }
        }
    }
}`,
  },
  {
    id: "counter-nested",
    name: "Counter (Nested Lists)",
    code: `package yel:counter@1.0.0;

record Item {
    name: string,
    subitems: list<string>,
}

export component Counter {
    count: s32 = 0;
    label: string = "Count";
    items: list<Item> = [
        { name: "Alice", subitems: ["a1", "a2"] },
        { name: "Bob", subitems: ["b1", "b2", "b3"] },
    ];

    VStack {
        Text { "{label}: {count}" }

        HStack {
            Button {
                "-"
                clicked: { count -= 1; }
            }
            Button {
                "+"
                clicked: { count += 1; }
            }
        }

        if count > 10 {
            Text { "High count!" }
        } else if count < 0 {
            Text { "Negative!" }
        }

        for item in items key(item.name) {
            VStack {
                Text { "{item.name}" }
                for sub in item.subitems key(sub) {
                    Text { "- {sub}" }
                }
            }
        }
    }
}`,
  },
  {
    id: "nested-for",
    name: "Nested For Loops",
    code: `package yel:nested@1.0.0;

record Item {
    name: string,
    count: u32,
}

export component NestedFor {
    items: list<Item> = [
        { name: "Alice", count: 2 },
        { name: "Bob", count: 3 },
    ];
    numbers: list<u32> = [1, 2, 3];

    VStack {
        for item in items key(item.name) {
            VStack {
                Text { "{item.name}" }
                HStack {
                    for n in numbers key(n) {
                        Text { "[{n}]" }
                    }
                }
            }
        }
    }
}`,
  },
  {
    id: "nested-parent",
    name: "Nested Parent Access",
    code: `package yel:nested-parent@1.0.0;

record Item {
    name: string,
    subitems: list<string>,
}

export component NestedParent {
    items: list<Item> = [
        { name: "Alice", subitems: ["a1", "a2"] },
        { name: "Bob", subitems: ["b1", "b2", "b3"] },
    ];

    VStack {
        for item in items key(item.name) {
            VStack {
                Text { "{item.name}" }
                for sub in item.subitems key(sub) {
                    Text { "- {sub}" }
                }
            }
        }
    }
}`,
  },
  {
    id: "checkerboard",
    name: "Checkerboard",
    code: `// Live-customisable checkerboard: type in the inputs, the grid
// re-renders. Each input uses a \`set value:\` closure (empty — we
// only want the auto-sync from the DOM's \`<input type="number">\`
// into the signal; no side effects).
package yel:checkerboard@1.0.0;

export component Checkerboard {
    rows: u32 = 8;
    cols: u32 = 8;
    cell-size: u32 = 24;

    VStack {
        HStack {
            Text { "Rows:" }
            IntegerInput { bind value: rows }
            Text { "Cols:" }
            IntegerInput { bind value: cols }
            Text { "Size:" }
            IntegerInput { bind value: cell-size }
        }
        VStack {
            for row in 0..rows {
                HStack {
                    for col in 0..cols {
                        if (row + col) % 2 == 0 {
                            Box {
                                style: "background-color: white; width: {cell-size}px; height: {cell-size}px;"
                            }
                        } else {
                            Box {
                                style: "background-color: dimgray; width: {cell-size}px; height: {cell-size}px;"
                            }
                        }
                    }
                }
            }
        }
    }
}`,
  },
  {
    id: "temperature",
    name: "Temperature Converter",
    code: `package yel:temperature@1.0.0;

export component TempConverter {
    celsius: f32 = 0.0;
    fahrenheit: f32 = 32.0;

    HStack {
        FloatInput {
            value: celsius
            set value: {
                fahrenheit = 32.0 + (9.0 / 5.0) * celsius;
            }
        }
        Text { "°C = " }
        FloatInput {
            value: fahrenheit
            set value: {
                celsius = (5.0 / 9.0) * (fahrenheit - 32.0);
            }
        }
        Text { "°F" }
    }
}`,
  },
  {
    id: "nested-components",
    name: "Nested Components",
    code: `package yel:nested-components@1.0.0;

// A simple nested component
component Nested {
    VStack {
        Text { "...and these styles won't!" }
    }
}

// Parent component that uses the Nested component
export component App {
    VStack {
        Text { "These styles..." }
        Nested { }
    }
}`,
  },
  {
    id: "crud",
    name: "CRUD (7GUIs)",
    code: `package yel:crud@1.0.0;

// 7GUIs CRUD Task
// https://eugenkiss.github.io/7guis/tasks#crud
//
// Challenges:
// - Separating domain and presentation logic
// - Managing mutation
// - Building a non-trivial layout
// - Filtering view by prefix (immediate, no submit)
// - BU/BD enabled only when entry selected

record Person {
    first: string,
    last: string,
}

export component Crud {
    // Domain: the source of truth
    people: list<Person> = [
        { first: "Hans", last: "Emil" },
        { first: "Max", last: "Mustermann" },
        { first: "Roman", last: "Tisch" },
    ];

    // Presentation state
    prefix: string = "";
    first: string = "";
    last: string = "";
    selected-index: s32 = -1;

    filtered: list<Person> = people.filter({ p -> p.last.starts-with(prefix) });
    selected: option<Person> = selected-index >= 0 ? some(filtered[selected-index]) : none;

    // TODO [UNSUPPORTED]: Effect to sync inputs when selection changes
    // effect(selected) {
    //     first = selected?.first ?? "";
    //     last = selected?.last ?? "";
    // }

    HStack {
        // Left panel: filter and listbox
        VStack {
            HStack {
                Text { "Filter prefix:" }
                TextInput {
                    value: prefix
                    set value: { selected-index = -1; }
                }
            }

            // Listbox L - shows filtered names
            // TODO: Should iterate over 'filtered' not 'people'
            Select {
                value: selected-index
                size: 5
                for person in people key(person.last) {
                    Option {
                        "{person.last}, {person.first}"
                    }
                }
            }
        }

        // Right panel: name editing
        VStack {
            HStack {
                Text { "Name:" }
                TextInput { value: first }
            }
            HStack {
                Text { "Surname:" }
                TextInput { value: last }
            }
        }
    }

    // Action buttons
    HStack {
        // BC: Create - always enabled
        Button {
            "Create"
            clicked: {
                // TODO [UNSUPPORTED]: list.append()
                // people.append({ first: first, last: last });
                selected-index = people.len() - 1;
            }
        }

        // BU: Update - enabled iff selected
        Button {
            "Update"
            disabled: selected-index < 0
            clicked: {
                // TODO [UNSUPPORTED]: let bindings, index-of, force unwrap
                // let idx = people.index-of(selected!);
                // people[idx].first = first;
                // people[idx].last = last;
            }
        }

        // BD: Delete - enabled iff selected
        Button {
            "Delete"
            disabled: selected-index < 0
            clicked: {
                // TODO [UNSUPPORTED]: let bindings, index-of, remove(at:)
                // let idx = people.index-of(selected!);
                // people.remove(at: idx);
                // selected-index = min(selected-index, people.len() - 1);  // min() is supported!
            }
        }
    }
}

// =====================================================
// UNSUPPORTED SYNTAX SUMMARY:
// =====================================================
//
// 1. list.filter({ p -> predicate })
//    - Filter list with closure
//
// 2. string.starts-with(prefix)
//    - String prefix matching
//
// 3. Person? (optional type shorthand)
//    - Currently must use option<Person>
//
// 4. cond ? a : none (ternary with none)
//    - Ternary works, but 'none' literal in this context
//
// 5. effect(dep) { body }
//    - Reactive effect that runs when dependency changes
//
// 6. selected?.first (optional chaining)
//    - Grammar supports it, but may not be fully wired
//
// 7. ?? (nil coalescing operator)
//    - e.g., selected?.first ?? ""
//
// 8. let x = expr; (local bindings in handlers)
//    - Variable declarations in event handlers
//
// 9. list.append(item)
//    - Mutating append to list
//
// 10. list.remove(at: index)
//     - Mutating remove with labeled argument
//
// 11. list.index-of(item)
//     - Find index of item in list
//
// 12. selected! (force unwrap)
//     - Unwrap optional, panic if none
// =====================================================`,
  },
  {
    id: "mail",
    name: "Mail app (globals-driven)",
    code: `package yel:mail@0.1.0;

// =====================================================
// A compact mail UI driven by a single 'MailStore' global.
// The host (Rust) owns every piece of business logic:
// JMAP session, filtering, sorting, threading, formatting.
// The Yel side is pure UI: tree structure, bindings, and
// event dispatch via callbacks on the global.
// =====================================================

enum Mailbox {
    inbox, sent, drafts, archive, spam, trash, flagged,
}

enum Filter { all, unread, flagged, attachments }

record Address {
    email: string,
    name: string,
}

record Message {
    id: string,
    mailbox: Mailbox,
    from: Address,
    subject: string,
    preview: string,
    body: string,
    date-display: string,
    read: bool,
    flagged: bool,
}

// Host-boundary: every field + callback here becomes part of the
// WIT interface the Rust backend implements. The component reads
// properties ('MailStore.current-mailbox'), the host pushes data
// via 'set-*' funcs, and the UI calls functions for user intents.
export global MailStore {
    in loading: bool;
    in current-mailbox: Mailbox;
    in current-filter: Filter;
    in visible-messages: list<Message>;
    in unread-inbox: s32;
    in unread-drafts: s32;

    // Selection + compose state — backend owns so bulk actions and
    // reply threads can mutate without prop-drilling.
    in selected-id: string;
    in has-selection: bool;
    in selected: Message;
    in compose-open: bool;
    in reply-body: string;

    select-mailbox: func(m: Mailbox);
    set-filter: func(f: Filter);
    select-message: func(id: string);
    clear-selection: func();
    toggle-flag: func(id: string);
    toggle-read: func(id: string);
    archive: func(id: string);
    delete: func(id: string);
    open-compose: func();
    close-compose: func();
    send-reply: func(body: string);
}

// Sidebar entry — one per mailbox.
component MailboxEntry {
    mailbox: Mailbox;
    title: string;
    unread: s32;
    active: bool;

    HStack {
        padding: 8px
        Text { content: title weight: active ? "medium" : "regular" }
        Spacer {}
        if unread > 0 {
            Badge { label: "{unread}" }
        }
        clicked: { MailStore.select-mailbox(mailbox); }
    }
}

// Single row in the visible-messages list. The host pre-builds
// display strings so Yel never touches Intl / Date / formatters.
component MessageRow {
    message: Message;
    selected: bool;

    HStack {
        padding: 10px
        background: selected ? #2563eb : #00000000

        VStack {
            Button {
                variant: "icon"
                Text { content: message.flagged ? "*" : " " }
                clicked: { MailStore.toggle-flag(message.id); }
            }
            if !message.read {
                Text { content: "." color: #f43f5e }
            }
        }

        VStack {
            grow: 1
            HStack {
                Text {
                    content: message.from.name
                    weight: message.read ? "regular" : "medium"
                }
                Spacer {}
                Text { content: message.date-display size: 12px }
            }
            Text { content: message.subject }
            Text { content: message.preview color: #9ca3af size: 12px }
        }

        clicked: { MailStore.select-message(message.id); }
    }
}

// Detail pane — shows the selected message with a reply composer.
component DetailPane {
    HStack {
        padding: 8px
        Button {
            "Reply"
            clicked: { MailStore.send-reply(MailStore.reply-body); }
        }
        Button { "Archive" clicked: { MailStore.archive(MailStore.selected.id); } }
        Button { "Delete"  clicked: { MailStore.delete(MailStore.selected.id); } }
        Spacer {}
        Button { "Close" clicked: { MailStore.clear-selection(); } }
    }

    VStack {
        padding: 16px
        Text { content: MailStore.selected.subject size: 18px weight: "medium" }
        Text {
            content: "{MailStore.selected.from.name} <{MailStore.selected.from.email}>"
            color: #9ca3af
        }
        Text { content: MailStore.selected.date-display color: #9ca3af size: 12px }
        Text { content: MailStore.selected.body }
    }

    // Inline reply. ComposeBox ships a value binding; host reads it
    // when send-reply fires.
    TextInput {
        placeholder: "Write a reply..."
        value: MailStore.reply-body
    }
}

// Compose dialog — trivial when 'compose-open' is false.
component ComposeDialog {
    body: string = "";
    to: string = "";

    if MailStore.compose-open {
        VStack {
            padding: 16px
            background: #0f172a
            Text { content: "New message" weight: "medium" }
            TextInput { placeholder: "To" value: to }
            TextInput { placeholder: "Write your message..." value: body }
            HStack {
                Button { "Send" clicked: {
                    MailStore.send-reply(body);
                    body = "";
                    to = "";
                    MailStore.close-compose();
                } }
                Button { "Cancel" clicked: { MailStore.close-compose(); } }
            }
        }
    }
}

// Root — three-pane layout: sidebar, list, detail.
export component App {
    HStack {
        // Sidebar
        VStack {
            padding: 8px
            width: 220px
            Button {
                "Compose"
                clicked: { MailStore.open-compose(); }
            }
            MailboxEntry {
                mailbox: Mailbox.inbox
                title: "Inbox"
                unread: MailStore.unread-inbox
                active: MailStore.current-mailbox == Mailbox.inbox
            }
            MailboxEntry {
                mailbox: Mailbox.drafts
                title: "Drafts"
                unread: MailStore.unread-drafts
                active: MailStore.current-mailbox == Mailbox.drafts
            }
            MailboxEntry {
                mailbox: Mailbox.sent
                title: "Sent"
                unread: 0
                active: MailStore.current-mailbox == Mailbox.sent
            }
            MailboxEntry {
                mailbox: Mailbox.archive
                title: "Archive"
                unread: 0
                active: MailStore.current-mailbox == Mailbox.archive
            }
            MailboxEntry {
                mailbox: Mailbox.trash
                title: "Trash"
                unread: 0
                active: MailStore.current-mailbox == Mailbox.trash
            }
        }

        // Message list
        VStack {
            grow: 1
            HStack {
                padding: 8px
                Button { "All"     clicked: { MailStore.set-filter(Filter.all); } }
                Button { "Unread"  clicked: { MailStore.set-filter(Filter.unread); } }
                Button { "Flagged" clicked: { MailStore.set-filter(Filter.flagged); } }
            }
            if MailStore.loading {
                Text { content: "Loading..." color: #9ca3af }
            } else {
                for msg in MailStore.visible-messages key(msg.id) {
                    MessageRow {
                        message: msg
                        selected: MailStore.selected-id == msg.id
                    }
                }
            }
        }

        // Detail pane
        VStack {
            grow: 2
            if MailStore.has-selection {
                DetailPane {}
            } else {
                Text { content: "No message selected" color: #9ca3af }
            }
        }
    }

    ComposeDialog {}
}`,
  },
  {
    id: "globals",
    name: "Globals (cross-component state)",
    code: `package yel:globals@0.1.0;

// =====================================================
// Globals — singleton objects shared across components
//
// Two modes, driven by member shape:
//
// 1. Pure in-tree shared state (no WIT, no host involved):
//      plain property with a default value
//
//      global AppState {
//          count: s32 = 0;
//      }
//
// 2. Host-boundary interface (emits WIT):
//      'in' / 'in-out' / 'out' properties
//      func-typed members the host implements
//
//      export global Theme {
//          in dark-mode: bool;
//          toggle-dark-mode: func();
//      }
//
// 'export' marks the global's WIT interface as published
// for other packages to import. Pure in-tree globals don't
// need 'export' — they emit no WIT regardless.
//
// Access in expressions:
//   GlobalName.property        // read
//   GlobalName.property = expr // write (in-tree or 'out')
//   GlobalName.function(args)  // call a function
// =====================================================

// Pure in-tree shared state: both components below read the
// same 'count' and 'label' via CounterStore. No WIT emitted.
global CounterStore {
    count: s32 = 0;
    label: string = "Shared count";
}

// Host-boundary theme — 'export' publishes the interface
// for cross-package import.
export global Theme {
    in dark-mode: bool;
    toggle-dark-mode: func();
}

// A widget that mutates the shared store — writes propagate
// to every other component reading CounterStore.count.
component Controls {
    HStack {
        Button {
            "-"
            clicked: { CounterStore.count = CounterStore.count - 1; }
        }
        Button {
            "reset"
            clicked: { CounterStore.count = 0; }
        }
        Button {
            "+"
            clicked: { CounterStore.count = CounterStore.count + 1; }
        }
    }
}

// Another widget that reads the same store — no prop-drilling,
// re-renders automatically when Controls writes.
component Readout {
    Text { "{CounterStore.label}: {CounterStore.count}" }
}

// Root component composes both; they share CounterStore
// and Theme through the global singletons.
export component App {
    VStack {
        HStack {
            Text { "Dark mode: {Theme.dark-mode}" }
            Button {
                "toggle"
                clicked: { Theme.toggle-dark-mode(); }
            }
        }

        Readout {}
        Controls {}

        // Read another global property inline
        if CounterStore.count > 10 {
            Text { "High count!" }
        }
    }
}`,
  },
  {
    id: "container-components",
    name: "Container components",
    code: `package yel:containers@0.1.0;

// =====================================================
// Container components — \`@children\` as a caller slot
// =====================================================
//
// A component body can include one \`@children\` placeholder to mark
// where caller-supplied child nodes splice in. At runtime, the
// component's \`mount(root) -> u32\` returns the DOM node id of the
// slot; the caller appends its children under that id.
//
// Type-check rejects passing children to a component without
// \`@children\`, and rejects multiple slots per component.
// =====================================================

component Card {
    title: string = "";
    VStack {
        Text { "{title}" }
        @children
    }
}

export component App {
    show-extra: bool = false;

    VStack {
        Card {
            title: "Settings"
            Text { "Body line 1" }
            Text { "Body line 2" }
            Button {
                "toggle extra"
                clicked: { show-extra = !show-extra; }
            }
            if show-extra {
                Text { "Extra content revealed!" }
            }
        }
    }
}`,
  },
  {
    id: "imported-components",
    name: "Imported components",
    code: `package yel:embed@0.1.0;

// =====================================================
// \`import component\` — the other half of the host boundary
// =====================================================
//
// Yel modules can DECLARE components that some OTHER module (or the host)
// supplies. The current module only sees the shape: property types and
// callable methods, no body.
//
// Think of it like forward-declaring a class: you agree on the interface,
// the runtime wires up the real implementation.
//
// Pair with \`export component\` when you build components the host uses,
// and \`global\` for shared state. \`import component\` is for *components*
// that live on the other side of the boundary.
// =====================================================

// Imported component declarations — shape only, no body. Each one shows
// up in the generated WIT so the host can type-check its bindings.

import component Dialog {
    // Props the host reads when rendering this instance.
    title: string;
    open: bool;

    // Imperative hooks the module can call on a Dialog instance.
    func show();
    func hide();
}

import component Icon {
    name: string;
    size: s32;
}

// =====================================================
// Use imported components at the call site like any other component.
// The module treats \`Dialog\` / \`Icon\` uniformly with built-ins;
// binding, handlers, control flow (\`if\`/\`for\`) all work.
// =====================================================

export component App {
    showing: bool = false;
    title: string = "Welcome";

    VStack {
        HStack {
            Icon {
                name: "info"
                size: 16
            }
            Text { "Imported components demo" }
        }

        HStack {
            Button {
                "toggle dialog"
                clicked: { showing = !showing; }
            }
        }

        // Only mount the Dialog while \`showing\` is true — the host gets
        // told to render/unmount through the normal component lifecycle.
        if showing {
            Dialog {
                title: title
                open: showing
            }
        }
    }
}`,
  },
];

// Default sample code (first example)
export const sampleCode = examples[0].code;
