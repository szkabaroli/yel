# Yel Language Reference

Yel is a declarative UI language that compiles to WebAssembly components. It uses
[WIT](https://component-model.bytecodealliance.org/design/wit.html)-aligned types
and produces reactive, tree-structured UIs that communicate with a host runtime
through the WebAssembly Component Model.

---

## Table of Contents

- [File Structure](#file-structure)
- [Package Declaration](#package-declaration)
- [Type System](#type-system)
  - [Primitive Types](#primitive-types)
  - [Compound Types](#compound-types)
  - [UI Types](#ui-types)
  - [Records](#records)
  - [Enums](#enums)
  - [Variants](#variants)
  - [Type Parameters](#type-parameters)
- [Components](#components)
  - [Properties (State)](#properties-state)
  - [Composition](#composition)
- [Template (View)](#template-view)
  - [Elements](#elements)
  - [Text and Interpolation](#text-and-interpolation)
  - [Bindings](#bindings)
  - [Event Handlers](#event-handlers)
  - [Two-Way Bindings](#two-way-bindings)
  - [Conditional Rendering](#conditional-rendering)
  - [Match Rendering](#match-rendering)
  - [List Rendering](#list-rendering)
- [Globals](#globals)
  - [In-Tree Shared State](#in-tree-shared-state)
  - [Host-Boundary Globals](#host-boundary-globals)
  - [Property Directions](#property-directions)
  - [Functions on Globals](#functions-on-globals)
  - [Accessing Globals](#accessing-globals)
- [Expressions](#expressions)
  - [Literals](#literals)
  - [Operators](#operators)
  - [Member Access](#member-access)
  - [Indexing](#indexing)
  - [Function and Method Calls](#function-and-method-calls)
  - [Closures](#closures)
  - [Ternary Expressions](#ternary-expressions)
  - [Match](#match)
  - [Function Bodies](#function-bodies)
  - [Ranges](#ranges)
- [Statements](#statements)
- [Built-in Elements](#built-in-elements)
- [Built-in Functions](#built-in-functions)
- [Comments](#comments)
- [Identifiers](#identifiers)

---

## File Structure

A `.yel` file contains an optional package declaration followed by any number of
top-level items: records, enums, variants, globals, and components.

```yel
package yel:counter@1.0.0;

record Person { name: string, age: u32 }

enum Status { pending, active, completed }

global AppState {
    count: s32 = 0;
}

export component App {
    // ...
}
```

Items can appear in any order. Multiple components can be defined in a single
file. Only items marked `export` are visible outside the package.

## Package Declaration

Every file starts with an optional package declaration that names the package
using WIT-style namespacing:

```yel
package namespace:name@version;
```

Examples:

```yel
package yel:counter@1.0.0;
package yel:mail@0.1.0;
```

The version is optional: `package yel:counter;` is valid.

---

## Type System

### Primitive Types

| Type | Description |
|------|-------------|
| `bool` | Boolean (`true` / `false`) |
| `s8`, `s16`, `s32`, `s64` | Signed integers |
| `u8`, `u16`, `u32`, `u64` | Unsigned integers |
| `f32`, `f64` | Floating-point numbers |
| `char` | Unicode character |
| `string` | UTF-8 string |
| `int` | Alias for `s32` |
| `float` | Alias for `f32` |

### Compound Types

| Type | Syntax | Description |
|------|--------|-------------|
| List | `list<T>` | Ordered sequence |
| Option | `option<T>` | Optional value |
| Result | `result<T, E>` | Success or error |
| Tuple | `tuple<T, U, ...>` | Fixed-size heterogeneous sequence |
| Function | `func(params) -> ret` | Callable signature |

### UI Types

These types are used for styling and layout properties:

`length`, `physical-length`, `angle`, `duration`, `percent`,
`relative-font-size`, `color`, `brush`, `image`, `easing`

Values are created with unit literals (see [Literals](#literals)).

### Records

Records are product types with named fields, separated by commas:

```yel
record Person {
    name: string,
    age: u32,
}

record Message {
    id: string,
    from: Address,
    subject: string,
    read: bool,
}
```

Record literals are written with `{ field: value }` syntax:

```yel
{ name: "Alice", age: 30 }
```

### Enums

Enums are WIT-style union types without payloads:

```yel
enum Status { pending, active, completed }

enum Mailbox {
    inbox,
    sent,
    drafts,
    archive,
    spam,
    trash,
    flagged,
}
```

Enum values are accessed with dot syntax: `Mailbox.inbox`, `Status.active`.

### Variants

Variants are WIT-style tagged unions where each case can carry a payload:

```yel
variant Filter {
    all,
    none,
    some(list<string>),
}
```

The built-in `option<T>` type is a variant with `some(T)` and `none` cases.
Construct option values with `some(value)` and `none`.

### Type Parameters

A function may declare type parameters, so one definition works for many types:

```yel
first: func<T>(items: list<T>) -> option<T>;
map: func<T, U>(items: list<T>, transform: func(T) -> U) -> list<U>;
```

The parameter list goes after `func` and before the arguments. Parameters are
named like types (`T`, `U`, `Item`) and are in scope for the whole signature.

**Type arguments are inferred at the call site** from the argument types — there
is no syntax for passing them explicitly:

```yel
names: list<string> = ["ana", "bo"];
head: option<string> = first(names);    // T is string
```

If a call does not determine every parameter, that is an error at the call, the
same as any other type that cannot be inferred.

**There are no constraints.** A type parameter accepts any type, and a generic
body may only do what works for every type — pass it, store it, return it. There
is no way to require that `T` is comparable or printable, so `func<T>(a: T, b: T)
-> bool` cannot compare `a` and `b`.

Generic **types** — a user-written `record Pair<T>` — are not part of this. The
built-in `list<T>`, `option<T>` and `result<T, E>` are the only parameterised
types, and user types are concrete.

---

## Components

A component declares reactive state and a template. Components marked `export`
are published in the package interface.

```yel
export component Counter {
    count: s32 = 0;
    label: string = "Count";
    incremented: func();

    VStack {
        Text { "{label}: {count}" }
        Button {
            "+"
            clicked: { count += 1; incremented(); }
        }
    }
}
```

### Properties (State)

Properties declare the component's reactive state. Each property has a name,
type, and optional default value:

```yel
name: type;
name: type = default_value;
```

Examples:

```yel
count: s32 = 0;
label: string = "Count";
items: list<Person> = [{ name: "Alice", age: 30 }];
celsius: f32 = 0.0;
selected-index: s32 = -1;
```

When a property value changes (via assignment in a handler), all template
expressions that depend on it automatically re-render.

Properties with a `func()` type declare callable events that parent components
or the host can wire up:

```yel
incremented: func();
on-change: func(value: s32) -> string;
```

Invoked from handlers just like any other function call: `incremented();`

Computed properties are written as properties whose default expression depends
on other state:

```yel
filtered: list<Person> = people.filter({ p -> p.last.starts-with(prefix) });
selected: option<Person> = selected-index >= 0 ? some(filtered[selected-index]) : none;
```

### Composition

Components are used inside other components by name. Component names must start
with an uppercase letter:

```yel
component Nested {
    VStack {
        Text { "I'm nested" }
    }
}

export component App {
    VStack {
        Nested {}
        Nested {}
    }
}
```

Props are passed as named bindings:

```yel
MessageRow {
    message: msg
    selected: current-id == msg.id
}
```

---

## Template (View)

The template is the portion of a component body that describes the UI tree.
Template nodes are interleaved with property declarations in the component body
(there is no separate `view { }` block).

### Elements

Elements represent UI nodes. The name determines whether it's an intrinsic
element (provided by the runtime) or a user-defined component:

```yel
VStack {
    Text { "Hello" }
    Button { "Click me" }
}
```

Element content is a mix of named bindings, event handlers, child nodes, and
inline text. Items are separated by optional commas or whitespace.

### Text and Interpolation

Bare strings inside an element become text children:

```yel
Button { "Click me" }
Text { "Count: {count}" }
```

String interpolation uses single braces `{expr}` inside double-quoted strings:

```yel
"Hello {name}"
"{label}: {count}"
"{person.last}, {person.first}"
"Dark mode: {Theme.dark-mode}"
```

Any expression is valid inside interpolation braces. Values are automatically
converted to strings.

### Bindings

Named properties are bound with `name: expr`:

```yel
Text {
    content: message.subject
    weight: "medium"
    size: 12px
    color: #9ca3af
}

HStack {
    padding: 8px
    background: selected ? #2563eb : #00000000
    grow: 1
}
```

Binding names use kebab-case (e.g., `font-size`, `date-display`).

### Event Handlers

Event handlers are closures assigned to an event name. The name is any
kebab-case identifier; the value is a block `{ statements }`:

```yel
Button {
    "+"
    clicked: {
        count += 1;
        incremented();
    }
}

HStack {
    clicked: { MailStore.select-message(message.id); }
}
```

Handler bodies contain [statements](#statements) (assignments, calls, `if`,
`let`). The handler runs when the host fires the named event.

### Two-Way Bindings

Two-way bindings pair a value binding with a `set` modifier that runs when the
value changes from the outside (e.g., user input):

```yel
Input {
    value: celsius
    set value: {
        fahrenheit = 32.0 + (9.0 / 5.0) * celsius;
    }
}
```

`value: celsius` provides the current value. `set value: { ... }` runs after
`celsius` is updated by the element, allowing derived state to be recomputed.

### Conditional Rendering

Use `if` / `else if` / `else` to conditionally include UI subtrees:

```yel
if count > 10 {
    Text { "High count!" }
} else if count < 0 {
    Text { "Negative!" }
}

if MailStore.has-selection {
    DetailPane {}
} else {
    Text { content: "No message selected" color: #9ca3af }
}
```

Branches mount and unmount their children. The condition is any boolean
expression.

### Match Rendering

Use [`match`](#match) to pick a UI subtree by the *shape* of a value, rather than
by a boolean. Each arm's body is a subtree:

```yel
match filter {
    all -> Text { "everything" }
    none -> Text { "nothing" }
    some(items) -> VStack {
        for item in items { Text { "{item}" } }
    }
}
```

Arms mount and unmount their children exactly as `if` branches do, and the
subtree re-renders when the matched value changes. The same exhaustiveness rule
applies: every case needs an arm, or a `_` arm to cover the rest.

### List Rendering

Use `for item in collection` to render a list. An optional `key(expr)` clause
provides a stable identity for efficient diffing:

```yel
for item in items key(item.name) {
    Text { "{item.name}" }
}

for msg in MailStore.visible-messages key(msg.id) {
    MessageRow {
        message: msg
        selected: MailStore.selected-id == msg.id
    }
}
```

Loops can be nested, and the inner loop can access variables from the outer:

```yel
for item in items key(item.name) {
    VStack {
        Text { "{item.name}" }
        for sub in item.subitems key(sub) {
            Text { "- {sub}" }
        }
    }
}
```

---

## Globals

Globals are singleton objects for state shared across components. They come
in two modes.

### In-Tree Shared State

Plain properties with defaults create reactive state shared across all
components that reference the global. No WIT interface is emitted — this is
purely in-tree:

```yel
global CounterStore {
    count: s32 = 0;
    label: string = "Shared count";
}
```

Any component can read and write:

```yel
Text { "{CounterStore.label}: {CounterStore.count}" }

clicked: { CounterStore.count = CounterStore.count + 1; }
```

### Host-Boundary Globals

Properties with direction modifiers (`in`, `out`, `in-out`) and `func`-typed
members create a WIT interface. The host implements the functions and pushes
property values:

```yel
export global Theme {
    in dark-mode: bool;
    toggle-dark-mode: func();
}

export global MailStore {
    in loading: bool;
    in current-mailbox: Mailbox;
    in visible-messages: list<Message>;

    select-mailbox: func(m: Mailbox);
    select-message: func(id: string);
    send-reply: func(body: string);
}
```

### Property Directions

| Direction | Meaning |
|-----------|---------|
| *(none)* | In-tree only; no host boundary. Requires a default value. |
| `in` | Host pushes value into the component tree. |
| `out` | Component writes; host is notified. |
| `in-out` | Both directions. |

### Functions on Globals

Functions declare operations the host implements. The component calls them
using dot syntax — the same way event handlers call component functions:

```yel
// In the global:
toggle-dark-mode: func();
select-mailbox: func(m: Mailbox);
send-reply: func(body: string);

// In a handler:
Theme.toggle-dark-mode();
MailStore.select-mailbox(Mailbox.inbox);
MailStore.send-reply(body);
```

### Accessing Globals

```yel
// Read a property
CounterStore.count
Theme.dark-mode
MailStore.selected.from.name

// Write a property (in-tree or out)
CounterStore.count = 0;
CounterStore.count = CounterStore.count + 1;

// Call a global function
Theme.toggle-dark-mode();
MailStore.select-message(message.id);
```

---

## Expressions

### Literals

| Kind | Examples |
|------|----------|
| Integer | `0`, `42`, `-1` |
| Float | `0.0`, `3.14`, `-9.5` |
| Boolean | `true`, `false` |
| String | `"hello"`, `"Count: {count}"` |
| Character | `'a'`, `'\n'` |
| Color | `#2563eb`, `#ff0000`, `#00000000` (3–8 hex digits) |
| Unit | `8px`, `100ms`, `45deg`, `50%`, `1.5rem` |
| List | `[]`, `[1, 2, 3]`, `["a", "b"]` |
| Tuple | `(1, 2)`, `(x, y, z)`, `(x,)` (trailing comma for 1-tuple) |
| Record | `{ name: "Alice", age: 30 }` |
| Option | `some(value)`, `none` |

**Unit suffixes:** `px`, `pt`, `in`, `mm`, `cm`, `phx` (lengths); `deg`,
`rad`, `turn` (angles); `ms`, `s` (durations); `rem`, `%` (relative).

### Operators

Listed from lowest to highest precedence:

| Precedence | Operators | Description |
|------------|-----------|-------------|
| 1 | `..`, `..=` | Range, inclusive range |
| 2 | `\|\|` | Logical OR |
| 3 | `&&` | Logical AND |
| 4 | `==`, `!=`, `<`, `<=`, `>`, `>=` | Comparison |
| 5 | `+`, `-` | Addition, subtraction |
| 6 | `*`, `/`, `%` | Multiplication, division, modulo |
| 7 (prefix) | `-`, `!` | Negation, logical NOT |

### Member Access

Dot syntax accesses record fields, enum cases, and global properties:

```yel
message.from.name
Mailbox.inbox
CounterStore.count
```

Optional chaining accesses a field on an optional value, returning the inner
value or propagating `none`:

```yel
selected?.first
```

### Indexing

Square brackets index into lists:

```yel
items[0]
items[selected-index]
filtered[i].name
```

### Function and Method Calls

Free function calls:

```yel
some(value)
min(a, b)
```

Method-style calls on values:

```yel
items.len()
items.filter({ p -> p.last.starts-with(prefix) })
text.starts-with(prefix)
```

### Closures

Closures are written with `{ params -> body }` or `{ body }`:

```yel
// No parameters
{ count += 1; }

// Inferred parameter types
{ p -> p.last.starts-with(prefix) }

// Typed parameters
{ x: s32 -> x + 1 }
{ x: s32, y: s32 -> x + y }
```

The body can contain statements followed by an optional trailing expression
(the return value):

```yel
{ x: s32 ->
    let doubled = x * 2;
    doubled + 1
}
```

### Ternary Expressions

```yel
condition ? then_value : else_value

active ? "medium" : "regular"
selected ? #2563eb : #00000000
selected-index >= 0 ? some(filtered[selected-index]) : none
```

### Match

`match` branches on the *shape* of a value, binding any payload it carries. It is
the only way to take a variant apart.

```yel
match filter {
    all -> "everything"
    none -> "nothing"
    some(items) -> "{items.len()} selected"
}
```

Arms use `->`, the same producer arrow as closures. A block-bodied arm needs no
separator; an expression arm is followed by a comma when another arm follows on
the same line.

**Patterns**

| Pattern | Example | Matches |
|---------|---------|---------|
| Case | `none`, `all` | a case of the value's variant or enum, carrying no payload |
| Case with payload | `some(items)` | that case, binding the payload to `items` |
| Nested | `some(some(x))` | patterns nest to any depth |
| Boolean | `true`, `false` | a `bool` value |
| Binding | `rest` | anything, binding it to `rest` |
| Wildcard | `_` | anything, binding nothing |

A bare lowercase name is a **case pattern** when it names a case of the value's
type, and a **binding** otherwise. So in a `match` on an `option<s32>`, `none`
matches the empty case, while `total` binds the whole option.

**Matches must be exhaustive.** Every case of the value's type needs an arm, or a
`_` arm must cover the rest. A `match` missing a case is a compile error naming
the cases you left out — this is what makes variants safe to add cases to.

```yel
match status {
    pending -> "waiting"
    active -> "running"
    // error: non-exhaustive match, missing `completed`
}
```

**Where a `match` can appear.** Like `if`, the same keyword means three things
depending on *where you write it*, and the position is what decides — there is no
different syntax to remember:

| Written in | It is | Arms produce |
|-------------|-------|--------------|
| a value position | a match **expression** (below) | a value; every arm the same type |
| a component body | a [match in a template](#match-rendering) | UI subtrees |
| a handler, closure or `set` block | a [match **statement**](#match-statements) | nothing |

As an expression, every arm must produce the same type:

```yel
label: string = match status {
    pending -> "waiting"
    active -> "running"
    completed -> "done"
};
```

### Function Bodies

A function declaration may carry a body, written as a block after the signature:

```yel
export global Math {
    double: func(n: s32) -> s32 { n * 2 }

    clamp: func(value: s32, low: s32, high: s32) -> s32 {
        if value < low { low }
        else if value > high { high }
        else { value }
    }
}
```

Parameters are declared **once**, in the signature, and are in scope for the
body. A block's value is its final expression; a block with no final expression
produces nothing.

**A function body and a [closure](#closures) body are the same construct** — the
same statements, the same scoping, the same value rule. The difference is only
where the parameters come from:

```yel
double: func(n: s32) -> s32 { n * 2 }   // parameters from the signature
{ n: s32 -> n * 2 }                     // parameters from the closure head
```

A declaration **without** a body stays a declaration — that is how a `global`
declares a callback the host implements, and how a component declares one its
parent supplies:

```yel
export global Clock {
    now: func() -> s64;                 // the host implements this
}
```

### Ranges

Range expressions create sequences (used with `for` loops or list operations):

```yel
0..10       // exclusive
0..=10      // inclusive
```

---

## Statements

Statements appear inside event handlers, closures, and `set` blocks.

### Assignment

```yel
count = 0;
celsius = (5.0 / 9.0) * (fahrenheit - 32.0);
CounterStore.count = CounterStore.count + 1;
```

### Compound Assignment

```yel
count += 1;
count -= 1;
total *= 2;
remaining /= 3;
```

### Let Bindings

```yel
let doubled = count * 2;
let name: string = "Alice";
```

### If Statements

```yel
if count > 10 {
    label = "high";
} else {
    label = "normal";
}
```

### For Statements

`for` iterates in statement position, with the same syntax it uses in a
[template](#list-rendering):

```yel
for item in items {
    total = total + item.price;
}

for i in 0..count {
    array-set(out, i, none);
}
```

Over a list, the loop variable is each element. Over a [range](#ranges), it is
each value in turn.

Like [`if`](#if-statements) and [`match`](#match-statements), `for` is the same
construct in both positions — the difference is only what the body contains:
UI nodes in a template, statements in a block.

There is no `while`. A loop that is not over a list or a range has no way to be
written; if one is needed, that is a separate decision.

### Return

`return` exits a function early:

```yel
starts-with: func(text: string, prefix: string) -> bool {
    if bytes-len(prefix) > bytes-len(text) { return false; }

    for i in 0..bytes-len(prefix) {
        if byte-at(text, i) != byte-at(prefix, i) { return false; }
    }

    true
}
```

`return expr;` produces a value; `return;` exits a function that returns nothing.
The value must match the function's declared return type.

A function's last expression is still its value — `return` is for leaving
*before* the end, not for producing the result. `return` inside a closure exits
the closure, not the enclosing function.

### Match Statements

[`match`](#match) as a statement runs the arm that matches and produces no value:

```yel
match status {
    pending -> { label = "waiting"; }
    active -> { label = "running"; }
    completed -> { label = "done"; done-at = now(); }
}
```

Arms are blocks here, so they may contain several statements. Exhaustiveness is
required, the same as everywhere else.

### Expression Statements

Any expression followed by a semicolon:

```yel
incremented();
MailStore.select-mailbox(Mailbox.inbox);
```

---

## Built-in Elements

The runtime provides these intrinsic UI elements:

### Layout

| Element | Description |
|---------|-------------|
| `VStack` | Vertical stack layout |
| `HStack` | Horizontal stack layout |
| `ZStack` | Overlay / z-axis stack |
| `Box` | Generic container |
| `Spacer` | Flexible space |
| `Divider` | Visual separator |
| `ScrollView` | Scrollable container |
| `Fragment` | Grouping without a DOM node |
| `Group` | Logical grouping |

### Content

| Element | Description |
|---------|-------------|
| `Text` | Text display |
| `Image` | Image display |
| `Badge` | Inline label / badge |

### Input

| Element | Description |
|---------|-------------|
| `Button` | Clickable button |
| `Input` / `TextField` | Text input field |
| `Checkbox` | Boolean toggle |
| `Select` | Dropdown select |
| `Option` | Option inside `Select` |

### Common Properties

These properties are available on most elements:

| Property | Type | Description |
|----------|------|-------------|
| `padding` | length | Inner spacing |
| `width`, `height` | length | Dimensions |
| `grow` | number | Flex grow factor |
| `background` | color | Background color |
| `style` | string | Inline CSS |
| `content` | string | Text content (`Text`) |
| `weight` | string | Font weight (`Text`) |
| `size` | length | Font size (`Text`) |
| `color` | color | Text color |
| `value` | varies | Input value (`Input`, `Select`) |
| `placeholder` | string | Placeholder text (`Input`) |
| `disabled` | bool | Disabled state |
| `variant` | string | Visual variant (e.g., `"icon"`) |
| `label` | string | Display label (`Badge`) |

---

## Built-in Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `len` | `list<T> -> s32` / `string -> s32` | Length of list or string |
| `filter` | `(list<T>, func(T) -> bool) -> list<T>` | Filter a list by predicate |
| `starts-with` | `(string, string) -> bool` | String prefix test |
| `min` | `(s32, s32) -> s32` | Minimum of two values |
| `max` | `(s32, s32) -> s32` | Maximum of two values |
| `some` | `T -> option<T>` | Wrap in `some` |
| `none` | `option<T>` | The empty option |

Method-style calls are supported: `items.len()`, `items.filter(...)`,
`text.starts-with(prefix)`.

---

## Comments

```yel
// Line comment

/* Block comment */
```

---

## Identifiers

Identifiers support kebab-case: letters, digits, underscores, and hyphens.
They must start with a letter or underscore:

```
count
selected-index
date-display
reply-body
toggle-dark-mode
_private
```

Component and type names conventionally use PascalCase (`Counter`, `MailStore`,
`Person`). Property and variable names use kebab-case (`selected-id`,
`dark-mode`). Enum cases use kebab-case (`inbox`, `in-progress`).

### Hyphens

A hyphen is part of an identifier only when a letter, digit, or underscore
follows it. Everywhere else it is the subtraction or negation operator:

```
selected-id          one identifier
count-1              one identifier — `1` continues the name
count - 1            subtraction
count -= 1           compound assignment, not a name `count-`
{ p: s32 -> p }      a closure; so is `{ p: s32->p }`
```

That rule is what lets kebab-case names and the `-` operator coexist without
whitespace being significant.

### Keywords are reserved at word boundaries

`component`, `global`, `record`, `enum`, `variant`, `element`, `extern`,
`package`, `export`, `func`, `callback`, `if`, `else`, `for`, `in`, `key`,
`let`, `set`, and `bind` are keywords **only when a name character does not
follow them**. A longer identifier that merely begins with one is an ordinary
identifier:

```
if active { … }      an if-node
ifactive { … }       an element named `ifactive`
record Person { … }  a record declaration
recordPerson         an identifier
for item in xs { … } a for-loop
format { … }         an element named `format`
```

So a keyword never claims the front of a longer name, and no identifier is
rejected merely for starting with one.
