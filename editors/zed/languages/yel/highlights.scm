; Queries over the flat token grammar in editors/tree-sitter-yel.
; The tree has no declaration structure on purpose (see its grammar.js), so
; everything here is token classes plus sibling-anchored name rules.

(line_comment) @comment
(block_comment) @comment

; A string colors by its parts: the quotes and literal chunks are string, an
; interpolation's `{…}` is code — its identifiers, numbers and operators take
; their normal colors from the rules below, not the string color.
(string_content) @string
(escape_sequence) @string.escape
(string "\"" @string)
(interpolation
  "{" @punctuation.special
  "}" @punctuation.special)
(char) @string
(color) @string.special
(number) @number


["true" "false"] @boolean

[
  "component"
  "global"
  "record"
  "enum"
  "variant"
  "element"
  "extern"
  "package"
  "export"
  "func"
  "callback"
  "let"
  "if"
  "else"
  "for"
  "from"
  "include"
  "return"
  "in"
  "out"
  "in-out"
  "key"
  "set"
  "bind"
  "children"
  "match"
  "module"
  "impl"
  "use"
  "primitive"
] @keyword

; Capitalized names are components/types by convention (VStack, Counter, Color).
((identifier) @type
  (#match? @type "^[A-Z]"))

; Primitive type names are contextual identifiers, never keywords
; (yelc-syntax/src/token.rs — reserving them would be a language change).
; The full inventory from LANGUAGE.md § Type System: primitives, the
; `int`/`float` aliases, compound heads (`tuple<T, U>` — `func` is a keyword),
; and the UI types.
((identifier) @type.builtin
  (#match? @type.builtin "^(s8|s16|s32|s64|u8|u16|u32|u64|f32|f64|bool|char|string|int|float|list|option|result|tuple|ref|any|length|physical-length|angle|duration|percent|relative-font-size|color|brush|image|easing)$"))

; The name directly after a declaring keyword.
(source_file "component" . (identifier) @type)
(source_file "record" . (identifier) @type)
(source_file "enum" . (identifier) @type)
(source_file "variant" . (identifier) @type)
(source_file "element" . (identifier) @type)
(source_file "global" . (identifier) @type)
(source_file "module" . (identifier) @type)
(source_file "func" . (identifier) @function)

["{" "}" "(" ")" "[" "]"] @punctuation.bracket

["," ";" ":" "." "@"] @punctuation.delimiter

[
  "->"
  "=>"
  ".."
  "&&"
  "||"
  "=="
  "!="
  "<="
  ">="
  "+="
  "-="
  "*="
  "/="
  "="
  "+"
  "-"
  "*"
  "/"
  "%"
  "!"
  "?"
  "&"
  "|"
  "<"
  ">"
] @operator

; Attributes color as a unit (Rust-in-Zed style): the whole `@name(…)` node is
; @attribute, so bare argument names inherit the attribute color. These sit
; last because later patterns win — the string value inside re-captures as
; @string.special, the same class as a color literal, so it reads as data.
(attribute) @attribute
(attribute (paren_group (string "\"" @string.special)))
(attribute (paren_group (string (string_content) @string.special)))
