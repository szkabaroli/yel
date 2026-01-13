import { StreamLanguage, StringStream } from "@codemirror/language";
import { Tag, tags } from "@lezer/highlight";
import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";

// Define custom tags for Yel-specific tokens
export const yelTags = {
  // Use existing tags
  keyword: tags.keyword,
  type: tags.typeName,
  component: tags.className,      // PascalCase components like VStack, Button
  property: tags.propertyName,    // Properties in bindings
  variable: tags.variableName,
  string: tags.string,
  number: tags.number,
  comment: tags.lineComment,
  operator: tags.operator,
  punctuation: tags.punctuation,
  brace: tags.brace,

  // Custom tags for yel-specific tokens
  unit: Tag.define(),             // 8px, 100ms, etc.
  colorLiteral: Tag.define(),     // #ff0000
  interpolation: Tag.define(),    // {} inside strings
  handler: Tag.define(),          // clicked, onchange, etc.
  packageName: Tag.define(),      // namespace:name@version
  functionCall: Tag.define(),     // function calls like incremented()
  element: Tag.define(),          // UI elements like VStack, Button
  enumCase: Tag.define(),         // enum cases like pending, active
};

interface YelState {
  inString: boolean;
  interpolationDepth: number;
  expectTypeName: boolean;  // After record/enum/variant keyword
  typeAngleBracketDepth: number;  // Depth inside <...> for generic types
  pendingEnumBody: boolean;  // After enum type name, waiting for {
  inEnumBody: boolean;  // Inside enum { ... } braces
}

// Known Yel keywords
const keywords = new Set([
  "package", "component", "record", "enum", "variant",
  "if", "else", "for", "in", "key", "export", "func", "set"
]);

// Boolean literals (highlighted as values, not keywords)
const booleanLiterals = new Set(["true", "false"]);

// Known Yel types
const types = new Set([
  "s8", "s16", "s32", "s64", "u8", "u16", "u32", "u64",
  "f32", "f64", "bool", "char", "string",
  "list", "option", "result", "tuple",
  "length", "duration", "angle", "percent",
  "color", "brush", "image", "easing"
]);

// Known event handlers
const handlers = new Set([
  "clicked", "pressed", "released", "changed", "input",
  "focus", "blur", "submit", "scroll"
]);

// Known UI elements (from stdlib)
const elements = new Set([
  "VStack", "HStack", "ZStack",
  "Input",
  "List", "ScrollView",
  "div", "Box",
  "Text",
  "Button",
  "TextField",
  "Checkbox",
  "Image",
  "Spacer",
  "Divider",
  "Badge",
  "Fragment",
  "Portal",
  "Group"
]);

// Type-defining keywords that should highlight the following identifier as a type
const typeDefKeywords = new Set(["record", "enum", "variant"]);

// Generic types that take type parameters
const genericTypes = new Set(["list", "option", "result", "tuple"]);

// Stream-based tokenizer for Yel with interpolation support
const yelStreamParser = {
  startState(): YelState {
    return { inString: false, interpolationDepth: 0, expectTypeName: false, typeAngleBracketDepth: 0, pendingEnumBody: false, inEnumBody: false };
  },

  token(stream: StringStream, state: YelState): string | null {

    // Inside a string
    if (state.inString) {
      if (stream.peek() === "{") {
        stream.next();
        state.interpolationDepth++;
        state.inString = false;
        return "interpolation";
      }

      if (stream.peek() === '"') {
        stream.next();
        state.inString = false;
        return "string";
      }

      if (stream.peek() === "\\") {
        stream.next();
        stream.next();
        return "string";
      }

      // Consume string content until we hit quote, interpolation, or escape
      while (stream.peek() && stream.peek() !== '"' && stream.peek() !== '{' && stream.peek() !== '\\') {
        stream.next();
      }
      return "string";
    }

    // Inside interpolation
    if (state.interpolationDepth > 0 && stream.peek() === "}") {
      stream.next();
      state.interpolationDepth--;
      if (state.interpolationDepth === 0) {
        state.inString = true;
      }
      return "interpolation";
    }

    if (state.interpolationDepth > 0 && stream.peek() === "{") {
      stream.next();
      state.interpolationDepth++;
      return "brace";
    }

    if (stream.eatSpace()) return null;

    // Comments
    if (stream.match(/\/\/.*/)) {
      return "comment";
    }
    if (stream.match(/\/\*/)) {
      while (!stream.match(/\*\//)) {
        if (!stream.next()) break;
      }
      return "comment";
    }

    // String start
    if (stream.peek() === '"') {
      stream.next();
      state.inString = true;
      return "string";
    }

    // Package declaration: namespace:name@version
    if (stream.match(/[a-z][a-z0-9-]*:[a-z][a-z0-9-]*(@[0-9]+\.[0-9]+\.[0-9]+)?/)) {
      return "packageName";
    }

    // Colors (hex)
    if (stream.match(/#[0-9a-fA-F]{6,8}\b/)) {
      return "colorLiteral";
    }

    // Numbers with units
    if (stream.match(/\d+(\.\d+)?(px|pt|em|rem|%|ms|s|deg|rad)\b/)) {
      return "unit";
    }

    // Numbers
    if (stream.match(/\d+(\.\d+)?/)) {
      return "number";
    }

    // Identifiers and keywords
    if (stream.match(/[a-zA-Z_][a-zA-Z0-9_-]*/)) {
      const word = stream.current();

      // Check if it's a boolean literal (true/false) - highlight as value
      if (booleanLiterals.has(word)) {
        return "number";
      }

      // Check if it's a keyword
      if (keywords.has(word)) {
        // Set flag if this is a type-defining keyword (record, enum, variant)
        if (typeDefKeywords.has(word)) {
          state.expectTypeName = true;
          // Track specifically if it's an enum for body highlighting
          if (word === "enum") {
            state.pendingEnumBody = true;
          }
        }
        return "keyword";
      }

      // If we're inside enum body, highlight as enum case
      if (state.inEnumBody) {
        return "enumCase";
      }

      // If we're expecting a type name (after record/enum/variant) or inside generic <...>
      if (state.expectTypeName || state.typeAngleBracketDepth > 0) {
        state.expectTypeName = false;
        // Check if this is a generic type - set flag for nested type params
        if (genericTypes.has(word)) {
          state.expectTypeName = true;  // Expect < next
        }
        return "type";
      }

      // Check if it's a built-in type
      if (types.has(word)) {
        // If it's a generic type, set flag to expect type parameter
        if (genericTypes.has(word)) {
          state.expectTypeName = true;  // Expect < next
        }
        return "type";
      }

      // Check if it's a handler
      if (handlers.has(word)) {
        return "handler";
      }

      // Check if it's a known UI element (green)
      if (elements.has(word)) {
        return "element";
      }

      // PascalCase = type/component definition (orange)
      if (/^[A-Z]/.test(word)) {
        return "component";
      }

      // Check if it's a function call (followed by parenthesis)
      if (stream.peek() === "(") {
        return "functionCall";
      }

      return "variable";
    }

    // Operators
    if (stream.match(/[+\-*/%=<>!&|?:]+/)) {
      return "operator";
    }

    // Braces - track enum body
    if (stream.eat("{")) {
      if (state.pendingEnumBody) {
        state.inEnumBody = true;
        state.pendingEnumBody = false;
      }
      return "brace";
    }
    if (stream.eat("}")) {
      if (state.inEnumBody) {
        state.inEnumBody = false;
      }
      return "brace";
    }

    // Angle brackets for generics - track depth
    if (stream.eat("<")) {
      if (state.typeAngleBracketDepth > 0 || state.expectTypeName) {
        state.typeAngleBracketDepth++;
        state.expectTypeName = false;
      }
      return "punctuation";
    }
    if (stream.eat(">")) {
      if (state.typeAngleBracketDepth > 0) {
        state.typeAngleBracketDepth--;
      }
      return "punctuation";
    }

    // Other punctuation
    if (stream.match(/[()\[\];,\.@]/)) {
      return "punctuation";
    }

    stream.next();
    return null;
  },
};

// Token table maps string token names to Tag objects for highlighting
const tokenTable = {
  type: tags.typeName,
  component: tags.className,
  variable: tags.variableName,
  handler: yelTags.handler,
  unit: yelTags.unit,
  colorLiteral: yelTags.colorLiteral,
  interpolation: yelTags.interpolation,
  packageName: yelTags.packageName,
  functionCall: yelTags.functionCall,
  element: yelTags.element,
  enumCase: yelTags.enumCase,
};

export const yelLanguage = StreamLanguage.define({
  ...yelStreamParser,
  tokenTable,
});

// GitHub Dark theme colors for Yel
// Following Rust token styling from GitHub
export const yelHighlightStyle = HighlightStyle.define([
  // Standard tokens (GitHub Dark palette - Rust style)
  { tag: tags.keyword, color: "#ff7b72" },          // Keywords - red
  { tag: tags.typeName, color: "#ffa657" },         // Types (structs/enums) - orange
  { tag: tags.className, color: "#ffa657" },        // Components (like structs) - orange
  { tag: tags.propertyName, color: "#79c0ff" },     // Properties/fields - blue
  { tag: tags.variableName, color: "#c9d1d9" },     // Variables - default text
  { tag: tags.string, color: "#a5d6ff" },           // Strings - light blue
  { tag: tags.number, color: "#79c0ff" },           // Numbers - blue
  { tag: tags.lineComment, color: "#8b949e", fontStyle: "italic" }, // Comments - gray
  { tag: tags.operator, color: "#ff7b72" },         // Operators - red
  { tag: tags.punctuation, color: "#c9d1d9" },      // Punctuation - default text
  { tag: tags.brace, color: "#c9d1d9" },            // Braces - default text

  // Custom Yel tokens
  { tag: yelTags.functionCall, color: "#d2a8ff" }, // Function calls - purple
  { tag: yelTags.unit, color: "#79c0ff" },         // Units (8px, 100ms) - blue
  { tag: yelTags.colorLiteral, color: "#79c0ff" }, // Color literals - blue
  { tag: yelTags.interpolation, color: "#c9d1d9" }, // Interpolation braces
  { tag: yelTags.handler, color: "#d2a8ff" },      // Event handlers - purple
  { tag: yelTags.packageName, color: "#ffa657" },  // Package names - orange
  { tag: yelTags.element, color: "#7ee787" },      // UI elements - green
  { tag: yelTags.enumCase, color: "#79c0ff" },     // Enum cases - blue (like WIT types)
]);

export const yelSyntax = [yelLanguage, syntaxHighlighting(yelHighlightStyle)];
