<script lang="ts">
  import { onMount, onDestroy } from "svelte";
  import { EditorView, lineNumbers } from "@codemirror/view";
  import { EditorState } from "@codemirror/state";
  import { StreamLanguage } from "@codemirror/language";
  import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";
  import { tags } from "@lezer/highlight";

  interface Props {
    code: string;
  }

  let { code }: Props = $props();

  let editorContainer: HTMLDivElement;
  let view: EditorView;

  // WIT syntax highlighting
  const witLanguage = StreamLanguage.define({
    token(stream) {
      // Skip whitespace
      if (stream.eatSpace()) return null;

      // Comments (// ...)
      if (stream.match("//")) {
        stream.skipToEnd();
        return "comment";
      }

      // Block comments (/* ... */)
      if (stream.match("/*")) {
        while (!stream.match("*/") && !stream.eol()) {
          stream.next();
        }
        return "comment";
      }

      // Strings
      if (stream.match('"')) {
        while (!stream.eol()) {
          const ch = stream.next();
          if (ch === '"') break;
          if (ch === "\\") stream.next();
        }
        return "string";
      }

      // Keywords
      const keywords = /^(package|world|interface|resource|func|type|record|variant|enum|flags|use|import|export|as|constructor|static)/;
      if (stream.match(keywords)) {
        return "keyword";
      }

      // Built-in types
      const types = /^(u8|u16|u32|u64|s8|s16|s32|s64|f32|f64|bool|char|string|list|option|result|tuple|own|borrow)/;
      if (stream.match(types)) {
        return "type";
      }

      // Version annotations (@0.1.0)
      if (stream.match(/@\d+\.\d+\.\d+/)) {
        return "meta";
      }

      // Arrows and operators
      if (stream.match("->")) {
        return "operator";
      }

      // Punctuation
      if (stream.eat(":") || stream.eat(";") || stream.eat(",")) {
        return "punctuation";
      }

      // Brackets
      if (stream.eat("{") || stream.eat("}") || stream.eat("(") || stream.eat(")") || stream.eat("<") || stream.eat(">")) {
        return "bracket";
      }

      // Identifiers with namespace - two patterns:
      // 1. With path: yel:ui/dom@0.1.0
      // 2. Package name: yel:counter@1.0.0 or yel:counter (no path)
      // Include optional @version in the match so entire identifier is one color
      if (stream.match(/[a-zA-Z_][a-zA-Z0-9_-]*:[a-zA-Z_][a-zA-Z0-9_-]*\/[a-zA-Z_][a-zA-Z0-9_-]*(@\d+\.\d+\.\d+)?/)) {
        return "namespace";
      }
      // Package name without path (namespace:name with optional @version)
      if (stream.match(/[a-zA-Z_][a-zA-Z0-9_-]*:[a-zA-Z_][a-zA-Z0-9_-]*(@\d+\.\d+\.\d+)?/)) {
        return "namespace";
      }

      // Function/method names (followed by :)
      if (stream.match(/[a-zA-Z_][a-zA-Z0-9_-]*(?=\s*:)/)) {
        return "function";
      }

      // Regular identifiers
      if (stream.match(/[a-zA-Z_][a-zA-Z0-9_-]*/)) {
        return "variable";
      }

      stream.next();
      return null;
    },
  });

  // WIT highlight style
  const witHighlightStyle = HighlightStyle.define([
    { tag: tags.keyword, color: "#ff7b72" },      // Red for keywords
    { tag: tags.typeName, color: "#79c0ff" },     // Blue for types
    { tag: tags.function(tags.variableName), color: "#d2a8ff" }, // Purple for functions
    { tag: tags.variableName, color: "#ffa657" }, // Orange for variables
    { tag: tags.string, color: "#a5d6ff" },       // Light blue for strings
    { tag: tags.comment, color: "#8b949e" },      // Gray for comments
    { tag: tags.meta, color: "#7ee787" },         // Green for version annotations
    { tag: tags.bracket, color: "#8b949e" },      // Gray for brackets
    { tag: tags.operator, color: "#ff7b72" },     // Red for arrows
    { tag: tags.punctuation, color: "#8b949e" },  // Gray for punctuation
    { tag: tags.namespace, color: "#ffa657" },    // Orange for namespaces (matches Yel source)
  ]);

  onMount(() => {
    const state = EditorState.create({
      doc: code,
      extensions: [
        EditorView.editable.of(false),
        EditorState.readOnly.of(true),
        lineNumbers(),
        witLanguage,
        syntaxHighlighting(witHighlightStyle),
        EditorView.theme({
          "&": {
            height: "100%",
            fontSize: "14px",
            backgroundColor: "#000000",
          },
          ".cm-scroller": {
            overflow: "auto",
            fontFamily: "'JetBrains Mono', 'Fira Code', monospace",
          },
          ".cm-content": {
            caretColor: "#fafafa",
          },
          ".cm-gutters": {
            backgroundColor: "#000000",
            color: "#52525b",
            borderRight: "1px solid #27272a",
          },
          ".cm-activeLineGutter": {
            backgroundColor: "#09090b",
          },
          ".cm-activeLine": {
            backgroundColor: "#09090b",
          },
        }, { dark: true }),
      ],
    });

    view = new EditorView({
      state,
      parent: editorContainer,
    });
  });

  onDestroy(() => {
    view?.destroy();
  });

  // Update editor content when code prop changes
  $effect(() => {
    if (view && code !== view.state.doc.toString()) {
      view.dispatch({
        changes: {
          from: 0,
          to: view.state.doc.length,
          insert: code,
        },
      });
    }
  });
</script>

<div class="flex flex-col h-full bg-background">
  <!-- Header -->
  <div class="flex items-center justify-between h-12 shrink-0 px-4 bg-card border-b border-border">
    <h3 class="text-xs font-semibold text-muted-foreground">WebAssembly Interface Types</h3>
  </div>

  <!-- Editor -->
  <div class="flex-1 overflow-hidden">
    <div class="editor-container" bind:this={editorContainer}></div>
  </div>
</div>

<style>
  .editor-container {
    height: 100%;
    width: 100%;
    overflow: hidden;
  }

  .editor-container :global(.cm-editor) {
    height: 100%;
  }

  /* Match ScrollArea scrollbar styling */
  .editor-container :global(.cm-scroller::-webkit-scrollbar) {
    width: 16px;
    height: 16px;
  }

  .editor-container :global(.cm-scroller::-webkit-scrollbar-track) {
    background: transparent;
  }

  .editor-container :global(.cm-scroller::-webkit-scrollbar-thumb) {
    background: #27272a;
    border-radius: 9999px;
    border: 4px solid transparent;
    background-clip: content-box;
  }

  .editor-container :global(.cm-scroller::-webkit-scrollbar-corner) {
    background: transparent;
  }
</style>
