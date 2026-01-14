<script lang="ts">
  import { onMount, onDestroy } from "svelte";
  import { EditorView, lineNumbers } from "@codemirror/view";
  import { EditorState } from "@codemirror/state";
  import { StreamLanguage } from "@codemirror/language";
  import { syntaxHighlighting } from "@codemirror/language";
  import { tags, tagHighlighter } from "@lezer/highlight";

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

  // WIT tag highlighter - colors defined in CSS
  const witHighlighter = tagHighlighter([
    { tag: tags.keyword, class: "wit-keyword" },
    { tag: tags.typeName, class: "wit-type" },
    { tag: tags.function(tags.variableName), class: "wit-function" },
    { tag: tags.variableName, class: "wit-variable" },
    { tag: tags.string, class: "wit-string" },
    { tag: tags.comment, class: "wit-comment" },
    { tag: tags.meta, class: "wit-meta" },
    { tag: tags.bracket, class: "wit-bracket" },
    { tag: tags.operator, class: "wit-operator" },
    { tag: tags.punctuation, class: "wit-punctuation" },
    { tag: tags.namespace, class: "wit-namespace" },
  ]);

  onMount(() => {
    const state = EditorState.create({
      doc: code,
      extensions: [
        EditorView.editable.of(false),
        EditorState.readOnly.of(true),
        lineNumbers(),
        witLanguage,
        syntaxHighlighting(witHighlighter),
        EditorView.theme({
          "&": {
            height: "100%",
            fontSize: "14px",
          },
          ".cm-scroller": {
            overflow: "auto",
            fontFamily: "'JetBrains Mono', 'Fira Code', monospace",
          },
        }),
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
    background-color: var(--color-background);
  }

  .editor-container :global(.cm-content) {
    caret-color: var(--color-foreground);
  }

  .editor-container :global(.cm-gutters) {
    background-color: var(--color-card);
    color: var(--color-muted-foreground);
    border-right: 1px solid var(--color-border);
  }

  .editor-container :global(.cm-activeLineGutter) {
    background-color: var(--color-secondary);
  }

  .editor-container :global(.cm-activeLine) {
    background-color: var(--color-secondary);
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
    background: var(--color-border);
    border-radius: 9999px;
    border: 4px solid transparent;
    background-clip: content-box;
  }

  .editor-container :global(.cm-scroller::-webkit-scrollbar-corner) {
    background: transparent;
  }
</style>
