<script lang="ts">
  import { onMount, onDestroy } from "svelte";
  import { EditorView, Decoration, type DecorationSet, lineNumbers } from "@codemirror/view";
  import { EditorState, StateField, StateEffect } from "@codemirror/state";
  import { linter, lintGutter, type Diagnostic as CMDiagnostic } from "@codemirror/lint";
  import { yelSyntax } from "../yelLanguage";
  import type { Span } from "../types";
  import type { Diagnostic } from "../compiler";

  interface Props {
    code: string;
    highlightSpan?: Span | null;
    editable?: boolean;
    diagnostics?: Diagnostic[];
    onCodeChange?: (code: string) => void;
  }

  let { code, highlightSpan = null, editable = false, diagnostics = [], onCodeChange }: Props = $props();

  let editorContainer: HTMLDivElement;
  let view: EditorView;

  // Effect to set highlighted range
  const setHighlight = StateEffect.define<Span | null>();

  // Effect to set diagnostics
  const setDiagnostics = StateEffect.define<CMDiagnostic[]>();

  // Decoration for highlighting
  const highlightMark = Decoration.mark({ class: "cm-highlight" });

  // State field to track and display highlights
  const highlightField = StateField.define<DecorationSet>({
    create() {
      return Decoration.none;
    },
    update(decorations, tr) {
      for (const effect of tr.effects) {
        if (effect.is(setHighlight)) {
          if (effect.value) {
            const { start, end } = effect.value;
            // Clamp to document bounds
            const docLen = tr.state.doc.length;
            const safeStart = Math.max(0, Math.min(start, docLen));
            const safeEnd = Math.max(safeStart, Math.min(end, docLen));
            if (safeStart < safeEnd) {
              return Decoration.set([highlightMark.range(safeStart, safeEnd)]);
            }
          }
          return Decoration.none;
        }
      }
      return decorations;
    },
    provide: (f) => EditorView.decorations.from(f),
  });

  // State field to store diagnostics for the linter
  const diagnosticsField = StateField.define<CMDiagnostic[]>({
    create: () => [],
    update(diags, tr) {
      for (const effect of tr.effects) {
        if (effect.is(setDiagnostics)) {
          return effect.value;
        }
      }
      return diags;
    }
  });

  // Linter that reads from the diagnostics field
  const externalLinter = linter((view) => view.state.field(diagnosticsField));

  onMount(() => {
    const extensions = [
      lineNumbers(),
      ...yelSyntax,
      highlightField,
      diagnosticsField,
      externalLinter,
      lintGutter(),
    ];

    if (!editable) {
      extensions.push(EditorView.editable.of(false));
      extensions.push(EditorState.readOnly.of(true));
    } else {
      // Add update listener for editable mode
      extensions.push(EditorView.updateListener.of((update) => {
        if (update.docChanged && onCodeChange) {
          onCodeChange(update.state.doc.toString());
        }
      }));
    }

    extensions.push(EditorView.theme({
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
      ".cm-highlight": {
        backgroundColor: "rgba(250, 204, 21, 0.25)",
        borderRadius: "2px",
      },
      // Lint gutter styling
      ".cm-lint-marker-error": {
        content: '"●"',
        color: "#ef4444",
      },
      ".cm-lint-marker-warning": {
        content: '"●"',
        color: "#eab308",
      },
      // Diagnostic underlines
      ".cm-lintRange-error": {
       
      },
      ".cm-lintRange-warning": {
        
        
      },
      // Tooltip panel
      ".cm-tooltip-lint": {
        backgroundColor: "black",
        border: "1px solid #27272a",
        padding: "8px 12px",
        fontSize: "12px",
      },
    }, { dark: true }));

    const state = EditorState.create({
      doc: code,
      extensions,
    });

    view = new EditorView({
      state,
      parent: editorContainer,
    });
  });

  onDestroy(() => {
    view?.destroy();
  });

  // React to highlight changes
  $effect(() => {
    if (view) {
      view.dispatch({
        effects: setHighlight.of(highlightSpan),
      });

      // Scroll highlighted span into view
      if (highlightSpan) {
        const docLen = view.state.doc.length;
        const safeStart = Math.max(0, Math.min(highlightSpan.start, docLen));
        view.dispatch({
          effects: EditorView.scrollIntoView(safeStart, { y: "center" }),
        });
      }
    }
  });

  // React to external code changes (e.g., selecting a different example)
  $effect(() => {
    if (view && code !== view.state.doc.toString()) {
      view.dispatch({
        changes: { from: 0, to: view.state.doc.length, insert: code },
      });
    }
  });

  // React to diagnostics changes
  $effect(() => {
    if (view) {
      const doc = view.state.doc;
      const cmDiagnostics: CMDiagnostic[] = diagnostics.map(d => {
        // Convert line:column to character offset
        const lineNum = Math.max(1, Math.min(d.line, doc.lines));
        const line = doc.line(lineNum);
        const col = Math.max(0, Math.min(d.column - 1, line.length));
        const from = line.from + col;
        // Use length from diagnostic, default to 1 if not provided
        const length = d.length > 0 ? d.length : 1;
        const to = Math.min(from + length, doc.length);

        return {
          from,
          to,
          severity: d.severity === "error" ? "error" : d.severity === "warning" ? "warning" : "info",
          message: d.message,
        };
      });

      view.dispatch({
        effects: setDiagnostics.of(cmDiagnostics),
      });
    }
  });
</script>

<div class="editor-container" bind:this={editorContainer}></div>

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
