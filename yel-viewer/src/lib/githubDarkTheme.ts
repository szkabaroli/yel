import { EditorView } from "@codemirror/view";

// GitHub Dark theme for CodeMirror
export const githubDarkTheme = EditorView.theme({
  "&": {
    backgroundColor: "#0d1117",
    color: "#c9d1d9",
  },
  ".cm-content": {
    caretColor: "#c9d1d9",
    fontFamily: "'JetBrains Mono', 'Fira Code', 'Consolas', monospace",
  },
  ".cm-cursor, .cm-dropCursor": {
    borderLeftColor: "#c9d1d9",
  },
  "&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection": {
    backgroundColor: "#264f78",
  },
  ".cm-activeLine": {
    backgroundColor: "#161b22",
  },
  ".cm-gutters": {
    backgroundColor: "#0d1117",
    color: "#484f58",
    borderRight: "1px solid #21262d",
  },
  ".cm-activeLineGutter": {
    backgroundColor: "#161b22",
    color: "#c9d1d9",
  },
  ".cm-lineNumbers .cm-gutterElement": {
    padding: "0 8px",
  },
  ".cm-foldPlaceholder": {
    backgroundColor: "#21262d",
    border: "none",
    color: "#8b949e",
  },
  ".cm-tooltip": {
    backgroundColor: "#161b22",
    border: "1px solid #30363d",
    color: "#c9d1d9",
  },
  ".cm-tooltip-autocomplete": {
    "& > ul > li[aria-selected]": {
      backgroundColor: "#264f78",
    },
  },
}, { dark: true });
