<script lang="ts">
    import { onMount, onDestroy } from 'svelte'
    import { EditorView, lineNumbers, keymap } from '@codemirror/view'
    import { EditorState } from '@codemirror/state'
    import {
        StreamLanguage,
        syntaxHighlighting,
        foldService,
        foldGutter,
        foldKeymap,
    } from '@codemirror/language'
    import { tags, tagHighlighter } from '@lezer/highlight'
    import CopyButton from './CopyButton.svelte'

    interface Props {
        code: string
    }

    let { code }: Props = $props()

    let editorContainer: HTMLDivElement
    let view: EditorView

    // WAT/WAST syntax highlighting based on TextMate grammar
    const wastLanguage = StreamLanguage.define({
        token(stream) {
            // Skip whitespace
            if (stream.eatSpace()) return null

            // Line comments (;; ...)
            if (stream.match(';;')) {
                stream.skipToEnd()
                return 'comment'
            }

            // Block comments (; ... ;)
            if (stream.match('(;')) {
                while (!stream.match(';)') && !stream.eol()) {
                    stream.next()
                }
                return 'comment'
            }

            // Strings
            if (stream.match('"')) {
                while (!stream.eol()) {
                    const ch = stream.next()
                    if (ch === '"') break
                    if (ch === '\\') stream.next() // escape sequences
                }
                return 'string'
            }

            // Attributes: offset=, align=, string-encoding=, etc.
            if (stream.match(/^[a-z][a-z0-9\-]*=/)) {
                return 'attributeName'
            }

            // Control flow instructions (keyword.control.wat)
            // IMPORTANT: Longer patterns first
            const controlFlow =
                /^(return_call_indirect|return_call|call_indirect|call_ref|call|block|loop|if|then|else|end|br_on_cast_fail|br_on_cast|br_on_non_null|br_on_exn|br_table|br_if|br|return|unreachable|nop|try|catch|throw|rethrow)\b/
            if (stream.match(controlFlow)) {
                return 'controlKeyword'
            }

            // Instructions with type prefix (keyword.operator.word.wat)
            // Numeric, memory, variable, reference, vector, atomic, struct, array instructions
            const typedInstructions =
                /^(i32|i64|f32|f64|v128|i8x16|i16x8|i32x4|i64x2|f32x4|f64x2|v8x16|v16x8|v32x4|v64x2|local|global|memory|table|ref|struct|array|i31|extern|atomic)\.[a-z_0-9]+/
            if (stream.match(typedInstructions)) {
                return 'operator'
            }

            // Parametric instructions (keyword.operator.word.wat)
            if (stream.match(/^(drop|select)\b/)) {
                return 'operator'
            }

            // Storage modifiers (storage.modifier.wat). Returned as
            // `keyword` because lezer's `tags.modifier` is a tag
            // transformer (function), not a terminal tag — using it as
            // a standalone class falls through to default styling.
            if (stream.match(/^(mut|shared|passive)\b/)) {
                return 'keyword'
            }

            // Module elements (storage.type.wat) - must follow ( in WAT but we're lenient
            const moduleElements =
                /^(module|component|import|export|memory|data|table|elem|start|func|type|param|result|global|local|instance|core|alias|canon|instantiate|lift|lower|with|realloc)\b/
            if (stream.match(moduleElements)) {
                return 'keyword'
            }

            // Component model keywords
            const componentKeywords =
                /^(own|borrow|resource|dtor|rep|record|variant|enum|flags|tuple|list|option|result|string|char)\b/
            if (stream.match(componentKeywords)) {
                return 'keyword'
            }

            // Type names (entity.name.type.wat) - only when NOT followed by dot
            const typeNames =
                /^(i32|i64|f32|f64|v128|funcref|externref|anyref|eqref|i31ref|nullfuncref|nullexternref|structref|arrayref|nullref|exnref|i8|i16)\b(?!\.)/
            if (stream.match(typeNames)) {
                return 'typeName'
            }

            // Component model types
            const componentTypes =
                /^(u8|u16|u32|u64|s8|s16|s32|s64|float32|float64|bool)\b(?!\.)/
            if (stream.match(componentTypes)) {
                return 'typeName'
            }

            // GC structural keywords (type-section forms, not types themselves)
            const gcStructural =
                /^(struct|array|field|sub|final|rec)\b(?!\.)/
            if (stream.match(gcStructural)) {
                return 'keyword'
            }

            // GC heap-type abstract names
            const heapTypes =
                /^(func|extern|any|eq|nofunc|noextern|none)\b(?!\.)/
            if (stream.match(heapTypes)) {
                return 'typeName'
            }

            // Encoding values and other constants
            if (stream.match(/^(utf8|utf16|latin1)\b/)) {
                return 'typeName'
            }

            // Floating point special values
            if (
                stream.match(/^[+-]?inf\b/) ||
                stream.match(/^[+-]?nan(:0x[0-9a-fA-F]+)?\b/)
            ) {
                return 'number'
            }

            // Floating point hex literal with exponent
            if (
                stream.match(
                    /^[+-]?0x[0-9a-fA-F]*\.?[0-9a-fA-F]+[Pp][+-]?[0-9]+\b/
                )
            ) {
                return 'number'
            }

            // Hex integer
            if (stream.match(/^[+-]?0x[0-9a-fA-F]+\b/)) {
                return 'number'
            }

            // Decimal float or integer
            if (stream.match(/^[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?\b/)) {
                return 'number'
            }

            // Variable/function names ($name)
            if (stream.match(/^\$"[^"]*"/)) {
                return 'variableName'
            }
            if (stream.match(/^\$[0-9A-Za-z!#$%&'*+\-./:<=>\?@\\^_`|~]+/)) {
                return 'variableName'
            }

            // Index annotations (;N;)
            if (stream.match(/^;\d+;/)) {
                return 'meta'
            }

            // Annotation syntax (@name, @producers, @custom, etc.)
            if (stream.match(/^@[a-z][a-z0-9\-]*/)) {
                return 'meta'
            }

            // Annotation field names (processed-by, language, sdk, etc.)
            if (stream.match(/^(processed-by|language|sdk)\b/)) {
                return 'meta'
            }

            // Parentheses
            if (stream.eat('(') || stream.eat(')')) {
                return 'bracket'
            }

            // Any remaining word (catch-all for unknown identifiers)
            if (stream.match(/^[a-zA-Z_][a-zA-Z0-9_\-.:@/]*/)) {
                return 'name'
            }

            stream.next()
            return null
        },
    })

    // Paren-based fold service. WAT is fully `(...)`-nested, so any
    // line that opens a paren whose match lives on a later line can
    // be folded down to its first line. We scan from the line's
    // outermost unmatched `(` forward, respecting string literals
    // and `(;...;)` block comments. `;;` line comments end at EOL,
    // so they don't disturb depth.
    const wastFoldService = foldService.of((state, lineStart, lineEnd) => {
        const doc = state.doc
        const lineText = doc.sliceString(lineStart, lineEnd)
        // Find the position of the *outermost* still-open `(` on the
        // line — i.e., the last `(` whose matching `)` doesn't also
        // sit on this line. Walking the line once tracking depth
        // gives us that.
        let openPos = -1
        let depth = 0
        let i = 0
        while (i < lineText.length) {
            const ch = lineText[i]
            // Line comment runs to EOL.
            if (ch === ';' && lineText[i + 1] === ';') break
            // Block comment.
            if (ch === '(' && lineText[i + 1] === ';') {
                i += 2
                while (i < lineText.length) {
                    if (lineText[i] === ';' && lineText[i + 1] === ')') {
                        i += 2
                        break
                    }
                    i++
                }
                continue
            }
            // String.
            if (ch === '"') {
                i++
                while (i < lineText.length) {
                    if (lineText[i] === '\\') { i += 2; continue }
                    if (lineText[i] === '"') { i++; break }
                    i++
                }
                continue
            }
            if (ch === '(') {
                if (depth === 0) openPos = i
                depth++
            } else if (ch === ')') {
                depth--
            }
            i++
        }
        if (openPos < 0 || depth <= 0) return null

        // Walk forward from after the open paren to find the matching
        // close, respecting strings + block comments, treating `;;`
        // line comments as run-to-EOL.
        const total = doc.length
        let pos = lineStart + openPos + 1
        let d = 1
        while (pos < total) {
            const c = doc.sliceString(pos, pos + 1)
            const c2 = pos + 1 < total ? doc.sliceString(pos + 1, pos + 2) : ''
            if (c === ';' && c2 === ';') {
                // skip to next line
                const ln = doc.lineAt(pos)
                pos = ln.to + 1
                continue
            }
            if (c === '(' && c2 === ';') {
                pos += 2
                while (pos < total) {
                    const a = doc.sliceString(pos, pos + 1)
                    const b = pos + 1 < total ? doc.sliceString(pos + 1, pos + 2) : ''
                    if (a === ';' && b === ')') { pos += 2; break }
                    pos++
                }
                continue
            }
            if (c === '"') {
                pos++
                while (pos < total) {
                    const a = doc.sliceString(pos, pos + 1)
                    if (a === '\\') { pos += 2; continue }
                    if (a === '"') { pos++; break }
                    pos++
                }
                continue
            }
            if (c === '(') d++
            else if (c === ')') {
                d--
                if (d === 0) {
                    // Fold from end of opening line to end of matching `)`.
                    if (pos <= lineEnd) return null // entire form fits on one line
                    return { from: lineEnd, to: pos + 1 }
                }
            }
            pos++
        }
        return null
    })

    // WAST tag highlighter - colors defined in CSS
    const wastHighlighter = tagHighlighter([
        { tag: tags.keyword, class: 'wast-keyword' },
        { tag: tags.typeName, class: 'wast-type' },
        { tag: tags.operator, class: 'wast-operator' },
        { tag: tags.controlKeyword, class: 'wast-control' },
        { tag: tags.variableName, class: 'wast-variable' },
        { tag: tags.string, class: 'wast-string' },
        { tag: tags.number, class: 'wast-number' },
        { tag: tags.comment, class: 'wast-comment' },
        { tag: tags.meta, class: 'wast-meta' },
        { tag: tags.bracket, class: 'wast-bracket' },
        { tag: tags.name, class: 'wast-name' },
        { tag: tags.modifier, class: 'wast-modifier' },
        { tag: tags.attributeName, class: 'wast-attr' },
    ])

    onMount(() => {
        const state = EditorState.create({
            doc: code,
            extensions: [
                EditorView.editable.of(false),
                EditorState.readOnly.of(true),
                lineNumbers(),
                wastLanguage,
                wastFoldService,
                foldGutter(),
                keymap.of(foldKeymap),
                syntaxHighlighting(wastHighlighter),
                EditorView.theme({
                    '&': {
                        height: '100%',
                        fontSize: '14px',
                    },
                    '.cm-scroller': {
                        overflow: 'auto',
                        fontFamily:
                            "'JetBrains Mono', 'Fira Code', monospace",
                    },
                }),
            ],
        })

        view = new EditorView({
            state,
            parent: editorContainer,
        })
    })

    onDestroy(() => {
        view?.destroy()
    })

    // Update editor content when code prop changes
    $effect(() => {
        if (view && code !== view.state.doc.toString()) {
            view.dispatch({
                changes: {
                    from: 0,
                    to: view.state.doc.length,
                    insert: code,
                },
            })
        }
    })
</script>

<div class="flex flex-col h-full bg-background">
    <!-- Header -->
    <div
        class="flex items-center justify-between h-12 shrink-0 px-4 bg-card border-b border-border"
    >
        <h3 class="text-xs font-semibold text-muted-foreground">
            WebAssembly Text Format
        </h3>
        <CopyButton text={code} title="Copy WAT" />
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
