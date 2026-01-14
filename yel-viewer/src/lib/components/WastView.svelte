<script lang="ts">
    import { onMount, onDestroy } from 'svelte'
    import { EditorView, lineNumbers } from '@codemirror/view'
    import { EditorState } from '@codemirror/state'
    import { StreamLanguage } from '@codemirror/language'
    import { syntaxHighlighting } from '@codemirror/language'
    import { tags, tagHighlighter } from '@lezer/highlight'

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

            // Storage modifiers (storage.modifier.wat)
            if (stream.match(/^(mut|shared|passive)\b/)) {
                return 'modifier'
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

            // GC heap types
            const heapTypes =
                /^(func|extern|any|eq|nofunc|noextern|struct|array|none|sub|final|rec|field)\b(?!\.)/
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
