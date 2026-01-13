<script lang="ts">
    import { onMount, onDestroy } from 'svelte'
    import { EditorView, lineNumbers } from '@codemirror/view'
    import { EditorState } from '@codemirror/state'
    import { StreamLanguage } from '@codemirror/language'
    import { HighlightStyle, syntaxHighlighting } from '@codemirror/language'
    import { tags } from '@lezer/highlight'

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

    // WAST highlight style based on TextMate grammar scopes
    const wastHighlightStyle = HighlightStyle.define([
        // storage.type.wat - module elements
        { tag: tags.keyword, color: '#ff7b72' }, // Red for module keywords
        // entity.name.type.wat - type names
        { tag: tags.typeName, color: '#79c0ff' }, // Blue for types
        // keyword.operator.word.wat - instructions
        { tag: tags.operator, color: '#e4e4e7' }, // White for instructions
        // keyword.control.wat - control flow
        { tag: tags.controlKeyword, color: '#d2a8ff' }, // Purple for control flow
        // variable.other.wat / entity.name.function.wat
        { tag: tags.variableName, color: '#ffa657' }, // Orange for $variables
        // string.quoted.double.wat
        { tag: tags.string, color: '#a5d6ff' }, // Light blue for strings
        // constant.numeric.wat
        { tag: tags.number, color: '#79c0ff' }, // Blue for numbers
        // comment.wat
        { tag: tags.comment, color: '#8b949e' }, // Gray for comments
        // annotations
        { tag: tags.meta, color: '#ff7b72' }, // Red for annotations (same as keywords)
        // brackets
        { tag: tags.bracket, color: '#8b949e' }, // Gray for brackets
        // catch-all names
        { tag: tags.name, color: '#7ee787' }, // Green for names/identifiers
        // storage.modifier.wat - mut, shared, passive
        { tag: tags.modifier, color: '#ff7b72' }, // Red for modifiers
        // entity.other.attribute-name.wat - offset=, align=, string-encoding=
        { tag: tags.attributeName, color: '#ff7b72' }, // Reddish orange for attributes
    ])

    onMount(() => {
        const state = EditorState.create({
            doc: code,
            extensions: [
                EditorView.editable.of(false),
                EditorState.readOnly.of(true),
                lineNumbers(),
                wastLanguage,
                syntaxHighlighting(wastHighlightStyle),
                EditorView.theme(
                    {
                        '&': {
                            height: '100%',
                            fontSize: '14px',
                            backgroundColor: '#000000',
                        },
                        '.cm-scroller': {
                            overflow: 'auto',
                            fontFamily:
                                "'JetBrains Mono', 'Fira Code', monospace",
                        },
                        '.cm-content': {
                            caretColor: '#fafafa',
                        },
                        '.cm-gutters': {
                            backgroundColor: '#000000',
                            color: '#52525b',
                            borderRight: '1px solid #27272a',
                        },
                        '.cm-activeLineGutter': {
                            backgroundColor: '#09090b',
                        },
                        '.cm-activeLine': {
                            backgroundColor: '#09090b',
                        },
                    },
                    { dark: true }
                ),
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
