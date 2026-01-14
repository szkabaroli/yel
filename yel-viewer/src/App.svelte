<script lang="ts">
    import CodeEditor from './lib/components/CodeEditor.svelte'
    import TreeView from './lib/components/TreeView.svelte'
    import JsonTreeView from './lib/components/JsonTreeView.svelte'
    //import UIPreview from "./lib/components/UIPreview.svelte";
    import WasmPreview from './lib/components/WasmPreview.svelte'
    import WastView from './lib/components/WastView.svelte'
    import WitView from './lib/components/WitView.svelte'
    import ConsolePanel from './lib/components/ConsolePanel.svelte'
    import { sampleCode, examples } from './lib/sampleData'
    import { convertHirToTree, resetHirIdCounter } from './lib/hirToTree'
    import { convertThirToTree } from './lib/thirToTree'
    import { createLirTree } from './lib/lirData'
    import {
        compile,
        check,
        parseToJson,
        compileToHir,
        compileToThir,
        compileToWasm,
        getVersion,
        type Diagnostic,
        type VersionInfo,
    } from './lib/compiler'
    import type {
        TreeNode,
        Span,
        AstFile,
        HirOutput,
        ThirOutput,
    } from './lib/types'

    import * as Tabs from '$lib/components/ui/tabs'
    import * as Select from '$lib/components/ui/select'
    import * as Resizable from '$lib/components/ui/resizable'

    type ViewType =
        | 'preview'
        | 'ast'
        | 'hir'
        | 'hir-json'
        | 'thir'
        | 'lir'
        | 'wasm'
        | 'wit'

    const views: { id: ViewType; label: string }[] = [
        { id: 'preview', label: 'Preview' },
        { id: 'ast', label: 'AST' },
        { id: 'hir', label: 'HIR' },
        { id: 'hir-json', label: 'HIR JSON' },
        { id: 'thir', label: 'THIR' },
        { id: 'lir', label: 'LIR' },
        { id: 'wasm', label: 'WASM' },
        { id: 'wit', label: 'WIT' },
    ]

    let currentView = $state<ViewType>('preview')

    // Selected example
    let selectedExample = $state(examples[0].id)

    // Editable source code
    let sourceCode = $state(sampleCode)

    function handleExampleChange(exampleId: string) {
        selectedExample = exampleId
        const example = examples.find((e) => e.id === exampleId)
        if (example) {
            sourceCode = example.code
        }
    }

    // Compilation results
    let compiledWit = $state<string>('')
    let compiledWast = $state<string>('')
    let compiledWasm = $state<Uint8Array | null>(null)
    let diagnostics = $state<Diagnostic[]>([])

    // Compile when source code changes
    $effect(() => {
        // Check for errors (for status display)
        const errors = check('source.yel', sourceCode)
        diagnostics = errors

        // Always try to compile to WIT and WAST, even with errors
        const witResult = compile('source.yel', sourceCode, 'wit')
        if (witResult.tag === 'success') {
            compiledWit = witResult.val.witCode
        } else {
            const errorMsg = witResult.val
                .map((d) => `${d.line}:${d.column}: ${d.message}`)
                .join('\n;; ')
            compiledWit = `;; Errors:\n;; ${errorMsg}`
        }

        const wastResult = compile('source.yel', sourceCode, 'wast')
        if (wastResult.tag === 'success') {
            compiledWast = wastResult.val.wastCode
        } else {
            const errorMsg = wastResult.val
                .map((d) => `${d.line}:${d.column}: ${d.message}`)
                .join('\n;; ')
            compiledWast = `;; Errors:\n;; ${errorMsg}`
        }

        // Compile to WASM for live preview
        const wasmResult = compileToWasm('source.yel', sourceCode)
        compiledWasm = wasmResult
    })

    function handleCodeChange(newCode: string) {
        sourceCode = newCode
    }

    // Parse AST, HIR, THIR reactively when source changes
    let hirTree = $state<TreeNode | null>(null)
    let thirTree = $state<TreeNode | null>(null)
    let rawAstData = $state<AstFile | null>(null)
    let rawHirData = $state<HirOutput | null>(null)

    $effect(() => {
        // Parse AST
        const astResult = parseToJson(sourceCode)
        if (astResult.ok) {
            try {
                const ast: AstFile = JSON.parse(astResult.value)
                rawAstData = ast
            } catch (e) {
                rawAstData = null
            }
        } else {
            rawAstData = null
        }

        // Compile to HIR
        const hirResult = compileToHir('source.yel', sourceCode)
        if (hirResult.ok) {
            try {
                const hir: HirOutput = JSON.parse(hirResult.value)
                hirTree = convertHirToTree(hir)
                rawHirData = hir
            } catch (e) {
                hirTree = null
                rawHirData = null
            }
        } else {
            hirTree = null
            rawHirData = null
        }

        // Compile to THIR
        const thirResult = compileToThir('source.yel', sourceCode)
        if (thirResult.ok) {
            try {
                const thir: ThirOutput = JSON.parse(thirResult.value)
                thirTree = convertThirToTree(thir)
            } catch (e) {
                thirTree = null
            }
        } else {
            thirTree = null
        }
    })

    // Static tree for LIR (not yet implemented with real data)
    const lirTree = createLirTree()

    const currentTree = $derived.by<TreeNode | null>(() => {
        switch (currentView) {
            case 'hir':
                return hirTree
            case 'thir':
                return thirTree
            case 'lir':
                return lirTree
            default:
                return null
        }
    })

    let selectedNode = $state<TreeNode | null>(null)
    let hoveredNode = $state<TreeNode | null>(null)
    let selectedSpan = $state<Span | null>(null)
    let hoveredSpan = $state<Span | null>(null)

    // Get the span to highlight (selected takes priority over hovered)
    const highlightSpan = $derived.by<Span | null>(() => {
        if (currentView === 'preview') {
            return selectedSpan ?? hoveredSpan ?? null
        }
        return selectedNode?.span ?? hoveredNode?.span ?? null
    })

    function handleSelect(node: TreeNode) {
        selectedNode = selectedNode?.id === node.id ? null : node
    }

    function handleHover(node: TreeNode | null) {
        hoveredNode = node
    }

    function handleSelectSpan(span: Span) {
        selectedSpan = selectedSpan?.start === span.start ? null : span
    }

    function handleHoverSpan(span: Span | null) {
        hoveredSpan = span
    }

    function switchView(view: ViewType) {
        currentView = view
        selectedNode = null
        hoveredNode = null
        selectedSpan = null
        hoveredSpan = null
    }

    // Keyboard shortcut to clear selection
    function handleKeydown(e: KeyboardEvent) {
        if (e.key === 'Escape') {
            selectedNode = null
            selectedSpan = null
        }
    }

    // Track code pane height for calculating console panel minSize
    let codePaneHeight = $state(0)
    const consoleMinSize = $derived((36 / codePaneHeight) * 100)

    // Compiler version info
    let versionInfo = $state<VersionInfo | null>(null)
    $effect(() => {
        try {
            versionInfo = getVersion()
        } catch (e) {
            console.error('Failed to get version info:', e)
        }
    })
</script>

<svelte:window onkeydown={handleKeydown} />

<div class="flex flex-col h-screen bg-background text-foreground">
    <!-- Header -->
    <header
        class="flex items-center gap-5 px-5 py-1.5 bg-card border-b border-border"
    >
        <div class="flex items-center gap-2">
            <picture>
                <source media="(prefers-color-scheme: dark)" srcset="/yel_logo_dark.svg" />
                <img src="/yel_logo_light.svg" alt="Yel" class="h-6 w-6" />
            </picture>
            <h1 class="text-sm font-bold text-foreground" style="font-family: 'Red Hat Display', sans-serif;">Yel Viewer</h1>
        </div>

        <Select.Root
            type="single"
            value={selectedExample}
            onValueChange={(v) => v && handleExampleChange(v)}
        >
            <Select.Trigger class="w-[180px] h-8 text-xs">
                {examples.find((e) => e.id === selectedExample)?.name ??
                    'Select example'}
            </Select.Trigger>
            <Select.Content>
                {#each examples as example (example.id)}
                    <Select.Item value={example.id}>
                        {example.name}
                    </Select.Item>
                {/each}
            </Select.Content>
        </Select.Root>

        <Tabs.Root
            value={currentView}
            onValueChange={(v) => v && switchView(v as ViewType)}
        >
            <Tabs.List class="bg-secondary/50 h-8">
                {#each views as view}
                    <Tabs.Trigger value={view.id} class="text-xs">
                        {view.label}
                    </Tabs.Trigger>
                {/each}
            </Tabs.List>
        </Tabs.Root>

        <!-- Spacer to push version to right -->
        <div class="flex-1"></div>

        <!-- Version Info -->
        {#if versionInfo}
            <div class="flex items-center gap-2 text-xs text-muted-foreground">
                <span class="font-mono">
                    v{versionInfo.version}
                </span>
                <span class="text-muted-foreground/60">•</span>
                <a
                    href="https://github.com/szkabaroli/yel/commit/{versionInfo.commit}"
                    target="_blank"
                    rel="noopener noreferrer"
                    class="font-mono hover:text-foreground transition-colors"
                    title="View commit on GitHub"
                >
                    {versionInfo.commit.slice(0, 7)}
                </a>
            </div>
        {/if}
    </header>

    <!-- Main Content -->
    <Resizable.PaneGroup direction="horizontal" class="flex-1">
        <!-- Code Pane with Console -->
        <Resizable.Pane defaultSize={50} minSize={20}>
            <div class="h-full" bind:clientHeight={codePaneHeight}>
                <Resizable.PaneGroup
                    direction="vertical"
                    class="h-full"
                    autoSaveId="code-console-layout"
                >
                    <!-- Code Editor -->
                    <Resizable.Pane defaultSize={70} minSize={20}>
                        <div class="flex flex-col h-full">
                            <div
                                class="flex items-center justify-between h-12 shrink-0 px-4 bg-card border-b border-border text-xs font-semibold text-muted-foreground"
                            >
                                <span>Source Code</span>
                                <div class="flex items-center gap-3">
                                    {#if compiledWasm}
                                        <span class="text-muted-foreground font-normal">
                                            Compiled size: {(
                                                compiledWasm.byteLength / 1024
                                            ).toFixed(1)} KB
                                        </span>
                                    {/if}
                                </div>
                            </div>
                            <div class="flex-1 overflow-hidden">
                                <CodeEditor
                                    code={sourceCode}
                                    {highlightSpan}
                                    {diagnostics}
                                    editable={true}
                                    onCodeChange={handleCodeChange}
                                />
                            </div>
                        </div>
                    </Resizable.Pane>

                    <Resizable.Handle />

                    <!-- Console Panel -->
                    <Resizable.Pane defaultSize={30} minSize={consoleMinSize}>
                        <ConsolePanel {diagnostics} />
                    </Resizable.Pane>
                </Resizable.PaneGroup>
            </div>
        </Resizable.Pane>

        <Resizable.Handle />

        <!-- Tree/Preview Pane -->
        <Resizable.Pane defaultSize={50} minSize={20}>
            <div class="flex flex-col h-full">
                {#if currentView === 'preview'}
                    <WasmPreview
                        wasmBytes={compiledWasm}
                        onError={(err) =>
                            console.error('[App] WASM Preview error:', err)}
                    />
                {:else if currentView === 'wasm'}
                    <WastView code={compiledWast} />
                {:else if currentView === 'wit'}
                    <WitView code={compiledWit} />
                {:else if currentView === 'ast' && rawAstData}
                    <JsonTreeView data={rawAstData} label="AST" />
                {:else if currentView === 'hir-json' && rawHirData}
                    <JsonTreeView data={rawHirData} label="HIR" />
                {:else if currentTree}
                    <TreeView
                        tree={currentTree}
                        {selectedNode}
                        {hoveredNode}
                        onSelect={handleSelect}
                        onHover={handleHover}
                    />
                {/if}
            </div>
        </Resizable.Pane>
    </Resizable.PaneGroup>
</div>

<style>
    :global(*) {
        box-sizing: border-box;
    }
</style>
