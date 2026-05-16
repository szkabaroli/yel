//! Graphviz DOT output for the signal/effect dependency graph.
//!
//! Visualises a component's reactive wiring: signals (ovals) flow into
//! effects (boxes) via `reads` edges, effects flow back into signals via
//! `writes` edges, and DOM-updating effects emit an additional edge to a
//! sink node describing the DOM mutation (text / attribute / class / style).
//!
//! Used for debugging and teaching — run with `yelc compile -o dot file.yel`
//! and render via `dot -Tpng -o graph.png`.
//!
//! This renderer walks LIR directly and does not depend on any codegen
//! helpers; emit order is deterministic for snapshot stability.
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

use yel_core::{
    CompilerContext, DefId, InternedTyKind, NodeId, Ty,
    ids::BlockId,
    lir::{LirNode, LirNodeKind, LirOp, LirResource, StringId, block::TreeBoundaryKind},
};

use crate::CodegenError;

/// Options for DOT generation.
#[derive(Debug, Clone, Default)]
pub struct DotOptions {
    /// Include `(type)` beside each signal name. Default true.
    pub include_types: bool,
    /// Include a sink node per effect describing its DOM mutation (text
    /// content, attribute name, style/class target). Default true.
    pub include_dom_sinks: bool,
    /// For each effect / handler, render the chain of update-block
    /// functions reached via `CallBlock` / `CallBlock2` as separate
    /// rounded nodes connected by `calls` edges. Surfaces which named
    /// `update_b<boundary>_s<signal>` / `for-update` / `if-update` /
    /// `derived-update` blocks each signal write fans out into. Default
    /// true.
    pub include_update_fns: bool,
}

impl DotOptions {
    pub fn new() -> Self {
        Self {
            include_types: true,
            include_dom_sinks: true,
            include_update_fns: true,
        }
    }
}

/// Render the signal/effect graph for `components` as a DOT document.
pub fn generate_dot(
    components: &[LirResource],
    ctx: &CompilerContext,
    options: &DotOptions,
) -> Result<String, CodegenError> {
    let mut out = String::new();
    writeln!(out, "digraph yel_signal_graph {{").unwrap();
    writeln!(out, "  rankdir=LR;").unwrap();
    writeln!(out, "  compound=true;").unwrap();
    writeln!(out, "  node [fontname=\"Helvetica\", fontsize=10];").unwrap();
    writeln!(out, "  edge [fontname=\"Helvetica\", fontsize=9];").unwrap();

    // Build the set of DefIds owned by each component's local signals so
    // every downstream `reads` / `writes` edge can resolve its target either
    // to that component's cluster or to the module-scope globals cluster.
    // Without this resolution the DOT referenced dangling nodes like
    // `c0_sig_372` — a global property DefId that no component "owns" —
    // producing an unrenderable graph.
    let mut local_signals: HashSet<DefId> = HashSet::new();
    for comp in components {
        for sig in &comp.signals {
            local_signals.insert(sig.def_id);
        }
    }

    // Referenced-but-not-local signals are globals. Collect them in
    // discovery order so the globals cluster is stable across runs.
    let mut global_refs: Vec<DefId> = Vec::new();
    for comp in components {
        for eff in &comp.effects {
            for &d in &eff.dependencies {
                if !local_signals.contains(&d) && !global_refs.contains(&d) {
                    global_refs.push(d);
                }
            }
            let (writes, _) = collect_effect_outputs(comp, ctx, eff.update_block, &mut Vec::new());
            for w in writes {
                if !local_signals.contains(&w) && !global_refs.contains(&w) {
                    global_refs.push(w);
                }
            }
        }
        for (_, handler_block) in discover_event_handlers(comp) {
            let (writes, _) = collect_effect_outputs(comp, ctx, handler_block, &mut Vec::new());
            for w in writes {
                if !local_signals.contains(&w) && !global_refs.contains(&w) {
                    global_refs.push(w);
                }
            }
            // Two-way bind targets are written by codegen-only logic;
            // they won't appear in the op walk above.
            if let Some(&target) = comp.input_binding_handlers.get(&handler_block) {
                if !local_signals.contains(&target) && !global_refs.contains(&target) {
                    global_refs.push(target);
                }
            }
        }
    }

    if !global_refs.is_empty() {
        render_globals(&mut out, &global_refs, ctx, options);
    }

    for (comp_idx, comp) in components.iter().enumerate() {
        render_component(&mut out, comp_idx, comp, ctx, options);
    }

    writeln!(out, "}}").unwrap();
    Ok(out)
}

/// Render each referenced global as its own cluster, same shape as
/// component clusters. A property belongs to its owning `GlobalDef`
/// (the property is a `FieldDef` whose `owner` points at the enclosing
/// global), so group properties by that owner. Discovery order is
/// preserved — the first-seen global gets rendered first, its first-seen
/// property first within the cluster.
fn render_globals(
    out: &mut String,
    globals: &[DefId],
    ctx: &CompilerContext,
    options: &DotOptions,
) {
    // Group properties by their owning GlobalDef DefId. Properties without
    // an owner (shouldn't happen for parsed source — guard defensively)
    // land under a synthetic "<orphan>" bucket.
    let mut owners_in_order: Vec<DefId> = Vec::new();
    let mut by_owner: HashMap<DefId, Vec<DefId>> = HashMap::new();
    let mut orphans: Vec<DefId> = Vec::new();
    for &def_id in globals {
        match ctx.defs.as_field(def_id) {
            Some(field) => {
                if !by_owner.contains_key(&field.owner) {
                    owners_in_order.push(field.owner);
                }
                by_owner.entry(field.owner).or_default().push(def_id);
            }
            None => orphans.push(def_id),
        }
    }

    for owner_id in owners_in_order {
        let owner_name = ctx.str(ctx.defs.name(owner_id));
        writeln!(out, "  subgraph cluster_global_{} {{", owner_id.0).unwrap();
        writeln!(out, "    label=\"global {}\";", escape_dot(&owner_name)).unwrap();
        writeln!(out, "    style=\"rounded,dashed\";").unwrap();
        writeln!(out, "    color=\"#b56576\";").unwrap();
        for &prop_def in by_owner.get(&owner_id).unwrap() {
            render_global_property_node(out, prop_def, ctx, options);
        }
        writeln!(out, "  }}").unwrap();
    }

    // Unowned orphans — render bare, outside any cluster, as a last-resort
    // fallback that still surfaces the edge target.
    for def_id in orphans {
        render_global_property_node(out, def_id, ctx, options);
    }
}

/// Emit the DOT node for a single global property. Shared between the
/// owned-by-global and orphan paths so both get the same styling.
fn render_global_property_node(
    out: &mut String,
    def_id: DefId,
    ctx: &CompilerContext,
    options: &DotOptions,
) {
    let name = ctx.str(ctx.defs.name(def_id));
    let ty = ctx.defs.type_of(def_id);
    let label = match (options.include_types, ty) {
        (true, Some(t)) => format!("{}\\n: {}", name, type_label(ctx, t)),
        _ => name.to_string(),
    };
    writeln!(
        out,
        "    \"{node}\" [shape=ellipse, style=\"filled,dashed\", fillcolor=\"#fce4ec\", label=\"{label}\", class=\"yel-signal-global\"];",
        node = global_node_id(def_id.0),
        label = escape_dot(&label),
    )
    .unwrap();
}

/// Resolve a signal `DefId` referenced by an effect or handler to the DOT
/// node that renders it. A DefId present in this component's signals maps
/// to the local cluster node; otherwise it's a module-scope global and
/// refers to the shared globals cluster.
fn resolve_signal_ref(comp_idx: usize, comp: &LirResource, def_id: DefId) -> String {
    if comp.signals.iter().any(|s| s.def_id == def_id) {
        signal_node_id(comp_idx, def_id.0)
    } else {
        global_node_id(def_id.0)
    }
}

fn render_component(
    out: &mut String,
    comp_idx: usize,
    comp: &LirResource,
    ctx: &CompilerContext,
    options: &DotOptions,
) {
    let comp_name = ctx.str(comp.name);
    writeln!(out, "  subgraph cluster_c{} {{", comp_idx).unwrap();
    writeln!(
        out,
        "    label=\"component {}{}\";",
        comp_name,
        if comp.is_export { " (export)" } else { "" }
    )
    .unwrap();
    writeln!(out, "    style=rounded;").unwrap();
    writeln!(out, "    color=\"#888\";").unwrap();

    // Signal nodes.
    for sig in &comp.signals {
        let sig_name = ctx.str(ctx.defs.name(sig.def_id));
        let label = if options.include_types {
            format!("{}\\n: {}", sig_name, type_label(ctx, sig.ty))
        } else {
            sig_name.to_string()
        };
        writeln!(
            out,
            "    \"{node}\" [shape=ellipse, style=filled, fillcolor=\"#eaf4ff\", label=\"{label}\", class=\"yel-signal\"];",
            node = signal_node_id(comp_idx, sig.def_id.0),
            label = escape_dot(&label),
        )
        .unwrap();
    }

    // Track update-fn block nodes already emitted in this component
    // so the same callee reached from multiple effects/handlers fans
    // in to a single node (preserving the "what gets called for what
    // update" view at a glance). Per-component because BlockIds are
    // component-local.
    let mut fn_nodes_seen: HashSet<u32> = HashSet::new();

    // Effect nodes + edges. The yellow effect box is the entry update
    // fn dispatched by the dependency tracker; its label includes the
    // entry block's debug name so it's visually consistent with the
    // purple callee fns rendered by `render_call_chain`. Writes / DOM
    // sinks are attached per-block (not aggregated on the entry) so
    // each `update_b*` / `for-update` / `if-branch-mount` node owns the
    // mutations it actually performs — call edges then describe how
    // control flows between them.
    for eff in &comp.effects {
        let eff_node = effect_node_id(comp_idx, eff.id);
        let entry_fn = block_label(comp, ctx, eff.update_block);
        writeln!(
            out,
            "    \"{node}\" [shape=box, style=\"filled,rounded\", fillcolor=\"#fff5d6\", label=\"effect {id}\\n{entry} #{bid}\", class=\"yel-effect\"];",
            node = eff_node,
            id = eff.id,
            entry = escape_dot(&entry_fn),
            bid = eff.update_block.0,
        )
        .unwrap();

        for &dep_id in &eff.dependencies {
            writeln!(
                out,
                "    \"{from}\" -> \"{to}\" [label=\"reads\", color=\"#3366cc\", class=\"yel-reads\"];",
                from = resolve_signal_ref(comp_idx, comp, dep_id),
                to = eff_node,
            )
            .unwrap();
        }

        render_block_outputs(
            out,
            comp_idx,
            comp,
            ctx,
            &eff_node,
            eff.update_block,
            options,
        );

        if options.include_update_fns {
            render_call_chain(
                out,
                comp_idx,
                comp,
                ctx,
                &eff_node,
                eff.update_block,
                &mut fn_nodes_seen,
                &mut Vec::new(),
                options,
            );
        }
    }

    // Source elements — every body-tree `Element` that has handlers
    // and/or dynamic bindings becomes a rect node. Effects fan out
    // into them via `binds` edges (read path: signal → effect → DOM
    // update on this element); the element fans out into its handler
    // diamonds via `fires` edges (write path: user action → handler
    // → signal write). DFS order matches `AddEventListener` op order
    // in mount blocks, so positional pairing with discover-handlers
    // produces accurate `fires` edges in the common case.
    let body_elements = walk_body_elements(&comp.body_tree);
    for elem in &body_elements {
        let elem_node = element_node_id(comp_idx, elem.node_id);
        writeln!(
            out,
            "    \"{node}\" [shape=box, style=\"filled\", fillcolor=\"#e0f7fa\", label=\"{tag} #{nid}\", class=\"yel-element\"];",
            node = elem_node,
            tag = escape_dot(&elem.tag),
            nid = elem.node_id.0,
        )
        .unwrap();

        // Read path: each dynamic binding effect drives this element.
        for &eff_id in &elem.dynamic_binding_ids {
            if comp.effects.iter().any(|e| e.id == eff_id) {
                writeln!(
                    out,
                    "    \"{from}\" -> \"{to}\" [label=\"binds\", color=\"#2e7d32\", class=\"yel-updates yel-binds\"];",
                    from = effect_node_id(comp_idx, eff_id),
                    to = elem_node,
                )
                .unwrap();
            }
        }
    }

    // Event handlers — `AddEventListener` ops name a block whose body
    // contains the user-authored handler logic. Treat each handler as a
    // source node that *writes* signals (not a reactive effect that reads
    // them). This surfaces user-action → signal flow in the same graph.
    let handler_blocks = discover_event_handlers(comp);

    // Pair body-tree handlers with discovered handler blocks
    // positionally + by event-name match. For each element, consume
    // exactly its handler count from the discovered list, verifying
    // event names align before emitting the edge — mismatches
    // (synthesis vs DFS order divergence) silently skip rather than
    // pointing the wrong way.
    let mut handler_cursor: usize = 0;
    let mut element_to_handler_indices: Vec<(NodeId, Vec<usize>)> = Vec::new();
    for elem in &body_elements {
        let mut idxs = Vec::new();
        for ev in &elem.handler_events {
            if handler_cursor < handler_blocks.len() && handler_blocks[handler_cursor].0 == *ev {
                idxs.push(handler_cursor);
                handler_cursor += 1;
            }
        }
        if !idxs.is_empty() {
            element_to_handler_indices.push((elem.node_id, idxs));
        }
    }
    let element_to_handler_indices: std::collections::HashMap<_, _> =
        element_to_handler_indices.into_iter().collect();
    for (i, (event_name, handler_block)) in handler_blocks.iter().enumerate() {
        let node_id = format!("c{}_handler_{}", comp_idx, i);
        writeln!(
            out,
            "    \"{node}\" [shape=diamond, style=filled, fillcolor=\"#f3e5f5\", label=\"on {event}\", class=\"yel-handler\"];",
            node = node_id,
            event = escape_dot(event_name),
        )
        .unwrap();

        // Find which body-tree element fires this handler and emit a
        // `fires` edge from the element rect to this diamond.
        for (&elem_nid, idxs) in &element_to_handler_indices {
            if idxs.contains(&i) {
                writeln!(
                    out,
                    "    \"{from}\" -> \"{to}\" [label=\"fires\", color=\"#7e57c2\", class=\"yel-fires\"];",
                    from = element_node_id(comp_idx, elem_nid),
                    to = node_id,
                )
                .unwrap();
            }
        }

        render_block_outputs(out, comp_idx, comp, ctx, &node_id, *handler_block, options);

        // Two-way binds (`<input value:bind="signal" />`) compile to a
        // handler block whose signal write is emitted by codegen at
        // WASM time (see `dispatch.rs` honouring
        // `input_binding_handlers`). The LIR block itself has no
        // `SignalWrite` op, so the shallow walker misses it. Surface
        // the implicit write here so the graph shows the user-action
        // → signal edge for two-way binds.
        if let Some(&target) = comp.input_binding_handlers.get(handler_block) {
            writeln!(
                out,
                "    \"{from}\" -> \"{to}\" [label=\"writes (bind)\", color=\"#cc3333\", class=\"yel-writes yel-writes-bind\"];",
                from = node_id,
                to = resolve_signal_ref(comp_idx, comp, target),
            )
            .unwrap();
        }

        if options.include_update_fns {
            render_call_chain(
                out,
                comp_idx,
                comp,
                ctx,
                &node_id,
                *handler_block,
                &mut fn_nodes_seen,
                &mut Vec::new(),
                options,
            );
        }
    }

    writeln!(out, "  }}").unwrap();
}

/// One source-tree Element that participates in the reactive graph
/// (has handlers and/or dynamic bindings). Surfaced as a rect node so
/// the read path (effect → element) and write path (element →
/// handler → signal) are visible in one diagram.
struct ElementInfo {
    node_id: NodeId,
    tag: String,
    handler_events: Vec<String>,
    dynamic_binding_ids: Vec<u32>,
}

/// DFS-walk the body tree collecting every Element node that has
/// handlers or dynamic bindings. DFS order is deliberate: it matches
/// the `AddEventListener` op order produced by mount-block lowering,
/// so caller can pair handler events positionally with the flat
/// `discover_event_handlers` list.
fn walk_body_elements(nodes: &[LirNode]) -> Vec<ElementInfo> {
    let mut out = Vec::new();
    fn rec(nodes: &[LirNode], out: &mut Vec<ElementInfo>) {
        for node in nodes {
            match &node.kind {
                LirNodeKind::Element {
                    tag,
                    handlers,
                    dynamic_binding_ids,
                    children,
                    ..
                } => {
                    if !handlers.is_empty() || !dynamic_binding_ids.is_empty() {
                        out.push(ElementInfo {
                            node_id: node.id,
                            tag: tag.clone(),
                            handler_events: handlers.iter().map(|h| h.event.clone()).collect(),
                            dynamic_binding_ids: dynamic_binding_ids.clone(),
                        });
                    }
                    rec(children, out);
                }
                LirNodeKind::If {
                    then_branch,
                    else_if_branches,
                    else_branch,
                    ..
                } => {
                    rec(then_branch, out);
                    for (_, b) in else_if_branches {
                        rec(b, out);
                    }
                    if let Some(b) = else_branch {
                        rec(b, out);
                    }
                }
                LirNodeKind::For { body, .. } => rec(body, out),
                _ => {}
            }
        }
    }
    rec(nodes, &mut out);
    out
}

/// DOT node id for a source-tree element rect.
fn element_node_id(comp_idx: usize, node_id: yel_core::NodeId) -> String {
    format!("c{}_elem_{}", comp_idx, node_id.0)
}

/// Human-readable label for a block, reconstructed from the
/// structured `BlockDebugName` registered at lowering time. Format
/// matches the WASM name-section style:
/// `<kind>[-b<bid>]*[-s<sid>]` (boundary ids come from
/// `block.boundary_params`). Falls back to `block_<id>` if no name was
/// recorded.
fn block_label(comp: &LirResource, ctx: &CompilerContext, block_id: BlockId) -> String {
    let Some(info) = ctx.get_block_name(comp.def_id, block_id) else {
        return format!("block_{}", block_id.0);
    };
    let block = comp.get_block(block_id);
    let mut s = info.kind.into_owned();
    // Stage 5c: derive boundary-id list from slots.
    for bp in block.boundary_param_ids_from_slots(&comp.slots) {
        s.push_str(&format!("-b{}", bp.0));
    }
    if let Some(sig) = info.signal {
        s.push_str(&format!("-s{}", sig));
    }
    s
}

/// DOT node id for an update-block fn rendered inside a component cluster.
fn fn_node_id(comp_idx: usize, block_id: BlockId) -> String {
    format!("c{}_fn_{}", comp_idx, block_id.0)
}

/// Walk `entry_block`'s ops collecting every `CallBlock` / `CallBlock2`
/// target (recursing into nested If/Loop bodies) and emit a "fn" node
/// per unique callee plus a `calls` edge from `caller_node`. Recurses
/// transitively so deep fan-out chains (effect → root update fn →
/// for-update → for-item-mount → …) are visible. `seen` dedupes nodes
/// across effects/handlers within the same component so shared callees
/// fan-in correctly. `path` guards against pathological cycles in
/// CallBlock chains (mirrors `collect_effect_outputs`).
fn render_call_chain(
    out: &mut String,
    comp_idx: usize,
    comp: &LirResource,
    ctx: &CompilerContext,
    caller_node: &str,
    entry_block: BlockId,
    seen: &mut std::collections::HashSet<u32>,
    path: &mut Vec<BlockId>,
    options: &DotOptions,
) {
    if path.contains(&entry_block) {
        return;
    }
    path.push(entry_block);

    let block = comp.get_block(entry_block);
    let mut callees: Vec<BlockId> = Vec::new();
    collect_call_targets(&block.ops, &mut callees);

    for callee in callees {
        let callee_node = fn_node_id(comp_idx, callee);
        let first_emit = seen.insert(callee.0);
        if first_emit {
            let name = block_label(comp, ctx, callee);
            let summary = describe_block_local(comp, ctx, callee);
            let label = if summary.is_empty() {
                format!("{} #{}", name, callee.0)
            } else {
                format!("{} #{}\\n{}", name, callee.0, summary)
            };
            writeln!(
                out,
                "    \"{node}\" [shape=box, style=\"filled,rounded\", fillcolor=\"#ede7f6\", label=\"{label}\", class=\"yel-fn\"];",
                node = callee_node,
                label = escape_dot(&label),
            )
            .unwrap();
        }
        writeln!(
            out,
            "    \"{from}\" -> \"{to}\" [label=\"calls\", color=\"#6a4ca5\", style=dashed, class=\"yel-calls\"];",
            from = caller_node,
            to = callee_node,
        )
        .unwrap();

        // Outputs (writes + DOM sinks) for the callee fan out from the
        // callee node itself. Emit only on first emission so multi-caller
        // fan-in doesn't duplicate sinks/edges.
        if first_emit {
            render_block_outputs(out, comp_idx, comp, ctx, &callee_node, callee, options);
        }

        render_call_chain(
            out,
            comp_idx,
            comp,
            ctx,
            &callee_node,
            callee,
            seen,
            path,
            options,
        );
    }

    path.pop();
}

/// Emit the `writes` edges (block → signal) and DOM-sink ovals
/// (`updates` → green oval) for a single block's *own* ops. Walks
/// shallow (does not descend into `CallBlock` callees) so each fn node
/// only owns the mutations it directly performs — callees own theirs
/// via their own emission. Sink ids are scoped by `block_id` so
/// per-component fan-in to the same block fuses to one set of leafs.
fn render_block_outputs(
    out: &mut String,
    comp_idx: usize,
    comp: &LirResource,
    ctx: &CompilerContext,
    block_node_id: &str,
    block_id: BlockId,
    options: &DotOptions,
) {
    let block = comp.get_block(block_id);
    let mut writes: Vec<DefId> = Vec::new();
    let mut mutations: Vec<String> = Vec::new();
    walk_ops_shallow(&block.ops, comp, ctx, &mut writes, &mut mutations);

    for &written in &writes {
        writeln!(
            out,
            "    \"{from}\" -> \"{to}\" [label=\"writes\", color=\"#cc3333\", class=\"yel-writes\"];",
            from = block_node_id,
            to = resolve_signal_ref(comp_idx, comp, written),
        )
        .unwrap();
    }

    if options.include_dom_sinks {
        for (i, target) in mutations.iter().enumerate() {
            let sink_id = format!("c{}_blk{}_dom{}", comp_idx, block_id.0, i);
            writeln!(
                out,
                "    \"{sink}\" [shape=oval, style=filled, fillcolor=\"#e8f5e9\", label=\"{label}\", class=\"yel-domsink\"];",
                sink = sink_id,
                label = escape_dot(target),
            )
            .unwrap();
            writeln!(
                out,
                "    \"{from}\" -> \"{to}\" [label=\"updates\", color=\"#2e7d32\", class=\"yel-updates\"];",
                from = block_node_id,
                to = sink_id,
            )
            .unwrap();
        }
    }
}

/// One-line summary of a block's *own* work — signal writes plus DOM
/// mutation kinds — without descending into `CallBlock` callees (those
/// are rendered as their own fn nodes, so attributing their work to
/// the caller would double-count). Boundary kind is prepended for
/// `update_b{B}_s{S}` blocks so it's clear whether a generic-named
/// `update_b3_s12` runs at the root, inside an if-anchor, an if-branch,
/// a for-anchor, or per-iter inside a for body.
fn describe_block_local(comp: &LirResource, ctx: &CompilerContext, block_id: BlockId) -> String {
    let block = comp.get_block(block_id);
    let mut writes: Vec<DefId> = Vec::new();
    let mut mutations: Vec<String> = Vec::new();
    walk_ops_shallow(&block.ops, comp, ctx, &mut writes, &mut mutations);

    let mut parts: Vec<String> = Vec::new();
    if let Some((bid, kind)) = boundary_kind_for_block(comp, block_id) {
        parts.push(format!("b{}:{}", bid, kind));
    }
    for w in &writes {
        let name = ctx.str(ctx.defs.name(*w));
        parts.push(format!("writes {}", name));
    }
    for m in mutations {
        parts.push(m);
    }
    parts.join(", ")
}

/// If the block's first `boundary_param` corresponds to a tree
/// boundary, return a short tag for that boundary's kind ("root",
/// "if-anchor", "if-branch", "for-anchor", "for-iter-body"). `None` for
/// blocks that don't take a boundary param (handler / mount / structural
/// blocks whose role is already in their debug name).
fn boundary_kind_for_block(comp: &LirResource, block_id: BlockId) -> Option<(u32, &'static str)> {
    let block = comp.get_block(block_id);
    // Stage 5c: derive first boundary id from slots.
    let bid = block.boundary_param_ids_from_slots(&comp.slots).next()?;
    // Stage 5d: read kind from the resource registry.
    let struct_decl = comp.struct_types.get(bid.0 as usize)?;
    let kind = match struct_decl.kind {
        TreeBoundaryKind::Root => "root",
        TreeBoundaryKind::IfAnchor { .. } => "if-anchor",
        TreeBoundaryKind::IfBranch { .. } => "if-branch",
        TreeBoundaryKind::ForAnchor { .. } => "for-anchor",
        TreeBoundaryKind::ForIterBody { .. } => "for-iter-body",
    };
    Some((bid.0, kind))
}

/// Variant of `walk_ops` that does NOT recurse through `CallBlock` /
/// `CallBlock2` — used for per-block local summaries so each fn node's
/// label only describes that block's own work. Still descends into
/// `If` / `Loop` bodies because those are inline within the same block.
fn walk_ops_shallow(
    ops: &[LirOp],
    comp: &LirResource,
    ctx: &CompilerContext,
    writes: &mut Vec<DefId>,
    mutations: &mut Vec<String>,
) {
    for op in ops {
        match op {
            LirOp::SignalWrite { signal, .. } | LirOp::SignalWriteExpr { signal, .. } => {
                if !writes.contains(signal) {
                    writes.push(*signal);
                }
            }
            // DOM mutations flow through `CallFunction` against a
            // `dom_imports` DefId; map the callee back to its entry.
            LirOp::CallFunction { func: callee, .. } => {
                let dom = ctx.dom_imports();
                if *callee == dom.set_text_content {
                    push_unique(mutations, "set text".into());
                } else if *callee == dom.create_text {
                    push_unique(mutations, "create text".into());
                } else if *callee == dom.set_attribute {
                    push_unique(mutations, "attr".into());
                } else if *callee == dom.create_element {
                    push_unique(mutations, "create element".into());
                } else if *callee == dom.create_comment {
                    push_unique(mutations, "create anchor".into());
                } else if *callee == dom.remove {
                    push_unique(mutations, "remove dom".into());
                }
            }
            LirOp::If {
                then_ops, else_ops, ..
            } => {
                walk_ops_shallow(then_ops, comp, ctx, writes, mutations);
                walk_ops_shallow(else_ops, comp, ctx, writes, mutations);
            }
            LirOp::Loop { body_ops, .. } => {
                walk_ops_shallow(body_ops, comp, ctx, writes, mutations);
            }
            _ => {}
        }
    }
}

/// Recursively collect `CallBlock` / `CallBlock2` targets from an op
/// list (descending into If / Loop bodies). Order preserved; duplicates
/// allowed at this layer — `render_call_chain` dedupes node emission.
fn collect_call_targets(ops: &[LirOp], out: &mut Vec<BlockId>) {
    for op in ops {
        match op {
            LirOp::CallBlock { block, .. } => out.push(*block),
            LirOp::If {
                then_ops, else_ops, ..
            } => {
                collect_call_targets(then_ops, out);
                collect_call_targets(else_ops, out);
            }
            LirOp::Loop { body_ops, .. } => {
                collect_call_targets(body_ops, out);
            }
            _ => {}
        }
    }
}

/// Walk every block in the component looking for `AddEventListener` ops
/// and return `(event_name, handler_block)` pairs in discovery order.
fn discover_event_handlers(comp: &LirResource) -> Vec<(String, BlockId)> {
    let mut out = Vec::new();
    for block in &comp.blocks {
        collect_add_event_listener(&block.ops, comp, &mut out);
    }
    out
}

fn collect_add_event_listener(
    ops: &[LirOp],
    _comp: &LirResource,
    out: &mut Vec<(String, BlockId)>,
) {
    // `AddEventListener` is no longer a single op — the event string is
    // pushed as a `PushStringPtr`/`PushStringLen` pair just before each
    // `PushHandlerId`, so walk the stream and track the most recent pair.
    let mut last_event_string: Option<StringId> = None;
    for op in ops {
        let _ = (op, &mut last_event_string);
        match op {
            LirOp::PushStringPtr { string_id } => {
                last_event_string = Some(*string_id);
            }
            LirOp::PushHandlerId { handler } => {
                let event_name = last_event_string
                    .map(|sid| _comp.get_string(sid).to_string())
                    .unwrap_or_else(|| "".to_string());
                out.push((event_name, *handler));
                last_event_string = None;
            }
            LirOp::If {
                then_ops, else_ops, ..
            } => {
                collect_add_event_listener(then_ops, _comp, out);
                collect_add_event_listener(else_ops, _comp, out);
            }
            LirOp::Loop { body_ops, .. } => {
                collect_add_event_listener(body_ops, _comp, out);
            }
            _ => {}
        }
    }
}

/// Unique DOT node id per (component, signal DefId).
fn signal_node_id(comp_idx: usize, def_id_raw: u32) -> String {
    format!("c{}_sig_{}", comp_idx, def_id_raw)
}

/// Stable DOT node id for a module-scope global signal. Shared across
/// components — every reference resolves to the same node so you can see
/// fan-in / fan-out on shared state.
fn global_node_id(def_id_raw: u32) -> String {
    format!("global_sig_{}", def_id_raw)
}

/// Unique DOT node id per (component, effect id).
fn effect_node_id(comp_idx: usize, effect_id: u32) -> String {
    format!("c{}_eff_{}", comp_idx, effect_id)
}

/// Short human-readable type label. Falls back to `{:?}` debug when the
/// context doesn't have a prettier rendering — DOT is for humans, so the
/// tradeoff is mostly cosmetic.
fn type_label(ctx: &CompilerContext, ty: Ty) -> String {
    match ctx.ty_kind(ty) {
        InternedTyKind::Bool => "bool".into(),
        InternedTyKind::S8 => "s8".into(),
        InternedTyKind::S16 => "s16".into(),
        InternedTyKind::S32 => "s32".into(),
        InternedTyKind::S64 => "s64".into(),
        InternedTyKind::U8 => "u8".into(),
        InternedTyKind::U16 => "u16".into(),
        InternedTyKind::U32 => "u32".into(),
        InternedTyKind::U64 => "u64".into(),
        InternedTyKind::F32 => "f32".into(),
        InternedTyKind::F64 => "f64".into(),
        InternedTyKind::Char => "char".into(),
        InternedTyKind::String => "string".into(),
        InternedTyKind::List(inner) => format!("list<{}>", type_label(ctx, *inner)),
        InternedTyKind::Option(inner) => format!("option<{}>", type_label(ctx, *inner)),
        InternedTyKind::Result { ok, err } => {
            let ok_s = ok.map(|t| type_label(ctx, t)).unwrap_or_else(|| "_".into());
            let err_s = err
                .map(|t| type_label(ctx, t))
                .unwrap_or_else(|| "_".into());
            format!("result<{}, {}>", ok_s, err_s)
        }
        InternedTyKind::Tuple(elts) => {
            let parts: Vec<String> = elts.iter().map(|t| type_label(ctx, *t)).collect();
            format!("tuple<{}>", parts.join(", "))
        }
        InternedTyKind::Adt(def_id) => ctx.str(ctx.defs.name(*def_id)).to_string(),
        InternedTyKind::Func { .. } => "func(..)".into(),
        other => format!("{:?}", other),
    }
}

/// Walk the effect's update block (recursing into nested If/Loop bodies)
/// and return `(signals_written, dom_mutations)`.
///
/// `dom_mutations` entries are short human labels like "text content",
/// "attr:name", "style:background", etc. Signal writes that piggy-back
/// inside the same block (e.g. an effect that both writes the DOM and
/// re-stores a derived value) surface in both lists.
fn collect_effect_outputs(
    comp: &LirResource,
    ctx: &CompilerContext,
    block_id: BlockId,
    visited: &mut Vec<BlockId>,
) -> (Vec<DefId>, Vec<String>) {
    // Guard against pathological cycles in CallBlock chains.
    if visited.contains(&block_id) {
        return (Vec::new(), Vec::new());
    }
    visited.push(block_id);

    let block = comp.get_block(block_id);
    let mut writes = Vec::new();
    let mut mutations = Vec::new();
    walk_ops(&block.ops, comp, ctx, visited, &mut writes, &mut mutations);
    (writes, mutations)
}

fn walk_ops(
    ops: &[LirOp],
    comp: &LirResource,
    ctx: &CompilerContext,
    visited: &mut Vec<BlockId>,
    writes: &mut Vec<DefId>,
    mutations: &mut Vec<String>,
) {
    for op in ops {
        match op {
            LirOp::SignalWrite { signal, .. } | LirOp::SignalWriteExpr { signal, .. } => {
                if !writes.contains(signal) {
                    writes.push(*signal);
                }
            }
            // DOM mutations route through `CallFunction` on `dom_imports`
            // DefIds; classify into text/attr/structural buckets by callee.
            LirOp::CallFunction { func: callee, .. } => {
                let dom = ctx.dom_imports();
                if *callee == dom.set_text_content || *callee == dom.create_text {
                    push_unique(mutations, "text content".into());
                } else if *callee == dom.set_attribute {
                    push_unique(mutations, "attr".into());
                } else if *callee == dom.create_element
                    || *callee == dom.create_comment
                    || *callee == dom.create_fragment
                    || *callee == dom.append_child
                    || *callee == dom.insert_after
                    || *callee == dom.remove
                {
                    push_unique(mutations, "mount/unmount branch".into());
                }
            }
            LirOp::If {
                then_ops, else_ops, ..
            } => {
                walk_ops(then_ops, comp, ctx, visited, writes, mutations);
                walk_ops(else_ops, comp, ctx, visited, writes, mutations);
            }
            LirOp::Loop { body_ops, .. } => {
                walk_ops(body_ops, comp, ctx, visited, writes, mutations);
            }
            LirOp::CallBlock { block, .. } => {
                let (inner_writes, inner_mut) = collect_effect_outputs(comp, ctx, *block, visited);
                for w in inner_writes {
                    if !writes.contains(&w) {
                        writes.push(w);
                    }
                }
                for m in inner_mut {
                    push_unique(mutations, m);
                }
            }
            _ => {}
        }
    }
}

fn push_unique(v: &mut Vec<String>, s: String) {
    if !v.contains(&s) {
        v.push(s);
    }
}

/// Escape a string for inclusion in a DOT double-quoted label. Only needs
/// to cover `"` and `\` — newlines in Graphviz labels are `\n` as two
/// characters, so we let callers insert those literally.
fn escape_dot(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => {
                // Preserve pre-escaped sequences like `\n` that the caller
                // already embedded (see `label = "foo\\n: bar"` above).
                out.push('\\');
            }
            _ => out.push(c),
        }
    }
    out
}
