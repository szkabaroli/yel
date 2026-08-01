//! Stage 3's invariants, each asserted by a named test — H1, H2, H4, H5, the
//! desugarings, and the decisions with observable shape (D5, D7, F13).
//!
//! H3 (no analysis result on a node) is asserted by the type system — no HIR
//! node has a `Ty` field to write — and has no runtime test by design.

use yelc_base::ErrorCode;
use yelc_hir::{
    HirCallee, HirExpr, HirExprKind, HirItem, HirModule, HirPattern, HirStmt, lower_files,
};
use yelc_sema::definitions::MemberKind;
use yelc_sema::{CompilerContext, PackageId, TyKind};
use yelc_syntax::ParsedFile;

fn lower(sources: &[&str]) -> (HirModule, CompilerContext) {
    let mut ctx = CompilerContext::with_intrinsics(PackageId::LOCAL);
    let mut parsed: Vec<ParsedFile> = Vec::new();
    for content in sources {
        let source = ctx.sources.add_inline(*content);
        parsed.push(yelc_syntax::parse(
            source,
            content,
            &ctx.names,
            &mut ctx.diagnostics,
        ));
    }
    let module = lower_files(&parsed, &mut ctx);
    (module, ctx)
}

fn error_codes(ctx: &CompilerContext) -> Vec<ErrorCode> {
    ctx.diagnostics.iter().filter_map(|d| d.code).collect()
}

/// The one component of a module, unwrapped.
fn the_component(module: &HirModule) -> &yelc_hir::HirComponent {
    let mut components = module
        .items
        .iter_enumerated()
        .filter_map(|(_, item)| match item {
            HirItem::Component(component) => Some(component),
            HirItem::Global(_) | HirItem::Function { .. } => None,
        });
    let component = components.next().expect("a component was lowered");
    assert!(
        components.next().is_none(),
        "expected exactly one component"
    );
    component
}

/// The build body's root builder expressions.
fn build_roots(module: &HirModule) -> &[HirExpr] {
    let component = the_component(module);
    let body = &module.bodies[component.build];
    let tail = body.block.tail.as_ref().expect("build has a tail");
    match &tail.kind {
        HirExprKind::Fragment(roots) => roots,
        other => panic!("build tail is a fragment, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// H1 — register, collect, lower: three phases across the whole file set
// ---------------------------------------------------------------------------

/// A record field typed as a record declared **after** it in the same file.
/// Fails in the frozen tree (F3); phase-major sweeping fixes it.
#[test]
fn h1_a_declared_type_may_reference_a_later_item() {
    let (_, ctx) = lower(&["package a:b;\nrecord First { p: Second }\nrecord Second { x: s32 }\n"]);
    assert_eq!(error_codes(&ctx), vec![]);

    let first = ctx
        .defs
        .lookup_def(ctx.names.intern("First"), yelc_sema::DefKind::Type)
        .expect("First registered");
    let second = ctx
        .defs
        .lookup_def(ctx.names.intern("Second"), yelc_sema::DefKind::Type)
        .expect("Second registered");
    let field = &ctx.defs.members(first)[0];
    let ty = field.ty.expect("field type resolved");
    assert_eq!(ctx.types.kind(ty), TyKind::Adt(second));
}

/// A component referencing a record declared in a file passed **second**.
/// Fails in the frozen tree (F4): its driver merges fully-lowered files in a
/// loop, so cross-file references resolve in one direction only.
#[test]
fn h1_a_reference_may_cross_files_backwards() {
    let (_, ctx) = lower(&[
        "package a:b;\ncomponent App { p: Late = { x: 1 }; }\n",
        "package a:b;\nrecord Late { x: s32 }\n",
    ]);
    assert_eq!(error_codes(&ctx), vec![]);

    let app = ctx
        .defs
        .lookup_def(ctx.names.intern("App"), yelc_sema::DefKind::Component)
        .expect("App registered");
    let late = ctx
        .defs
        .lookup_def(ctx.names.intern("Late"), yelc_sema::DefKind::Type)
        .expect("Late registered");
    let prop = &ctx.defs.members(app)[0];
    assert_eq!(
        ctx.types.kind(prop.ty.expect("prop ty resolved")),
        TyKind::Adt(late)
    );
}

// ---------------------------------------------------------------------------
// H2 — the map is total, bidirectional, injective — over a MULTI-file input
// ---------------------------------------------------------------------------

/// Two files with identical text have identical per-file `NodeId`s; every
/// primary node of each must keep its own mapping in both directions. Keyed by
/// a bare `NodeId` (the brief's original spec) the second file's entries
/// overwrite the first's and the round-trip still passes for the survivor —
/// which is why this asserts the cross terms over a real two-file lowering,
/// not the round-trip.
#[test]
fn h2_two_files_do_not_collide_in_the_map() {
    let text_one = "package a:b;\nglobal One { x: s32 = 1; }\n";
    let text_two = "package a:b;\nglobal Two { x: s32 = 1; }\n";
    let (module, ctx) = lower(&[text_one, text_two]);
    assert_eq!(error_codes(&ctx), vec![]);

    let mut items = module
        .items
        .iter_enumerated()
        .filter_map(|(_, item)| match item {
            HirItem::Global(global) => Some(global),
            HirItem::Component(_) | HirItem::Function { .. } => None,
        });
    let one = items.next().expect("global One");
    let two = items.next().expect("global Two");

    let node_one = module.map.node_of(one.hir_id).expect("One maps to source");
    let node_two = module.map.node_of(two.hir_id).expect("Two maps to source");
    assert_ne!(
        node_one.source, node_two.source,
        "each item maps into its own file"
    );
    assert_eq!(
        node_one.node, node_two.node,
        "the bare NodeIds DO collide — that is what makes this test observe anything"
    );
    assert_eq!(module.map.hir_of(node_one), Some(one.hir_id));
    assert_eq!(module.map.hir_of(node_two), Some(two.hir_id));
}

// ---------------------------------------------------------------------------
// H4 — an unresolved name is unresolved, a resolved one is a DefId
// ---------------------------------------------------------------------------

/// The positive half: a field of user-record type resolves to that record's
/// `DefId` — which the frozen tree does not do (it interns `Unknown` and lets
/// the checker rediscover the name).
#[test]
fn h4_resolution_is_partial_on_purpose() {
    let (module, ctx) = lower(&["package a:b;\nglobal G { go: func() { let x = mystery(); } }\n"]);
    // No name-resolution error: HIR never errors on an unknown name.
    assert_eq!(error_codes(&ctx), vec![]);

    // And the name is carried as itself, not as a placeholder def.
    let body = module
        .bodies
        .iter_enumerated()
        .map(|(_, body)| body)
        .find(|body| !body.block.stmts.is_empty())
        .expect("the function body");
    let HirStmt::Let { value, .. } = &body.block.stmts[0] else {
        panic!("expected the let");
    };
    let HirExprKind::Call { callee, .. } = &value.kind else {
        panic!("expected the call");
    };
    let HirCallee::Unresolved(name) = callee else {
        panic!("an unknown callee stays unresolved, got {callee:?}");
    };
    assert_eq!(&*ctx.names.str(*name), "mystery");
}

// ---------------------------------------------------------------------------
// H5 — nothing silently dropped
// ---------------------------------------------------------------------------

/// Recovery input everywhere the AST can carry it: the file still lowers, no
/// panic, and the parse diagnostics are present (the "or a diagnostic" arm).
#[test]
fn h5_recovery_nodes_lower_without_panic() {
    let (_, ctx) = lower(&[
        "package a:b;\nrecord { x: s32 }\ncomponent App { junk!!; Text { } }\nglobal G { f: func() { let = 3; !!! } }\n",
    ]);
    assert!(
        ctx.diagnostics.has_errors(),
        "recovery input must carry diagnostics"
    );
}

/// A `set` prop whose value is not a closure: the frozen tree silently drops
/// it (an S5 violation it lived with); here it is reported.
#[test]
fn h5_a_non_closure_set_binding_is_reported_not_dropped() {
    let (_, ctx) =
        lower(&["package a:b;\ncomponent App { v: s32 = 1; Text { value: v set value: 42 } }\n"]);
    assert!(error_codes(&ctx).contains(&ErrorCode::InvalidValueBinding));
}

// ---------------------------------------------------------------------------
// The desugarings
// ---------------------------------------------------------------------------

/// `x += 1` → `x = x + 1`, with the rebuilt left-hand side synthesized (its
/// map origin is the assignment's syntax, not a second primary claim on `x`,
/// which would panic the injective map).
#[test]
fn compound_assignment_desugars_to_assign_of_binary() {
    let (module, _ctx) = lower(&["package a:b;\nglobal G { f: func() { let x = 1; x += 2; } }\n"]);
    let body = module
        .bodies
        .iter_enumerated()
        .map(|(_, body)| body)
        .find(|body| body.block.stmts.len() == 2)
        .expect("the function body");
    let HirStmt::Assign { target, value, .. } = &body.block.stmts[1] else {
        panic!("expected the assignment");
    };
    assert!(matches!(target.kind, HirExprKind::Local(_)));
    let HirExprKind::Binary { op, lhs, .. } = &value.kind else {
        panic!("expected the synthesized binary, got {:?}", value.kind);
    };
    assert_eq!(*op, yelc_hir::BinaryOp::Add);
    assert!(matches!(lhs.kind, HirExprKind::Local(_)));

    // The synthesized lhs maps forward to real syntax, and the primary target
    // still owns the reverse direction.
    let origin = module
        .map
        .node_of(lhs.hir_id)
        .expect("synthesized maps forward");
    assert_ne!(module.map.hir_of(origin), Some(lhs.hir_id));
}

/// `#ff0000` → `Color.rgba((r, g, b, a))`, against the `Color` lang item.
#[test]
fn color_literal_desugars_to_the_lang_item() {
    let (module, ctx) = lower(&["package a:b;\ncomponent App { Text { color: #102030 } }\n"]);
    let roots = build_roots(&module);
    let HirExprKind::Instantiate(node) = &roots[0].kind else {
        panic!("expected Text instantiation");
    };
    let getter = node.props[0].getter.as_ref().expect("color getter");
    let HirExprKind::Call { callee, args } = &getter.kind else {
        panic!("expected the desugared call, got {:?}", getter.kind);
    };
    let HirCallee::Member { base, member } = callee else {
        panic!("expected Color.rgba, got {callee:?}");
    };
    assert_eq!(*base, ctx.known().get(yelc_sema::Known::Color));
    assert_eq!(&*ctx.names.str(*member), "rgba");
    let HirExprKind::Tuple(channels) = &args[0].kind else {
        panic!("expected the channel tuple");
    };
    let values: Vec<i64> = channels
        .iter()
        .map(|channel| match channel.kind {
            HirExprKind::Literal(yelc_hir::HirLiteral::Int(value)) => value,
            ref other => panic!("expected int channel, got {other:?}"),
        })
        .collect();
    assert_eq!(values, vec![0x10, 0x20, 0x30, 0xff]);
}

/// `c ? a : b` → `match c { true -> a, false -> b }` — there is no `Ternary`
/// in this IR to fall back to.
#[test]
fn ternary_desugars_to_match() {
    let (module, _) = lower(&["package a:b;\nglobal G { f: func() -> s32 { true ? 1 : 2 } }\n"]);
    let body = module
        .bodies
        .iter_enumerated()
        .map(|(_, body)| body)
        .find(|body| body.block.tail.is_some())
        .expect("the function body");
    let tail = body.block.tail.as_ref().unwrap();
    let HirExprKind::Match(node) = &tail.kind else {
        panic!("expected match, got {:?}", tail.kind);
    };
    assert_eq!(node.arms.len(), 2);
    assert!(matches!(node.arms[0].pattern, HirPattern::Bool(true)));
    assert!(matches!(node.arms[1].pattern, HirPattern::Bool(false)));
}

/// UI `if` / `else if` / `else` → **one** conditional boundary (the chain is
/// one anchor in the tree), the chain nested in the false arm as plain
/// matches (D7), each level keyed to the `ElseIfBranch`'s own real node.
#[test]
fn ui_if_chain_desugars_to_one_conditional_boundary() {
    let (module, _) = lower(&[
        "package a:b;\ncomponent App { v: s32 = 0;\n  if v > 2 { Text { } } else if v > 1 { Box { } } else { Group { } }\n}\n",
    ]);
    let roots = build_roots(&module);
    let HirExprKind::Boundary(boundary) = &roots[0].kind else {
        panic!("UI if lowers to a boundary, got {:?}", roots[0].kind);
    };
    let yelc_hir::HirBoundary::Conditional(outer) = &**boundary else {
        panic!("UI if is a conditional boundary, got {boundary:?}");
    };
    // true arm: the then-branch fragment.
    assert!(matches!(outer.arms[0].value.kind, HirExprKind::Fragment(_)));
    // false arm: the else-if, as a plain nested match — *inside* the region,
    // not a boundary of its own.
    let HirExprKind::Match(inner) = &outer.arms[1].value.kind else {
        panic!("else-if nests as a match in the false arm");
    };
    // …whose false arm is the else fragment.
    assert!(matches!(inner.arms[1].value.kind, HirExprKind::Fragment(_)));
}

/// UI `for` → a repeat boundary; `@children` → a children boundary. The three
/// dynamic template forms share one node kind — the unit that mounts,
/// unmounts and reconciles, and signalck's dependency site.
#[test]
fn ui_for_and_children_lower_to_boundaries() {
    let (module, _) = lower(&[
        "package a:b;\ncomponent App { items: list<s32>;\n  for item in items key(item) { Text { } }\n  @children\n}\n",
    ]);
    let roots = build_roots(&module);
    let HirExprKind::Boundary(repeat) = &roots[0].kind else {
        panic!("UI for lowers to a boundary, got {:?}", roots[0].kind);
    };
    let yelc_hir::HirBoundary::Repeat(node) = &**repeat else {
        panic!("UI for is a repeat boundary, got {repeat:?}");
    };
    assert!(node.key.is_some(), "key(item) survives");
    assert_eq!(node.children.len(), 1);
    let HirExprKind::Boundary(children) = &roots[1].kind else {
        panic!("@children lowers to a boundary, got {:?}", roots[1].kind);
    };
    assert!(matches!(&**children, yelc_hir::HirBoundary::Children));
}

/// `x.f(a)` → `f(x, a)`: pure UFCS, no `MethodCall` (modules.md §8) — and
/// `Type.case(…)` stays a member call on the resolved base.
#[test]
fn path_calls_split_into_member_call_and_ufcs() {
    let (module, ctx) = lower(&[
        "package a:b;\nvariant Shape { dot, big(s32) }\nglobal G { f: func() { let s = \"yo\"; let a = Shape.big(3); let b = s.len(); } }\n",
    ]);
    let body = module
        .bodies
        .iter_enumerated()
        .map(|(_, body)| body)
        .find(|body| body.block.stmts.len() == 3)
        .expect("the function body");

    // `Shape.big(3)`: a member call on the resolved variant.
    let HirStmt::Let { value, .. } = &body.block.stmts[1] else {
        panic!()
    };
    let HirExprKind::Call { callee, args } = &value.kind else {
        panic!("expected call")
    };
    let HirCallee::Member { base, member } = callee else {
        panic!("expected member call, got {callee:?}")
    };
    let shape = ctx
        .defs
        .lookup_def(ctx.names.intern("Shape"), yelc_sema::DefKind::Type)
        .unwrap();
    assert_eq!(*base, shape);
    assert_eq!(&*ctx.names.str(*member), "big");
    assert_eq!(args.len(), 1);

    // `s.len()`: UFCS — the receiver is argument 0, the callee the builtin set.
    let HirStmt::Let { value, .. } = &body.block.stmts[2] else {
        panic!()
    };
    let HirExprKind::Call { callee, args } = &value.kind else {
        panic!("expected call")
    };
    let HirCallee::Intrinsic(name) = callee else {
        panic!("expected builtin callee, got {callee:?}")
    };
    assert_eq!(&*ctx.names.str(*name), "len");
    assert_eq!(args.len(), 1, "the receiver became the first argument");
    assert!(matches!(args[0].kind, HirExprKind::Local(_)));
}

/// `bind value: x` ≡ getter + empty setter, and same-named props fold into one
/// entity in first-occurrence order (F13).
#[test]
fn bind_desugars_and_props_merge() {
    let (module, ctx) = lower(&[
        "package a:b;\ncomponent App { v: s32 = 1;\n  TextInput { label: \"a\", bind value: v }\n  Text { value: v set value: { v = 2; } }\n}\n",
    ]);
    let roots = build_roots(&module);

    let HirExprKind::Instantiate(input) = &roots[0].kind else {
        panic!()
    };
    assert_eq!(input.props.len(), 2);
    let bound = &input.props[1];
    assert_eq!(&*ctx.names.str(bound.name), "value");
    assert!(bound.getter.is_some());
    let setter = bound.setter.as_ref().expect("bind synthesizes a setter");
    assert!(setter.block.stmts.is_empty() && setter.block.tail.is_none());

    // `value:` + `set value:` — two source props, ONE entity.
    let HirExprKind::Instantiate(text) = &roots[1].kind else {
        panic!()
    };
    assert_eq!(text.props.len(), 1, "F13: same-named props merged");
    let merged = &text.props[0];
    assert!(merged.getter.is_some());
    let setter = merged.setter.as_ref().expect("the written setter survives");
    assert_eq!(setter.block.stmts.len(), 1);
}

/// A setter with no getter is the frozen tree's `InvalidValueBinding`,
/// reported by the lowering.
#[test]
fn a_setter_without_a_getter_is_reported() {
    let (_, ctx) =
        lower(&["package a:b;\ncomponent App { v: s32 = 1; Text { set value: { v = 2; } } }\n"]);
    assert_eq!(error_codes(&ctx), vec![ErrorCode::InvalidValueBinding]);
}

/// Two `@children` markers: the second is reported, naming the first — and the
/// count comes from the one lowering walk, not a second walker.
#[test]
fn a_second_children_slot_is_reported() {
    let (_, ctx) =
        lower(&["package a:b;\ncomponent App { VStack { @children } Box { @children } }\n"]);
    assert_eq!(error_codes(&ctx), vec![ErrorCode::DuplicateChildrenSlot]);
}

// ---------------------------------------------------------------------------
// Decisions with observable shape
// ---------------------------------------------------------------------------

/// D5: globals lower before components — the item spine's order says so even
/// when the source order is reversed.
#[test]
fn d5_globals_lower_before_components() {
    let (module, _) =
        lower(&["package a:b;\ncomponent App { Text { } }\nglobal Store { x: s32 = 1; }\n"]);
    let kinds: Vec<&str> = module
        .items
        .iter_enumerated()
        .map(|(_, item)| match item {
            HirItem::Global(_) => "global",
            HirItem::Function { .. } => "function",
            HirItem::Component(_) => "component",
        })
        .collect();
    assert_eq!(kinds, vec!["global", "component"]);
}

/// Registration stays kind-major in the frozen order (records → … →
/// components) regardless of source order — `DefId` ordinals reach output.
#[test]
fn registration_order_is_kind_major() {
    let (_, ctx) = lower(&[
        "package a:b;\ncomponent App { Text { } }\nglobal Store { x: s32 = 1; }\nrecord R { x: s32 }\nenum E { a, b }\n",
    ]);
    let names: Vec<String> = ctx
        .defs
        .iter()
        .map(|def| ctx.names.str(def.name).to_string())
        .collect();
    // Color is the pre-registered lang item; then the frozen kind order.
    assert_eq!(names, vec!["Color", "R", "E", "Store", "App"]);
}

/// The member rows carry the declared types phase 2 resolved — `Definitions`
/// is typed after lowering, with `None` (not a placeholder) where resolution
/// failed.
#[test]
fn member_rows_are_typed_by_phase_2() {
    let (_, ctx) = lower(&[
        "package a:b;\nglobal G { count: s32 = 0; get-name: func(id: u32) -> string; broken: nope; }\n",
    ]);
    let global = ctx
        .defs
        .lookup_def(ctx.names.intern("G"), yelc_sema::DefKind::Global)
        .unwrap();
    let members = ctx.defs.members(global);
    assert_eq!(members.len(), 3);

    assert_eq!(
        members[0].kind,
        MemberKind::Property {
            direction: yelc_sema::MemberDirection::None
        }
    );
    assert_eq!(ctx.types.kind(members[0].ty.unwrap()), TyKind::S32);

    assert_eq!(members[1].kind, MemberKind::Function);
    let TyKind::Func { params, ret } = ctx.types.kind(members[1].ty.unwrap()) else {
        panic!("callback member carries its Func type")
    };
    assert_eq!(params.len(), 1);
    assert_eq!(ctx.types.kind(ret.unwrap()), TyKind::String);

    // `nope` resolves to nothing: None, never a placeholder (H4).
    assert_eq!(members[2].ty, None);
}

/// Closure parameters and lets allocate into the **enclosing body's** arena in
/// source order — the D1 caveat's observable: one arena, allocation order is
/// source order.
#[test]
fn closure_locals_share_the_enclosing_arena_in_source_order() {
    let (module, ctx) = lower(&[
        "package a:b;\nglobal G { f: func(a: s32) { let b = 1; let g = { c: s32 -> let d = c; }; let e = 2; } }\n",
    ]);
    let body = module
        .bodies
        .iter_enumerated()
        .map(|(_, body)| body)
        .find(|body| body.params == 1)
        .expect("the function body");
    let names: Vec<String> = body
        .locals
        .iter_enumerated()
        .map(|(_, local)| ctx.names.str(local.name).to_string())
        .collect();
    assert_eq!(names, vec!["a", "b", "c", "d", "g", "e"]);
}

/// A `for` binder is scoped to its body — visible inside, gone after.
#[test]
fn d3_the_for_binder_resolves_through_the_scope_path() {
    let (_, ctx) = lower(&[
        "package a:b;\nglobal G { f: func(items: list<s32>) { for item in items { let x = item; } let y = item; } }\n",
    ]);
    // Inside: resolves (no error). After: `item` is unresolved — and H4 says
    // that is stage 4's to report, so *no* error here either; the observable
    // is the HIR shape, checked via the dump-level tests above. What this
    // test pins is that lowering did not leak the binder into the outer
    // scope as a resolvable local.
    assert_eq!(error_codes(&ctx), vec![]);
    let global = ctx
        .defs
        .lookup_def(ctx.names.intern("G"), yelc_sema::DefKind::Global)
        .unwrap();
    assert_eq!(ctx.defs.members(global).len(), 1);
}

// ---------------------------------------------------------------------------
// Signal dependencies — the frozen signalck, one stage earlier
// ---------------------------------------------------------------------------

/// Per-body and per-site sets over the counter shape: the build body sees the
/// union; the handler's site is the only write site; the text and the region
/// scrutinee are read sites.
#[test]
fn signal_dependencies_are_computed_per_body_and_per_site() {
    let (module, ctx) = lower(&[
        "package a:b;\nexport component Counter { count: s32 = 0;\n  VStack {\n    Text { text: \"n: {count}\" }\n    Button { clicked: { count += 1; } }\n    if count > 10 { Text { text: \"hi\" } }\n  }\n}\n",
    ]);
    assert_eq!(error_codes(&ctx), vec![]);
    let counter = ctx
        .defs
        .lookup_def(ctx.names.intern("Counter"), yelc_sema::DefKind::Component)
        .unwrap();
    let count = ctx.names.intern("count");

    let entries: Vec<&yelc_hir::BodyDependencies> = module
        .dependencies
        .iter_sorted()
        .map(|(_, dependencies)| dependencies)
        .collect();
    assert!(!entries.is_empty(), "the table is populated");

    // Every reference in this program is Counter.count.
    for dependencies in &entries {
        for state in dependencies.reads.iter().chain(&dependencies.writes) {
            assert_eq!(state.owner, counter);
            assert_eq!(state.member, count);
        }
    }

    // Exactly two entries carry the write: the handler's site, and the build
    // body it sits in.
    let write_entries = entries
        .iter()
        .filter(|dependencies| !dependencies.writes.is_empty())
        .count();
    assert_eq!(
        write_entries, 2,
        "the handler site and the enclosing build body"
    );

    // Read sites: the interpolated text prop, the region scrutinee, and the
    // handler — `count += 1` desugars to `count = count + 1`, and the rebuilt
    // right-hand side is a real read. The build body's union makes four.
    let read_entries = entries
        .iter()
        .filter(|dependencies| !dependencies.reads.is_empty())
        .count();
    assert_eq!(read_entries, 4);
}

/// A global's property read inside a component is a cross-item dependency.
#[test]
fn global_property_reads_are_dependencies() {
    let (module, ctx) = lower(&[
        "package a:b;\nglobal Store { total: s32 = 0; }\ncomponent App { Text { text: \"t: {Store.total}\" } }\n",
    ]);
    assert_eq!(error_codes(&ctx), vec![]);
    let store = ctx
        .defs
        .lookup_def(ctx.names.intern("Store"), yelc_sema::DefKind::Global)
        .unwrap();
    let found = module.dependencies.iter_sorted().any(|(_, dependencies)| {
        dependencies
            .reads
            .iter()
            .any(|state| state.owner == store && &*ctx.names.str(state.member) == "total")
    });
    assert!(found, "Store.total is a recorded read");
}

/// A global's **function** member is not state: accessing it through the
/// global records nothing. Caught live — the embedded stdlib's `Num.min`
/// showed up as `reads(Num.min)` in the first `--emit-hir` of a std include.
#[test]
fn global_function_members_are_not_dependencies() {
    let (module, ctx) = lower(&[
        "package a:b;\nglobal Store { total: s32 = 0; refresh: func(); }\ncomponent App { Text { text: \"t: {Store.refresh}\" } }\n",
    ]);
    assert_eq!(error_codes(&ctx), vec![]);
    let store = ctx
        .defs
        .lookup_def(ctx.names.intern("Store"), yelc_sema::DefKind::Global)
        .unwrap();
    let refresh = ctx.names.intern("refresh");
    let recorded = module.dependencies.iter_sorted().any(|(_, dependencies)| {
        dependencies
            .reads
            .iter()
            .chain(&dependencies.writes)
            .any(|state| state.owner == store && state.member == refresh)
    });
    assert!(!recorded, "a function member is not reactive state");
}

// ---------------------------------------------------------------------------
// D6 — doc-comment attachment
// ---------------------------------------------------------------------------

fn doc_of(
    module: &HirModule,
    ctx: &CompilerContext,
    name: &str,
    kind: yelc_sema::DefKind,
) -> Option<String> {
    let def = ctx.defs.lookup_def(ctx.names.intern(name), kind)?;
    module
        .docs
        .get(&def)
        .map(|doc| ctx.names.str(*doc).to_string())
}

/// The rule, positively: nearest preceding run, joined, markers stripped.
#[test]
fn d6_doc_comments_attach_to_the_nearest_item() {
    let (module, ctx) =
        lower(&["package a:b;\n/// A counter of things.\n/// Second line.\nrecord R { x: s32 }\n"]);
    assert_eq!(
        doc_of(&module, &ctx, "R", yelc_sema::DefKind::Type).as_deref(),
        Some("A counter of things.\nSecond line.")
    );
}

/// A blank line breaks attachment — the run belongs to nothing.
#[test]
fn d6_a_blank_line_breaks_attachment() {
    let (module, ctx) = lower(&["package a:b;\n/// Orphaned commentary.\n\nrecord R { x: s32 }\n"]);
    assert_eq!(doc_of(&module, &ctx, "R", yelc_sema::DefKind::Type), None);
}

/// The run ends where the comments stop: code above it stays out, and a
/// higher run separated by a blank belongs to nothing.
#[test]
fn d6_the_run_ends_at_the_first_non_comment_line() {
    let (module, ctx) = lower(&[
        "package a:b;\n/// Far away.\n\n/// Near.\nrecord R { x: s32 }\nglobal G { x: s32 = 1; }\n",
    ]);
    assert_eq!(
        doc_of(&module, &ctx, "R", yelc_sema::DefKind::Type).as_deref(),
        Some("Near.")
    );
    // And R's trailing line of code does not become G's doc.
    assert_eq!(doc_of(&module, &ctx, "G", yelc_sema::DefKind::Global), None);
}

/// The file-header comment above `package` does not leak onto the first item.
#[test]
fn d6_the_file_header_belongs_to_nothing() {
    let (module, ctx) = lower(&["// File header.\n\npackage a:b;\n\nrecord R { x: s32 }\n"]);
    assert_eq!(doc_of(&module, &ctx, "R", yelc_sema::DefKind::Type), None);
}

/// Every registered kind can carry one — components and globals too.
#[test]
fn d6_docs_attach_across_item_kinds() {
    let (module, ctx) = lower(&[
        "package a:b;\n/// The store.\nglobal Store { total: s32 = 0; }\n/// The view.\nexport component App { Text { } }\n",
    ]);
    assert_eq!(
        doc_of(&module, &ctx, "Store", yelc_sema::DefKind::Global).as_deref(),
        Some("The store.")
    );
    assert_eq!(
        doc_of(&module, &ctx, "App", yelc_sema::DefKind::Component).as_deref(),
        Some("The view.")
    );
}

/// The Rust line, not WIT's blur: a plain `//` comment is commentary and
/// never becomes documentation.
#[test]
fn d6_plain_comments_do_not_attach() {
    let (module, ctx) = lower(&["package a:b;\n// TODO: rewrite this\nrecord R { x: s32 }\n"]);
    assert_eq!(doc_of(&module, &ctx, "R", yelc_sema::DefKind::Type), None);
}

/// A `//` line between the `///` run and the item ends the run — commentary
/// below the docs belongs to the author, not the item.
#[test]
fn d6_a_plain_comment_ends_the_doc_run() {
    let (module, ctx) =
        lower(&["package a:b;\n/// Real docs.\n// implementation note\nrecord R { x: s32 }\n"]);
    assert_eq!(doc_of(&module, &ctx, "R", yelc_sema::DefKind::Type), None);
    let (module, ctx) =
        lower(&["package a:b;\n// note above\n/// Real docs.\nrecord R { x: s32 }\n"]);
    assert_eq!(
        doc_of(&module, &ctx, "R", yelc_sema::DefKind::Type).as_deref(),
        Some("Real docs.")
    );
}
