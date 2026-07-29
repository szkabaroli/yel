//! The package artifact round trip.
//!
//! # The test that carries this file
//!
//! [`a_type_survives_a_differently_populated_interner`]. A round trip through
//! *one* interner passes even if every `Ty` is written as its raw handle,
//! because the same table is on both sides — see
//! [`the_same_interner_round_trip_proves_nothing`], which is kept precisely to
//! be the control. The load side must therefore use an interner primed with
//! *different* types first, so the same structural type lands on a *different*
//! handle. Written structurally, that passes; written as an index, it cannot.
//!
//! That asymmetry is what makes decision
//! [B1](../../../plans/rewrite/open-decisions.md) tested rather than asserted.

use pretty_assertions::assert_eq;
use yelc_base::{Interner, SourceId, Span};
use yelc_sema::artifact::{SerializedDefPath, StructuralTy, decode, encode};
use yelc_sema::definitions::DefKind;
use yelc_sema::ids::PackageId;
use yelc_sema::{Artifact, Definitions, LoadError, PackageName, Stamp, Ty, TyKind, TypeInterner};

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

fn span() -> Span {
    Span::new(SourceId::new(7), 3, 11)
}

fn package() -> PackageName {
    PackageName::new("test", "artifact", "1.0.0")
}

/// A compilation that produces an artifact.
struct Producer {
    names: Interner,
    types: TypeInterner,
    defs: Definitions,
}

impl Producer {
    fn new() -> Self {
        Self {
            names: Interner::new(),
            types: TypeInterner::new(),
            defs: Definitions::new(PackageId::LOCAL),
        }
    }

    fn declare(&mut self, name: &str, kind: DefKind, ty: Option<Ty>) -> yelc_sema::DefId {
        let id = self
            .defs
            .register(self.names.intern(name), kind, span(), true)
            .expect("unique in this fixture");
        if let Some(ty) = ty {
            self.defs.set_ty(id, ty);
        }
        id
    }

    fn build(&self) -> Artifact {
        Artifact::build(package(), &self.names, &self.types, &self.defs)
    }
}

/// Render a type's full structure as text, so two interners can be compared on
/// *meaning* rather than on handles — which is the only comparison that means
/// anything across a boundary.
fn render(types: &TypeInterner, defs: &Definitions, names: &Interner, ty: Ty) -> String {
    match types.kind(ty) {
        TyKind::Bool => "bool".into(),
        TyKind::S8 => "s8".into(),
        TyKind::S16 => "s16".into(),
        TyKind::S32 => "s32".into(),
        TyKind::S64 => "s64".into(),
        TyKind::U8 => "u8".into(),
        TyKind::U16 => "u16".into(),
        TyKind::U32 => "u32".into(),
        TyKind::U64 => "u64".into(),
        TyKind::F32 => "f32".into(),
        TyKind::F64 => "f64".into(),
        TyKind::Char => "char".into(),
        TyKind::String => "string".into(),
        TyKind::Unit => "unit".into(),
        TyKind::Error => "error".into(),
        TyKind::List(element) => format!("list<{}>", render(types, defs, names, element)),
        TyKind::Option(element) => format!("option<{}>", render(types, defs, names, element)),
        TyKind::Result { ok, err } => {
            let part = |t: Option<Ty>| match t {
                Some(t) => render(types, defs, names, t),
                None => "_".into(),
            };
            format!("result<{}, {}>", part(ok), part(err))
        }
        TyKind::Tuple(elements) => {
            let rendered: Vec<_> = elements
                .into_iter()
                .map(|e| render(types, defs, names, e))
                .collect();
            format!("tuple<{}>", rendered.join(", "))
        }
        TyKind::Func { params, ret } => {
            let rendered: Vec<_> = params
                .into_iter()
                .map(|p| render(types, defs, names, p))
                .collect();
            let ret = match ret {
                Some(r) => render(types, defs, names, r),
                None => "unit".into(),
            };
            format!("func({}) -> {ret}", rendered.join(", "))
        }
        // The name, not the DefId: the index is exactly what must not be
        // compared across a boundary.
        TyKind::Adt(def) => format!("adt {}", names.str(defs.get(def).name)),
        TyKind::Param(index) => format!("param#{index}"),
        TyKind::Infer(index) => format!("infer#{index}"),
    }
}

fn path(name: &str, kind: DefKind) -> SerializedDefPath {
    SerializedDefPath {
        package: package(),
        kind,
        segments: vec![name.to_string()],
        overload: Vec::new(),
    }
}

// ---------------------------------------------------------------------------
// The point of the exercise
// ---------------------------------------------------------------------------

/// **The test this format exists to pass.**
///
/// The consumer's interner is primed with types the producer never had, so the
/// artifact's types cannot land on the handles the producer used. If the type
/// table wrote `Ty` handles, the consumer would read handles that mean something
/// else here and the rendered structures would disagree.
///
/// The handle inequality is asserted too, and it is not decoration: without it
/// the test would silently go vacuous the day the two interners happened to
/// agree.
#[test]
fn a_type_survives_a_differently_populated_interner() {
    let mut producer = Producer::new();
    // list<option<s32>> — producer interning order: option<s32>, then list<…>.
    let option_s32 = producer.types.intern(TyKind::Option(Ty::S32));
    let list_option_s32 = producer.types.intern(TyKind::List(option_s32));
    producer.declare("holder", DefKind::Value, Some(list_option_s32));

    let artifact = producer.build();

    // A fresh interner with *different* types interned first. Each one pushes
    // the artifact's types further down, so the same structural type cannot
    // land on the same handle.
    let consumer_names = Interner::new();
    let consumer_types = TypeInterner::new();
    let before_decoys = consumer_types.len();
    consumer_types.intern(TyKind::List(Ty::STRING));
    consumer_types.intern(TyKind::Option(Ty::BOOL));
    consumer_types.intern(TyKind::Tuple(vec![Ty::F64, Ty::CHAR]));
    assert_eq!(
        consumer_types.len(),
        before_decoys + 3,
        "three distinct decoys must land, or the interners are not skewed and \
         this test degrades to the same-interner control",
    );

    let loaded = artifact
        .load(PackageId(1), &consumer_names, &consumer_types)
        .expect("stamp matches, so this loads");

    let holder = loaded
        .defs()
        .lookup_def(consumer_names.intern("holder"), DefKind::Value)
        .expect("registered by the load");

    let consumer_ty = loaded
        .defs()
        .get(holder)
        .ty
        .expect("declared type survives");

    assert_ne!(
        consumer_ty, list_option_s32,
        "the two interners are not skewed — this test would pass with raw \
         handles on the wire and prove nothing",
    );
    assert_eq!(
        render(&consumer_types, loaded.defs(), &consumer_names, consumer_ty),
        render(
            &producer.types,
            &producer.defs,
            &producer.names,
            list_option_s32
        ),
        "the handle moved; the type must not have",
    );
    assert_eq!(
        render(&consumer_types, loaded.defs(), &consumer_names, consumer_ty),
        "list<option<s32>>",
    );
}

/// **The control.** Kept, and kept named, because it is the shape two earlier
/// agents shipped: it passes whether types are written structurally or as raw
/// interner indices, so on its own it is evidence of nothing.
#[test]
fn the_same_interner_round_trip_proves_nothing() {
    let mut producer = Producer::new();
    let ty = producer.types.intern(TyKind::List(Ty::S32));
    producer.declare("x", DefKind::Value, Some(ty));

    let artifact = producer.build();
    let loaded = artifact
        .load(PackageId(1), &producer.names, &producer.types)
        .unwrap();

    let x = loaded
        .defs()
        .lookup_def(producer.names.intern("x"), DefKind::Value)
        .unwrap();
    assert_eq!(loaded.defs().get(x).ty, Some(ty));
}

/// An ADT crosses as a path, and the path resolves to a handle in the
/// consumer's own interner — including through a nesting.
#[test]
fn an_adt_survives_a_differently_populated_interner() {
    let mut producer = Producer::new();
    let widget = producer.declare("Widget", DefKind::Type, None);
    let widget_ty = producer.types.intern(TyKind::Adt(widget));
    let list_widget = producer.types.intern(TyKind::List(widget_ty));
    producer.declare("widgets", DefKind::Value, Some(list_widget));

    let artifact = producer.build();

    let consumer_names = Interner::new();
    let consumer_types = TypeInterner::new();
    consumer_types.intern(TyKind::Option(Ty::U64));
    consumer_types.intern(TyKind::List(Ty::CHAR));

    let loaded = artifact
        .load(PackageId(4), &consumer_names, &consumer_types)
        .unwrap();

    let widgets = loaded
        .defs()
        .lookup_def(consumer_names.intern("widgets"), DefKind::Value)
        .unwrap();
    let ty = loaded.defs().get(widgets).ty.unwrap();
    assert_ne!(ty, list_widget, "the interners must be skewed");
    assert_eq!(
        render(&consumer_types, loaded.defs(), &consumer_names, ty),
        "list<adt Widget>",
    );

    // The ADT's DefId is the consumer's, qualified with the consumer's
    // PackageId — never the producer's.
    let TyKind::List(element) = consumer_types.kind(ty) else {
        panic!("expected a list");
    };
    let TyKind::Adt(def) = consumer_types.kind(element) else {
        panic!("expected an adt");
    };
    assert_eq!(def.package, PackageId(4));
    assert_ne!(def, widget, "a DefId does not cross a package boundary");
}

// ---------------------------------------------------------------------------
// DefPath resolution
// ---------------------------------------------------------------------------

/// A path resolves to a *local* `DefId`, and the local index is not the
/// producer's. The consumer's table already holds definitions, which is the
/// case a serialized `DefId` would get wrong.
#[test]
fn a_path_resolves_to_a_local_defid_with_a_different_index() {
    let mut producer = Producer::new();
    let produced = producer.declare("Panel", DefKind::Type, None);
    assert_eq!(produced.index, 0, "first registration in the producer");

    let artifact = producer.build();

    let consumer_names = Interner::new();
    let consumer_types = TypeInterner::new();
    let mut consumer_defs = Definitions::new(PackageId(2));
    for existing in ["alpha", "beta", "gamma"] {
        consumer_defs
            .register(
                consumer_names.intern(existing),
                DefKind::Type,
                span(),
                false,
            )
            .unwrap();
    }

    let loaded = artifact
        .load_into(consumer_defs, &consumer_names, &consumer_types)
        .unwrap();

    let resolved = loaded
        .def(&path("Panel", DefKind::Type))
        .expect("the path resolves");
    assert_eq!(resolved.package, PackageId(2));
    assert_eq!(
        resolved.index, 3,
        "the consumer's table was not empty, so the index moved",
    );
    assert_ne!(
        resolved.index, produced.index,
        "a serialized DefId would have read back as index 0 and named `alpha`",
    );
    assert_eq!(
        &*consumer_names.str(loaded.defs().get(resolved).name),
        "Panel",
    );
}

/// **Inverted deliberately on 2026-07-29.** This test was
/// `a_path_distinguishes_two_namespaces_sharing_a_name` and asserted that a
/// record and a component sharing a name load as two definitions, because
/// `Definitions` permitted the reuse. The symbol table is now
/// single-namespace: the pair cannot be produced, and an artifact that carries
/// it anyway is rejected rather than half-loaded. Flipped rather than deleted so
/// the reversal stays visible — see `plans/rewrite/scope.md`.
///
/// The artifact is hand-built because `Producer::declare` can no longer make
/// one, which is itself the point.
#[test]
fn two_kinds_sharing_a_name_no_longer_load_as_two_definitions() {
    let mut artifact = Producer::new().build();
    artifact.defs.push(yelc_sema::artifact::SerializedDef {
        path: path("Panel", DefKind::Type),
        ty: None,
        is_export: true,
    });
    artifact.defs.push(yelc_sema::artifact::SerializedDef {
        path: path("Panel", DefKind::Component),
        ty: None,
        is_export: true,
    });

    let consumer_names = Interner::new();
    let consumer_types = TypeInterner::new();
    assert!(matches!(
        artifact.load(PackageId(1), &consumer_names, &consumer_types),
        Err(LoadError::DuplicateDefinition(_)),
    ));
}

/// The path's `kind` field survived the single-namespace change with a different
/// job: it no longer disambiguates, it **reconstructs**. Without it a loaded
/// record would be indistinguishable from a loaded component.
#[test]
fn a_definitions_kind_survives_the_round_trip() {
    let mut producer = Producer::new();
    for &kind in yelc_sema::DefKind::ALL {
        producer.declare(&format!("{kind:?}"), kind, None);
    }

    let artifact = producer.build();
    let consumer_names = Interner::new();
    let consumer_types = TypeInterner::new();
    let loaded = artifact
        .load(PackageId(1), &consumer_names, &consumer_types)
        .unwrap();

    for &kind in yelc_sema::DefKind::ALL {
        let id = loaded
            .defs()
            .lookup_def(consumer_names.intern(&format!("{kind:?}")), kind)
            .unwrap_or_else(|| panic!("{kind:?} did not load as a {kind:?}"));
        assert_eq!(loaded.defs().get(id).kind, kind);
    }
}

#[test]
fn an_unknown_path_does_not_resolve() {
    let producer = Producer::new();
    let artifact = producer.build();
    let names = Interner::new();
    let types = TypeInterner::new();
    let loaded = artifact.load(PackageId(1), &names, &types).unwrap();

    assert!(matches!(
        loaded.def(&path("Nope", DefKind::Type)),
        Err(LoadError::UnresolvedDefPath(_)),
    ));
}

/// The **loader** cannot rebuild an overload set — its registration pass runs
/// before the type table resolves — so an artifact carrying one is rejected
/// rather than silently losing a definition.
///
/// The producer *can* now hold one; see
/// [`an_overload_set_is_refused_by_the_loader_not_lost`], which reaches this
/// same rejection from a real registration instead of a hand-built artifact.
#[test]
fn a_colliding_definition_is_rejected_not_dropped() {
    let mut artifact = Producer::new().build();
    artifact.defs.push(yelc_sema::artifact::SerializedDef {
        path: path("len", DefKind::Value),
        ty: None,
        is_export: true,
    });
    artifact.defs.push(yelc_sema::artifact::SerializedDef {
        path: path("len", DefKind::Value),
        ty: None,
        is_export: true,
    });

    let names = Interner::new();
    let types = TypeInterner::new();
    assert!(matches!(
        artifact.load(PackageId(1), &names, &types),
        Err(LoadError::DuplicateDefinition(_)),
    ));
}

/// B3, as far as it reaches: `Definitions` holds the overload set, the writer
/// flattens both entries to the same path because
/// `SerializedDefPath::overload` cannot be filled, and the loader refuses
/// loudly. A silent drop here would lose a definition with no diagnostic.
#[test]
fn an_overload_set_is_refused_by_the_loader_not_lost() {
    let mut producer = Producer::new();
    let len = producer.names.intern("len");
    for param in [Ty::STRING, Ty::S32] {
        producer
            .defs
            .register_overload(
                len,
                span(),
                true,
                yelc_sema::OverloadKey {
                    params: vec![param],
                },
            )
            .expect("distinct keys form an overload set");
    }
    assert_eq!(producer.defs.lookup(len).len(), 2, "the set was registered");

    let artifact = producer.build();
    assert_eq!(artifact.defs.len(), 2);
    assert_eq!(
        artifact.defs[0].path, artifact.defs[1].path,
        "the wire cannot yet tell two overloads apart — that is why load refuses",
    );

    let names = Interner::new();
    let types = TypeInterner::new();
    assert!(matches!(
        artifact.load(PackageId(1), &names, &types),
        Err(LoadError::DuplicateDefinition(_)),
    ));
}

// ---------------------------------------------------------------------------
// The stamp
// ---------------------------------------------------------------------------

#[test]
fn a_compiler_mismatch_rejects_the_artifact() {
    let mut artifact = Producer::new().build();
    artifact.stamp.compiler = "some-other-build".to_string();
    assert_eq!(
        artifact.stamp.format,
        Stamp::FORMAT,
        "only `compiler` moved"
    );

    let names = Interner::new();
    let types = TypeInterner::new();
    match artifact.load(PackageId(1), &names, &types) {
        Err(LoadError::CompilerMismatch { expected, found }) => {
            assert_eq!(expected, Stamp::COMPILER);
            assert_eq!(found, "some-other-build");
        }
        Err(other) => panic!("expected a compiler mismatch, got {other:?}"),
        Ok(_) => panic!("expected a compiler mismatch, the artifact loaded"),
    }
}

#[test]
fn a_format_mismatch_rejects_the_artifact() {
    let mut artifact = Producer::new().build();
    artifact.stamp.format = Stamp::FORMAT + 1;
    assert_eq!(
        artifact.stamp.compiler,
        Stamp::COMPILER,
        "only `format` moved",
    );

    let names = Interner::new();
    let types = TypeInterner::new();
    match artifact.load(PackageId(1), &names, &types) {
        Err(LoadError::FormatMismatch { expected, found }) => {
            assert_eq!(expected, Stamp::FORMAT);
            assert_eq!(found, Stamp::FORMAT + 1);
        }
        Err(other) => panic!("expected a format mismatch, got {other:?}"),
        Ok(_) => panic!("expected a format mismatch, the artifact loaded"),
    }
}

/// Rejection is total: nothing is registered and nothing is interned before the
/// stamp is checked. A partial load is worse than no load, because it is
/// invisible.
///
/// # All three tables, because checking one is checking none
///
/// The first version of this test asserted only that the type interner was
/// untouched — and moving the stamp check to *after* definition registration
/// left it green, because registration touches the name interner and the
/// definition table and not the type interner. Found by mutation, not by
/// reading. Every table the loader can reach is now asserted.
#[test]
fn a_rejected_artifact_touches_no_table() {
    let mut producer = Producer::new();
    producer.declare("Widget", DefKind::Type, Some(Ty::S32));
    let mut artifact = producer.build();
    artifact.stamp.format = 999;

    let names = Interner::new();
    let types = TypeInterner::new();
    let mut defs = Definitions::new(PackageId(1));
    defs.register(names.intern("existing"), DefKind::Value, span(), false)
        .unwrap();

    let types_before = types.len();
    assert!(artifact.load_into(defs, &names, &types).is_err());

    assert_eq!(types.len(), types_before, "no type was interned");
    // `Definitions` is moved into the failed load, so its state is observed
    // through the interner instead: a fresh name lands at the next index only
    // if nothing else was interned in between.
    let probe = names.intern("probe");
    assert_eq!(
        probe,
        yelc_base::Name(1),
        "the rejected load interned `Widget` before checking the stamp — \
         registration ran on a stale artifact",
    );
}

// ---------------------------------------------------------------------------
// Type coverage
// ---------------------------------------------------------------------------

/// `Param` and `Infer` are different holes (decisions A3 / A4). An encoding that
/// could not tell them apart would turn a loud bug into a quiet miscompile.
#[test]
fn param_and_infer_survive_and_stay_distinct() {
    let mut producer = Producer::new();
    let param = producer.types.intern(TyKind::Param(0));
    let infer = producer.types.intern(TyKind::Infer(0));
    assert_ne!(param, infer, "distinct in the producer to begin with");
    producer.declare("templated", DefKind::Value, Some(param));
    producer.declare("unsolved", DefKind::Value, Some(infer));

    let artifact = producer.build();

    let consumer_names = Interner::new();
    let consumer_types = TypeInterner::new();
    consumer_types.intern(TyKind::Infer(0));
    consumer_types.intern(TyKind::Param(0));

    let loaded = artifact
        .load(PackageId(1), &consumer_names, &consumer_types)
        .unwrap();

    let get = |name: &str| {
        let id = loaded
            .defs()
            .lookup_def(consumer_names.intern(name), DefKind::Value)
            .unwrap();
        loaded.defs().get(id).ty.unwrap()
    };
    let loaded_param = get("templated");
    let loaded_infer = get("unsolved");

    assert_ne!(loaded_param, loaded_infer, "still two different holes");
    assert_eq!(consumer_types.kind(loaded_param), TyKind::Param(0));
    assert_eq!(consumer_types.kind(loaded_infer), TyKind::Infer(0));
}

/// A4 obligation 2: a published artifact must contain no unsolved variable. The
/// encoding can carry one so the check can *report* it; the policy is here.
#[test]
fn an_inference_hole_is_reported_to_the_producer() {
    let mut producer = Producer::new();
    let param = producer.types.intern(TyKind::Param(0));
    producer.declare("fine", DefKind::Value, Some(param));
    assert!(
        producer.build().inference_holes().is_empty(),
        "a Param is not a hole",
    );

    let infer = producer.types.intern(TyKind::Infer(3));
    producer.declare("broken", DefKind::Value, Some(infer));
    let holes = producer.build().inference_holes();
    assert_eq!(holes.len(), 1, "the Infer is found");
    assert_eq!(
        producer.build().types[holes[0] as usize],
        StructuralTy::Infer(3)
    );
}

/// Every composite shape, through a skewed interner in one pass. A structural
/// writer that dropped or reordered any child would show up here.
#[test]
fn every_composite_shape_survives() {
    let mut producer = Producer::new();
    let list_string = producer.types.intern(TyKind::List(Ty::STRING));
    let tuple = producer
        .types
        .intern(TyKind::Tuple(vec![Ty::S32, Ty::BOOL, list_string]));
    let ok_only = producer.types.intern(TyKind::Result {
        ok: Some(Ty::U8),
        err: None,
    });
    let err_only = producer.types.intern(TyKind::Result {
        ok: None,
        err: Some(Ty::STRING),
    });
    let both = producer.types.intern(TyKind::Result {
        ok: Some(Ty::CHAR),
        err: Some(Ty::F32),
    });
    let func = producer.types.intern(TyKind::Func {
        params: vec![Ty::S16, tuple],
        ret: Some(ok_only),
    });
    let procedure = producer.types.intern(TyKind::Func {
        params: Vec::new(),
        ret: None,
    });
    let deep = producer.types.intern(TyKind::Option(list_string));

    let expected = [
        ("list_string", list_string, "list<string>"),
        ("tuple", tuple, "tuple<s32, bool, list<string>>"),
        ("ok_only", ok_only, "result<u8, _>"),
        ("err_only", err_only, "result<_, string>"),
        ("both", both, "result<char, f32>"),
        (
            "func",
            func,
            "func(s16, tuple<s32, bool, list<string>>) -> result<u8, _>",
        ),
        ("procedure", procedure, "func() -> unit"),
        ("deep", deep, "option<list<string>>"),
        ("unit", Ty::UNIT, "unit"),
        ("error", Ty::ERROR, "error"),
    ];
    for (name, ty, _) in expected {
        producer.declare(name, DefKind::Value, Some(ty));
    }

    let artifact = producer.build();

    let consumer_names = Interner::new();
    let consumer_types = TypeInterner::new();
    // Skew hard: seven decoys before anything from the artifact.
    for decoy in [
        TyKind::List(Ty::U16),
        TyKind::Option(Ty::S8),
        TyKind::Tuple(vec![Ty::BOOL]),
        TyKind::Result {
            ok: Some(Ty::BOOL),
            err: Some(Ty::BOOL),
        },
        TyKind::Func {
            params: vec![Ty::BOOL],
            ret: None,
        },
        TyKind::Param(9),
        TyKind::Infer(9),
    ] {
        consumer_types.intern(decoy);
    }

    let loaded = artifact
        .load(PackageId(1), &consumer_names, &consumer_types)
        .unwrap();

    for (name, produced, rendered) in expected {
        let id = loaded
            .defs()
            .lookup_def(consumer_names.intern(name), DefKind::Value)
            .unwrap();
        let ty = loaded.defs().get(id).ty.unwrap();
        assert_eq!(
            render(&consumer_types, loaded.defs(), &consumer_names, ty),
            rendered,
            "`{name}` did not survive",
        );
        // The primitives keep their handles by construction; the composites
        // must not, or the interners are not skewed.
        if !matches!(produced, Ty::UNIT | Ty::ERROR) {
            assert_ne!(ty, produced, "`{name}` landed on the producer's handle");
        }
    }
}

/// Interning is preserved: two definitions with the same type share one table
/// entry on the wire and one handle after loading.
#[test]
fn equal_types_share_one_table_entry() {
    let mut producer = Producer::new();
    let ty = producer.types.intern(TyKind::List(Ty::S32));
    producer.declare("a", DefKind::Value, Some(ty));
    producer.declare("b", DefKind::Value, Some(ty));

    let artifact = producer.build();
    assert_eq!(
        artifact.types.len(),
        2,
        "s32 and list<s32> — the list is written once, not twice",
    );
    assert_eq!(artifact.defs[0].ty, artifact.defs[1].ty);
}

// ---------------------------------------------------------------------------
// Encoding
// ---------------------------------------------------------------------------

#[test]
fn an_artifact_round_trips_through_bytes() {
    let mut producer = Producer::new();
    let widget = producer.declare("Widget", DefKind::Type, None);
    let widget_ty = producer.types.intern(TyKind::Adt(widget));
    let list = producer.types.intern(TyKind::List(widget_ty));
    producer.declare("widgets", DefKind::Value, Some(list));

    let artifact = producer.build();
    let decoded = decode(&encode(&artifact)).expect("valid bytes decode");
    assert_eq!(decoded, artifact);
}

/// A6: serialized bytes are output, so they must be byte-stable.
#[test]
fn encoding_is_deterministic() {
    let mut producer = Producer::new();
    for name in ["zeta", "alpha", "mu"] {
        let ty = producer.types.intern(TyKind::List(Ty::S32));
        producer.declare(name, DefKind::Value, Some(ty));
    }
    let artifact = producer.build();
    assert_eq!(encode(&artifact), encode(&artifact));
    assert_eq!(encode(&producer.build()), encode(&artifact));
}

/// **The guard `Stamp::FORMAT` cannot be given by itself.**
///
/// Renaming `SerializedDefPath.namespace: Namespace` to `kind: DefKind`
/// (2026-07-29) changed the wire *schema* and not one byte of the encoding: the
/// two enums have the same four variants in the same order, and postcard writes
/// neither field names nor variant names. So nothing in this file could have
/// noticed a missing `FORMAT` bump, and a stale artifact would have loaded and
/// been reinterpreted silently — the exact failure the stamp exists to prevent.
///
/// This pins the bytes of one fixed artifact. Any change to [`wire`]'s shape
/// moves them and fails here; a change that does *not* move them still fails,
/// because `FORMAT` is in the bytes. Either way the author is made to look, and
/// the decision to bump becomes deliberate.
///
/// **When this fails:** re-read `wire.rs`, decide whether old artifacts can
/// still be read (they almost never can), bump `Stamp::FORMAT`, and re-bless the
/// constant below with the diff explained.
///
/// [`wire`]: yelc_sema::artifact::wire
#[test]
fn the_wire_bytes_are_pinned_so_a_schema_change_cannot_be_silent() {
    let mut producer = Producer::new();
    let list = producer.types.intern(TyKind::List(Ty::S32));
    producer.declare("Widget", DefKind::Type, None);
    producer.declare("widgets", DefKind::Value, Some(list));
    producer.declare("Panel", DefKind::Component, None);
    producer.declare("State", DefKind::Global, None);

    /// The format version is byte 6; `FORMAT_BYTE` below pins that.
    const PINNED: &[u8] = &[
        5, 48, 46, 49, 46, 48, 2, 4, 116, 101, 115, 116, 8, 97, 114, 116, 105, 102, 97, 99, 116, 5,
        49, 46, 48, 46, 48, 2, 3, 13, 0, 4, 4, 116, 101, 115, 116, 8, 97, 114, 116, 105, 102, 97,
        99, 116, 5, 49, 46, 48, 46, 48, 0, 1, 6, 87, 105, 100, 103, 101, 116, 0, 0, 1, 4, 116, 101,
        115, 116, 8, 97, 114, 116, 105, 102, 97, 99, 116, 5, 49, 46, 48, 46, 48, 1, 1, 7, 119, 105,
        100, 103, 101, 116, 115, 0, 1, 1, 1, 4, 116, 101, 115, 116, 8, 97, 114, 116, 105, 102, 97,
        99, 116, 5, 49, 46, 48, 46, 48, 2, 1, 5, 80, 97, 110, 101, 108, 0, 0, 1, 4, 116, 101, 115,
        116, 8, 97, 114, 116, 105, 102, 97, 99, 116, 5, 49, 46, 48, 46, 48, 3, 1, 5, 83, 116, 97,
        116, 101, 0, 0, 1,
    ];

    assert_eq!(
        encode(&producer.build()),
        PINNED,
        "the artifact encoding moved. If `wire.rs` changed shape, bump \
         `Stamp::FORMAT` before re-blessing this — old artifacts must not load \
         into a schema that means something else.",
    );
    // The stamp's `format` is byte 6 — after a length-prefixed "0.1.0". Pinned
    // positionally, because `contains` would be satisfied by any of the length
    // prefixes and this guard would be blind to the one case it exists for.
    assert_eq!(
        u32::from(PINNED[6]),
        Stamp::FORMAT,
        "the pinned bytes do not carry the current format version",
    );
}

/// Criterion 3 of the codec choice, asserted rather than trusted: both sides
/// know the schema, so field and variant names are not on the wire.
#[test]
fn the_encoding_carries_no_field_names() {
    let mut producer = Producer::new();
    producer.declare("Widget", DefKind::Type, Some(Ty::S32));
    let bytes = encode(&producer.build());

    for name in [
        "stamp",
        "compiler",
        "format",
        "is_export",
        "segments",
        "Type",
    ] {
        assert!(
            !bytes
                .windows(name.len())
                .any(|window| window == name.as_bytes()),
            "the schema name `{name}` reached the bytes",
        );
    }
    // The data does, of course.
    assert!(
        bytes.windows(6).any(|window| window == b"Widget"),
        "the definition's name is data and must be present",
    );
}

#[test]
fn garbage_bytes_do_not_decode() {
    assert!(matches!(decode(&[0xff; 3]), Err(LoadError::Decode(_))));
}

// ---------------------------------------------------------------------------
// Malformed artifacts
// ---------------------------------------------------------------------------

#[test]
fn a_type_index_past_the_table_is_rejected() {
    let mut artifact = Producer::new().build();
    artifact.types.push(StructuralTy::List(99));

    let names = Interner::new();
    let types = TypeInterner::new();
    assert!(matches!(
        artifact.load(PackageId(1), &names, &types),
        Err(LoadError::TypeIndexOutOfRange { referenced: 99, .. }),
    ));
}

/// The loader resolves the table in one forward pass, which is sound only
/// because the writer emits children first. A table that violates it is
/// rejected rather than silently reading a stale entry.
#[test]
fn a_parent_before_its_child_is_rejected() {
    let mut artifact = Producer::new().build();
    artifact.types.push(StructuralTy::List(1));
    artifact.types.push(StructuralTy::S32);

    let names = Interner::new();
    let types = TypeInterner::new();
    assert!(matches!(
        artifact.load(PackageId(1), &names, &types),
        Err(LoadError::ForwardTypeReference {
            entry: 0,
            referenced: 1
        }),
    ));
}

#[test]
fn a_path_with_no_segments_is_rejected() {
    let mut artifact = Producer::new().build();
    artifact.defs.push(yelc_sema::artifact::SerializedDef {
        path: SerializedDefPath {
            package: package(),
            kind: DefKind::Type,
            segments: Vec::new(),
            overload: Vec::new(),
        },
        ty: None,
        is_export: true,
    });

    let names = Interner::new();
    let types = TypeInterner::new();
    assert!(matches!(
        artifact.load(PackageId(1), &names, &types),
        Err(LoadError::PathWithoutSegments(_)),
    ));
}

// ---------------------------------------------------------------------------
// Definition fields
// ---------------------------------------------------------------------------

#[test]
fn export_visibility_and_declared_types_survive() {
    let names = Interner::new();
    let types = TypeInterner::new();
    let mut defs = Definitions::new(PackageId::LOCAL);
    let public = defs
        .register(names.intern("shown"), DefKind::Value, span(), true)
        .unwrap();
    let private = defs
        .register(names.intern("hidden"), DefKind::Value, span(), false)
        .unwrap();
    defs.set_ty(public, Ty::STRING);

    let artifact = Artifact::build(package(), &names, &types, &defs);
    let consumer_names = Interner::new();
    let consumer_types = TypeInterner::new();
    let loaded = artifact
        .load(PackageId(3), &consumer_names, &consumer_types)
        .unwrap();

    let shown = loaded
        .defs()
        .lookup_def(consumer_names.intern("shown"), DefKind::Value)
        .unwrap();
    let hidden = loaded
        .defs()
        .lookup_def(consumer_names.intern("hidden"), DefKind::Value)
        .unwrap();
    assert!(loaded.defs().get(shown).is_export);
    assert!(!loaded.defs().get(hidden).is_export);
    assert_eq!(loaded.defs().get(shown).ty, Some(Ty::STRING));
    assert_eq!(
        loaded.defs().get(hidden).ty,
        None,
        "absent stays absent, never a placeholder",
    );
    assert_eq!(public.index, 0);
    assert_eq!(private.index, 1);
}

/// A producer span indexes the producer's `SourceMap`, so it cannot cross. The
/// loaded span says "synthetic" rather than aliasing the consumer's first file.
#[test]
fn a_span_does_not_cross_the_boundary() {
    let mut producer = Producer::new();
    producer.declare("x", DefKind::Value, None);
    assert_eq!(producer.defs.get(yelc_sema::DefId::local(0)).span, span());

    let artifact = producer.build();
    let names = Interner::new();
    let types = TypeInterner::new();
    let loaded = artifact.load(PackageId(1), &names, &types).unwrap();
    let x = loaded
        .defs()
        .lookup_def(names.intern("x"), DefKind::Value)
        .unwrap();
    assert_eq!(loaded.defs().get(x).span, Span::default());
    assert!(!loaded.defs().get(x).span.source.is_valid());
}

#[test]
fn the_package_identity_survives() {
    let artifact = Producer::new().build();
    assert_eq!(artifact.package.to_string(), "test:artifact@1.0.0");

    let names = Interner::new();
    let types = TypeInterner::new();
    let loaded = artifact.load(PackageId(1), &names, &types).unwrap();
    assert_eq!(loaded.package(), &package());
}

#[test]
fn an_empty_package_round_trips() {
    let artifact = Producer::new().build();
    let names = Interner::new();
    let types = TypeInterner::new();
    let loaded = decode(&encode(&artifact))
        .unwrap()
        .load(PackageId(1), &names, &types)
        .unwrap();
    assert_eq!(loaded.defs().len(), 0);
    assert_eq!(loaded.type_count(), 0);
}
