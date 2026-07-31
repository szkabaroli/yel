//! `signalck` — the signal-dependency pass: which reactive state each body
//! reads and writes.
//!
//! The file name matches the frozen `thir/signalck.rs` deliberately, and sets
//! the convention: **every analysis pass in this crate is `<pass>ck.rs`**
//! (`packageck`, `signalck`, stage 4's `typeck`). The lowering is not a pass
//! in this sense — it constructs; these check and annotate.
//!
//! The frozen `thir/signalck.rs` (426 lines) runs after type checking and
//! reads only `Def`/`Local` — never a type. That fact is why it belongs
//! **here**: computed at the end of stage 3, on name-resolved untyped HIR,
//! before stage 4 exists. Running it *after* the UI desugaring is equivalent
//! to the pre-desugaring order the plans sketched, because the desugaring
//! keeps every reactive site explicit — a prop getter, a `UiText` content, a
//! build-position `Match` scrutinee, a `Repeat` iterable, a handler closure —
//! and resolves every reference to exactly the form dependency tracking wants.
//!
//! # What counts as reactive state
//!
//! A [`Prop`](HirExprKind::Prop) reference — the enclosing item's own
//! property — and a global property access (`Store.count`, a
//! [`Field`](HirExprKind::Field) whose base is a global's def). A `Def`
//! reference alone is not a dependency: a function is not state.
//!
//! # Shape
//!
//! One [`BodyDependencies`] per body, in a side table on
//! [`HirModule`](crate::HirModule) keyed by the body's `hir_id` (B3: analysis
//! result off the node). Reads and writes are **sorted and deduplicated** —
//! they feed effect wiring and eventually output, so hash-map iteration order
//! must never reach them (A6).

use crate::expr::{HirExpr, HirExprKind, HirStmt};
use crate::module::HirModule;
use crate::visit::{self, Visitor};
use yelc_base::Name;
use yelc_sema::definitions::MemberKind;
use yelc_sema::{CompilerContext, DefId, DefKind};

/// One reactive-state reference: a property of a component or global.
///
/// No derived `Ord`: `Name` deliberately has none — an interner index is not
/// an order — so the sort below keys on the resolved *string*, which is
/// stable under interner changes.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct StateRef {
    /// The component or global declaring the property.
    pub owner: DefId,
    /// The property's name — the member address, matching
    /// [`Definitions::members`](yelc_sema::Definitions::members) rows.
    pub member: Name,
}

/// What one body reads and writes. Sorted, deduplicated.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct BodyDependencies {
    pub reads: Vec<StateRef>,
    pub writes: Vec<StateRef>,
}

impl BodyDependencies {
    pub fn is_empty(&self) -> bool {
        self.reads.is_empty() && self.writes.is_empty()
    }
}

/// Compute dependency sets — one per **body**, and one per reactive **site**
/// inside it, all keyed by `hir_id` in one table.
///
/// The two granularities serve different consumers: a function body's set is
/// what effect *triggering* reads (the frozen `resolve_global_triggers`
/// shape), while a per-site set — this prop's getter, this text's content,
/// this region's scrutinee — is what effect *wiring* subscribes (the frozen
/// `signalck` shape, one stage earlier).
pub fn compute(
    module: &HirModule,
    sema: &CompilerContext,
) -> Vec<(crate::HirId, BodyDependencies)> {
    let mut all = Vec::new();
    for (_, body) in module.bodies.iter_enumerated() {
        let deps = collect(sema, |collector| collector.visit_body(body));
        if !deps.is_empty() {
            all.push((body.hir_id, deps));
        }
        let mut sites = Sites {
            sema,
            all: &mut all,
        };
        sites.visit_body(body);
    }
    all
}

/// Run one collector over `walk`, returning its sorted, deduplicated sets.
fn collect(sema: &CompilerContext, walk: impl FnOnce(&mut Collector)) -> BodyDependencies {
    let mut collector = Collector {
        sema,
        deps: BodyDependencies::default(),
        writing: false,
    };
    walk(&mut collector);
    let sort = |refs: &mut Vec<StateRef>| {
        refs.sort_by_key(|state| (state.owner.index, sema.names.str(state.member).to_string()));
        refs.dedup();
    };
    sort(&mut collector.deps.reads);
    sort(&mut collector.deps.writes);
    collector.deps
}

/// Finds the reactive sites and gives each its own entry: a prop's getter and
/// setter, a text's content, a repeat's iterable, a UI conditional's
/// scrutinee. Keys are the *site's* ids, which cannot collide with a body's.
struct Sites<'a, 'out> {
    sema: &'a CompilerContext,
    all: &'out mut Vec<(crate::HirId, BodyDependencies)>,
}

impl Sites<'_, '_> {
    fn site(&mut self, key: crate::HirId, walk: impl FnOnce(&mut Collector)) {
        let deps = collect(self.sema, walk);
        if !deps.is_empty() {
            self.all.push((key, deps));
        }
    }
}

impl Visitor for Sites<'_, '_> {
    fn visit_expr(&mut self, expr: &HirExpr) {
        match &expr.kind {
            HirExprKind::Instantiate(node) => {
                for prop in &node.props {
                    if let Some(getter) = &prop.getter {
                        self.site(prop.hir_id, |collector| collector.visit_expr(getter));
                    }
                    if let Some(setter) = &prop.setter {
                        self.site(setter.hir_id, |collector| collector.visit_closure(setter));
                    }
                }
                // Children recurse for their own sites; props were consumed
                // above, so walk children directly rather than re-walking the
                // instantiate.
                for child in &node.children {
                    self.visit_expr(child);
                }
            }
            HirExprKind::UiText(content) => {
                self.site(expr.hir_id, |collector| collector.visit_expr(content));
            }
            HirExprKind::Repeat(node) => {
                self.site(expr.hir_id, |collector| {
                    collector.visit_expr(&node.iterable);
                    if let Some(key) = &node.key {
                        collector.visit_expr(key);
                    }
                });
                for child in &node.children {
                    self.visit_expr(child);
                }
            }
            HirExprKind::Match(node) => {
                self.site(expr.hir_id, |collector| {
                    collector.visit_expr(&node.scrutinee)
                });
                for arm in &node.arms {
                    self.visit_expr(&arm.value);
                }
            }
            _ => visit::walk_expr(self, expr),
        }
    }
}

struct Collector<'a> {
    sema: &'a CompilerContext,
    deps: BodyDependencies,
    /// Set while visiting an assignment's target — the same reference is a
    /// write there and a read anywhere else.
    writing: bool,
}

impl Collector<'_> {
    fn record(&mut self, state: StateRef) {
        if self.writing {
            self.deps.writes.push(state);
        } else {
            self.deps.reads.push(state);
        }
    }

    /// The state a reference names, if it names state.
    fn state_of(&self, expr: &HirExpr) -> Option<StateRef> {
        match &expr.kind {
            HirExprKind::Prop { owner, member } => Some(StateRef {
                owner: *owner,
                member: *member,
            }),
            // `Store.count` — a **property** on a global's def. The member
            // kind matters: `Num.min` is a function member reached the same
            // way, and a function is not state — recording it as a read was
            // this pass's first false positive, caught by the embedded-stdlib
            // dump showing `reads(Num.min)`.
            HirExprKind::Field { base, field } => match &base.kind {
                // `definition`/`members_of`, not `defs.get`: the def may be an
                // included package's, and the per-table accessors panic on
                // foreign ids.
                HirExprKind::Def(def)
                    if self
                        .sema
                        .definition(*def)
                        .is_some_and(|definition| definition.kind == DefKind::Global)
                        && self.sema.members_of(*def).iter().any(|member| {
                            member.name == *field
                                && matches!(member.kind, MemberKind::Property { .. })
                        }) =>
                {
                    Some(StateRef {
                        owner: *def,
                        member: *field,
                    })
                }
                _ => None,
            },
            _ => None,
        }
    }
}

impl Visitor for Collector<'_> {
    fn visit_stmt(&mut self, stmt: &HirStmt) {
        if let HirStmt::Assign { target, value, .. } = stmt {
            // The target is a write; anything *inside* it beyond the state
            // reference itself (an index expression, a field base) is reads.
            if let Some(state) = self.state_of(target) {
                let was = self.writing;
                self.writing = true;
                self.record(state);
                self.writing = was;
            } else {
                self.visit_expr(target);
            }
            self.visit_expr(value);
            return;
        }
        visit::walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &HirExpr) {
        if let Some(state) = self.state_of(expr) {
            self.record(state);
            // A `Field` base already consumed; nothing below is a dependency.
            return;
        }
        visit::walk_expr(self, expr);
    }
}
