//! WIT AST builder for programmatic WIT construction.
//!
//! This module builds a WIT AST using wit-parser types and then uses
//! wit-component to embed the component-type metadata into the core module.

use rustc_hash::FxHashMap as HashMap;

use semver::Version;
use wit_parser::{
    Case, Docs, Enum, EnumCase, Field, Function, FunctionKind, Handle, Interface, InterfaceId,
    Package, PackageId, PackageName, Param, Record, Resolve, Result_, Stability, Tuple, Type,
    TypeDef, TypeDefKind, TypeId, TypeOwner, Variant, World, WorldId, WorldItem, WorldKey,
};

use yel_core::DefId;
use yel_core::context::CompilerContext;
use yel_core::definitions::DefKind;
use yel_core::lir::{InterfaceDirection, LirIfaceFn, LirInterface, LirReceiver, LirResource};
use yel_core::types::{InternedTyKind, Ty};

use super::CodegenError;

/// Builder for constructing WIT AST programmatically.
/// One free-function export passed to
/// [`WitAstBuilder::build_function_world`]. The caller (typically
/// `wasm::functions`) flattens its [`crate::FunctionInput`] entries
/// into this shape — keeping `WitAstBuilder` independent of the
/// module-assembly types up the call stack.
pub struct FreeFunctionExport<'a> {
    /// Source-level function name; gets kebab-cased before emission.
    pub name: &'a str,
    /// Parameter names, in declared order. Empty entries are
    /// replaced with `arg{i}`. Kebab-cased before emission.
    pub param_names: &'a [&'a str],
    /// Parameter yel-level types, parallel to `param_names`.
    pub param_tys: &'a [Ty],
    /// Single-value return type, if any.
    pub result_ty: Option<Ty>,
}

pub struct WitAstBuilder<'a> {
    ctx: &'a CompilerContext,
    resolve: Resolve,
    package_id: PackageId,
    /// Cached id of the shared `yel:ui@0.1.0` package. Owns both the DOM
    /// and dispatch interfaces — the language-level syscall surface every
    /// Yel module plugs into.
    yel_ui_package_id: Option<PackageId>,
    /// Dedicated interface that owns every canonical ADT (record/enum/variant)
    /// type. Other interfaces reference those types via `use` aliases.
    types_interface_id: Option<InterfaceId>,
    /// Resource-only interfaces (one per exported component, named
    /// `{component}-resource`). Each owns just the bare `resource X;`
    /// declaration; the matching `{component}-component` interface aliases
    /// the resource in to attach methods, and callbacks/host-boundary
    /// interfaces alias the same resource to take `borrow<X>` params.
    /// Tracked so the world-builder can add them to imports.
    resource_interface_ids: Vec<InterfaceId>,
    /// Map from LIR type to WIT TypeId (canonical ids, owned by
    /// `types_interface_id` for ADTs).
    type_map: HashMap<Ty, TypeId>,
    /// Map from DefId to WIT TypeId for ADT types (canonical ids).
    adt_map: HashMap<DefId, TypeId>,
    /// Cache of use-aliases: for each (importing interface, canonical type id)
    /// pair, stores the aliased TypeId owned by the importing interface.
    alias_map: HashMap<(InterfaceId, TypeId), TypeId>,
    /// Import-side boundary contract (foreign-package interfaces — DOM
    /// today). Rendered directly by `render_import_interface`; replaces the
    /// hardcoded `create_dom_interface`.
    import_contract: &'a [LirInterface],
    /// While rendering a foreign interface, the interface its `owned_types`
    /// (and any nested ADTs) must be defined inline in — set per the
    /// contract, NOT a DOM special-case.
    inline_types_owner: Option<InterfaceId>,
}

impl<'a> WitAstBuilder<'a> {
    /// Create a new WIT AST builder.
    pub fn new(ctx: &'a CompilerContext, namespace: &str, name: &str, version: &str) -> Self {
        let mut resolve = Resolve::default();

        // Create package. The namespace/name must already be valid
        // WIT kebab-case identifiers; `Compiler::validate_package` rejects
        // non-compliant packages at parse time, so we trust the caller here.
        let package = resolve.packages.alloc(Package {
            name: PackageName {
                namespace: namespace.to_string(),
                name: name.to_string(),
                version: Some(parse_version(version)),
            },
            docs: Docs::default(),
            interfaces: Default::default(),
            worlds: Default::default(),
        });

        Self {
            ctx,
            resolve,
            package_id: package,
            yel_ui_package_id: None,
            types_interface_id: None,
            resource_interface_ids: Vec::new(),
            type_map: HashMap::default(),
            adt_map: HashMap::default(),
            alias_map: HashMap::default(),
            import_contract: &[],
            inline_types_owner: None,
        }
    }

    /// Supply the import-side boundary contract (foreign-package
    /// interfaces) the renderer should emit. Set by `generate_wit`.
    pub fn set_import_contract(&mut self, interfaces: &'a [LirInterface]) {
        self.import_contract = interfaces;
    }

    /// Render every foreign-package interface in the import contract into
    /// WIT, returning their ids (for the world's import list). Each
    /// interface lives in its own package and defines its `owned_types`
    /// inline; its functions are emitted from their signatures. This is the
    /// generic replacement for the hardcoded `create_dom_interface` — DOM
    /// is just one row in the contract.
    /// Render every import interface in the contract (`LirModule.interfaces`)
    /// into WIT: `{component}-callbacks` interfaces, local-global interfaces,
    /// and foreign-package interfaces (DOM). Returns their ids for the
    /// world's import list. This is the single import-interface renderer —
    /// the former `create_per_component_callbacks_interfaces` /
    /// `create_globals_interfaces` are gone; a function's receiver and its
    /// interface's package drive the shape, not the kind of item it came
    /// from.
    fn render_import_contract(
        &mut self,
        component_resources: &HashMap<DefId, TypeId>,
    ) -> Result<Vec<InterfaceId>, CodegenError> {
        let mut ids = Vec::new();
        for idx in 0..self.import_contract.len() {
            // Index rather than borrow the slice across `&mut self` calls.
            if self.import_contract[idx].direction != InterfaceDirection::Import {
                continue;
            }
            let iface = self.import_contract[idx].clone();
            ids.push(self.render_import_interface(&iface, component_resources)?);
        }
        Ok(ids)
    }

    /// Render a single import interface from its contract entry. Foreign
    /// interfaces (`package: Some`) own their ADTs inline; local interfaces
    /// reference the shared-types interface via `use` aliases.
    fn render_import_interface(
        &mut self,
        iface: &LirInterface,
        component_resources: &HashMap<DefId, TypeId>,
    ) -> Result<InterfaceId, CodegenError> {
        let is_foreign = iface.package.is_some();
        // Foreign interfaces own a package; today that's always `yel:ui`.
        let pkg = if is_foreign {
            self.ensure_yel_ui_package()
        } else {
            self.package_id
        };
        let iface_name = to_kebab_case(&self.ctx.str(iface.name));
        let interface_id = self.resolve.interfaces.alloc(Interface {
            name: Some(iface_name.clone()),
            docs: Docs::default(),
            types: Default::default(),
            functions: Default::default(),
            package: Some(pkg),
            stability: Stability::default(),
            span: Default::default(),
            clone_of: None,
        });

        // Define the interface's owned ADTs INLINE (owned by this
        // interface), driven by the contract — a foreign host package can't
        // `use` the module's shared types. Local interfaces leave
        // `owned_types` empty and `use` the shared-types interface instead.
        if is_foreign {
            self.inline_types_owner = Some(interface_id);
            for &ty in &iface.owned_types {
                self.register_type(ty)?;
            }
            self.inline_types_owner = None;
        }

        // Emit each function from its signature (+ receiver).
        for f in &iface.functions {
            self.render_iface_function(f, interface_id, component_resources)?;
        }

        self.resolve.packages[pkg]
            .interfaces
            .insert(iface_name, interface_id);
        Ok(interface_id)
    }

    /// Emit one `LirIfaceFn` as a WIT function in `interface_id`. A
    /// `Borrow(component)` receiver becomes a leading `self: borrow<resource>`
    /// parameter (component callbacks); `None` is freestanding.
    fn render_iface_function(
        &mut self,
        f: &LirIfaceFn,
        interface_id: InterfaceId,
        component_resources: &HashMap<DefId, TypeId>,
    ) -> Result<(), CodegenError> {
        let mut params = Vec::new();
        if let LirReceiver::Borrow(comp_def) = f.receiver {
            // Non-exported components have no WIT-visible resource of their
            // own — borrow the first exported component's resource (core ABI
            // is unaffected: `borrow<X>` lowers to i32 at the boundary).
            let resource_ty = component_resources
                .get(&comp_def)
                .copied()
                .or_else(|| component_resources.values().next().copied())
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "no resource type registered for component owning callback `{}`",
                        self.ctx.str(f.name)
                    ))
                })?;
            let aliased = self.use_resource_in(resource_ty, interface_id);
            let borrow_ty = Type::Id(self.resolve.types.alloc(TypeDef {
                name: None,
                kind: TypeDefKind::Handle(Handle::Borrow(aliased)),
                owner: TypeOwner::Interface(interface_id),
                docs: Docs::default(),
                stability: Stability::default(),
                span: Default::default(),
            }));
            params.push(Param {
                name: "self".to_string(),
                ty: borrow_ty,
                span: Default::default(),
            });
        }
        for (pname, pty) in &f.params {
            params.push(Param {
                name: to_kebab_case(&self.ctx.str(*pname)),
                ty: self.use_type_in(*pty, interface_id)?,
                span: Default::default(),
            });
        }
        let result = match f.result {
            Some(rty) => Some(self.use_type_in(rty, interface_id)?),
            None => None,
        };
        let name = to_kebab_case(&self.ctx.str(f.name));
        let function = Function {
            name: name.clone(),
            kind: FunctionKind::Freestanding,
            params,
            result,
            docs: Docs::default(),
            stability: Stability::default(),
            span: Default::default(),
        };
        self.resolve.interfaces[interface_id]
            .functions
            .insert(name, function);
        Ok(())
    }

    /// Create or return the shared `types` interface. Every ADT (record,
    /// enum, variant) is owned by this interface; other interfaces `use`
    /// them via aliases.
    fn ensure_types_interface(&mut self) -> InterfaceId {
        if let Some(id) = self.types_interface_id {
            return id;
        }
        let id = self.resolve.interfaces.alloc(Interface {
            name: Some("shared-types".to_string()),
            docs: Docs::default(),
            types: Default::default(),
            functions: Default::default(),
            package: Some(self.package_id),
            stability: Stability::default(),
            span: Default::default(),
            clone_of: None,
        });
        self.resolve.packages[self.package_id]
            .interfaces
            .insert("shared-types".to_string(), id);
        self.types_interface_id = Some(id);
        id
    }

    /// Resolve `ty` to a `Type` visible from `in_interface`. For ADT types,
    /// this emits (or reuses) a `use types.{name};` alias in `in_interface`
    /// and returns the alias's `Type::Id`.
    fn use_type_in(&mut self, ty: Ty, in_interface: InterfaceId) -> Result<Type, CodegenError> {
        let resolved = self.ty_to_wit_type(ty)?;
        let type_id = match resolved {
            Type::Id(id) => id,
            other => return Ok(other),
        };

        let owner = self.resolve.types[type_id].owner;
        match owner {
            TypeOwner::Interface(owner_iface) if owner_iface != in_interface => {}
            _ => return Ok(Type::Id(type_id)),
        }

        if let Some(&alias) = self.alias_map.get(&(in_interface, type_id)) {
            return Ok(Type::Id(alias));
        }

        let alias_name = self.resolve.types[type_id]
            .name
            .clone()
            .unwrap_or_else(|| "anon".to_string());
        let alias_id = self.resolve.types.alloc(TypeDef {
            name: Some(alias_name.clone()),
            kind: TypeDefKind::Type(Type::Id(type_id)),
            owner: TypeOwner::Interface(in_interface),
            docs: Docs::default(),
            stability: Stability::default(),
            span: Default::default(),
        });
        self.resolve.interfaces[in_interface]
            .types
            .insert(alias_name, alias_id);
        self.alias_map.insert((in_interface, type_id), alias_id);
        Ok(Type::Id(alias_id))
    }

    /// Make a resource type visible inside `in_interface` via a `use` alias.
    ///
    /// Parallels [`use_type_in`] but is specialised to resources: wit-parser
    /// aliases use `TypeDefKind::Type(Type::Id(original))` and the original's
    /// owner stays where it was. The returned `TypeId` is the one callers
    /// should wrap in `Handle::Borrow(..)`.
    fn use_resource_in(&mut self, resource_ty: TypeId, in_interface: InterfaceId) -> TypeId {
        let owner = self.resolve.types[resource_ty].owner;
        match owner {
            TypeOwner::Interface(owner_iface) if owner_iface != in_interface => {}
            _ => return resource_ty,
        }
        if let Some(&alias) = self.alias_map.get(&(in_interface, resource_ty)) {
            return alias;
        }
        let alias_name = self.resolve.types[resource_ty]
            .name
            .clone()
            .unwrap_or_else(|| "resource".to_string());
        let alias_id = self.resolve.types.alloc(TypeDef {
            name: Some(alias_name.clone()),
            kind: TypeDefKind::Type(Type::Id(resource_ty)),
            owner: TypeOwner::Interface(in_interface),
            docs: Docs::default(),
            stability: Stability::default(),
            span: Default::default(),
        });
        self.resolve.interfaces[in_interface]
            .types
            .insert(alias_name, alias_id);
        self.alias_map.insert((in_interface, resource_ty), alias_id);
        alias_id
    }

    /// Build the WIT AST with an explicit full component list.
    ///
    /// `exported` drives export-facing WIT: resource interfaces, world
    /// exports, etc. `all` drives import-facing WIT: any `func`-typed
    /// property on *any* component — exported or not — becomes a host
    /// callback imported through the shared `{pkg}-callbacks` interface,
    /// because the component's body can invoke it from its event handlers
    /// and the core module wires those Call sites through that import.
    pub fn build_wit_with_all(
        &mut self,
        exported: &[&LirResource],
        // Import-facing WIT (callbacks, globals) now derives entirely from the
        // module's import contract, so the full component list is no longer
        // consulted here. Retained for signature stability with the callers.
        _all: &[&LirResource],
    ) -> Result<WorldId, CodegenError> {
        let components = exported;
        // Register all types referenced by components + globals up front so
        // every interface builder sees the same resolved type map.
        for c in components {
            self.register_types_for_component(c)?;
        }
        self.register_types_for_globals()?;

        // Per-component resource interfaces — one per exported component.
        // Kept per-component because each component is genuinely its own
        // resource with its own constructor/method shape.
        let mut component_interfaces: Vec<(InterfaceId, Option<InterfaceId>)> = Vec::new();
        let mut component_resources: HashMap<DefId, TypeId> = HashMap::default();
        for c in components {
            let resource_name = to_kebab_case(&self.ctx.str(c.name));
            let (iface, resource_ty) = self.create_component_interface(c, &resource_name)?;
            component_resources.insert(c.def_id, resource_ty);
            // Callbacks are module-scoped (see below), so each component
            // entry gets `None` for its callback interface id.
            component_interfaces.push((iface, None));
        }

        // Every import interface — `{component}-callbacks`, local-global,
        // and foreign-package (DOM) — is rendered from the one LIR boundary
        // contract (`LirModule.interfaces`). A function's receiver and its
        // interface's package drive the WIT shape, so there is a single
        // renderer instead of the former per-kind creators.
        let import_interface_ids = self.render_import_contract(&component_resources)?;

        // Module-level dispatch interface — exported exactly once regardless
        // of component count. Only emitted when the module has at least one
        // component (libraries / globals-only files have no event handlers).
        let dispatch_interface_id = if components.is_empty() {
            None
        } else {
            Some(self.create_module_dispatch_interface()?)
        };

        // `import component X { ... }` declarations become imported
        // interfaces — the host or an upstream module supplies the
        // implementation.
        let import_component_interface_ids = self.create_import_component_interfaces()?;

        // World name is consistent regardless of whether the module has
        // exported components or is globals/lib-only — a Yel module is a
        // Yel module either way.
        let pkg_name = self.resolve.packages[self.package_id].name.name.clone();
        let world_name = pkg_name;

        let resource_interfaces = self.resource_interface_ids.clone();
        self.create_world(
            &world_name,
            &import_interface_ids,
            dispatch_interface_id,
            &component_interfaces,
            &resource_interfaces,
            &import_component_interface_ids,
        )
    }

    /// Register all types used by the component's signals.
    fn register_types_for_component(
        &mut self,
        component: &LirResource,
    ) -> Result<(), CodegenError> {
        for signal in &component.signals {
            self.register_type(signal.ty)?;
        }
        Ok(())
    }

    /// Register a type in the resolve, returning its TypeId.
    fn register_type(&mut self, ty: Ty) -> Result<Option<TypeId>, CodegenError> {
        // Check if already registered
        if let Some(&type_id) = self.type_map.get(&ty) {
            return Ok(Some(type_id));
        }

        match self.ctx.ty_kind(ty) {
            InternedTyKind::List(elem_ty) => {
                // Register the element type first
                self.register_type(*elem_ty)?;
                Ok(None) // Lists are inline, not separate type definitions
            }
            InternedTyKind::Adt(def_id) => {
                // Check if already registered
                if let Some(&type_id) = self.adt_map.get(def_id) {
                    return Ok(Some(type_id));
                }

                // When rendering a foreign interface, its owned ADTs (and
                // any nested ones reached here via recursion) are defined
                // inline in that interface rather than in shared-types.
                let types_iface = match self.inline_types_owner {
                    Some(iface) => iface,
                    None => self.ensure_types_interface(),
                };

                // Register the record type
                if let Some(record) = self.ctx.defs.as_record(*def_id) {
                    // The WebAssembly component model requires a `record` to
                    // have at least one field, so an empty record cannot be
                    // expressed at the WIT boundary. Reject it here with a
                    // clear message rather than letting `wit-component` fail
                    // later with an opaque "decoding custom section
                    // component-type" error. Empty records remain usable
                    // internally — only crossing the boundary is forbidden.
                    if record.fields.is_empty() {
                        return Err(CodegenError::UnsupportedType(format!(
                            "empty record `{}` cannot cross the component boundary — the \
                             WebAssembly component model requires a record to have at least \
                             one field. Give it a field, or keep it internal (do not expose \
                             it through an exported/imported component signal, callback \
                             parameter/return, or a field of another exposed type).",
                            self.ctx.str(record.name)
                        )));
                    }
                    let record_name = to_kebab_case(&self.ctx.str(record.name));

                    // Build field types
                    let mut fields = Vec::new();
                    for &field_def_id in &record.fields {
                        if let DefKind::Field(field) = self.ctx.defs.kind(field_def_id) {
                            let field_name = to_kebab_case(&self.ctx.str(field.name));
                            let field_type = self.ty_to_wit_type(field.ty)?;
                            fields.push((field_name, field_type));
                        }
                    }

                    let type_id = self.resolve.types.alloc(TypeDef {
                        name: Some(record_name.clone()),
                        kind: TypeDefKind::Record(Record {
                            fields: fields
                                .into_iter()
                                .map(|(name, ty)| Field {
                                    name,
                                    ty,
                                    docs: Docs::default(),
                                    span: Default::default(),
                                })
                                .collect(),
                        }),
                        owner: TypeOwner::Interface(types_iface),
                        docs: Docs::default(),
                        stability: Stability::default(),
                        span: Default::default(),
                    });
                    self.resolve.interfaces[types_iface]
                        .types
                        .insert(record_name, type_id);

                    self.adt_map.insert(*def_id, type_id);
                    self.type_map.insert(ty, type_id);

                    Ok(Some(type_id))
                } else if let Some(enum_def) = self.ctx.defs.as_enum(*def_id) {
                    // The component model forbids a zero-case enum (see the
                    // empty-record note above); reject it with a clear message.
                    if enum_def.cases.is_empty() {
                        return Err(CodegenError::UnsupportedType(format!(
                            "empty enum `{}` cannot cross the component boundary — the \
                             WebAssembly component model requires an enum to have at least \
                             one case. Give it a case, or keep it internal.",
                            self.ctx.str(enum_def.name)
                        )));
                    }
                    let enum_name = to_kebab_case(&self.ctx.str(enum_def.name));

                    let mut cases = Vec::new();
                    for &case_def_id in &enum_def.cases {
                        if let DefKind::VariantCase(case) = self.ctx.defs.kind(case_def_id) {
                            let case_name = to_kebab_case(&self.ctx.str(case.name));
                            cases.push(EnumCase {
                                name: case_name,
                                docs: Docs::default(),
                                span: Default::default(),
                            });
                        }
                    }

                    let type_id = self.resolve.types.alloc(TypeDef {
                        name: Some(enum_name.clone()),
                        kind: TypeDefKind::Enum(Enum { cases }),
                        owner: TypeOwner::Interface(types_iface),
                        docs: Docs::default(),
                        stability: Stability::default(),
                        span: Default::default(),
                    });
                    self.resolve.interfaces[types_iface]
                        .types
                        .insert(enum_name, type_id);

                    self.adt_map.insert(*def_id, type_id);
                    self.type_map.insert(ty, type_id);

                    Ok(Some(type_id))
                } else if let Some(variant_def) = self.ctx.defs.as_variant(*def_id) {
                    // The component model forbids a zero-case variant.
                    if variant_def.cases.is_empty() {
                        return Err(CodegenError::UnsupportedType(format!(
                            "empty variant `{}` cannot cross the component boundary — the \
                             WebAssembly component model requires a variant to have at least \
                             one case. Give it a case, or keep it internal.",
                            self.ctx.str(variant_def.name)
                        )));
                    }
                    let variant_name = to_kebab_case(&self.ctx.str(variant_def.name));

                    let mut cases = Vec::new();
                    for &case_def_id in &variant_def.cases {
                        if let DefKind::VariantCase(case) = self.ctx.defs.kind(case_def_id) {
                            let case_name = to_kebab_case(&self.ctx.str(case.name));
                            let payload_ty = if let Some(payload) = case.payload {
                                self.register_type(payload)?;
                                Some(self.ty_to_wit_type(payload)?)
                            } else {
                                None
                            };
                            cases.push(Case {
                                name: case_name,
                                ty: payload_ty,
                                docs: Docs::default(),
                                span: Default::default(),
                            });
                        }
                    }

                    let type_id = self.resolve.types.alloc(TypeDef {
                        name: Some(variant_name.clone()),
                        kind: TypeDefKind::Variant(Variant { cases }),
                        owner: TypeOwner::Interface(types_iface),
                        docs: Docs::default(),
                        stability: Stability::default(),
                        span: Default::default(),
                    });
                    self.resolve.interfaces[types_iface]
                        .types
                        .insert(variant_name, type_id);

                    self.adt_map.insert(*def_id, type_id);
                    self.type_map.insert(ty, type_id);

                    Ok(Some(type_id))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None), // Primitive types don't need registration
        }
    }

    /// Convert a LIR type to a WIT type.
    /// For complex types (List, Option), this creates a TypeDef and returns Type::Id.
    fn ty_to_wit_type(&mut self, ty: Ty) -> Result<Type, CodegenError> {
        // Check if we've already created a TypeDef for this type
        if let Some(&type_id) = self.type_map.get(&ty) {
            return Ok(Type::Id(type_id));
        }

        Ok(match self.ctx.ty_kind(ty) {
            InternedTyKind::Bool => Type::Bool,
            InternedTyKind::S8 => Type::S8,
            InternedTyKind::S16 => Type::S16,
            InternedTyKind::S32 => Type::S32,
            InternedTyKind::S64 => Type::S64,
            InternedTyKind::U8 => Type::U8,
            InternedTyKind::U16 => Type::U16,
            InternedTyKind::U32 => Type::U32,
            InternedTyKind::U64 => Type::U64,
            InternedTyKind::F32 => Type::F32,
            InternedTyKind::F64 => Type::F64,
            InternedTyKind::Char => Type::Char,
            InternedTyKind::String => Type::String,
            InternedTyKind::List(elem_ty) => {
                let elem_type = self.ty_to_wit_type(*elem_ty)?;
                // Create a TypeDef for the list
                let type_id = self.resolve.types.alloc(TypeDef {
                    name: None, // Anonymous type
                    kind: TypeDefKind::List(elem_type),
                    owner: TypeOwner::None,
                    docs: Docs::default(),
                    stability: Stability::default(),
                    span: Default::default(),
                });
                self.type_map.insert(ty, type_id);
                Type::Id(type_id)
            }
            InternedTyKind::Option(inner) => {
                let inner_type = self.ty_to_wit_type(*inner)?;
                // Create a TypeDef for the option
                let type_id = self.resolve.types.alloc(TypeDef {
                    name: None,
                    kind: TypeDefKind::Option(inner_type),
                    owner: TypeOwner::None,
                    docs: Docs::default(),
                    stability: Stability::default(),
                    span: Default::default(),
                });
                self.type_map.insert(ty, type_id);
                Type::Id(type_id)
            }
            InternedTyKind::Result { ok, err } => {
                let ok_type = ok.map(|t| self.ty_to_wit_type(t)).transpose()?;
                let err_type = err.map(|t| self.ty_to_wit_type(t)).transpose()?;
                // Create a TypeDef for the result
                let type_id = self.resolve.types.alloc(TypeDef {
                    name: None,
                    kind: TypeDefKind::Result(Result_ {
                        ok: ok_type,
                        err: err_type,
                    }),
                    owner: TypeOwner::None,
                    docs: Docs::default(),
                    stability: Stability::default(),
                    span: Default::default(),
                });
                self.type_map.insert(ty, type_id);
                Type::Id(type_id)
            }
            InternedTyKind::Tuple(elements) => {
                let element_types: Vec<Type> = elements
                    .iter()
                    .map(|t| self.ty_to_wit_type(*t))
                    .collect::<Result<_, _>>()?;
                // Create a TypeDef for the tuple
                let type_id = self.resolve.types.alloc(TypeDef {
                    name: None,
                    kind: TypeDefKind::Tuple(Tuple {
                        types: element_types,
                    }),
                    owner: TypeOwner::None,
                    docs: Docs::default(),
                    stability: Stability::default(),
                    span: Default::default(),
                });
                self.type_map.insert(ty, type_id);
                Type::Id(type_id)
            }
            InternedTyKind::Adt(def_id) => {
                if let Some(&type_id) = self.adt_map.get(def_id) {
                    Type::Id(type_id)
                } else {
                    // Lazily register ADTs that are referenced only from
                    // expressions (e.g. stdlib variants like `Color`) rather
                    // than directly from a signal/prop type.
                    match self.register_type(ty)? {
                        Some(type_id) => Type::Id(type_id),
                        None => {
                            return Err(CodegenError::MissingDefinition(format!(
                                "Type not registered: {:?}",
                                def_id
                            )));
                        }
                    }
                }
            }
            // Map UI types to primitives
            InternedTyKind::Length | InternedTyKind::PhysicalLength => Type::F32,
            InternedTyKind::Angle | InternedTyKind::Duration | InternedTyKind::Percent => Type::F32,
            InternedTyKind::Color | InternedTyKind::Brush => Type::U32,
            _ => Type::String, // Fallback
        })
    }

    /// Create the component interface with resource. Returns the interface
    /// id plus the resource type id. The resource + methods live in this
    /// interface as usual; callbacks and other module-scoped interfaces
    /// reference it via `use {component}-component.{resource}` aliasing.
    fn create_component_interface(
        &mut self,
        component: &LirResource,
        resource_name: &str,
    ) -> Result<(InterfaceId, TypeId), CodegenError> {
        let interface_name = format!("{}-component", resource_name);

        // Create the resource type
        let resource_type_id = self.resolve.types.alloc(TypeDef {
            name: Some(resource_name.to_string()),
            kind: TypeDefKind::Resource,
            owner: TypeOwner::None,
            docs: Docs::default(),
            stability: Stability::default(),
            span: Default::default(),
        });

        // Create interface
        let interface_id = self.resolve.interfaces.alloc(Interface {
            name: Some(interface_name.clone()),
            docs: Docs::default(),
            types: Default::default(),
            functions: Default::default(),
            package: Some(self.package_id),
            stability: Stability::default(),
            span: Default::default(),
            clone_of: None,
        });

        self.resolve.types[resource_type_id].owner = TypeOwner::Interface(interface_id);
        self.resolve.interfaces[interface_id]
            .types
            .insert(resource_name.to_string(), resource_type_id);

        // ADTs live in the dedicated `types` interface; this interface pulls
        // them in via `use types.{...};` on demand through `use_type_in`.

        // Create own handle type for constructor return
        let own_type_id = self.resolve.types.alloc(TypeDef {
            name: None,
            kind: TypeDefKind::Handle(Handle::Own(resource_type_id)),
            owner: TypeOwner::Interface(interface_id),
            docs: Docs::default(),
            stability: Stability::default(),
            span: Default::default(),
        });

        // Add constructor - returns own<resource>
        let constructor_func = Function {
            name: String::new(), // Empty for constructors
            kind: FunctionKind::Constructor(resource_type_id),
            params: vec![],
            result: Some(Type::Id(own_type_id)), // Constructor returns own<resource>
            docs: Docs::default(),
            stability: Stability::default(),
            span: Default::default(),
        };
        self.resolve.interfaces[interface_id]
            .functions
            .insert(format!("[constructor]{}", resource_name), constructor_func);

        // Create borrow type for self parameter
        let borrow_type_id = self.resolve.types.alloc(TypeDef {
            name: None,
            kind: TypeDefKind::Handle(Handle::Borrow(resource_type_id)),
            owner: TypeOwner::Interface(interface_id),
            docs: Docs::default(),
            stability: Stability::default(),
            span: Default::default(),
        });
        let self_type = Type::Id(borrow_type_id);

        // Add mount method. Container components (those with `@children`)
        // return a `u32` children-root node id — the caller appends its
        // children under that returned id. Non-containers return nothing.
        let has_children_slot = self
            .ctx
            .defs
            .as_component(component.def_id)
            .map(|c| c.has_children_slot)
            .unwrap_or(false);
        let mount_name = format!("[method]{}.mount", resource_name);
        let mount_func = Function {
            name: mount_name.clone(),
            kind: FunctionKind::Method(resource_type_id),
            params: vec![
                Param {
                    name: "self".to_string(),
                    ty: self_type,
                    span: Default::default(),
                },
                Param {
                    name: "root".to_string(),
                    ty: Type::U32,
                    span: Default::default(),
                },
            ],
            result: if has_children_slot {
                Some(Type::U32)
            } else {
                None
            },
            docs: Docs::default(),
            stability: Stability::default(),
            span: Default::default(),
        };
        self.resolve.interfaces[interface_id]
            .functions
            .insert(mount_name, mount_func);

        // Add unmount method
        let unmount_name = format!("[method]{}.unmount", resource_name);
        let unmount_func = Function {
            name: unmount_name.clone(),
            kind: FunctionKind::Method(resource_type_id),
            params: vec![Param {
                name: "self".to_string(),
                ty: self_type,
                span: Default::default(),
            }],
            result: None,
            docs: Docs::default(),
            stability: Stability::default(),
            span: Default::default(),
        };
        self.resolve.interfaces[interface_id]
            .functions
            .insert(unmount_name, unmount_func);

        // Dispatch is module-scoped and lives in a dedicated dispatch
        // interface — see `create_module_dispatch_interface`. It's exported
        // exactly once per module regardless of how many components exist.

        // Add getter/setter for each signal (skip function-typed signals - those are callbacks)
        for signal in &component.signals {
            // Skip function-typed signals - they're callbacks, not data properties
            if matches!(self.ctx.ty_kind(signal.ty), InternedTyKind::Func { .. }) {
                continue;
            }

            let signal_name = to_kebab_case(&self.ctx.str(self.ctx.defs.name(signal.def_id)));
            let wit_type = self.use_type_in(signal.ty, interface_id)?;

            // Getter
            let getter_name = format!("[method]{}.get-{}", resource_name, signal_name);
            let getter_func = Function {
                name: getter_name.clone(),
                kind: FunctionKind::Method(resource_type_id),
                params: vec![Param {
                    name: "self".to_string(),
                    ty: self_type,
                    span: Default::default(),
                }],
                result: Some(wit_type),
                docs: Docs::default(),
                stability: Stability::default(),
                span: Default::default(),
            };
            self.resolve.interfaces[interface_id]
                .functions
                .insert(getter_name, getter_func);

            // Setter
            let setter_name = format!("[method]{}.set-{}", resource_name, signal_name);
            let setter_func = Function {
                name: setter_name.clone(),
                kind: FunctionKind::Method(resource_type_id),
                params: vec![
                    Param {
                        name: "self".to_string(),
                        ty: self_type,
                        span: Default::default(),
                    },
                    Param {
                        name: "value".to_string(),
                        ty: wit_type,
                        span: Default::default(),
                    },
                ],
                result: None,
                docs: Docs::default(),
                stability: Stability::default(),
                span: Default::default(),
            };
            self.resolve.interfaces[interface_id]
                .functions
                .insert(setter_name, setter_func);
        }

        // Register interface in package
        self.resolve.packages[self.package_id]
            .interfaces
            .insert(interface_name, interface_id);

        Ok((interface_id, resource_type_id))
    }

    /// Allocate (or return the cached) `yel:ui@0.1.0` package id. Both the
    /// DOM interface and the dispatch interface live here — they're the
    /// language-level syscall surface every Yel module shares, not
    /// per-module artefacts.
    fn ensure_yel_ui_package(&mut self) -> PackageId {
        if let Some(id) = self.yel_ui_package_id {
            return id;
        }
        let id = self.resolve.packages.alloc(Package {
            name: PackageName {
                namespace: "yel".to_string(),
                name: "ui".to_string(),
                version: Some(parse_version("0.1.0")),
            },
            docs: Docs::default(),
            interfaces: Default::default(),
            worlds: Default::default(),
        });
        self.yel_ui_package_id = Some(id);
        id
    }

    /// Create the DOM interface in the shared `yel:ui@0.1.0` package.
    /// Create a module-scoped dispatch interface.
    ///
    /// The host calls `dispatch(handler-id: u32)` with the id returned from
    /// `add-event-listener` to route any DOM event back to whichever
    /// component's handler registered it. There's exactly one such function
    /// per module — it's not a per-component concern.
    /// Ensure the shared `yel:ui/dispatch@0.1.0` interface exists and return
    /// its id. Dispatch has a fixed signature (`func(handler-id: u32)`) and
    /// identical semantics across every Yel module, so it lives next to
    /// `yel:ui/dom` — a language-level syscall surface, not a per-module
    /// interface.
    fn create_module_dispatch_interface(&mut self) -> Result<InterfaceId, CodegenError> {
        let yel_ui_pkg = self.ensure_yel_ui_package();

        // Reuse if another call already created it.
        if let Some(&existing) = self.resolve.packages[yel_ui_pkg].interfaces.get("dispatch") {
            return Ok(existing);
        }

        let interface_id = self.resolve.interfaces.alloc(Interface {
            name: Some("dispatch".to_string()),
            docs: Docs::default(),
            types: Default::default(),
            functions: Default::default(),
            package: Some(yel_ui_pkg),
            stability: Stability::default(),
            span: Default::default(),
            clone_of: None,
        });

        // Flat single-level variant — one arm per payload shape.
        // Simpler to pattern-match in the guest and to construct in
        // the host than a nested variant. Each input-type maps 1:1 to
        // the signal type it can drive. Extend with mouse / keyboard
        // arms when those codegen paths land.
        //
        //   <input type="text">     → input-text(string)
        //   <input type="number">   → input-f64(f64)     // or input-f32/s32
        //   <input type="checkbox"> → input-bool(bool)
        let event_value_id = self.resolve.types.alloc(TypeDef {
            name: Some("event-value".to_string()),
            kind: TypeDefKind::Variant(Variant {
                cases: vec![
                    Case {
                        name: "none".to_string(),
                        ty: None,
                        docs: Docs::default(),
                        span: Default::default(),
                    },
                    Case {
                        name: "input-text".to_string(),
                        ty: Some(Type::String),
                        docs: Docs::default(),
                        span: Default::default(),
                    },
                    Case {
                        name: "input-f64".to_string(),
                        ty: Some(Type::F64),
                        docs: Docs::default(),
                        span: Default::default(),
                    },
                    Case {
                        name: "input-f32".to_string(),
                        ty: Some(Type::F32),
                        docs: Docs::default(),
                        span: Default::default(),
                    },
                    Case {
                        name: "input-s32".to_string(),
                        ty: Some(Type::S32),
                        docs: Docs::default(),
                        span: Default::default(),
                    },
                    Case {
                        name: "input-bool".to_string(),
                        ty: Some(Type::Bool),
                        docs: Docs::default(),
                        span: Default::default(),
                    },
                    // Cold drag/drop lifecycle. Appended after the input
                    // arms so their discriminants stay stable. `drop`
                    // carries the payload string, `drag-enter` the media
                    // type, `drag-leave` nothing. The host constructs
                    // these from a brokered DropEvent and calls
                    // `dispatch(handler-id, event-value::drop(payload))`.
                    Case {
                        name: "drop".to_string(),
                        ty: Some(Type::String),
                        docs: Docs::default(),
                        span: Default::default(),
                    },
                    Case {
                        name: "drag-enter".to_string(),
                        ty: Some(Type::String),
                        docs: Docs::default(),
                        span: Default::default(),
                    },
                    Case {
                        name: "drag-leave".to_string(),
                        ty: None,
                        docs: Docs::default(),
                        span: Default::default(),
                    },
                ],
            }),
            owner: TypeOwner::Interface(interface_id),
            docs: Docs::default(),
            stability: Stability::default(),
            span: Default::default(),
        });
        self.resolve.interfaces[interface_id]
            .types
            .insert("event-value".to_string(), event_value_id);

        let dispatch_func = Function {
            name: "dispatch".to_string(),
            kind: FunctionKind::Freestanding,
            params: vec![
                Param {
                    name: "handler-id".to_string(),
                    ty: Type::U32,
                    span: Default::default(),
                },
                Param {
                    name: "event".to_string(),
                    ty: Type::Id(event_value_id),
                    span: Default::default(),
                },
            ],
            result: None,
            docs: Docs::default(),
            stability: Stability::default(),
            span: Default::default(),
        };

        self.resolve.interfaces[interface_id]
            .functions
            .insert("dispatch".to_string(), dispatch_func);

        self.resolve.packages[yel_ui_pkg]
            .interfaces
            .insert("dispatch".to_string(), interface_id);

        Ok(interface_id)
    }

    /// Register types referenced by any global's properties or callback
    /// signatures. Mirrors `register_types_for_component` but walks the
    /// file-level globals instead of a single component's signals.
    fn register_types_for_globals(&mut self) -> Result<(), CodegenError> {
        let global_ids: Vec<_> = self.ctx.defs.globals().collect();
        for g_id in global_ids {
            let g = match self.ctx.defs.as_global(g_id) {
                Some(g) => g.clone(),
                None => continue,
            };
            // Foreign-package globals (the built-in `Dom`) carry their own
            // host-defined types inline in `create_dom_interface`; don't
            // also hoist them into the shared-types interface.
            if g.package.is_some() {
                continue;
            }
            for prop_id in &g.properties {
                if let Some(ty) = self.ctx.defs.type_of(*prop_id) {
                    self.register_type(ty)?;
                }
            }
            for cb_id in &g.callbacks {
                if let Some(f) = self.ctx.defs.as_function(*cb_id) {
                    for pid in &f.params {
                        if let Some(ty) = self.ctx.defs.type_of(*pid) {
                            self.register_type(ty)?;
                        }
                    }
                    if f.ret_ty != Ty::UNIT {
                        self.register_type(f.ret_ty)?;
                    }
                }
            }
        }
        Ok(())
    }

    /// Create one WIT interface per global that has host-boundary members.
    ///
    /// A global's interface contains:
    /// - `set-<prop>: func(v: T)` for each `in` or `in-out` property
    /// - `on-<prop>-changed: func(v: T)` for each `out` or `in-out` property
    /// - `<name>: func(...) -> ret?` for each callback
    ///
    /// Emit one WIT interface per `import component` declaration in the
    /// module. Each interface exposes the component as a resource with the
    /// declared properties (as `get-X`/`set-X` method pairs) and methods.
    /// The world imports every such interface so the host / upstream
    /// module supplies the concrete implementation.
    fn create_import_component_interfaces(&mut self) -> Result<Vec<InterfaceId>, CodegenError> {
        let mut out = Vec::new();
        let ids: Vec<_> = self.ctx.defs.import_components().collect();
        for id in ids {
            let (name, prop_ids, method_ids) = match self.ctx.defs.as_import_component(id) {
                Some(ic) => (
                    self.ctx.str(ic.name).to_string(),
                    ic.properties.clone(),
                    ic.methods.clone(),
                ),
                None => continue,
            };

            let resource_name = to_kebab_case(&name);
            let interface_name = format!("{}-component", resource_name);

            // Allocate the interface + its resource type up front so
            // subsequent `use_type_in` / method declarations can reference
            // them. Layout mirrors `create_component_interface` but the
            // interface is module-imported rather than exported.
            let interface_id = self.resolve.interfaces.alloc(Interface {
                name: Some(interface_name.clone()),
                docs: Docs::default(),
                types: Default::default(),
                functions: Default::default(),
                package: Some(self.package_id),
                stability: Stability::default(),
                span: Default::default(),
                clone_of: None,
            });
            let resource_ty = self.resolve.types.alloc(TypeDef {
                name: Some(resource_name.clone()),
                kind: TypeDefKind::Resource,
                owner: TypeOwner::Interface(interface_id),
                docs: Docs::default(),
                stability: Stability::default(),
                span: Default::default(),
            });
            self.resolve.interfaces[interface_id]
                .types
                .insert(resource_name.clone(), resource_ty);

            let self_borrow = Type::Id(self.resolve.types.alloc(TypeDef {
                name: None,
                kind: TypeDefKind::Handle(Handle::Borrow(resource_ty)),
                owner: TypeOwner::Interface(interface_id),
                docs: Docs::default(),
                stability: Stability::default(),
                span: Default::default(),
            }));
            let self_ty = self_borrow;

            // Constructor: `func() -> own<resource>`. The host's
            // implementation is responsible for allocating instances.
            let own_ty = Type::Id(self.resolve.types.alloc(TypeDef {
                name: None,
                kind: TypeDefKind::Handle(Handle::Own(resource_ty)),
                owner: TypeOwner::Interface(interface_id),
                docs: Docs::default(),
                stability: Stability::default(),
                span: Default::default(),
            }));
            let ctor = Function {
                name: String::new(),
                kind: FunctionKind::Constructor(resource_ty),
                params: vec![],
                result: Some(own_ty),
                docs: Docs::default(),
                stability: Stability::default(),
                span: Default::default(),
            };
            self.resolve.interfaces[interface_id]
                .functions
                .insert(format!("[constructor]{}", resource_name), ctor);

            // Properties → getter/setter method pairs. Same shape as
            // exported component interfaces.
            for prop_id in &prop_ids {
                let prop_name = self.ctx.str(self.ctx.defs.name(*prop_id)).to_string();
                let prop_ty = match self.ctx.defs.type_of(*prop_id) {
                    Some(t) => t,
                    None => continue,
                };
                let wit_ty = self.use_type_in(prop_ty, interface_id)?;
                let kebab = to_kebab_case(&prop_name);

                let getter_name = format!("[method]{}.get-{}", resource_name, kebab);
                let getter = Function {
                    name: getter_name.clone(),
                    kind: FunctionKind::Method(resource_ty),
                    params: vec![Param {
                        name: "self".to_string(),
                        ty: self_ty,
                        span: Default::default(),
                    }],
                    result: Some(wit_ty),
                    docs: Docs::default(),
                    stability: Stability::default(),
                    span: Default::default(),
                };
                self.resolve.interfaces[interface_id]
                    .functions
                    .insert(getter_name, getter);

                let setter_name = format!("[method]{}.set-{}", resource_name, kebab);
                let setter = Function {
                    name: setter_name.clone(),
                    kind: FunctionKind::Method(resource_ty),
                    params: vec![
                        Param {
                            name: "self".to_string(),
                            ty: self_ty,
                            span: Default::default(),
                        },
                        Param {
                            name: "value".to_string(),
                            ty: wit_ty,
                            span: Default::default(),
                        },
                    ],
                    result: None,
                    docs: Docs::default(),
                    stability: Stability::default(),
                    span: Default::default(),
                };
                self.resolve.interfaces[interface_id]
                    .functions
                    .insert(setter_name, setter);
            }

            // Declared methods become resource methods with their own
            // signatures. `self: borrow<resource>` is prepended.
            for method_id in &method_ids {
                let func_def = match self.ctx.defs.as_function(*method_id) {
                    Some(f) => f.clone(),
                    None => continue,
                };
                let method_name = self.ctx.str(func_def.name).to_string();
                let kebab = to_kebab_case(&method_name);

                let mut params: Vec<Param> = vec![Param {
                    name: "self".to_string(),
                    ty: self_ty,
                    span: Default::default(),
                }];
                for pid in &func_def.params {
                    let pname = self.ctx.str(self.ctx.defs.name(*pid)).to_string();
                    let pty = match self.ctx.defs.type_of(*pid) {
                        Some(t) => t,
                        None => continue,
                    };
                    params.push(Param {
                        name: to_kebab_case(&pname),
                        ty: self.use_type_in(pty, interface_id)?,
                        span: Default::default(),
                    });
                }
                let result = if func_def.ret_ty == Ty::UNIT {
                    None
                } else {
                    Some(self.use_type_in(func_def.ret_ty, interface_id)?)
                };

                let wit_method_name = format!("[method]{}.{}", resource_name, kebab);
                let method = Function {
                    name: wit_method_name.clone(),
                    kind: FunctionKind::Method(resource_ty),
                    params,
                    result,
                    docs: Docs::default(),
                    stability: Stability::default(),
                    span: Default::default(),
                };
                self.resolve.interfaces[interface_id]
                    .functions
                    .insert(wit_method_name, method);
            }

            self.resolve.packages[self.package_id]
                .interfaces
                .insert(interface_name, interface_id);
            out.push(interface_id);
        }
        Ok(out)
    }

    /// Create the world.
    ///
    /// `import_interface_ids` are every import interface rendered from the
    /// boundary contract (component callbacks, local globals, foreign DOM).
    /// `component_interfaces` is a list of (resource interface, optional
    /// callbacks interface) pairs — one entry per LIR component; each resource
    /// interface becomes an export. `dispatch_interface_id`, when present, is
    /// exported once at module scope — it carries the single freestanding
    /// `dispatch(handler-id)` function.
    fn create_world(
        &mut self,
        world_name: &str,
        import_interface_ids: &[InterfaceId],
        dispatch_interface_id: Option<InterfaceId>,
        component_interfaces: &[(InterfaceId, Option<InterfaceId>)],
        resource_interface_ids: &[InterfaceId],
        import_component_interface_ids: &[InterfaceId],
    ) -> Result<WorldId, CodegenError> {
        let world_id = self.resolve.worlds.alloc(World {
            name: world_name.to_string(),
            docs: Docs::default(),
            imports: Default::default(),
            exports: Default::default(),
            includes: Default::default(),
            package: Some(self.package_id),
            stability: Stability::default(),
            span: Default::default(),
        });

        // Imports: shared types interface (if any ADTs were registered),
        // then every contract-rendered import interface.
        if let Some(types_id) = self.types_interface_id {
            self.resolve.worlds[world_id].imports.insert(
                WorldKey::Interface(types_id),
                WorldItem::Interface {
                    id: types_id,
                    stability: Stability::default(),
                    span: Default::default(),
                },
            );
        }

        // Component resource interfaces are exports, but wit-component
        // requires them in the import map when encoding a later-imported
        // interface (a `{component}-callbacks` interface) that references the
        // component's resource via `use`. Insert them as imports FIRST — only
        // when some import interface actually borrows a component resource
        // (a component-callbacks interface, marked by a non-empty `resources`
        // list in the contract). Otherwise skip, to keep the world clean.
        let has_component_callbacks = self
            .import_contract
            .iter()
            .any(|i| !i.resources.is_empty());
        if has_component_callbacks {
            for &(iface_id, _) in component_interfaces {
                self.resolve.worlds[world_id].imports.insert(
                    WorldKey::Interface(iface_id),
                    WorldItem::Interface {
                        id: iface_id,
                        stability: Stability::default(),
                        span: Default::default(),
                    },
                );
            }
        }

        for &iid in import_interface_ids {
            self.resolve.worlds[world_id].imports.insert(
                WorldKey::Interface(iid),
                WorldItem::Interface {
                    id: iid,
                    stability: Stability::default(),
                    span: Default::default(),
                },
            );
        }

        // `import component` declarations — the host (or an upstream
        // module) provides the concrete implementation of each.
        for &ic_id in import_component_interface_ids {
            self.resolve.worlds[world_id].imports.insert(
                WorldKey::Interface(ic_id),
                WorldItem::Interface {
                    id: ic_id,
                    stability: Stability::default(),
                    span: Default::default(),
                },
            );
        }

        // Shared resource-only interfaces (reserved for future work; the
        // current pipeline declares resources inside `{component}-component`).
        for &r_id in resource_interface_ids {
            self.resolve.worlds[world_id].imports.insert(
                WorldKey::Interface(r_id),
                WorldItem::Interface {
                    id: r_id,
                    stability: Stability::default(),
                    span: Default::default(),
                },
            );
        }

        // Exports: one per component's resource interface, plus the
        // module-scoped dispatch interface (emitted once when any component
        // exists). Dispatch routes any DOM event back to whichever
        // component's handler registered for it via add-event-listener.
        if let Some(dispatch_id) = dispatch_interface_id {
            self.resolve.worlds[world_id].exports.insert(
                WorldKey::Interface(dispatch_id),
                WorldItem::Interface {
                    id: dispatch_id,
                    stability: Stability::default(),
                    span: Default::default(),
                },
            );
        }
        for &(iface_id, _) in component_interfaces {
            self.resolve.worlds[world_id].exports.insert(
                WorldKey::Interface(iface_id),
                WorldItem::Interface {
                    id: iface_id,
                    stability: Stability::default(),
                    span: Default::default(),
                },
            );
        }

        // Register world in package
        self.resolve.packages[self.package_id]
            .worlds
            .insert(world_name.to_string(), world_id);

        Ok(world_id)
    }

    /// Get the resolve and world ID for embedding.
    /// Build a world that exports a flat list of free functions —
    /// the non-UI sibling of [`Self::build_wit_with_all`]. Each
    /// `(name, params, result)` entry becomes a world-level `export
    /// foo: func(...)`. Compound types route through `ty_to_wit_type`,
    /// so records / variants / lists work the moment the matching
    /// `WireTypeDecl`s are registered in `ctx.defs`.
    ///
    /// Used by the non-UI module-assembly path
    /// (`wasm::functions::generate_component`) which produces a wasm
    /// component out of a list of top-level functions.
    pub fn build_function_world(
        &mut self,
        world_name: &str,
        functions: &[FreeFunctionExport<'_>],
    ) -> Result<WorldId, CodegenError> {
        // Register every ADT referenced by the exports' param /
        // return types up front so `ty_to_wit_type` produces canonical
        // ids (and so the shared `types` interface is created on
        // demand). The walk goes through `ty_to_wit_type` which
        // memoises in `type_map`, so duplicates across functions are
        // harmless.
        for f in functions {
            for &pty in f.param_tys {
                self.ty_to_wit_type(pty)?;
            }
            if let Some(rty) = f.result_ty {
                self.ty_to_wit_type(rty)?;
            }
        }

        let world_id = self.resolve.worlds.alloc(World {
            name: world_name.to_string(),
            docs: Docs::default(),
            imports: Default::default(),
            exports: Default::default(),
            includes: Default::default(),
            package: Some(self.package_id),
            stability: Stability::default(),
            span: Default::default(),
        });

        // If any ADTs were registered, the world has to import the
        // shared types interface — otherwise the use-aliases on the
        // exports would reference an out-of-scope type.
        if let Some(types_id) = self.types_interface_id {
            self.resolve.worlds[world_id].imports.insert(
                WorldKey::Interface(types_id),
                WorldItem::Interface {
                    id: types_id,
                    stability: Stability::default(),
                    span: Default::default(),
                },
            );
        }

        for f in functions {
            // Param types: walk through `ty_to_wit_type` again so we
            // hit the cached id and (for ADTs) materialise the world's
            // own alias via `use shared-types.{name}` if needed. Free
            // functions live directly on the world, no owning
            // interface, so we use the canonical type id as-is.
            let mut params: Vec<Param> = Vec::with_capacity(f.param_tys.len());
            for (i, (name, ty)) in f.param_names.iter().zip(f.param_tys.iter()).enumerate() {
                let wit_ty = self.ty_to_wit_type(*ty)?;
                let param_name = if name.is_empty() {
                    format!("arg{i}")
                } else {
                    to_kebab_case(name)
                };
                params.push(Param {
                    name: param_name,
                    ty: wit_ty,
                    span: Default::default(),
                });
            }
            let result = match f.result_ty {
                Some(rty) => Some(self.ty_to_wit_type(rty)?),
                None => None,
            };

            let func_name = to_kebab_case(f.name);
            let func = Function {
                name: func_name.clone(),
                kind: FunctionKind::Freestanding,
                params,
                result,
                docs: Docs::default(),
                stability: Stability::default(),
                span: Default::default(),
            };
            self.resolve.worlds[world_id]
                .exports
                .insert(WorldKey::Name(func_name.clone()), WorldItem::Function(func));
        }

        // Register the world in the package's worlds map. UI's
        // `build_world_with_callbacks` does the same — it's how
        // `into_resolve_and_world` finds the world, and how
        // `wit_component` discovers it during component encoding.
        self.resolve.packages[self.package_id]
            .worlds
            .insert(world_name.to_string(), world_id);

        Ok(world_id)
    }

    pub fn into_resolve_and_world(self) -> (Resolve, WorldId) {
        // Get the first world from the package
        let world_id = *self.resolve.packages[self.package_id]
            .worlds
            .values()
            .next()
            .expect("No world created");

        (self.resolve, world_id)
    }
}

/// Convert a name to a valid WIT identifier.
///
/// Delegates to [`crate::wasm::to_wit_name`] so every name emitted into the
/// WIT document is both kebab-cased and guaranteed to start each
/// hyphen-separated segment with a letter. WIT identifiers reject digit-
/// starting segments (e.g. `field-0`), which `wit-component` detects while
/// decoding the component-type custom section.
fn to_kebab_case(s: &str) -> String {
    crate::wasm::to_wit_name(s)
}

/// Parse a version string into a semver::Version.
fn parse_version(version: &str) -> Version {
    Version::parse(version).unwrap_or_else(|_| Version::new(0, 1, 0))
}
