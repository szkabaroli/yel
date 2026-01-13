//! WIT AST builder for programmatic WIT construction.
//!
//! This module builds a WIT AST using wit-parser types and then uses
//! wit-component to embed the component-type metadata into the core module.

use std::collections::HashMap;

use wit_parser::{
    Docs, Function, FunctionKind, Interface, Package, PackageName,
    Resolve, Results, Stability, Type, TypeDef, TypeDefKind, TypeId,
    TypeOwner, World, WorldId, WorldItem, WorldKey,
};
use semver::Version;

use crate::context::CompilerContext;
use crate::definitions::DefKind;
use crate::lir::LirComponent;
use crate::types::{InternedTyKind, Ty};

use super::CodegenError;

/// Builder for constructing WIT AST programmatically.
pub struct WitAstBuilder<'a> {
    ctx: &'a CompilerContext,
    resolve: Resolve,
    package_id: wit_parser::PackageId,
    /// Map from LIR type to WIT TypeId
    type_map: HashMap<Ty, TypeId>,
    /// Map from DefId to WIT TypeId for ADT types
    adt_map: HashMap<crate::ids::DefId, TypeId>,
}

impl<'a> WitAstBuilder<'a> {
    /// Create a new WIT AST builder.
    pub fn new(ctx: &'a CompilerContext, namespace: &str, name: &str, version: &str) -> Self {
        let mut resolve = Resolve::default();
        
        // Create package
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
            type_map: HashMap::new(),
            adt_map: HashMap::new(),
        }
    }
    
    /// Build the WIT AST for a component.
    pub fn build_component_wit(
        &mut self,
        component: &LirComponent,
    ) -> Result<WorldId, CodegenError> {
        let resource_name = to_kebab_case(&self.ctx.str(component.name));
        
        // First, register all record types used in signals
        self.register_types_for_component(component)?;
        
        // Create the component interface with resource
        let interface_id = self.create_component_interface(component, &resource_name)?;
        
        // Create the DOM interface (imported)
        let dom_interface_id = self.create_dom_interface()?;
        
        // Create the DOM events interface (exported)
        let dom_events_interface_id = self.create_dom_events_interface()?;
        
        // Create callback interface if needed
        let callback_interface_id = self.create_callbacks_interface(component, &resource_name)?;
        
        // Create the world
        let world_id = self.create_world(
            &resource_name,
            dom_interface_id,
            dom_events_interface_id,
            interface_id,
            callback_interface_id,
        )?;
        
        Ok(world_id)
    }
    
    /// Register all types used by the component's signals.
    fn register_types_for_component(&mut self, component: &LirComponent) -> Result<(), CodegenError> {
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

                // Register the record type
                if let Some(record) = self.ctx.defs.as_record(*def_id) {
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

                    // Create the record type
                    let type_id = self.resolve.types.alloc(TypeDef {
                        name: Some(record_name),
                        kind: TypeDefKind::Record(wit_parser::Record {
                            fields: fields.into_iter().map(|(name, ty)| {
                                wit_parser::Field {
                                    name,
                                    ty,
                                    docs: Docs::default(),
                                }
                            }).collect(),
                        }),
                        owner: TypeOwner::None,
                        docs: Docs::default(),
                        stability: Stability::default(),
                    });

                    self.adt_map.insert(*def_id, type_id);
                    self.type_map.insert(ty, type_id);

                    Ok(Some(type_id))
                } else if let Some(enum_def) = self.ctx.defs.as_enum(*def_id) {
                    // Register the enum type
                    let enum_name = to_kebab_case(&self.ctx.str(enum_def.name));

                    // Build enum cases
                    let mut cases = Vec::new();
                    for &case_def_id in &enum_def.cases {
                        if let DefKind::VariantCase(case) = self.ctx.defs.kind(case_def_id) {
                            let case_name = to_kebab_case(&self.ctx.str(case.name));
                            cases.push(wit_parser::EnumCase {
                                name: case_name,
                                docs: Docs::default(),
                            });
                        }
                    }

                    // Create the enum type
                    let type_id = self.resolve.types.alloc(TypeDef {
                        name: Some(enum_name),
                        kind: TypeDefKind::Enum(wit_parser::Enum { cases }),
                        owner: TypeOwner::None,
                        docs: Docs::default(),
                        stability: Stability::default(),
                    });

                    self.adt_map.insert(*def_id, type_id);
                    self.type_map.insert(ty, type_id);

                    Ok(Some(type_id))
                } else if let Some(variant_def) = self.ctx.defs.as_variant(*def_id) {
                    // Register the variant type (enum with payloads)
                    let variant_name = to_kebab_case(&self.ctx.str(variant_def.name));

                    // Build variant cases
                    let mut cases = Vec::new();
                    for &case_def_id in &variant_def.cases {
                        if let DefKind::VariantCase(case) = self.ctx.defs.kind(case_def_id) {
                            let case_name = to_kebab_case(&self.ctx.str(case.name));
                            // Convert payload type if present
                            let payload_ty = if let Some(payload) = case.payload {
                                // Register the payload type first
                                self.register_type(payload)?;
                                Some(self.ty_to_wit_type(payload)?)
                            } else {
                                None
                            };
                            cases.push(wit_parser::Case {
                                name: case_name,
                                ty: payload_ty,
                                docs: Docs::default(),
                            });
                        }
                    }

                    // Create the variant type
                    let type_id = self.resolve.types.alloc(TypeDef {
                        name: Some(variant_name),
                        kind: TypeDefKind::Variant(wit_parser::Variant { cases }),
                        owner: TypeOwner::None,
                        docs: Docs::default(),
                        stability: Stability::default(),
                    });

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
                });
                self.type_map.insert(ty, type_id);
                Type::Id(type_id)
            }
            InternedTyKind::Adt(def_id) => {
                if let Some(&type_id) = self.adt_map.get(def_id) {
                    Type::Id(type_id)
                } else {
                    return Err(CodegenError::MissingDefinition(
                        format!("Type not registered: {:?}", def_id)
                    ));
                }
            }
            // Map UI types to primitives
            InternedTyKind::Length | InternedTyKind::PhysicalLength => Type::F32,
            InternedTyKind::Angle | InternedTyKind::Duration | InternedTyKind::Percent => Type::F32,
            InternedTyKind::Color | InternedTyKind::Brush => Type::U32,
            _ => Type::String, // Fallback
        })
    }
    
    /// Create the component interface with resource.
    fn create_component_interface(
        &mut self,
        component: &LirComponent,
        resource_name: &str,
    ) -> Result<wit_parser::InterfaceId, CodegenError> {
        let interface_name = format!("{}-component", resource_name);
        
        // Create the resource type
        let resource_type_id = self.resolve.types.alloc(TypeDef {
            name: Some(resource_name.to_string()),
            kind: TypeDefKind::Resource,
            owner: TypeOwner::None,
            docs: Docs::default(),
            stability: Stability::default(),
        });
        
        // Create interface
        let interface_id = self.resolve.interfaces.alloc(Interface {
            name: Some(interface_name.clone()),
            docs: Docs::default(),
            types: Default::default(),
            functions: Default::default(),
            package: Some(self.package_id),
            stability: Stability::default(),
        });
        
        // Update resource owner
        self.resolve.types[resource_type_id].owner = TypeOwner::Interface(interface_id);
        
        // Add resource type to interface
        self.resolve.interfaces[interface_id]
            .types
            .insert(resource_name.to_string(), resource_type_id);
        
        // Add record, enum, and variant types to interface
        for (def_id, &type_id) in &self.adt_map {
            if let Some(record) = self.ctx.defs.as_record(*def_id) {
                let record_name = to_kebab_case(&self.ctx.str(record.name));
                self.resolve.types[type_id].owner = TypeOwner::Interface(interface_id);
                self.resolve.interfaces[interface_id]
                    .types
                    .insert(record_name, type_id);
            } else if let Some(enum_def) = self.ctx.defs.as_enum(*def_id) {
                let enum_name = to_kebab_case(&self.ctx.str(enum_def.name));
                self.resolve.types[type_id].owner = TypeOwner::Interface(interface_id);
                self.resolve.interfaces[interface_id]
                    .types
                    .insert(enum_name, type_id);
            } else if let Some(variant_def) = self.ctx.defs.as_variant(*def_id) {
                let variant_name = to_kebab_case(&self.ctx.str(variant_def.name));
                self.resolve.types[type_id].owner = TypeOwner::Interface(interface_id);
                self.resolve.interfaces[interface_id]
                    .types
                    .insert(variant_name, type_id);
            }
        }
        
        // Create own handle type for constructor return
        let own_type_id = self.resolve.types.alloc(TypeDef {
            name: None,
            kind: TypeDefKind::Handle(wit_parser::Handle::Own(resource_type_id)),
            owner: TypeOwner::Interface(interface_id),
            docs: Docs::default(),
            stability: Stability::default(),
        });

        // Add constructor - returns own<resource>
        let constructor_func = Function {
            name: String::new(), // Empty for constructors
            kind: FunctionKind::Constructor(resource_type_id),
            params: vec![],
            results: Results::Anon(Type::Id(own_type_id)), // Constructor returns own<resource>
            docs: Docs::default(),
            stability: Stability::default(),
        };
        self.resolve.interfaces[interface_id]
            .functions
            .insert(format!("[constructor]{}", resource_name), constructor_func);

        // Create borrow type for self parameter
        let borrow_type_id = self.resolve.types.alloc(TypeDef {
            name: None,
            kind: TypeDefKind::Handle(wit_parser::Handle::Borrow(resource_type_id)),
            owner: TypeOwner::Interface(interface_id),
            docs: Docs::default(),
            stability: Stability::default(),
        });
        let self_type = Type::Id(borrow_type_id);

        // Add mount method
        let mount_name = format!("[method]{}.mount", resource_name);
        let mount_func = Function {
            name: mount_name.clone(),
            kind: FunctionKind::Method(resource_type_id),
            params: vec![
                ("self".to_string(), self_type.clone()),
                ("root".to_string(), Type::U32),
            ],
            results: Results::Named(vec![]),
            docs: Docs::default(),
            stability: Stability::default(),
        };
        self.resolve.interfaces[interface_id]
            .functions
            .insert(mount_name, mount_func);

        // Add unmount method
        let unmount_name = format!("[method]{}.unmount", resource_name);
        let unmount_func = Function {
            name: unmount_name.clone(),
            kind: FunctionKind::Method(resource_type_id),
            params: vec![("self".to_string(), self_type.clone())],
            results: Results::Named(vec![]),
            docs: Docs::default(),
            stability: Stability::default(),
        };
        self.resolve.interfaces[interface_id]
            .functions
            .insert(unmount_name, unmount_func);

        // Add dispatch method
        let dispatch_name = format!("[method]{}.dispatch", resource_name);
        let dispatch_func = Function {
            name: dispatch_name.clone(),
            kind: FunctionKind::Method(resource_type_id),
            params: vec![
                ("self".to_string(), self_type.clone()),
                ("handler-id".to_string(), Type::U32),
            ],
            results: Results::Named(vec![]),
            docs: Docs::default(),
            stability: Stability::default(),
        };
        self.resolve.interfaces[interface_id]
            .functions
            .insert(dispatch_name, dispatch_func);
        
        // Add getter/setter for each signal
        for signal in &component.signals {
            let signal_name = to_kebab_case(&self.ctx.str(self.ctx.defs.name(signal.def_id)));
            let wit_type = self.ty_to_wit_type(signal.ty)?;

            // Getter
            let getter_name = format!("[method]{}.get-{}", resource_name, signal_name);
            let getter_func = Function {
                name: getter_name.clone(),
                kind: FunctionKind::Method(resource_type_id),
                params: vec![("self".to_string(), self_type.clone())],
                results: Results::Anon(wit_type.clone()),
                docs: Docs::default(),
                stability: Stability::default(),
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
                    ("self".to_string(), self_type.clone()),
                    ("value".to_string(), wit_type),
                ],
                results: Results::Named(vec![]),
                docs: Docs::default(),
                stability: Stability::default(),
            };
            self.resolve.interfaces[interface_id]
                .functions
                .insert(setter_name, setter_func);
        }
        
        // Register interface in package
        self.resolve.packages[self.package_id]
            .interfaces
            .insert(interface_name, interface_id);
        
        Ok(interface_id)
    }
    
    /// Create the DOM interface in a separate package (yel:ui@0.1.0).
    fn create_dom_interface(&mut self) -> Result<wit_parser::InterfaceId, CodegenError> {
        // Create a separate package for DOM (yel:ui@0.1.0) to match core module imports
        let dom_package_id = self.resolve.packages.alloc(Package {
            name: PackageName {
                namespace: "yel".to_string(),
                name: "ui".to_string(),
                version: Some(parse_version("0.1.0")),
            },
            docs: Docs::default(),
            interfaces: Default::default(),
            worlds: Default::default(),
        });

        let interface_id = self.resolve.interfaces.alloc(Interface {
            name: Some("dom".to_string()),
            docs: Docs::default(),
            types: Default::default(),
            functions: Default::default(),
            package: Some(dom_package_id),
            stability: Stability::default(),
        });

        // Create the attribute-value variant type for set-attribute
        // This allows the host to perform type-to-string conversion
        let attr_value_type_id = self.resolve.types.alloc(TypeDef {
            name: Some("attribute-value".to_string()),
            kind: TypeDefKind::Variant(wit_parser::Variant {
                cases: vec![
                    wit_parser::Case { name: "str".to_string(), ty: Some(Type::String), docs: Docs::default() },
                    wit_parser::Case { name: "bool".to_string(), ty: Some(Type::Bool), docs: Docs::default() },
                    wit_parser::Case { name: "s8".to_string(), ty: Some(Type::S8), docs: Docs::default() },
                    wit_parser::Case { name: "s16".to_string(), ty: Some(Type::S16), docs: Docs::default() },
                    wit_parser::Case { name: "s32".to_string(), ty: Some(Type::S32), docs: Docs::default() },
                    wit_parser::Case { name: "s64".to_string(), ty: Some(Type::S64), docs: Docs::default() },
                    wit_parser::Case { name: "u8".to_string(), ty: Some(Type::U8), docs: Docs::default() },
                    wit_parser::Case { name: "u16".to_string(), ty: Some(Type::U16), docs: Docs::default() },
                    wit_parser::Case { name: "u32".to_string(), ty: Some(Type::U32), docs: Docs::default() },
                    wit_parser::Case { name: "u64".to_string(), ty: Some(Type::U64), docs: Docs::default() },
                    wit_parser::Case { name: "f32".to_string(), ty: Some(Type::F32), docs: Docs::default() },
                    wit_parser::Case { name: "f64".to_string(), ty: Some(Type::F64), docs: Docs::default() },
                    wit_parser::Case { name: "char".to_string(), ty: Some(Type::Char), docs: Docs::default() },
                ],
            }),
            owner: TypeOwner::Interface(interface_id),
            docs: Docs::default(),
            stability: Stability::default(),
        });

        // Add the variant type to the interface
        self.resolve.interfaces[interface_id]
            .types
            .insert("attribute-value".to_string(), attr_value_type_id);

        let attr_value_type = Type::Id(attr_value_type_id);

        // Add DOM functions
        let dom_funcs: Vec<(&str, Vec<(&str, Type)>, Option<Type>)> = vec![
            ("create-element", vec![("tag", Type::String)], Some(Type::U32)),
            ("create-text", vec![("content", Type::String)], Some(Type::U32)),
            ("create-comment", vec![("content", Type::String)], Some(Type::U32)),
            ("set-attribute", vec![("node", Type::U32), ("name", Type::String), ("value", attr_value_type.clone())], None),
            ("remove-attribute", vec![("node", Type::U32), ("name", Type::String)], None),
            ("set-text-content", vec![("node", Type::U32), ("content", Type::String)], None),
            ("set-style", vec![("node", Type::U32), ("property", Type::String), ("value", Type::String)], None),
            ("set-class", vec![("node", Type::U32), ("class-name", Type::String)], None),
            ("append-child", vec![("parent", Type::U32), ("child", Type::U32)], None),
            ("insert-before", vec![("parent", Type::U32), ("node", Type::U32), ("reference", Type::U32)], None),
            ("insert-after", vec![("parent", Type::U32), ("node", Type::U32), ("anchor", Type::U32)], None),
            ("remove-child", vec![("parent", Type::U32), ("child", Type::U32)], None),
            ("remove", vec![("node", Type::U32)], None),
            ("get-parent", vec![("node", Type::U32)], Some(Type::U32)),
            ("get-next-sibling", vec![("node", Type::U32)], Some(Type::U32)),
            ("add-event-listener", vec![("node", Type::U32), ("event", Type::String), ("handler-id", Type::U32)], None),
            ("remove-event-listener", vec![("node", Type::U32), ("event", Type::String), ("handler-id", Type::U32)], None),
        ];

        for (name, params, result) in dom_funcs {
            let func = Function {
                name: name.to_string(),
                kind: FunctionKind::Freestanding,
                params: params.into_iter().map(|(n, t)| (n.to_string(), t)).collect(),
                results: match result {
                    Some(ty) => Results::Anon(ty),
                    None => Results::Named(vec![]),
                },
                docs: Docs::default(),
                stability: Stability::default(),
            };
            self.resolve.interfaces[interface_id]
                .functions
                .insert(name.to_string(), func);
        }

        self.resolve.packages[dom_package_id]
            .interfaces
            .insert("dom".to_string(), interface_id);

        Ok(interface_id)
    }

    /// Create the DOM events interface.
    fn create_dom_events_interface(&mut self) -> Result<wit_parser::InterfaceId, CodegenError> {
        let interface_id = self.resolve.interfaces.alloc(Interface {
            name: Some("dom-events".to_string()),
            docs: Docs::default(),
            types: Default::default(),
            functions: Default::default(),
            package: Some(self.package_id),
            stability: Stability::default(),
        });
        
        let dispatch_func = Function {
            name: "dispatch".to_string(),
            kind: FunctionKind::Freestanding,
            params: vec![("handler-id".to_string(), Type::U32)],
            results: Results::Named(vec![]),
            docs: Docs::default(),
            stability: Stability::default(),
        };
        
        self.resolve.interfaces[interface_id]
            .functions
            .insert("dispatch".to_string(), dispatch_func);
        
        self.resolve.packages[self.package_id]
            .interfaces
            .insert("dom-events".to_string(), interface_id);
        
        Ok(interface_id)
    }
    
    /// Create callbacks interface if needed.
    fn create_callbacks_interface(
        &mut self,
        component: &LirComponent,
        resource_name: &str,
    ) -> Result<Option<wit_parser::InterfaceId>, CodegenError> {
        // Get exported callbacks from component definition
        let callbacks: Vec<_> = self.ctx.defs.as_component(component.def_id)
            .map(|c| {
                c.callbacks
                    .iter()
                    .filter_map(|&def_id| {
                        let func_def = self.ctx.defs.as_function(def_id)?;
                        if func_def.is_export {
                            Some((def_id, to_kebab_case(&self.ctx.str(func_def.name))))
                        } else {
                            None
                        }
                    })
                    .collect()
            })
            .unwrap_or_default();
        
        if callbacks.is_empty() {
            return Ok(None);
        }
        
        let interface_name = format!("{}-callbacks", resource_name);
        let interface_id = self.resolve.interfaces.alloc(Interface {
            name: Some(interface_name.clone()),
            docs: Docs::default(),
            types: Default::default(),
            functions: Default::default(),
            package: Some(self.package_id),
            stability: Stability::default(),
        });
        
        for (_, cb_name) in callbacks {
            let func = Function {
                name: cb_name.clone(),
                kind: FunctionKind::Freestanding,
                params: vec![],
                results: Results::Named(vec![]),
                docs: Docs::default(),
                stability: Stability::default(),
            };
            self.resolve.interfaces[interface_id]
                .functions
                .insert(cb_name, func);
        }
        
        self.resolve.packages[self.package_id]
            .interfaces
            .insert(interface_name, interface_id);
        
        Ok(Some(interface_id))
    }
    
    /// Create the world.
    fn create_world(
        &mut self,
        resource_name: &str,
        dom_interface_id: wit_parser::InterfaceId,
        dom_events_interface_id: wit_parser::InterfaceId,
        component_interface_id: wit_parser::InterfaceId,
        callback_interface_id: Option<wit_parser::InterfaceId>,
    ) -> Result<WorldId, CodegenError> {
        let world_name = format!("{}-ui", resource_name);
        
        let world_id = self.resolve.worlds.alloc(World {
            name: world_name.clone(),
            docs: Docs::default(),
            imports: Default::default(),
            exports: Default::default(),
            includes: Default::default(),
            include_names: Default::default(),
            package: Some(self.package_id),
            stability: Stability::default(),
        });
        
        // Add imports
        self.resolve.worlds[world_id]
            .imports
            .insert(
                WorldKey::Interface(dom_interface_id),
                WorldItem::Interface { id: dom_interface_id, stability: Stability::default() },
            );
        
        if let Some(cb_id) = callback_interface_id {
            self.resolve.worlds[world_id]
                .imports
                .insert(
                    WorldKey::Interface(cb_id),
                    WorldItem::Interface { id: cb_id, stability: Stability::default() },
                );
        }
        
        // Add exports
        // Note: dom-events is NOT exported separately - the resource's dispatch method handles event dispatch
        // This avoids needing a separate freestanding dispatch function in the core module
        let _ = dom_events_interface_id; // Keep param to avoid breaking signature

        self.resolve.worlds[world_id]
            .exports
            .insert(
                WorldKey::Interface(component_interface_id),
                WorldItem::Interface { id: component_interface_id, stability: Stability::default() },
            );
        
        // Register world in package
        self.resolve.packages[self.package_id]
            .worlds
            .insert(world_name, world_id);
        
        Ok(world_id)
    }
    
    /// Get the resolve and world ID for embedding.
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

/// Convert a PascalCase or camelCase string to kebab-case.
fn to_kebab_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('-');
            }
            result.push(c.to_ascii_lowercase());
        } else if c == '_' {
            result.push('-');
        } else {
            result.push(c);
        }
    }
    result
}

/// Parse a version string into a semver::Version.
fn parse_version(version: &str) -> Version {
    Version::parse(version).unwrap_or_else(|_| Version::new(0, 1, 0))
}
