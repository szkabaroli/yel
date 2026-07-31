//! The compilation's structure: packages, modules, and which file belongs
//! where.
//!
//! Ported from the model ark's `program_parser_alt.rs` imports and never
//! defines — `PackageDefinition`, `ModuleDefinition`, `PackageName` come from
//! **dora** (`dora-lang/dora`, ark's ancestor), where `Sema` carries arenas
//! of definition structs and every source file knows its package and module.
//! This file completes that transplant for yel
//! (`plans/rewrite/definition-arenas.md`, step 1).
//!
//! Deviations, forced only:
//!
//! - dora's `PackageName::{Stdlib, Boots, Program, External}` loses `Boots`
//!   (yel has no boot-compiler package) and `Stdlib` is plural here — yel
//!   ships one std *module per file* (`std:num`, `std:list`), each its own
//!   package row.
//! - `ModuleDefinition.table` is not an `OnceCell<Rc<SymTable>>`: yel's
//!   per-package declaration tables live in [`Definitions`] until step 2
//!   dissolves them, so the module row carries the binding name and package
//!   only.

use yelc_base::{Name, SourceId};

use crate::ids::PackageId;

/// Why a package is part of the compilation — dora's enum, minus what yel
/// does not have.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PackageRole {
    /// A compiler-shipped std module (`std:num`), from the embedded registry.
    Std(Name),
    /// The package being compiled.
    Program,
    /// A dependency loaded through `--include` (`from "geometry" …`).
    External(Name),
}

/// One package of the compilation.
#[derive(Debug)]
pub struct PackageDefinition {
    pub id: PackageId,
    pub name: PackageRole,
    /// The package's root module. Every package has exactly one today —
    /// nesting arrives with `module M { }` — so this is total, not optional.
    pub top_level_module_id: ModuleDefinitionId,
}

/// Identifies one module row.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ModuleDefinitionId(pub u32);

impl ModuleDefinitionId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// One module of the compilation.
#[derive(Debug)]
pub struct ModuleDefinition {
    pub id: ModuleDefinitionId,
    pub package_id: PackageId,
    /// `None` for a package's top-level module; nesting arrives with
    /// `module M { }`.
    pub parent_module_id: Option<ModuleDefinitionId>,
    /// The name the module is reachable by in the *including* scope —
    /// `Geo` in `from "geometry" include Geo;`. `None` for the program's own
    /// top-level module, which nothing needs to name.
    pub name: Option<Name>,
}

/// The arenas, plus which file belongs to which package and module.
#[derive(Debug, Default)]
pub struct Compilation {
    packages: Vec<PackageDefinition>,
    modules: Vec<ModuleDefinition>,
    /// `SourceId → (package, module)` in file-registration order. A `Vec`
    /// rather than a map: `SourceId`s are dense and the driver registers
    /// every file it reads.
    file_assignments: Vec<(SourceId, PackageId, ModuleDefinitionId)>,
}

impl Compilation {
    pub fn new() -> Compilation {
        Compilation::default()
    }

    /// Register a package with its top-level module — dora's `add_package`.
    pub fn add_package(
        &mut self,
        id: PackageId,
        name: PackageRole,
        module_name: Option<Name>,
    ) -> ModuleDefinitionId {
        let module_id = ModuleDefinitionId(self.modules.len() as u32);
        self.modules.push(ModuleDefinition {
            id: module_id,
            package_id: id,
            parent_module_id: None,
            name: module_name,
        });
        self.packages.push(PackageDefinition {
            id,
            name,
            top_level_module_id: module_id,
        });
        module_id
    }

    /// Record which package and module a file belongs to.
    pub fn assign_file(&mut self, file: SourceId, package: PackageId, module: ModuleDefinitionId) {
        self.file_assignments.push((file, package, module));
    }

    pub fn package(&self, id: PackageId) -> Option<&PackageDefinition> {
        self.packages.iter().find(|package| package.id == id)
    }

    pub fn module(&self, id: ModuleDefinitionId) -> &ModuleDefinition {
        &self.modules[id.index()]
    }

    /// Every package, in registration order (the program first, then loads).
    pub fn packages(&self) -> impl Iterator<Item = &PackageDefinition> {
        self.packages.iter()
    }

    pub fn modules(&self) -> impl Iterator<Item = &ModuleDefinition> {
        self.modules.iter()
    }

    /// The package and module a file was assigned to.
    pub fn file_assignment(&self, file: SourceId) -> Option<(PackageId, ModuleDefinitionId)> {
        self.file_assignments
            .iter()
            .find(|(candidate, _, _)| *candidate == file)
            .map(|(_, package, module)| (*package, *module))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use yelc_base::Interner;

    #[test]
    fn a_package_row_carries_its_top_level_module() {
        let interner = Interner::new();
        let mut compilation = Compilation::new();
        let program = compilation.add_package(PackageId::LOCAL, PackageRole::Program, None);
        let geo = compilation.add_package(
            PackageId(1),
            PackageRole::External(interner.intern("geometry")),
            Some(interner.intern("Geo")),
        );

        assert_eq!(
            compilation
                .package(PackageId::LOCAL)
                .unwrap()
                .top_level_module_id,
            program
        );
        assert_eq!(
            compilation
                .package(PackageId(1))
                .unwrap()
                .top_level_module_id,
            geo
        );
        assert_eq!(compilation.module(geo).name, Some(interner.intern("Geo")));
        assert_eq!(compilation.module(program).name, None);
    }

    #[test]
    fn file_assignments_answer_where_a_file_belongs() {
        let mut compilation = Compilation::new();
        let program = compilation.add_package(PackageId::LOCAL, PackageRole::Program, None);
        compilation.assign_file(SourceId(0), PackageId::LOCAL, program);

        assert_eq!(
            compilation.file_assignment(SourceId(0)),
            Some((PackageId::LOCAL, program))
        );
        assert_eq!(compilation.file_assignment(SourceId(9)), None);
    }
}
