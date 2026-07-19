//! Runtime functions for WASM string operations and memory management.
//!
//! Provides runtime support for:
//! - Memory allocation (alloc, free, cabi_realloc with inline memory.copy)
//! - String concatenation (concat2, concat3, ...) - uses bulk memory.copy internally
//! - Type-to-string conversions (s32_to_string, bool_to_string, ...)

pub mod memory;
pub mod strings;

use std::collections::HashMap;
pub use memory::{emit_alloc, emit_allocator_globals, emit_cabi_realloc, emit_free, emit_store_fat_ptr, emit_load_fat_ptr, emit_pack_fat_ptr_to_i64, AllocatorGlobals};
pub use strings::{
    emit_bool_to_string, emit_concat_n, emit_f32_to_string, emit_s32_to_string,
    emit_s64_to_string, emit_starts_with, StringData,
};
use yel_core::{DefId, Ty};

/// Indices of runtime functions in the module.
///
/// These are LOCAL functions generated in the main module.
/// Memory management functions (memcpy, alloc, free, cabi_realloc) are now
/// imported from the allocator module - see ImportLayout for their indices.
/// Demand-driven flags. Set by a pre-emit scan of the LIR; passed to
/// [`RuntimeFunctions::new`] so unreferenced helpers neither claim a
/// function index nor get emitted into the code section.
///
/// All flags default to false. The scan that populates them MUST mirror
/// the actual emit-side trigger conditions exactly — every `Call(idx)`
/// the codegen issues to a runtime helper must correspond to a `true`
/// here, otherwise `RuntimeFunctions::new` returns `None` for the index
/// and `.unwrap()` at the call site will panic.
#[derive(Debug, Default, Clone, Copy)]
pub struct RuntimeNeeds {
    pub s32_to_string: bool,
    pub s64_to_string: bool,
    pub bool_to_string: bool,
    pub f32_to_string: bool,
    pub starts_with: bool,
    pub store_fat_ptr: bool,
    pub load_fat_ptr: bool,
    pub pack_fat_ptr_to_i64: bool,
}

#[derive(Debug, Clone)]
pub struct RuntimeFunctions {
    // String operations (locally generated). `None` means the pre-emit
    // scan determined no callsite uses this helper, so it's neither
    // assigned an index nor written into the code section.
    /// s32_to_string function index
    pub s32_to_string: Option<u32>,
    /// s64_to_string function index
    /// Also used for u64 interpolation (matches s32_to_string/u32 policy).
    pub s64_to_string: Option<u32>,
    /// bool_to_string function index
    pub bool_to_string: Option<u32>,
    /// f32_to_string function index
    pub f32_to_string: Option<u32>,
    /// Map of concat arity -> function index (concat2, concat3, etc.)
    pub concat_indices: std::collections::HashMap<usize, u32>,

    // Fat pointer operations
    /// store_fat_ptr function index: (addr, ptr, len) -> ()
    pub store_fat_ptr: Option<u32>,
    /// load_fat_ptr function index: (addr) -> (ptr, len)
    pub load_fat_ptr: Option<u32>,

    // String operations
    /// starts_with function index: (str_ptr, str_len, prefix_ptr, prefix_len) -> bool
    pub starts_with: Option<u32>,

    // Record constructor helpers
    /// Map of record DefId -> function index for $ctor_X (allocates and returns ptr)
    pub record_ctors: HashMap<DefId, u32>,
    /// Map of record DefId -> function index for $ctor_X_at (stores at given address)
    pub record_ctors_at: HashMap<DefId, u32>,

    // List constructor helpers
    /// Map of (element_type, count) -> function index for list_ctor_N_T
    /// Each list constructor takes element values as params and returns (ptr, len)
    pub list_ctors: HashMap<(Ty, usize), u32>,

    /// Map of list type -> function index for the per-list-Ty
    /// `list_append` helper. Signature:
    /// `(src: ref null $list_arr, elem: <storage-ty>) -> (ref null $list_arr)`.
    /// One function per unique `list<T>` referenced by an
    /// `append` call site.
    pub list_appends: HashMap<Ty, u32>,

    /// pack_fat_ptr_to_i64 function index: (ptr, len) -> i64
    /// Packs fat pointer (ptr, len) into canonical ABI i64 format: (ptr << 32) | len
    pub pack_fat_ptr_to_i64: Option<u32>,

    // Filter operations
    /// Map of filter_call_id -> function index for $filter_0, $filter_1, etc.
    /// Each filter function takes (src_ptr, src_len) and returns (result_ptr, result_len).
    pub filter_indices: HashMap<usize, u32>,

    /// Total count of runtime functions (local only, not imports)
    pub count: u32,
}

impl RuntimeFunctions {
    /// Create runtime function indices starting at `base`.
    ///
    /// `concat_arities` specifies which concat functions to generate (e.g., [2, 3, 4]).
    /// `record_types` specifies which record types need constructor helpers.
    /// `list_constructs` specifies which list constructors to generate (element_type, count).
    /// `filter_count` specifies how many filter functions to generate.
    /// Note: allocator functions are imported, not generated here.
    pub fn new(
        base: u32,
        needs: RuntimeNeeds,
        concat_arities: &[usize],
        record_types: &[DefId],
        list_constructs: &[(Ty, usize)],
        list_appends: &[Ty],
        filter_count: usize,
    ) -> Self {
        let mut idx = base;
        let alloc_if = |idx: &mut u32, cond: bool| -> Option<u32> {
            if cond {
                let v = *idx;
                *idx += 1;
                Some(v)
            } else {
                None
            }
        };

        // String operations (locally generated when referenced).
        let s32_to_string = alloc_if(&mut idx, needs.s32_to_string);
        let s64_to_string = alloc_if(&mut idx, needs.s64_to_string);
        let bool_to_string = alloc_if(&mut idx, needs.bool_to_string);
        let f32_to_string = alloc_if(&mut idx, needs.f32_to_string);

        let mut concat_indices = std::collections::HashMap::new();
        for &arity in concat_arities {
            concat_indices.insert(arity, idx);
            idx += 1;
        }

        // Fat pointer helpers.
        let store_fat_ptr = alloc_if(&mut idx, needs.store_fat_ptr);
        let load_fat_ptr = alloc_if(&mut idx, needs.load_fat_ptr);

        // String comparison.
        let starts_with = alloc_if(&mut idx, needs.starts_with);

        // Record constructor helpers
        // For each record type, we generate two functions:
        // - $ctor_X_at(dest, ...fields) -> () - stores at given address
        // - $ctor_X(...fields) -> ptr - allocates and returns ptr
        let mut record_ctors_at = std::collections::HashMap::new();
        let mut record_ctors = std::collections::HashMap::new();
        for &def_id in record_types {
            record_ctors_at.insert(def_id, idx);
            idx += 1;
            record_ctors.insert(def_id, idx);
            idx += 1;
        }

        // List constructor helpers
        // For each (element_type, count) pair, generate a function:
        // - list_ctor_N_T(...element_values...) -> (ptr, len)
        let mut list_ctors = std::collections::HashMap::new();
        for &(elem_ty, count) in list_constructs {
            list_ctors.insert((elem_ty, count), idx);
            idx += 1;
        }

        // List append helpers (one per unique list type).
        let mut list_appends_map = std::collections::HashMap::new();
        for &list_ty in list_appends {
            list_appends_map.insert(list_ty, idx);
            idx += 1;
        }

        // Fat pointer packing helper.
        let pack_fat_ptr_to_i64 = alloc_if(&mut idx, needs.pack_fat_ptr_to_i64);

        // Filter functions
        // For each filter call site, generate a specialized filter function:
        // - $filter_N(src_ptr, src_len) -> (result_ptr, result_len)
        let mut filter_indices = std::collections::HashMap::new();
        for filter_id in 0..filter_count {
            filter_indices.insert(filter_id, idx);
            idx += 1;
        }

        Self {
            s32_to_string,
            s64_to_string,
            bool_to_string,
            f32_to_string,
            concat_indices,
            store_fat_ptr,
            load_fat_ptr,
            starts_with,
            record_ctors,
            record_ctors_at,
            list_ctors,
            list_appends: list_appends_map,
            pack_fat_ptr_to_i64,
            filter_indices,
            count: idx - base,
        }
    }

    /// Get the function index for concat with the given arity.
    pub fn concat(&self, arity: usize) -> Option<u32> {
        self.concat_indices.get(&arity).copied()
    }

    /// Get the function index for record constructor at address.
    pub fn record_ctor_at(&self, def_id: DefId) -> Option<u32> {
        self.record_ctors_at.get(&def_id).copied()
    }

    /// Get the function index for record constructor (allocating).
    pub fn record_ctor(&self, def_id: DefId) -> Option<u32> {
        self.record_ctors.get(&def_id).copied()
    }

    /// Get the function index for list constructor.
    pub fn list_ctor(&self, elem_ty: Ty, count: usize) -> Option<u32> {
        self.list_ctors.get(&(elem_ty, count)).copied()
    }

    /// Get the function index for the per-list-Ty append helper.
    pub fn list_append(&self, list_ty: Ty) -> Option<u32> {
        self.list_appends.get(&list_ty).copied()
    }

    /// Get the function index for filter with the given call ID.
    pub fn filter(&self, filter_id: usize) -> Option<u32> {
        self.filter_indices.get(&filter_id).copied()
    }
}

