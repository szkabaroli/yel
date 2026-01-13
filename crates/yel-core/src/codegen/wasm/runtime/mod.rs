//! Runtime functions for WASM string operations and memory management.
//!
//! Provides runtime support for:
//! - Memory allocation (alloc, free, cabi_realloc with inline memory.copy)
//! - String concatenation (concat2, concat3, ...) - uses bulk memory.copy internally
//! - Type-to-string conversions (s32_to_string, bool_to_string, ...)
//! - List operations (list_get, list_get_opt)

pub mod list;
pub mod memory;
pub mod strings;

pub use list::{emit_list_get, emit_list_get_opt};
pub use memory::{emit_alloc, emit_allocator_globals, emit_cabi_realloc, emit_free, emit_store_fat_ptr, emit_load_fat_ptr, emit_pack_fat_ptr_to_i64, AllocatorGlobals};
pub use strings::{emit_bool_to_string, emit_concat_n, emit_f32_to_string, emit_s32_to_string, StringData};

/// Indices of runtime functions in the module.
///
/// These are LOCAL functions generated in the main module.
/// Memory management functions (memcpy, alloc, free, cabi_realloc) are now
/// imported from the allocator module - see ImportLayout for their indices.
#[derive(Debug, Clone)]
pub struct RuntimeFunctions {
    // String operations (locally generated)
    /// s32_to_string function index
    pub s32_to_string: u32,
    /// bool_to_string function index
    pub bool_to_string: u32,
    /// f32_to_string function index
    pub f32_to_string: u32,
    /// Map of concat arity -> function index (concat2, concat3, etc.)
    pub concat_indices: std::collections::HashMap<usize, u32>,

    // Fat pointer operations
    /// store_fat_ptr function index: (addr, ptr, len) -> ()
    pub store_fat_ptr: u32,
    /// load_fat_ptr function index: (addr) -> (ptr, len)
    pub load_fat_ptr: u32,

    // List operations
    /// list_get function index: (ptr, len, idx, elem_size) -> elem_ptr
    /// Traps on out-of-bounds access.
    pub list_get: u32,
    /// list_get_opt function index: (ptr, len, idx, elem_size) -> (is_some, elem_ptr)
    /// Returns (0, 0) on out-of-bounds, (1, elem_ptr) on success.
    pub list_get_opt: u32,

    // Record constructor helpers
    /// Map of record DefId -> function index for $ctor_X (allocates and returns ptr)
    pub record_ctors: std::collections::HashMap<crate::ids::DefId, u32>,
    /// Map of record DefId -> function index for $ctor_X_at (stores at given address)
    pub record_ctors_at: std::collections::HashMap<crate::ids::DefId, u32>,

    // List constructor helpers
    /// Map of (element_type, count) -> function index for list_ctor_N_T
    /// Each list constructor takes element values as params and returns (ptr, len)
    pub list_ctors: std::collections::HashMap<(crate::types::Ty, usize), u32>,

    /// pack_fat_ptr_to_i64 function index: (ptr, len) -> i64
    /// Packs fat pointer (ptr, len) into canonical ABI i64 format: (ptr << 32) | len
    pub pack_fat_ptr_to_i64: u32,

    /// Total count of runtime functions (local only, not imports)
    pub count: u32,
}

impl RuntimeFunctions {
    /// Create runtime function indices starting at `base`.
    ///
    /// `concat_arities` specifies which concat functions to generate (e.g., [2, 3, 4]).
    /// `record_types` specifies which record types need constructor helpers.
    /// `list_constructs` specifies which list constructors to generate (element_type, count).
    /// Note: allocator functions are imported, not generated here.
    pub fn new(
        base: u32,
        concat_arities: &[usize],
        record_types: &[crate::ids::DefId],
        list_constructs: &[(crate::types::Ty, usize)],
    ) -> Self {
        let mut idx = base;

        // String operations (these are locally generated)
        let s32_to_string = idx;
        idx += 1;

        let bool_to_string = idx;
        idx += 1;

        let f32_to_string = idx;
        idx += 1;

        let mut concat_indices = std::collections::HashMap::new();
        for &arity in concat_arities {
            concat_indices.insert(arity, idx);
            idx += 1;
        }

        // Fat pointer helpers
        let store_fat_ptr = idx;
        idx += 1;

        let load_fat_ptr = idx;
        idx += 1;

        // List operations
        let list_get = idx;
        idx += 1;

        let list_get_opt = idx;
        idx += 1;

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

        // Fat pointer packing helper
        let pack_fat_ptr_to_i64 = idx;
        idx += 1;

        Self {
            s32_to_string,
            bool_to_string,
            f32_to_string,
            concat_indices,
            store_fat_ptr,
            load_fat_ptr,
            list_get,
            list_get_opt,
            record_ctors,
            record_ctors_at,
            list_ctors,
            pack_fat_ptr_to_i64,
            count: idx - base,
        }
    }

    /// Get the function index for concat with the given arity.
    pub fn concat(&self, arity: usize) -> Option<u32> {
        self.concat_indices.get(&arity).copied()
    }

    /// Get the function index for record constructor at address.
    pub fn record_ctor_at(&self, def_id: crate::ids::DefId) -> Option<u32> {
        self.record_ctors_at.get(&def_id).copied()
    }

    /// Get the function index for record constructor (allocating).
    pub fn record_ctor(&self, def_id: crate::ids::DefId) -> Option<u32> {
        self.record_ctors.get(&def_id).copied()
    }

    /// Get the function index for list constructor.
    pub fn list_ctor(&self, elem_ty: crate::types::Ty, count: usize) -> Option<u32> {
        self.list_ctors.get(&(elem_ty, count)).copied()
    }
}

/// Type indices for runtime function signatures.
pub mod types {
    /// () -> ()
    pub const VOID_VOID: u32 = 0;
    /// (i32) -> ()
    pub const I32_VOID: u32 = 1;
    /// () -> i32
    pub const VOID_I32: u32 = 2;
    /// (i32, i32) -> ()
    pub const I32_I32_VOID: u32 = 3;
    /// (i32) -> i32
    pub const I32_I32: u32 = 4;
    /// (i32, i32) -> i32
    pub const I32_I32_I32: u32 = 5;
    /// (i32, i32, i32) -> ()
    pub const I32_I32_I32_VOID: u32 = 6;
    /// (i32, i32, i32, i32, i32) -> ()
    pub const I32X5_VOID: u32 = 7;
    /// (i32, i32, i32, i32) -> ()
    pub const I32X4_VOID: u32 = 8;
    /// (i32, i32, i32, i32) -> i32
    pub const I32X4_I32: u32 = 9;

    // Extended types for runtime functions
    /// (i32) -> (i32, i32) - s32_to_string, bool_to_string
    pub const I32_TO_PTR_LEN: u32 = 16;
    /// (f32) -> (i32, i32) - f32_to_string
    pub const F32_TO_PTR_LEN: u32 = 35;
    /// (i32, i32, i32) -> () - memcpy (dst, src, len)
    pub const MEMCPY: u32 = 6; // Same as I32_I32_I32_VOID
    /// (i32, i32, i32, i32) -> (i32, i32) - concat2
    pub const CONCAT2: u32 = 17;
    /// (i32, i32, i32, i32, i32, i32) -> (i32, i32) - concat3
    pub const CONCAT3: u32 = 18;
    /// (i32, i32, i32, i32, i32, i32, i32, i32) -> (i32, i32) - concat4
    pub const CONCAT4: u32 = 19;

    // Allocator function types (aliases for clarity)
    /// (i32, i32) -> i32 - alloc(size, align) -> ptr
    pub const ALLOC: u32 = I32_I32_I32;
    /// (i32, i32) -> () - free(ptr, size)
    pub const FREE: u32 = I32_I32_VOID;
    /// (i32, i32, i32, i32) -> i32 - cabi_realloc(old_ptr, old_size, align, new_size) -> ptr
    pub const CABI_REALLOC: u32 = I32X4_I32;

    // List operation types
    /// (i32, i32, i32, i32) -> i32 - list_get(ptr, len, idx, elem_size) -> elem_ptr
    pub const LIST_GET: u32 = I32X4_I32;
    /// (i32, i32, i32, i32) -> (i32, i32) - list_get_opt function signature
    /// Same signature as CONCAT2, so reuse that type index
    pub const LIST_GET_OPT: u32 = CONCAT2;
    /// () -> (i32, i32) - for if block results in list_get_opt
    pub const VOID_I32_I32: u32 = 26;
    /// (i32, i32) -> (i32, i32) - for list_ctor with 2 params
    pub const I32_I32_TO_I32_I32: u32 = 27;

    // List constructor types for odd parameter counts
    /// (i32 x 3) -> (i32, i32) - for list_ctor with 3 params
    pub const I32X3_TO_I32_I32: u32 = 28;
    /// (i32 x 5) -> (i32, i32) - for list_ctor with 5 params
    pub const I32X5_TO_I32_I32: u32 = 29;
    /// (i32 x 7) -> (i32, i32) - for list_ctor with 7 params
    pub const I32X7_TO_I32_I32: u32 = 30;
    /// (i32 x 9) -> (i32, i32) - for list_ctor with 9 params
    pub const I32X9_TO_I32_I32: u32 = 31;
    /// (i32 x 11) -> (i32, i32) - for list_ctor with 11 params
    pub const I32X11_TO_I32_I32: u32 = 32;
    /// (i32 x 13) -> (i32, i32) - for list_ctor with 13 params
    pub const I32X13_TO_I32_I32: u32 = 33;
    /// (i32 x 15) -> (i32, i32) - for list_ctor with 15 params
    pub const I32X15_TO_I32_I32: u32 = 34;
}
