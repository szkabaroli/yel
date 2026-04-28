//! Module-level constants used by the core-module emitter.

/// Encoded handler-id layout: `(handle << HANDLER_ID_HANDLE_SHIFT) |
/// local_id`. `local_id` is a per-component AddEventListener ordinal
/// (low bits); `handle` is the host registry index (high bits). The
/// dispatch function decodes by masking with `HANDLER_ID_LOCAL_MASK`
/// for the ordinal and shifting right by `HANDLER_ID_HANDLE_SHIFT` for
/// the handle. `MAX_HANDLERS_PER_COMPONENT` caps how many distinct
/// listener sites a single component may emit.
pub(super) const HANDLER_ID_HANDLE_SHIFT: i32 = 16;
pub(super) const HANDLER_ID_LOCAL_MASK: i32 = 0xFFFF;
pub(super) const MAX_HANDLERS_PER_COMPONENT: u32 = 0x1_0000;
