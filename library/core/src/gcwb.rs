use crate::intrinsics::{AtomicOrdering, abort, atomic_load, atomic_store};

/// ROG GC Write Barrier switch.
/// Known to the relevant LLVM passes.
#[linkage = "weak_odr"]
#[unsafe(no_mangle)]
static mut rog_gcwb_switch: i32 = 0;

/// ROG GC Write Barrier stub, the real implementation is in ROG runtime.
/// Known to the relevant LLVM passes.
#[no_gcwb]
#[no_split]
#[no_checkpoint]
#[linkage = "weak"]
#[unsafe(no_mangle)]
unsafe extern "rog-cold" fn rog_write_barrier(_slot: usize, _ptr: usize) {
    abort();
}

/// ROG GC Bulk Write Barrier stub, the real implementation is in ROG runtime.
/// Known to the relevant LLVM passes.
#[no_gcwb]
#[no_split]
#[no_checkpoint]
#[linkage = "weak"]
#[unsafe(no_mangle)]
unsafe extern "rog-cold" fn rog_bulk_write_barrier(_dest: usize, _src: usize, _size: usize) {
    abort();
}

/// Enable ROG write barrier.
#[no_gcwb]
#[no_split]
#[no_checkpoint]
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub unsafe fn enable() {
    unsafe { atomic_store::<i32, { AtomicOrdering::SeqCst }>(&raw mut rog_gcwb_switch, 1) }
}

/// Disable ROG write barrier.
#[no_gcwb]
#[no_split]
#[no_checkpoint]
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub unsafe fn disable() {
    unsafe { atomic_store::<i32, { AtomicOrdering::SeqCst }>(&raw mut rog_gcwb_switch, 0) }
}

/// Check if ROG write barrier was enabled.
#[no_gcwb]
#[no_split]
#[no_checkpoint]
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub fn is_enabled() -> bool {
    unsafe { atomic_load::<i32, { AtomicOrdering::SeqCst }>(&raw const rog_gcwb_switch) != 0 }
}
