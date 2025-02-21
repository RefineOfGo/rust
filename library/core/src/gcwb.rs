use crate::ptr::{addr_of, addr_of_mut};

/// ROG GC Write Barrier switch.
/// Known to the relevant LLVM passes.
#[linkage = "weak_odr"]
#[no_mangle]
static mut rog_gcwb_switch: i32 = 0;

/// ROG GC Write Barrier stub, the real implementation is in ROG runtime.
/// Known to the relevant LLVM passes.
#[no_gcwb]
#[no_split]
#[no_mangle]
#[no_checkpoint]
#[linkage = "weak"]
extern "rog-cold" fn rog_write_barrier(_slot: usize, _ptr: usize) {
    crate::intrinsics::abort();
}

/// ROG GC Bulk Write Barrier stub, the real implementation is in ROG runtime.
/// Known to the relevant LLVM passes.
#[no_gcwb]
#[no_split]
#[no_mangle]
#[no_checkpoint]
#[linkage = "weak"]
extern "rog-cold" fn rog_bulk_write_barrier(_dest: usize, _src: usize, _size: usize) {
    crate::intrinsics::abort();
}

/// Enable ROG write barrier.
#[no_gcwb]
#[no_split]
#[no_checkpoint]
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub unsafe fn enable() {
    unsafe {
        crate::intrinsics::atomic_store_seqcst(addr_of_mut!(rog_gcwb_switch), 1);
    }
}

/// Disable ROG write barrier.
#[no_gcwb]
#[no_split]
#[no_checkpoint]
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub unsafe fn disable() {
    unsafe {
        crate::intrinsics::atomic_store_seqcst(addr_of_mut!(rog_gcwb_switch), 0);
    }
}

/// Check if ROG write barrier was enabled.
#[no_gcwb]
#[no_split]
#[no_checkpoint]
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub fn is_enabled() -> bool {
    unsafe { crate::intrinsics::atomic_load_seqcst(addr_of!(rog_gcwb_switch)) != 0 }
}
