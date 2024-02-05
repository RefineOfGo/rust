use crate::ptr::{addr_of, addr_of_mut};

/// ROG GC Write Barrier switch.
/// Known to the relevant LLVM passes.
#[no_mangle]
static mut rog_gcwb_switch: u32 = 0;

/// ROG GC Write Barrier stub, the real implementation is in ROG runtime.
/// Known to the relevant LLVM passes.
#[no_gcwb]
#[no_split]
#[no_mangle]
#[no_checkpoint]
#[linkage = "linkonce"]
extern "rog-cold" fn rog_write_barrier(_slot: usize, _ptr: usize) {
    crate::intrinsics::abort();
}

/// ROG GC Bulk Write Barrier stub, the real implementation is in ROG runtime.
/// Known to the relevant LLVM passes.
#[no_gcwb]
#[no_split]
#[no_mangle]
#[no_checkpoint]
#[linkage = "linkonce"]
extern "rog-cold" fn rog_bulk_write_barrier(_dest: usize, _src: usize, _size: usize) {
    crate::intrinsics::abort();
}

/// Prevent compiler from removing it as dead-code.
#[used]
static _X: extern "rog-cold" fn(usize, usize) = rog_write_barrier;

/// Prevent compiler from removing it as dead-code.
#[used]
static _Y: extern "rog-cold" fn(usize, usize, usize) = rog_bulk_write_barrier;

/// Enable ROG write barrier.
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub unsafe fn enable() {
    unsafe {
        crate::intrinsics::atomic_store_seqcst(addr_of_mut!(rog_gcwb_switch), 1u32);
    }
}

/// Disable ROG write barrier.
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub unsafe fn disable() {
    unsafe {
        crate::intrinsics::atomic_store_seqcst(addr_of_mut!(rog_gcwb_switch), 0u32);
    }
}

/// Check if ROG write barrier was enabled.
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub fn is_enabled() -> bool {
    unsafe { crate::intrinsics::atomic_load_seqcst(addr_of!(rog_gcwb_switch)) != 0u32 }
}
