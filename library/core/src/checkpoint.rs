use crate::ptr::{addr_of, addr_of_mut};

/// ROG Runtime Check Point switch.
/// Known to the relevant LLVM passes.
#[no_mangle]
static mut rog_checkpoint_switch: u32 = 0;

/// ROG Runtime Check Point handler stub, the real implementation is in ROG runtime.
/// Known to the relevant LLVM passes.
#[no_gcwb]
#[no_split]
#[no_mangle]
#[no_checkpoint]
#[linkage = "linkonce"]
extern "C" fn rog_checkpoint_abi() {
    crate::intrinsics::abort();
}

/// Prevent compiler from removing it as dead-code.
#[used]
static _X: extern "C" fn() = rog_checkpoint_abi;

/// Enable ROG runtime check-point.
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub unsafe fn enable() {
    unsafe {
        crate::intrinsics::atomic_store_seqcst(addr_of_mut!(rog_checkpoint_switch), 1u32);
    }
}

/// Disable ROG runtime check-point.
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub unsafe fn disable() {
    unsafe {
        crate::intrinsics::atomic_store_seqcst(addr_of_mut!(rog_checkpoint_switch), 0u32);
    }
}

/// Check if ROG runtime check-point was enabled.
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub fn is_enabled() -> bool {
    unsafe { crate::intrinsics::atomic_load_seqcst(addr_of!(rog_checkpoint_switch)) != 0u32 }
}
