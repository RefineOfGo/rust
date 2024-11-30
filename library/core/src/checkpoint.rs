/// ROG Runtime Check Point switch.
/// Known to the relevant LLVM passes.
#[linkage = "weak_odr"]
#[no_mangle]
static mut rog_checkpoint_switch: i32 = 0;

/// ROG Runtime Check Point handler stub, the real implementation is in ROG runtime.
/// Known to the relevant LLVM passes.
#[no_gcwb]
#[no_split]
#[no_mangle]
#[no_checkpoint]
#[linkage = "weak"]
extern "rog-cold" fn rog_checkpoint_abi() {
    crate::intrinsics::abort();
}

/// Enable ROG runtime check-point.
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub unsafe fn enable() {
    unsafe { crate::intrinsics::atomic_store_seqcst(&raw mut rog_checkpoint_switch, 1) }
}

/// Disable ROG runtime check-point.
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub unsafe fn disable() {
    unsafe { crate::intrinsics::atomic_store_seqcst(&raw mut rog_checkpoint_switch, 0) }
}

/// Check if ROG runtime check-point was enabled.
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub fn is_enabled() -> bool {
    unsafe { crate::intrinsics::atomic_load_seqcst(&raw const rog_checkpoint_switch) != 0 }
}
