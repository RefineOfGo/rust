/// ROG Runtime Check Point switch.
/// Known to the relevant LLVM passes.
#[linkage = "weak_odr"]
#[unsafe(no_mangle)]
static mut rog_checkpoint_switch: i32 = 0;

/// ROG Runtime Check Point handler stub, the real implementation is in ROG runtime.
/// Known to the relevant LLVM passes.
///
/// This function does not take any arguments, but due to how AArch64 saves it's
/// return address, it's unsafe to implement this in pure Rust. See `stack.rs` for a
/// very detailed explanation.
///
/// On AArch64, the original return address will be saved to R17 as well.
#[no_gcwb]
#[no_split]
#[no_checkpoint]
#[linkage = "weak"]
#[unsafe(no_mangle)]
unsafe extern "rog-cold" fn rog_checkpoint_abi() {
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
