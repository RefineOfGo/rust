/// ROG GC Write Barrier switch.
/// Known to the relevant LLVM passes.
#[no_mangle]
static mut rog_gcwb_switch: u32 = 0;

#[allow(dead_code)]
extern "rust-cold" {
    /// ROG GC Write Barrier stub, implemented in ROG runtime.
    /// Known to the relevant LLVM passes.
    fn rog_write_barrier(_slot: &usize, _ptr: usize);

    /// ROG GC Bulk Write Barrier stub, implemented in ROG runtime.
    /// Known to the relevant LLVM passes.
    fn rog_bulk_write_barrier(_dest: usize, _src: usize, _size: usize);
}

/// Enable ROG write barrier.
#[no_gcwb]
#[no_split]
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub unsafe fn enable() {
    unsafe {
        core::intrinsics::atomic_store_seqcst(&mut rog_gcwb_switch as *mut _, 1u32);
    }
}

/// Disable ROG write barrier.
#[no_gcwb]
#[no_split]
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub unsafe fn disable() {
    unsafe {
        core::intrinsics::atomic_store_seqcst(&mut rog_gcwb_switch as *mut _, 0u32);
    }
}

/// Check if ROG write barrier was enabled.
#[no_gcwb]
#[no_split]
#[inline(always)]
#[stable(feature = "rog", since = "1.0.0")]
pub fn is_enabled() -> bool {
    unsafe { core::intrinsics::atomic_load_seqcst(&rog_gcwb_switch as *const _) != 0u32 }
}
