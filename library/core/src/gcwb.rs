/// ROG GC Write Barrier switch.
/// Known to the relative LLVM passes.
#[no_mangle]
#[linkage = "weak"]
static mut rog_gcwb_enabled: u32 = 0;

/// ROG GC Write Barrier stub (uses C ABI, for stage0 bootstrap only).
/// Known to the relative LLVM passes.
#[no_mangle]
#[cfg(bootstrap)]
extern "C" fn rog_gcwb_store_ptr(_: *mut usize, _: usize) {
    unreachable!();
}

/// ROG GC Write Barrier stub, implemented in assembly.
/// Known to the relative LLVM passes.
#[no_mangle]
#[linkage = "weak"]
#[cfg(not(bootstrap))]
extern "rust-cold" fn rog_gcwb_store_ptr(_: *mut usize, _: usize) {
    unreachable!();
}
