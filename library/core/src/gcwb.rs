/// ROG GC Write Barrier switch.
/// Known to the relevant LLVM passes.
#[no_mangle]
#[linkage = "weak"]
static mut rog_gcwb_switch: u32 = 0;

/// ROG GC Write Barrier stub, implemented in ROG runtime.
/// Known to the relevant LLVM passes.
#[no_gcwb]
#[no_split]
#[no_mangle]
#[linkage = "weak"]
extern "rust-cold" fn rog_write_barrier(_slot: &usize, _ptr: usize) {
    unreachable!();
}

/// ROG GC Bulk Write Barrier stub, implemented in ROG runtime.
/// Known to the relevant LLVM passes.
#[no_gcwb]
#[no_split]
#[no_mangle]
#[linkage = "weak"]
extern "rust-cold" fn rog_bulk_write_barrier(_dest: usize, _src: usize, _size: usize) {
    unreachable!();
}
