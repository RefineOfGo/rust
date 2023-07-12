/// ROG GC Write Barrier switch.
/// Known to the relative LLVM passes.
#[no_mangle]
#[linkage = "weak"]
static mut rog_gcwb_switch: u32 = 0;

/// ROG GC Write Barrier stub, implemented in assembly.
/// Known to the relative LLVM passes.
#[no_mangle]
#[linkage = "weak"]
extern "rust-cold" fn rog_write_barrier(_: &usize, _: usize) {
    unreachable!();
}
