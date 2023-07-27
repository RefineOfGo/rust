use crate::arch::asm;

/// ROG Stack Growing stub, should be implemented in ROG runtime.
/// Known to the relative LLVM passes.
#[no_gcwb]
#[no_split]
#[no_mangle]
#[linkage = "weak"]
extern "rust-cold" fn rog_morestack_abi() {
    unreachable!();
}

/// Get the stack limit of the current rog-routine.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub fn get_stack_limit() -> usize {
    unsafe {
        let mut limit: usize;
        asm!("mov {limit}, fs:rog_stack_limit@tpoff", limit = out(reg) limit);
        limit
    }
}

/// Get the stack limit of the current rog-routine.
#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub fn get_stack_limit() -> usize {
    unsafe {
        let mut limit: usize;
        asm!(
            "mrs {temp}, tpidr_el0",
            "add {temp}, {temp}, :tprel_hi12:rog_stack_limit",
            "ldr {limit}, [{temp}, :tprel_lo12_nc:rog_stack_limit]",
            temp = out(reg) _,
            limit = out(reg) limit
        );
        limit
    }
}

/// Get the stack limit of the current rog-routine.
#[cfg(all(target_os = "macos", target_arch = "x86_64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub fn get_stack_limit() -> usize {
    unsafe {
        let mut limit: usize;
        asm!("mov {limit}, gs:0x30", limit = out(reg) limit);
        limit
    }
}

/// Get the stack limit of the current rog-routine.
#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub fn get_stack_limit() -> usize {
    unsafe {
        let mut tls: *const usize;
        asm!("mrs {tls}, tpidrro_el0", tls = out(reg) tls);
        *tls.offset(6)
    }
}

/// Set the stack limit of the current rog-routine to a new value.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub unsafe fn set_stack_limit(limit: usize) {
    unsafe {
        asm!("mov fs:rog_stack_limit@tpoff, {limit}", limit = in(reg) limit);
    }
}

/// Set the stack limit of the current rog-routine to a new value.
#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub unsafe fn set_stack_limit(limit: usize) {
    unsafe {
        asm!(
            "mrs {temp}, tpidr_el0",
            "add {temp}, {temp}, :tprel_hi12:rog_stack_limit",
            "str {limit}, [{temp}, :tprel_lo12_nc:rog_stack_limit]",
            temp = out(reg) _,
            limit = in(reg) limit
        );
    }
}

/// Set the stack limit of the current rog-routine to a new value.
#[cfg(all(target_os = "macos", target_arch = "x86_64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub unsafe fn set_stack_limit(limit: usize) {
    unsafe {
        asm!("mov gs:0x30, {limit}", limit = in(reg) limit);
    }
}

/// Set the stack limit of the current rog-routine to a new value.
#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub unsafe fn set_stack_limit(limit: usize) {
    unsafe {
        let mut tls: *mut usize;
        asm!("mrs {tls}, tpidrro_el0", tls = out(reg) tls);
        *tls.offset(6) = limit
    }
}
