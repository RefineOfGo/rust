use crate::arch::asm;

/// ROG Stack Growing stub, the real implementation is in ROG runtime.
/// Known to the relevant LLVM passes.
///
/// This function takes exactly 1 argument, which is the stack limit
/// requirement, but it uses it's own calling convention:
///
/// * x86_64  : passed in `%rax` register
/// * aarch64 : passed in `X16` register
///
/// This is because the function is called within the prologue region,
/// registers are not spilled yet, so the choose of parameter registers
/// are rather limited, but there are some general guidelines:
///
/// 1. It cannot be any of the parameter registers:
///
///     * For x86_64 they are %rdi, %rsi, %rdx, %rcx, %r8, %r9
///     * For aarch64 they are X0-X7, and X8 for struct results
///
/// 2. It cannot be any of the callee-saved registers, otherwise it may
///    corrupt the caller's state:
///
///     * For x86_64 they are %rbx, %rbp, %rsp, %r12-r15
///     * For aarch64 they are X19-X30
///
/// 3. As for aarch64, the X18 register is specially reserved for platform
///    specific data, and should not be used by user code, so it's out.
///
/// So... There are only a few registers available for us to choose from,
/// let's see what we've got:
///
///     * x86_64  : %rax, %r10, %r11
///     * aarch64 : X9-X15, IP0(X16), IP1(X17)
///
/// For x86_64, %rax seems to be a good choice, since it's the default
/// register to store return values, so it will be clobbered anyway. It also
/// have the benifit of saving a few bytes because some instructions have
/// shorter encodings when %rax is one of its operand.
///
/// AArch64 provided us with two registers IP0 and IP1 specifically for this
/// kind of usage (actually it was intended to give the linker some free
/// registers to clobber, but anyway), so it's the most nature selection.
///
/// Also accessing TLS in AArch64 is not as straightforward as x86_64 do, so
/// we also clobbers X17 for MRS instruction to read from TPIDR[RO]_EL0
/// register.
///
/// Also natually, this function MUST preserve every register accross the call
/// except the register said above, otherwise it may corrupt the program state.
///
/// Note that AArch64 does not save return address to stack, instead they use
/// a special register LR(X30) to store the return address. Branching to
/// another function clobbers LR. So under AArch64, LR will be saved before
/// calling this function, and that will be passed as an extra argument in X17.
///
/// As a side-effect of the unusual calling convention, this function MUST
/// implement in assembly, or at least a wrapper to the real function should.
#[no_gcwb]
#[no_split]
#[no_mangle]
#[linkage = "linkonce"]
extern "rog-cold" fn rog_morestack_abi() {
    crate::intrinsics::abort();
}

/// Prevent compiler from removing it as dead-code.
#[used]
static _X: extern "rog-cold" fn() = rog_morestack_abi;

/// Get the stack limit (lower boundary) of the current rog-routine.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub fn get_stack_limit() -> usize {
    unsafe {
        let mut limit: usize;
        asm!("mov {limit}, fs:0x80", limit = out(reg) limit);
        limit
    }
}

/// Get the stack limit (lower boundary) of the current rog-routine.
#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub fn get_stack_limit() -> usize {
    unsafe {
        let mut limit: usize;
        asm!(
            "mrs {tls}, tpidr_el0",
            "add {tls}, {tls}, :tprel_hi12:rog_stack_limit",
            "ldr {limit}, [{tls}, :tprel_lo12_nc:rog_stack_limit]",
            tls = out(reg) _,
            limit = out(reg) limit
        );
        limit
    }
}

/// Get the stack limit (lower boundary) of the current rog-routine.
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

/// Get the stack limit (lower boundary) of the current rog-routine.
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

/// Set the stack limit (lower boundary) of the current rog-routine.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub unsafe fn set_stack_limit(limit: usize) {
    unsafe {
        asm!("mov fs:0x80, {limit}", limit = in(reg) limit);
    }
}

/// Set the stack limit (lower boundary) of the current rog-routine.
#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub unsafe fn set_stack_limit(limit: usize) {
    unsafe {
        asm!(
            "mrs {tls}, tpidr_el0",
            "add {tls}, {tls}, :tprel_hi12:rog_stack_limit",
            "str {limit}, [{tls}, :tprel_lo12_nc:rog_stack_limit]",
            tls = out(reg) _,
            limit = in(reg) limit
        );
    }
}

/// Set the stack limit (lower boundary) of the current rog-routine.
#[cfg(all(target_os = "macos", target_arch = "x86_64"))]
#[stable(feature = "rog", since = "1.0.0")]
#[inline(always)]
pub unsafe fn set_stack_limit(limit: usize) {
    unsafe {
        asm!("mov gs:0x30, {limit}", limit = in(reg) limit);
    }
}

/// Set the stack limit (lower boundary) of the current rog-routine.
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
