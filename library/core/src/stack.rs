use crate::intrinsics::abort;

#[no_split]
#[linkage = "weak"]
#[unsafe(no_mangle)]
extern "rog-cold" fn rog_morestack_abi() {
    abort();
}
