//@ compile-flags: -Copt-level=0

#![crate_type = "lib"]
#![feature(core_intrinsics)]

#[repr(C)]
pub struct Mixed {
    pointer: *const u8,
    integer: usize,
    float: f64,
}

#[repr(C)]
pub union PointerOrInteger {
    pointer: *const u8,
    integer: usize,
}

#[repr(C)]
pub struct NineFloats(
    pub f64,
    pub f64,
    pub f64,
    pub f64,
    pub f64,
    pub f64,
    pub f64,
    pub f64,
    pub f64,
);

#[repr(C)]
pub struct FiveU128(pub u128, pub u128, pub u128, pub u128, pub u128);

#[unsafe(no_mangle)]
pub fn with_stack_check() {}

// CHECK-LABEL: define{{.*}} rogcc void @with_stack_check()
// CHECK-SAME: unnamed_addr #[[WITH_STACK_CHECK:[0-9]+]]

#[no_split]
#[unsafe(no_mangle)]
pub fn without_stack_check() {}

// CHECK-LABEL: define{{.*}} rogcc void @without_stack_check()
// CHECK-SAME: unnamed_addr #[[WITHOUT_STACK_CHECK:[0-9]+]]

#[unsafe(no_mangle)]
pub extern "C" fn c_abi() {}

// CHECK-LABEL: define{{.*}} void @c_abi()

#[unsafe(no_mangle)]
pub extern "rog" fn rog_abi() {}

// CHECK-LABEL: define{{.*}} rogcc void @rog_abi()

#[unsafe(no_mangle)]
pub extern "rog-cold" fn rog_cold_abi() {}

// CHECK-LABEL: define{{.*}} rog_coldcc void @rog_cold_abi()

#[unsafe(no_mangle)]
pub fn pass_mixed(value: Mixed) -> Mixed {
    value
}

// CHECK-LABEL: define{{.*}} rogcc { ptr, i64, double } @pass_mixed({ ptr, i64, double } %{{.*}})

#[unsafe(no_mangle)]
pub fn pass_pointer_or_integer(value: PointerOrInteger) -> PointerOrInteger {
    value
}

// CHECK-LABEL: define{{.*}} rogcc ptr @pass_pointer_or_integer(ptr{{.*}})

#[unsafe(no_mangle)]
pub fn pass_nine_floats(value: NineFloats) -> NineFloats {
    value
}

// CHECK-LABEL: define{{.*}} rogcc void @pass_nine_floats(ptr{{.*}}sret{{.*}}, ptr{{.*}})

#[unsafe(no_mangle)]
pub fn pass_five_u128(value: FiveU128) -> FiveU128 {
    value
}

// CHECK-LABEL: define{{.*}} rogcc void @pass_five_u128(ptr{{.*}}sret{{.*}}, ptr{{.*}})

#[no_split]
#[unsafe(no_mangle)]
pub fn stack_pointer() -> usize {
    core::intrinsics::get_stack_pointer()
}

// CHECK-LABEL: define{{.*}} rogcc i64 @stack_pointer()
// CHECK: call i64 @llvm.read_register.i64(metadata ![[STACK_POINTER_REGISTER:[0-9]+]])

// CHECK: attributes #[[WITH_STACK_CHECK]] = { {{.*}}"rog-stack-check"{{.*}} }
// CHECK-NOT: attributes #[[WITHOUT_STACK_CHECK]] = {{.*}}"rog-stack-check"
// CHECK: ![[STACK_POINTER_REGISTER]] = !{!"rsp"}
