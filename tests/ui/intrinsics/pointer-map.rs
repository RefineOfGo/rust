//@ run-pass

#![allow(dead_code)]

use std::mem::{is_pointer_map_exact, pointer_map_of};

#[repr(C)]
struct Pair {
    pointer: *const u8,
    value: usize,
}

#[repr(C)]
struct ReversePair {
    value: usize,
    pointer: *const u8,
}

union PointerOrInteger {
    pointer: *const u8,
    integer: usize,
}

#[repr(C, packed)]
struct PackedPointer {
    byte: u8,
    pointer: *const u8,
}

const POINTER_MAP: &[u64] = pointer_map_of::<*const u8>();
const POINTER_MAP_IS_EXACT: bool = is_pointer_map_exact::<*const u8>();

fn main() {
    assert_eq!(pointer_map_of::<()>(), &[]);
    assert_eq!(pointer_map_of::<usize>(), &[]);

    assert_eq!(POINTER_MAP, &[1]);
    assert!(POINTER_MAP_IS_EXACT);
    assert_eq!(pointer_map_of::<Pair>(), &[1]);
    assert_eq!(pointer_map_of::<ReversePair>(), &[2]);
    assert_eq!(pointer_map_of::<[*const u8; 65]>(), &[u64::MAX, 1]);

    assert_eq!(pointer_map_of::<Option<&u8>>(), &[1]);
    assert!(is_pointer_map_exact::<Option<&u8>>());

    assert_eq!(pointer_map_of::<PointerOrInteger>(), &[1]);
    assert!(!is_pointer_map_exact::<PointerOrInteger>());

    assert_eq!(pointer_map_of::<PackedPointer>(), &[3]);
    assert!(!is_pointer_map_exact::<PackedPointer>());
}
