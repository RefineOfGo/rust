//! Ensure the unused_attributes lint fires for externally exported functions with `#[inline]`,
//! because `#[inline]` is ignored for such functions.

#![crate_type = "lib"]
#![feature(linkage)]
#![deny(unused_attributes)]

#[inline]
//~^ ERROR: `#[inline]` is ignored on externally exported non-lto-aware functions
#[no_mangle]
fn no_mangle() {}

#[inline]
//~^ ERROR: `#[inline]` is ignored on externally exported non-lto-aware functions
#[export_name = "export_name"]
fn export_name() {}

#[inline]
//~^ ERROR: `#[inline]` is ignored on externally exported non-lto-aware functions
#[linkage = "external"]
fn external_linkage() {}

// C and ROG are LTO-aware, so their exported functions may still be inlined.
#[inline]
#[no_mangle]
pub extern "C" fn c_abi() {}

#[inline]
#[no_mangle]
pub extern "rog" fn rog_abi() {}

#[inline]
fn normal() {}

#[inline]
#[linkage = "internal"] // not exported
fn internal_linkage() {}
