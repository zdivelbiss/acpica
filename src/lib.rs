#![no_std]
#![feature(allocator_api)]

#[cfg(feature = "alloc")]
extern crate alloc;

mod sys;

use core::{ffi::c_void, ptr::NonNull};

pub type PhysicalAddress = u64;

pub trait AcpiHandler: Send + Sync {
    fn initialize(&self);
    fn terminate(&self);
    fn get_root_address(&self) -> PhysicalAddress;

    fn map_memory(&self, physical_address: PhysicalAddress, size: usize)
    -> Option<NonNull<c_void>>;
    fn unmap_memory(&self, pointer: NonNull<c_void>, size: usize);

    fn get_physical_address(&self, pointer: NonNull<c_void>) -> PhysicalAddress;
}

static OS_LAYER: spin::Once<&dyn AcpiHandler> = spin::Once::new();

pub fn install(handler: impl AcpiHandler + 'static) {
    debug_assert!(
        !OS_LAYER.is_completed(),
        "`acpica_sys::install` has been called more than once; this is likely an error"
    );

    OS_LAYER.call_once(|| &handler);
}

fn get_os_layer() -> &'static dyn AcpiHandler {
    OS_LAYER
        .get()
        .expect("ACPICA OS layer has not been installed")
}
