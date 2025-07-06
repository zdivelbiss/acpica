#![allow(non_snake_case, dead_code)]

use crate::get_os_layer;
use core::{ffi::c_void, ptr::NonNull};

extern "C" fn AcpiOsInitialize() {
    get_os_layer().initialize();
}

extern "C" fn AcpiOsTerminate() {
    get_os_layer().terminate();
}

extern "C" fn AcpiOsGetRootPointer() -> u64 {
    get_os_layer().get_root_address()
}

extern "C" fn AcpiOsMapMemory(PhysicalAddress: u64, Length: usize) -> *mut c_void {
    get_os_layer()
        .map_memory(PhysicalAddress, Length)
        .map(NonNull::cast)
        .map(NonNull::as_ptr)
        .unwrap_or_default()
}

extern "C" fn AcpiOsUnmapMemory(LogicalAddress: *mut c_void, Length: usize) {
    get_os_layer().unmap_memory(
        NonNull::new(LogicalAddress)
            .expect("ACPICA provided a null pointer for `AcpiOsUnmapMemory`")
            .cast(),
        Length,
    );
}

extern "C" fn AcpiOsAllocate(Size: usize) -> *mut c_void {
    get_os_layer()
        .allocate(Size)
        .map(NonNull::cast)
        .map(NonNull::as_ptr)
        .unwrap_or_default()
}

extern "C" fn AcpiOsFree(Memory: *mut c_void) {
    get_os_layer().deallocate(
        NonNull::new(Memory)
            .expect("ACPICA provided a null pointer for `AcpiOsFree`")
            .cast(),
    );
}

extern "C" fn AcpiOsReadable(Memory: *mut c_void, Length: usize) -> bool {
    get_os_layer().is_memory_readable(
        NonNull::new(Memory)
            .expect("ACPICA provided a null pointer for `AcpiOsReadable`")
            .cast(),
        Length,
    )
}

extern "C" fn AcpiOsWritable(Memory: *mut c_void, Length: usize) -> bool {
    get_os_layer().is_memory_writable(
        NonNull::new(Memory)
            .expect("ACPICA provided a null pointer for `AcpiOsWritable`")
            .cast(),
        Length,
    )
}

#[link(name = "acpica_sys")]
unsafe extern "C" {
    pub fn AcpiInitializeSubsystem();
}
