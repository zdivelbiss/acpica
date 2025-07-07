#![no_std]
#![feature(allocator_api)]
#![forbid(stable_features, unsafe_op_in_unsafe_fn)]
#![deny(
    clippy::debug_assert_with_mut_call,
    clippy::float_arithmetic,
    clippy::as_conversions
)]
#![warn(
    clippy::cargo,
    clippy::pedantic,
    clippy::undocumented_unsafe_blocks,
    clippy::semicolon_inside_block,
    clippy::semicolon_if_nothing_returned,
    clippy::missing_safety_doc
)]
#![allow(
    dead_code,
    clippy::missing_const_for_fn,
    clippy::missing_errors_doc,
    clippy::needless_for_each,
    clippy::if_not_else
)]

mod sys;

pub mod error;

use core::{ffi::c_void, ptr::NonNull};

pub type PhysicalAddress = u64;
pub type LogicalAddress = NonNull<u8>;
pub type ExecuteContext = *mut c_void;

/// Represents implementation of the OS-specific features that are required for use of the ACPICA
/// subsystem. Each function definition is an internal implementation of a function that is then
/// exported and used by the statically-linked ACPICA library.
///
/// Use [`crate::install`] to install your custom [`crate::OsLayer`] for use with the ACPICA
/// subsystem.
pub trait OsLayer: Send + Sync {
    type ThreadId: Into<u64>;
    type MutexHandle: From<sys::ACPI_MUTEX> + Into<sys::ACPI_MUTEX>;
    type SemaphoreHandle: From<sys::ACPI_SEMAPHORE> + Into<sys::ACPI_SEMAPHORE>;
    type SpinlockHandle: From<sys::ACPI_SPINLOCK> + Into<sys::ACPI_SPINLOCK>;
    type ProcessorFlags: From<sys::ACPI_CPU_FLAGS> + Into<sys::ACPI_CPU_FLAGS>;

    /// Allows the OS to initialize itself. It is called during initialization of the ACPICA subsystem.
    fn initialize(&self);

    /// Allows the OS to cleanup and terminate. It is called during termination of the ACPICA subsystem.
    fn terminate(&self);

    /// Returns the physical address of the ACPI RSDP (Root System Description Pointer) table.
    /// The mechanism used to obtain this pointer is platform and/or OS dependent.
    fn get_root_address(&self) -> PhysicalAddress;

    /// Maps a physical address into the caller’s address space. The logical address is returned.
    fn map_memory(&self, physical_address: PhysicalAddress, size: usize) -> Option<LogicalAddress>;

    /// Deletes a mapping that was created by [`OsLayer::map_memory`].
    fn unmap_memory(&self, logical_address: LogicalAddress, size: usize);

    /// Translates a logical address to its physical address location.
    fn get_physical_address(&self, logical_address: LogicalAddress) -> PhysicalAddress;

    /// Dynamically allocates memory that is not assumed to be initialized.
    fn allocate(&self, size: usize) -> Option<LogicalAddress>;

    /// Frees memory that was previously allocated via [`OsLayer::allocate`].
    fn deallocate(&self, logical_address: LogicalAddress);

    /// Validates that a pointer to a memory region is valid and the entire region is readable.
    /// Used to validate input parameters to the ACPICA subsystem.
    fn is_memory_readable(&self, logical_address: LogicalAddress, length: usize) -> bool;

    /// This function validates that a pointer to a memory region is valid and the entire region
    /// is both writable and readable. Used to validate input parameters to the ACPICA subsystem.
    fn is_memory_writable(&self, logical_address: LogicalAddress, length: usize) -> bool;

    /// This function returns the ID of the currently executing thread. The value must be non-zero
    /// and must be unique to the executing thread. The thread ID is a [`u64`]. It is up to the host
    /// to cast the native thread ID.
    ///
    /// # Remarks
    ///
    /// A [`u64`] is used since it is the only data type that can be used to handle all of the various
    /// native thread ID types (32-bit integer, 64-bit integer, 32-bit pointer, 64-bit pointer).
    fn get_thread_id(&self) -> Self::ThreadId;

    /// Queues a procedure for later scheduling and execution.
    fn execute_callback(
        &self,
        callback: unsafe extern "C" fn(ExecuteContext),
        context: ExecuteContext,
    );

    /// This function sleeps for the specified time. Execution of the running thread is suspended
    /// for this time. The sleep granularity is one millisecond.
    fn sleep_wait_ms(&self, milliseconds: u64);

    /// This function waits for the specified time. Execution of the running thread is not suspended
    /// for this time. The time granularity is one microsecond.
    fn spin_wait_us(&self, microseconds: u32);

    /// This function blocks until all asynchronous events initiated by [`OsLayer::execute_callback`]
    /// have completed. Within ACPICA, this function is called before removal of Notify and GPE handlers.
    /// For the host, this function may be useful in related areas, such as blocking for Embedded Controller
    /// event completion.
    fn wait_callbacks_complete(&self);

    /// Create a mutex object.
    ///
    /// Some host operating systems have separate mutex interfaces that can be used
    /// to implement this and the other OS mutex interfaces. If not, the the mutex interfaces can be
    /// implemented with semaphore interfaces.
    fn mutex_create(&self) -> Self::MutexHandle;

    /// Deletes a mutex object.
    fn mutex_delete(&self, handle: Self::MutexHandle);

    /// Acquire ownership of a mutex object.
    fn mutex_acquire(
        &self,
        handle: Self::MutexHandle,
        timeout_ms: u16,
    ) -> Result<(), error::MutexAcquireError>;

    /// Release a mutex object.
    ///
    /// The mutex must have be previously acquired via [`OsLayer::mutex_acquire`].
    fn mutex_release(&self, handle: Self::MutexHandle);

    /// Create a standard semaphore.
    ///
    /// The `max_units` parameter allows the semaphore to be tailored to specific uses. For example, a
    /// `max_units` value of one indicates that the semaphore is to be used as a mutex. The underlying
    /// OS object used to implement this semaphore may be different than if `max_units` is greater than
    /// one (thus indicating that the semaphore will be used as a general purpose semaphore.) The ACPICA
    /// Subsystem creates semaphores of both the mutex and general-purpose variety.
    fn semaphore_create(
        &self,
        max_units: u32,
        initial_units: u32,
    ) -> Result<Self::SemaphoreHandle, error::SemaphoreCreateError>;

    /// Delete a semaphore.
    fn semaphore_delete(
        &self,
        handle: Self::SemaphoreHandle,
    ) -> Result<(), error::SemaphoreDeleteError>;

    /// Wait for the specified number of units from a semaphore.
    ///
    /// Implementation notes:
    /// 1. The implementation of this interface must support timeout values of zero. This is frequently
    ///    used to determine if a call to the interface with an actual timeout value would block. In this
    ///    case, you must return either a [`Result::Ok`] if the units were obtained immediately, or a
    ///    [`SemaphoreAcquireError::TImeoutElapsed`][crate::error::SemaphoreAcquireError] to indicate that
    ///    the requested units are not available. Single-threaded OS implementations should always return
    ///    [`Result::Ok`] for this interface.
    /// 2. The implementation must also support arbitrary timed waits in order for ASL functions such as
    ///    Wait () to work properly.
    fn semaphore_acquire(
        &self,
        handle: Self::SemaphoreHandle,
        units: u32,
        timeout_ms: u16,
    ) -> Result<(), error::SemaphoreAcquireError>;

    /// Send the requested number of units to a semaphore. Single-threaded OS implementations should
    /// always return [`Result::Ok`] for this interface.
    fn semaphore_release(
        &self,
        handle: Self::SemaphoreHandle,
        units: u32,
    ) -> Result<(), error::SemaphoreReleaseError>;

    fn spinlock_create(&self) -> Result<Self::SpinlockHandle, error::SpinlockCreateError>;

    fn spinlock_delete(
        &self,
        handle: Self::SpinlockHandle,
    ) -> Result<(), error::SpinlockDeleteError>;

    fn spinlock_acquire(&self, handle: Self::SpinlockHandle) -> Self::ProcessorFlags;

    fn spinlock_release(&self, handle: Self::SpinlockHandle, flags: Self::ProcessorFlags);
}

pub fn install<T: OsLayer>(os_layer: &'static T) {
    debug_assert!(
        !sys::OS_LAYER.is_completed(),
        "`acpica_sys::install` has been called more than once; this is likely an error"
    );

    sys::OS_LAYER.call_once(|| {
        // Safety: `Dst` is `#[repr(transparent)]`
        #[allow(clippy::transmute_ptr_to_ptr)]
        unsafe {
            core::mem::transmute::<&T, &'static sys::OsLayerAdapter<T>>(os_layer)
        }
    });
}

/// This function locates and returns the ACPI Root System Description Pointer by scanning
/// within the first megabyte of physical memory for the RSDP signature. This mechanism is
/// only applicable to IA-32 systems.
///
/// This interface should only be called from [`OsLayer::get_root_address`] if this
/// memory scanning mechanism is appropropriate for the current platform.
#[cfg(target_arch = "x86")]
pub fn find_root_pointer() -> PhysicalAddress {
    let mut address: usize;
    let exception_code = unsafe { sys::AcpiFindRootPointer(&mut address) };

    match exception_code {
        sys::ACPI_STATUS::OK => address,
        sys::ACPI_STATUS::NO_MEMORY => panic!("insufficient dynamic memory"),
        sys::ACPI_STATUS::NOT_FOUND => panic!("valid RSDP could not be located"),
    }

    // TODO use `Result<PhysicalAddress, Error>` as return type
}

#[test]
fn test_init() {
    // Safety: `acpica_sys` exports symbol.
    unsafe {
        sys::AcpiInitializeSubsystem();
    }
}
