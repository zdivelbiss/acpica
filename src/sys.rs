#![allow(
    non_snake_case,
    non_camel_case_types,
    dead_code,
    clippy::upper_case_acronyms
)]

use crate::{
    OsLayer,
    error::{
        MutexAcquireError, SemaphoreAcquireError, SemaphoreCreateError, SemaphoreDeleteError,
        SemaphoreReleaseError, SpinlockCreateError,
    },
};
use core::{ffi::c_void, ptr::NonNull};

/// The width of all physical addresses is fixed at 64 bits, regardless of the platform or operating
/// system. Logical addresses (pointers) remain the natural width of the machine (i.e. 32-bit pointers on
/// 32-bit machines, 64-bit pointers on 64-bit machines.) This allows for a full 64-bit address space on
/// 64-bit machines as well as “extended” physical addresses (above 4Gbytes) on 32-bit machines.
pub type ACPI_PHYSICAL_ADDRESS = u64;

/// Similar to [`ACPI_PHYSICAL_ADDRESS`], except it is used for I/O addresses.
pub type ACPI_IO_ADDRESS = u64;

/// This data type is 32 bits or 64 bits depending on the platform. It is used in leiu of `size_t`,
/// which cannot be guaranteed to be available.
pub type ACPI_SIZE = usize;

/// This type is defined as a UINT64 and is returned by the [`AcpiOsGetThreadId`] interface.
///
/// There is no standard "thread ID"-type across operating systems or even the various UNIX systems.
/// Since ACPICA only needs the thread ID as a unique thread identifier, it uses a UINT64 as the only
/// common data type – a UINT64 will accommodate any type of pointer or any type of integer. It is up
/// to the host-dependent OSL to cast the native thread ID type to a UINT64 (in [`AcpiOsGetThreadId`])
/// before returning the value to ACPICA.
pub type ACPI_THREAD_ID = u64;

/// This type is an OS-dependent handle for a mutex. It is returned by the [`AcpiOsCreateMutex`]
/// interface, and passed as a parameter to the [`AcpiOsAcquireMutex`] and [`AcpiOsReleaseMutex`]
/// interfaces.
pub type ACPI_MUTEX = *mut c_void;

/// This type is an OS-dependent handle for a semaphore. It is returned by the [`AcpiOsCreateSemaphore`]
/// interface, and passed as a parameter to the [`AcpiOsWaitSemaphore`] and [`AcpiOsSignalSemaphore`]
/// interfaces.
pub type ACPI_SEMAPHORE = *mut c_void;

/// This type is an OS-dependent handle for a spinlock. It is returned by the [`AcpiOsCreateLock`]
/// interface, and passed as a parameter to the [`AcpiOsAcquireLock`] and [`AcpiOsReleaseLock`] interfaces.
pub type ACPI_SPINLOCK = *mut c_void;

/// This type is used for the value returned from [`AcpiOsAcquireLock`], and the value passed as a
/// parameter to [`AcpiOsReleaseLock`]. It can be configured to whatever type the host OS uses for CPU
/// flags that need to be saved and restored across the acquisition and release of a spinlock. The default
/// value is [`ACPI_SIZE`].
pub type ACPI_CPU_FLAGS = ACPI_SIZE;

/// Most of the external ACPI interfaces return an exception code of this type as the function return
/// value.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ACPI_STATUS {
    OK = 0x0000,

    /* General ACPICA environment */
    ERROR = 0x0001,
    NO_ACPI_TABLES = 0x0002,
    NO_NAMESPACE = 0x0003,
    NO_MEMORY = 0x0004,
    NOT_FOUND = 0x0005,
    NOT_EXIST = 0x0006,
    ALREADY_EXISTS = 0x0007,
    TYPE = 0x0008,
    NULL_OBJECT = 0x0009,
    NULL_ENTRY = 0x000A,
    BUFFER_OVERFLOW = 0x000B,
    STACK_OVERFLOW = 0x000C,
    STACK_UNDERFLOW = 0x000D,
    NOT_IMPLEMENTED = 0x000E,
    SUPPORT = 0x000F,
    LIMIT = 0x0010,
    TIME = 0x0011,
    ACQUIRE_DEADLOCK = 0x0012,
    RELEASE_DEADLOCK = 0x0013,
    NOT_ACQUIRED = 0x0014,
    ALREADY_ACQUIRED = 0x0015,
    NO_HARDWARE_RESPONSE = 0x0016,
    NO_GLOBAL_LOCK = 0x0017,
    ABORT_METHOD = 0x0018,
    SAME_HANDLER = 0x0019,
    NO_HANDLER = 0x001A,
    OWNER_ID_LIMIT = 0x001B,
    NOT_CONFIGURED = 0x001C,
    ACCESS = 0x001D,
    IO_ERROR = 0x001E,
    NUMERIC_OVERFLOW = 0x001F,
    HEX_OVERFLOW = 0x0020,
    DECIMAL_OVERFLOW = 0x0021,
    OCTAL_OVERFLOW = 0x0022,
    END_OF_TABLE = 0x0023,

    /* External ACPICA interface caller */
    BAD_PARAMETER = 0x1001,
    BAD_CHARACTER = 0x1002,
    BAD_PATHNAME = 0x1003,
    BAD_DATA = 0x1004,
    BAD_HEX_CONSTANT = 0x1005,
    BAD_OCTAL_CONSTANT = 0x1006,
    BAD_DECIMAL_CONSTANT = 0x1007,
    MISSING_ARGUMENTS = 0x1008,
    BAD_ADDRESS = 0x1009,

    /* ACPI tables  */
    BAD_SIGNATURE = 0x2001,
    BAD_HEADER = 0x2002,
    BAD_CHECKSUM = 0x2003,
    BAD_VALUE = 0x2004,
    INVALID_TABLE_LENGTH = 0x2005,

    /* AML code execution */
    AML_BAD_OPCODE = 0x3001,
    AML_NO_OPERAND = 0x3002,
    AML_OPERAND_TYPE = 0x3003,
    AML_OPERAND_VALUE = 0x3004,
    AML_UNINITIALIZED_LOCAL = 0x3005,
    AML_UNINITIALIZED_ARG = 0x3006,
    AML_UNINITIALIZED_ELEMENT = 0x3007,
    AML_NUMERIC_OVERFLOW = 0x3008,
    AML_REGION_LIMIT = 0x3009,
    AML_BUFFER_LIMIT = 0x300A,
    AML_PACKAGE_LIMIT = 0x300B,
    AML_DIVIDE_BY_ZERO = 0x300C,
    AML_BAD_NAME = 0x300D,
    AML_NAME_NOT_FOUND = 0x300E,
    AML_INTERNAL = 0x300F,
    AML_INVALID_SPACE_ID = 0x3010,
    AML_STRING_LIMIT = 0x3011,
    AML_NO_RETURN_VALUE = 0x3012,
    AML_METHOD_LIMIT = 0x3013,
    AML_NOT_OWNER = 0x3014,
    AML_MUTEX_ORDER = 0x3015,
    AML_MUTEX_NOT_ACQUIRED = 0x3016,
    AML_INVALID_RESOURCE_TYPE = 0x3017,
    AML_INVALID_INDEX = 0x3018,
    AML_REGISTER_LIMIT = 0x3019,
    AML_NO_WHILE = 0x301A,
    AML_ALIGNMENT = 0x301B,
    AML_NO_RESOURCE_END_TAG = 0x301C,
    AML_BAD_RESOURCE_VALUE = 0x301D,
    AML_CIRCULAR_REFERENCE = 0x301E,
    AML_BAD_RESOURCE_LENGTH = 0x301F,
    AML_ILLEGAL_ADDRESS = 0x3020,
    AML_LOOP_TIMEOUT = 0x3021,
    AML_UNINITIALIZED_NODE = 0x3022,
    AML_TARGET_TYPE = 0x3023,
    AML_PROTOCOL = 0x3024,
    AML_BUFFER_LENGTH = 0x3025,
    AML_TOO_FEW_ARGUMENTS = 0x3026,
    AML_TOO_MANY_ARGUMENTS = 0x3027,

    /* Internal control codes */
    CTRL_RETURN_VALUE = 0x4001,
    CTRL_PENDING = 0x4002,
    CTRL_TERMINATE = 0x4003,
    CTRL_TRUE = 0x4004,
    CTRL_FALSE = 0x4005,
    CTRL_DEPTH = 0x4006,
    CTRL_END = 0x4007,
    CTRL_TRANSFER = 0x4008,
    CTRL_BREAK = 0x4009,
    CTRL_CONTINUE = 0x400A,
    CTRL_PARSE_CONTINUE = 0x400B,
    CTRL_PARSE_PENDING = 0x400C,
}

impl ACPI_STATUS {
    pub fn is_success(self) -> bool {
        self == Self::OK
    }
}

pub type ACPI_OSD_EXEC_CALLBACK = unsafe extern "C" fn(*mut c_void);

#[repr(u32)]
pub enum ACPI_EXECUTE_TYPE {
    OSL_GLOBAL_LOCK_HANDLER = 0,
    OSL_NOTIFY_HANDLER = 1,
    OSL_GPE_HANDLER = 2,
    OSL_DEBUGGER_MAIN_THREAD = 3,
    OSL_DEBUGGER_EXEC_THREAD = 4,
    OSL_EC_POLL_HANDLER = 5,
    OSL_EC_BURST_HANDLER = 6,
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsInitialize() {
    get_os_layer().initialize();
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsTerminate() {
    get_os_layer().terminate();
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsGetRootPointer() -> ACPI_PHYSICAL_ADDRESS {
    get_os_layer().get_root_pointer()
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsMapMemory(
    PhysicalAddress: ACPI_PHYSICAL_ADDRESS,
    Length: ACPI_SIZE,
) -> *mut c_void {
    get_os_layer().map_memory(PhysicalAddress, Length)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsUnmapMemory(LogicalAddress: *mut c_void, Length: ACPI_SIZE) {
    get_os_layer().unmap_memory(LogicalAddress, Length);
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsGetPhysicalAddress(
    LogicalAddress: *mut c_void,
    PhysicalAddress: Option<NonNull<ACPI_PHYSICAL_ADDRESS>>,
) -> ACPI_STATUS {
    get_os_layer().get_physical_address(LogicalAddress, PhysicalAddress)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsAllocate(Size: ACPI_SIZE) -> *mut c_void {
    get_os_layer().allocate(Size)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsFree(Memory: *mut c_void) {
    get_os_layer().free(Memory);
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsReadable(Memory: *mut c_void, Length: ACPI_SIZE) -> bool {
    get_os_layer().readable(Memory, Length)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsWritable(Memory: *mut c_void, Length: ACPI_SIZE) -> bool {
    get_os_layer().writable(Memory, Length)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsGetThreadId() -> ACPI_THREAD_ID {
    get_os_layer().get_thread_id()
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsExecute(
    Type: ACPI_EXECUTE_TYPE,
    Function: Option<ACPI_OSD_EXEC_CALLBACK>,
    Context: *mut c_void,
) -> ACPI_STATUS {
    get_os_layer().execute(Type, Function, Context)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsSleep(Milliseconds: u64) {
    get_os_layer().sleep(Milliseconds);
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsStall(Microseconds: u32) {
    get_os_layer().stall(Microseconds);
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsWaitEventsComplete() {
    get_os_layer().wait_events_complete();
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsCreateMutex(OutHandle: Option<NonNull<ACPI_MUTEX>>) -> ACPI_STATUS {
    get_os_layer().create_mutex(OutHandle)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsDeleteMutex(Handle: ACPI_MUTEX) {
    get_os_layer().delete_mutex(Handle);
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsAcquireMutex(Handle: ACPI_MUTEX, Timeout: u16) -> ACPI_STATUS {
    get_os_layer().acquire_mutex(Handle, Timeout)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsReleaseMutex(Handle: ACPI_MUTEX) {
    get_os_layer().release_mutex(Handle);
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsCreateSemaphore(
    MaxUnits: u32,
    InitialUnits: u32,
    Handle: Option<NonNull<ACPI_SEMAPHORE>>,
) -> ACPI_STATUS {
    get_os_layer().create_semaphore(MaxUnits, InitialUnits, Handle)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsDeleteSemaphore(Handle: ACPI_SEMAPHORE) -> ACPI_STATUS {
    get_os_layer().delete_semaphore(Handle)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsWaitSemaphore(Handle: ACPI_SEMAPHORE, Units: u32, Timeout: u16) -> ACPI_STATUS {
    get_os_layer().wait_semaphore(Handle, Units, Timeout)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsSignalSemaphore(Handle: ACPI_SEMAPHORE, Units: u32) -> ACPI_STATUS {
    get_os_layer().signal_semaphore(Handle, Units)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsCreateLock(OutHandle: Option<NonNull<ACPI_SPINLOCK>>) -> ACPI_STATUS {
    get_os_layer().create_lock(OutHandle)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsDeleteLock(Handle: ACPI_SPINLOCK) {
    get_os_layer().delete_lock(Handle);
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsAcquireLock(Handle: ACPI_SPINLOCK) -> ACPI_CPU_FLAGS {
    get_os_layer().acquire_lock(Handle)
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsReleaseLock(Handle: ACPI_SPINLOCK, Flags: ACPI_CPU_FLAGS) {
    get_os_layer().release_lock(Handle, Flags);
}

/// Internal trait implementing ACPICA OS layer functions. It is intended to be
/// used in conjunction with [`OsLayerAdapter<T>`]. Additionally, breaking out
/// the direct implementation of the OS layer allows the API-facing trait
/// ([`OsLayer`][crate::OsLayer]) to utilize associated types for ease of use.
pub trait OsLayerInternal: Send + Sync {
    fn initialize(&self);
    fn terminate(&self);

    fn get_root_pointer(&self) -> ACPI_PHYSICAL_ADDRESS;

    fn map_memory(&self, PhysicalAddress: ACPI_PHYSICAL_ADDRESS, Length: ACPI_SIZE) -> *mut c_void;
    fn unmap_memory(&self, LogicalAddress: *mut c_void, Length: ACPI_SIZE);

    fn get_physical_address(
        &self,
        LogicalAddress: *mut c_void,
        PhysicalAddress: Option<NonNull<ACPI_PHYSICAL_ADDRESS>>,
    ) -> ACPI_STATUS;

    fn allocate(&self, Size: ACPI_SIZE) -> *mut c_void;
    fn free(&self, Memory: *mut c_void);

    fn readable(&self, Memory: *mut c_void, Length: ACPI_SIZE) -> bool;
    fn writable(&self, Memory: *mut c_void, Length: ACPI_SIZE) -> bool;

    fn get_thread_id(&self) -> ACPI_THREAD_ID;

    fn execute(
        &self,
        _Type: ACPI_EXECUTE_TYPE,
        Function: Option<ACPI_OSD_EXEC_CALLBACK>,
        Context: *mut c_void,
    ) -> ACPI_STATUS;

    fn sleep(&self, Milliseconds: u64);
    fn stall(&self, Microseconds: u32);

    fn wait_events_complete(&self);

    fn create_mutex(&self, OutHandle: Option<NonNull<ACPI_MUTEX>>) -> ACPI_STATUS;
    fn delete_mutex(&self, Handle: ACPI_MUTEX);
    fn acquire_mutex(&self, Handle: ACPI_MUTEX, Timeout: u16) -> ACPI_STATUS;
    fn release_mutex(&self, Handle: ACPI_MUTEX);

    fn create_semaphore(
        &self,
        MaxUnits: u32,
        InitialUnits: u32,
        Handle: Option<NonNull<ACPI_SEMAPHORE>>,
    ) -> ACPI_STATUS;
    fn delete_semaphore(&self, Handle: ACPI_SEMAPHORE) -> ACPI_STATUS;
    fn wait_semaphore(&self, Handle: ACPI_SEMAPHORE, Units: u32, Timeout: u16) -> ACPI_STATUS;
    fn signal_semaphore(&self, Handle: ACPI_SEMAPHORE, Units: u32) -> ACPI_STATUS;

    fn create_lock(&self, OutHandle: Option<NonNull<ACPI_SPINLOCK>>) -> ACPI_STATUS;
    fn delete_lock(&self, Handle: ACPI_SPINLOCK);
    fn acquire_lock(&self, Handle: ACPI_SPINLOCK) -> ACPI_CPU_FLAGS;
    fn release_lock(&self, Handle: ACPI_SPINLOCK, Flags: ACPI_CPU_FLAGS);
}

/// Adapter type for converting between [`OsLayer`][crate::OsLayer] and [`OsLayerInternal`].
#[repr(transparent)]
pub struct OsLayerAdapter<T>(T);

impl<T: OsLayer> OsLayerInternal for OsLayerAdapter<T> {
    fn initialize(&self) {
        self.0.initialize();
    }

    fn terminate(&self) {
        self.0.terminate();
    }

    fn get_root_pointer(&self) -> ACPI_PHYSICAL_ADDRESS {
        self.0.get_root_address()
    }

    fn map_memory(&self, PhysicalAddress: ACPI_PHYSICAL_ADDRESS, Length: ACPI_SIZE) -> *mut c_void {
        self.0
            .map_memory(PhysicalAddress, Length)
            .map(NonNull::cast)
            .map(NonNull::as_ptr)
            .unwrap_or_default()
    }

    fn unmap_memory(&self, LogicalAddress: *mut c_void, Length: ACPI_SIZE) {
        self.0.unmap_memory(
            NonNull::new(LogicalAddress)
                .expect("ACPICA provided a null pointer for `AcpiOsUnmapMemory`")
                .cast(),
            Length,
        );
    }

    fn get_physical_address(
        &self,
        LogicalAddress: *mut c_void,
        PhysicalAddress: Option<NonNull<ACPI_PHYSICAL_ADDRESS>>,
    ) -> ACPI_STATUS {
        let Some(PhysicalAddress) = PhysicalAddress else {
            return ACPI_STATUS::BAD_PARAMETER;
        };

        let Some(LogicalAddress) = NonNull::new(LogicalAddress).map(NonNull::cast) else {
            return ACPI_STATUS::BAD_PARAMETER;
        };

        unsafe {
            PhysicalAddress.write(self.0.get_physical_address(LogicalAddress));
        }

        ACPI_STATUS::OK
    }

    fn allocate(&self, Size: ACPI_SIZE) -> *mut c_void {
        self.0
            .allocate(Size)
            .map(NonNull::cast)
            .map(NonNull::as_ptr)
            .unwrap_or_default()
    }

    fn free(&self, Memory: *mut c_void) {
        self.0.deallocate(
            NonNull::new(Memory)
                .expect("ACPICA provided a null pointer for `AcpiOsFree`")
                .cast(),
        );
    }

    fn readable(&self, Memory: *mut c_void, Length: ACPI_SIZE) -> bool {
        self.0.is_memory_readable(
            NonNull::new(Memory)
                .expect("ACPICA provided a null pointer for `AcpiOsReadable`")
                .cast(),
            Length,
        )
    }

    fn writable(&self, Memory: *mut c_void, Length: ACPI_SIZE) -> bool {
        self.0.is_memory_writable(
            NonNull::new(Memory)
                .expect("ACPICA provided a null pointer for `AcpiOsWritable`")
                .cast(),
            Length,
        )
    }

    fn get_thread_id(&self) -> u64 {
        let thread_id = self.0.get_thread_id().into();

        assert!(
            thread_id != 0xFFFF_FFFF_FFFF_FFFF,
            "max thread ID is reserved"
        );

        thread_id
    }

    fn execute(
        &self,
        _Type: ACPI_EXECUTE_TYPE,
        Function: Option<ACPI_OSD_EXEC_CALLBACK>,
        Context: *mut c_void,
    ) -> ACPI_STATUS {
        let Some(Function) = Function else {
            return ACPI_STATUS::BAD_PARAMETER;
        };

        self.0.execute_callback(Function, Context);

        ACPI_STATUS::OK
    }

    fn sleep(&self, Milliseconds: u64) {
        self.0.sleep_wait_ms(Milliseconds);
    }

    fn stall(&self, Microseconds: u32) {
        self.0.spin_wait_us(Microseconds);
    }

    fn wait_events_complete(&self) {
        self.0.wait_callbacks_complete();
    }

    fn create_mutex(&self, OutHandle: Option<NonNull<ACPI_MUTEX>>) -> ACPI_STATUS {
        let Some(OutHandle) = OutHandle else {
            return ACPI_STATUS::BAD_PARAMETER;
        };

        unsafe {
            OutHandle.write(self.0.mutex_create().into());
        }

        ACPI_STATUS::OK
    }

    fn delete_mutex(&self, Handle: ACPI_MUTEX) {
        self.0.mutex_delete(Handle.into());
    }

    fn acquire_mutex(&self, Handle: ACPI_MUTEX, Timeout: u16) -> ACPI_STATUS {
        match self.0.mutex_acquire(Handle.into(), Timeout) {
            Ok(()) => ACPI_STATUS::OK,
            Err(MutexAcquireError::TimeoutElapsed) => ACPI_STATUS::TIME,
        }
    }

    fn release_mutex(&self, Handle: ACPI_MUTEX) {
        self.0.mutex_release(Handle.into());
    }

    fn create_semaphore(
        &self,
        MaxUnits: u32,
        InitialUnits: u32,
        OutHandle: Option<NonNull<ACPI_SEMAPHORE>>,
    ) -> ACPI_STATUS {
        let Some(OutHandle) = OutHandle else {
            return ACPI_STATUS::BAD_PARAMETER;
        };

        match self.0.semaphore_create(MaxUnits, InitialUnits) {
            Ok(handle) => {
                unsafe {
                    OutHandle.write(handle.into());
                }

                ACPI_STATUS::OK
            }
            Err(SemaphoreCreateError::InvalidInitialUnits) => ACPI_STATUS::BAD_PARAMETER,
        }
    }

    fn delete_semaphore(&self, Handle: ACPI_SEMAPHORE) -> ACPI_STATUS {
        match self.0.semaphore_delete(Handle.into()) {
            Ok(()) => ACPI_STATUS::OK,
            Err(SemaphoreDeleteError::InvalidHandle) => ACPI_STATUS::BAD_PARAMETER,
        }
    }

    fn wait_semaphore(&self, Handle: ACPI_SEMAPHORE, Units: u32, Timeout: u16) -> ACPI_STATUS {
        match self.0.semaphore_acquire(Handle.into(), Units, Timeout) {
            Ok(()) => ACPI_STATUS::OK,
            Err(SemaphoreAcquireError::InvalidHandle) => ACPI_STATUS::BAD_PARAMETER,
            Err(SemaphoreAcquireError::TimeoutElapsed) => ACPI_STATUS::TIME,
        }
    }

    fn signal_semaphore(&self, Handle: ACPI_SEMAPHORE, Units: u32) -> ACPI_STATUS {
        match self.0.semaphore_release(Handle.into(), Units) {
            Ok(()) => ACPI_STATUS::OK,
            Err(SemaphoreReleaseError::InvalidHandle) => ACPI_STATUS::BAD_PARAMETER,
            Err(SemaphoreReleaseError::LimitExceeded) => ACPI_STATUS::LIMIT,
        }
    }

    fn create_lock(&self, OutHandle: Option<NonNull<ACPI_SPINLOCK>>) -> ACPI_STATUS {
        let Some(OutHandle) = OutHandle else {
            return ACPI_STATUS::BAD_PARAMETER;
        };

        match self.0.spinlock_create() {
            Ok(handle) => {
                // Safety: 
                unsafe {
                    OutHandle.write(handle.into());
                }

                ACPI_STATUS::OK
            }

            Err(SpinlockCreateError::OutOfMemory) => ACPI_STATUS::NO_MEMORY,
        }
    }

    fn delete_lock(&self, Handle: ACPI_SPINLOCK) {
        todo!()
    }

    fn acquire_lock(&self, Handle: ACPI_SPINLOCK) -> ACPI_CPU_FLAGS {
        todo!()
    }

    fn release_lock(&self, Handle: ACPI_SPINLOCK, Flags: ACPI_CPU_FLAGS) {
        todo!()
    }
}

pub static OS_LAYER: spin::Once<&'static (dyn OsLayerInternal)> = spin::Once::new();

fn get_os_layer() -> &'static dyn OsLayerInternal {
    *OS_LAYER
        .get()
        .expect("ACPICA OS layer has not been installed")
}

#[link(name = "acpica_sys")]
unsafe extern "C" {
    pub fn AcpiInitializeSubsystem();

    #[cfg(target_arch = "x86")]
    pub fn AcpiFindRootPointer(table_address: &mut usize) -> ACPI_STATUS;
}
