#![allow(
    non_snake_case,
    non_camel_case_types,
    dead_code,
    clippy::upper_case_acronyms
)]

use crate::get_os_layer;
use core::{ffi::c_void, ptr::NonNull};

/// The width of all physical addresses is fixed at 64 bits, regardless of the platform or operating
/// system. Logical addresses (pointers) remain the natural width of the machine (i.e. 32-bit pointers on
/// 32-bit machines, 64-bit pointers on 64-bit machines.) This allows for a full 64-bit address space on
/// 64-bit machines as well as “extended” physical addresses (above 4Gbytes) on 32-bit machines.
pub type ACPI_PHYSICAL_ADDRESS = u64;

/// Similar to [`ACPI_PHYSICAL_ADDRESS`], except it is used for I/O addresses.
pub type ACPI_IO_ADDRESS = u64;

/// This data type is 32 bits or 64 bits depending on the platform. It is used in leiu of size_t,
/// which cannot be guaranteed to be available.
pub type ACPI_SIZE = usize;

/// This type is defined as a UINT64 and is returned by the [`AcpiOsGetThreadId`] interface.
/// There is no standard "thread_id" across operating systems or even the various UNIX systems. Since
/// ACPICA only needs the thread ID as a unique thread identifier, it uses a UINT64 as the only
/// common data type – a UINT64 will accommodate any type of pointer or any type of integer. It is up
/// to the host-dependent OSL to cast the native thread ID type to a UINT64 (in [`AcpiOsGetThreadId`])
/// before returning the value to ACPICA.
pub type ACPI_THREAD_ID = u64;

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
    get_os_layer().get_root_address()
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsMapMemory(
    PhysicalAddress: ACPI_PHYSICAL_ADDRESS,
    Length: ACPI_SIZE,
) -> *mut c_void {
    get_os_layer()
        .map_memory(PhysicalAddress, Length)
        .map(NonNull::cast)
        .map(NonNull::as_ptr)
        .unwrap_or_default()
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsUnmapMemory(LogicalAddress: *mut c_void, Length: ACPI_SIZE) {
    get_os_layer().unmap_memory(
        NonNull::new(LogicalAddress)
            .expect("ACPICA provided a null pointer for `AcpiOsUnmapMemory`")
            .cast(),
        Length,
    );
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsGetPhysicalAddress(
    LogicalAddress: *mut c_void,
    PhysicalAddress: &mut ACPI_PHYSICAL_ADDRESS,
) -> ACPI_STATUS {
    let Some(LogicalAddress) = NonNull::new(LogicalAddress).map(NonNull::cast) else {
        return ACPI_STATUS::BAD_PARAMETER;
    };

    *PhysicalAddress = get_os_layer().get_physical_address(LogicalAddress);

    ACPI_STATUS::OK
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsAllocate(Size: ACPI_SIZE) -> *mut c_void {
    get_os_layer()
        .allocate(Size)
        .map(NonNull::cast)
        .map(NonNull::as_ptr)
        .unwrap_or_default()
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsFree(Memory: *mut c_void) {
    get_os_layer().deallocate(
        NonNull::new(Memory)
            .expect("ACPICA provided a null pointer for `AcpiOsFree`")
            .cast(),
    );
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsReadable(Memory: *mut c_void, Length: ACPI_SIZE) -> bool {
    get_os_layer().is_memory_readable(
        NonNull::new(Memory)
            .expect("ACPICA provided a null pointer for `AcpiOsReadable`")
            .cast(),
        Length,
    )
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsWritable(Memory: *mut c_void, Length: ACPI_SIZE) -> bool {
    get_os_layer().is_memory_writable(
        NonNull::new(Memory)
            .expect("ACPICA provided a null pointer for `AcpiOsWritable`")
            .cast(),
        Length,
    )
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsGetThreadId() -> ACPI_THREAD_ID {
    let thread_id = u64::from(get_os_layer().get_thread_id());

    assert!(
        thread_id != 0xFFFF_FFFF_FFFF_FFFF,
        "max thread ID is reserved"
    );

    thread_id
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsExecute(
    _Type: ACPI_EXECUTE_TYPE,
    Function: Option<ACPI_OSD_EXEC_CALLBACK>,
    Context: *mut c_void,
) -> ACPI_STATUS {
    let Some(Function) = Function else {
        return ACPI_STATUS::BAD_PARAMETER;
    };

    get_os_layer().execute_callback(Function, Context);

    ACPI_STATUS::OK
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsSleep(Milliseconds: u64) {
    get_os_layer().sleep_wait_ms(Milliseconds);
}

#[unsafe(no_mangle)]
extern "C" fn AcpiOsStall(Microseconds: u32) {
    get_os_layer().spin_wait_us(Microseconds);
}

#[link(name = "acpica_sys")]
unsafe extern "C" {
    pub fn AcpiInitializeSubsystem();

    #[cfg(target_arch = "x86")]
    pub fn AcpiFindRootPointer(table_address: &mut usize) -> ACPI_STATUS;
}

#[test]
fn test_init() {
    // Safety: `acpica_sys` exports symbol.
    unsafe {
        AcpiInitializeSubsystem();
    }
}
