error_set::error_set! {
    MutexAcquireError = {
        TimeoutElapsed
    };

    SemaphoreCreateError = {
        InvalidInitialUnits
    };

    SemaphoreDeleteError = {
        InvalidHandle
    };

    SemaphoreAcquireError = {
        InvalidHandle,
        TimeoutElapsed
    };

    SemaphoreReleaseError = {
        InvalidHandle,
        LimitExceeded
    };

    SpinlockCreateError = {
        OutOfMemory
    };

    InterruptCallbackInstallError = {
        InvalidInterrupt,
        AlreadyInstalled
    };

    InterruptCallbackRemoveError = {
        InvalidInterrupt,
        CallbackMismatch,
        NotInstalled
    };

    MemoryIoError = {
        InvalidAddress,
    };

    PortIoError = {
        InvalidAddress,
    };

    PciIoError = {
        InvalidAddress,
    };
}
