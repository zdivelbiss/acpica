error_set::error_set! {
    MutexAcquireError = {
        TimeoutElapsed
    };

    SemaphoreError =
        SemaphoreCreateError
        || SemaphoreDeleteError
        || SemaphoreAcquireError
        || SemaphoreReleaseError;

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

    SpinlockDeleteError = {
        InvalidHandle
    };
}
