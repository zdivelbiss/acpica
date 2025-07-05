#[link(name = "acpica")]
unsafe extern "C" {
    pub fn AcpiInitializeSubsystem();
}
