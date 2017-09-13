
// FIXME endianness differs

// @root
ELF: struct {
    magic: byte[4], // FIXME = [0x7F, 'E', 'L', 'F'],
    class: byte = 1 | 2, // 32-bit or 64-bit
    data: byte = 1 | 2, // little or big endian
    version1: byte, // FIXME = 1 ?
    os_abi: byte, // FIXME
    abi_version: byte, // FIXME unused on Linux
    pad: byte[7],
    type: uint16 = 1 | 2 | 3 | 4, // relocatable, executable, shared, core
    machine: uint16, // target instruction set architecture
    version2: byte // FIXME version again???
}
