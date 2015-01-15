
// @pragma big_endian;

// @root
PNG: struct {
    signature: byte[8], // FIXME = [137, 80, 78, 71, 13, 10, 26, 10],
    chunks: Chunk*
}

// FIXME must begin with IHDR and end with IEND
// FIXME chunk ordering must follow certain constraints

Chunk: struct {
    length: uint32, // FIXME < 2^31
    type: uint32, // FIXME [a-zA-Z]{4} also includes chunk property bits
    data: byte[length], // FIXME chunk data
    crc: uint32 // FIXME derived from type+data bytes
}

IHDR: struct {
    width: uint32, // FIXME <> 0
    height: uint32, // FIXME <> 0
    bit_depth: byte = 1 | 2 | 4 | 8 | 16, // FIXME depends on color_type
    color_type: byte = 0 | 2 | 3 | 4 | 6, // FIXME depends on bit_depth
    compression_method: byte = 0,
    filter_method: byte = 0,
    interlace_method: byte = 0 | 1
}

PLTE: struct {
    // FIXME number of entries == chunk length / 3, from 1..256
    entries: struct* {
	red: byte,
	green: byte,
	blue: byte
    }
}

IDAT: struct {
    // FIXME can be multiple consecutive IDAT chunks, concatenated
}

IEND: struct {
    // empty
}

// FIXME text chunks are interesting

