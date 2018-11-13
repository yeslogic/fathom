module gif;

// https://www.w3.org/Graphics/GIF/spec-gif89a.txt
// http://formats.kaitai.io/gif/

struct Gif {
    header : Header,
    logical_screen : LogicalScreenDescriptor,
    // TODO:
    // global_color_table : if logical_screen.has_color_table {
    //     GlobalColorTable logical_screen.color_table_size
    // },
    // blocks : Array _ Block,
};

/// Logical Screen Descriptor
///
/// [GIF89a Specification: Section 18](https://www.w3.org/Graphics/GIF/spec-gif89a.txt)
struct LogicalScreenDescriptor {
    image_width : U16Le,
    image_height : U16Le,
    flags : U8, // TODO: Interpret fields
    bg_color_index : U8,
    pixel_aspect_ratio : U8,
};

/// Header
///
/// [GIF89a Specification: Section 17](https://www.w3.org/Graphics/GIF/spec-gif89a.txt)
struct Header {
    magic : Array 3 U8, // TODO: (= "GIF" : Ascii),
    version : Array 3 U8,
};

/// Global Color Table
///
/// [GIF89a Specification: Section 19](https://www.w3.org/Graphics/GIF/spec-gif89a.txt)
GlobalColorTable : U64 -> Type;
GlobalColorTable len = Array len ColorTableEntry;

/// Global Color Table Entry
///
/// [GIF89a Specification: Section 19](https://www.w3.org/Graphics/GIF/spec-gif89a.txt)
struct ColorTableEntry {
    red : U8,
    green : U8,
    blue : U8,
};

/// Block Terminator
///
/// [GIF89a Specification: Section 16](https://www.w3.org/Graphics/GIF/spec-gif89a.txt)
struct BlockTerminator {
    value : U8, // TODO: (= 0x00 : U8),
};
