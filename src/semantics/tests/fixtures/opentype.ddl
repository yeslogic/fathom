module opentype;

struct Unknown {};

Tag = Array 4 U8;

struct OffsetTableRecord {
    tag : U32Be,
    checksum : U32Be,
    offset : U32Be,
    length : U32Be,
};

struct OffsetTable {
    num_tables : U16Be,
    search_range : U16Be,
    entry_selector : U16Be,
    range_shift : U16Be,
    table_records : Array num_tables OffsetTableRecord,
};

struct TtcHeader1 {};
struct TtcHeader2 {};

struct TtcHeader {
    version : U32Be,
    body : match version {
        // FIXME: 0x00010000 => TtcHeader1,
        // FIXME: 0x00020000 => TtcHeader2,
        _ => Unknown,
    },
};

struct OpenType {
    tag : Tag,
    body : match tag {
        // FIXME: 0x00010000
        "OTTO" => OffsetTable,
        "ttcf" => TtcHeader,
        _ => Unknown,
    },
};
