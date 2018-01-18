import prelude (..);

/// Binary data
BinData = struct {
    /// The number of bytes stored
    len : i32be,
    /// Binary data subtype
    ///
    /// - `0x00`: Generic binary subtype
    /// - `0x01`: Function
    /// - `0x02`: Binary (Old)
    /// - `0x03`: UUID (Old)
    /// - `0x04`: UUID
    /// - `0x05`: MD5
    /// - `0x80`: User defined
    subtype : u8,
    /// The binary data
    data: [u8; len as u64],
};

ObjectId = struct {
    epoch_time: i32be,
    machine_id: u24be,
    process_id: u16be,
    counter: u24be,
};

Element = struct {
    elem_type : u8,
    content: cond {
        /// 64-bit floating point
        double : elem_type == 0x01u8 => f64le,
        // TODO: strings
        // /// UTF8 string
        // string : elem_type == 0x02u8 => String,
        // FIXME: mutual recursion
        // /// Embedded document
        // document : elem_type == 0x03u8 => Document,
        // /// Array
        // array : elem_type == 0x04u8 => Array,
        /// Binary data
        bin_data : elem_type == 0x05u8 => BinData,
        /// Undefined (value) - deprecated
        undefined : elem_type == 0x06u8 => empty,
        /// ObjectId
        object_id : elem_type == 0x07u8 => ObjectId,
        /// Boolean
        ///
        /// - `0x00`: false
        /// - `0x01`: true
        boolean : elem_type == 0x08u8 => u8,
        /// UTC datetime
        utc_datetime : elem_type == 0x09u8 => i64le,
        /// Null value
        null : elem_type == 0x0au8 => empty,
        // TODO: Regular Expression
        // reg_ex : elem_type == 0x0bu8 => RegEx,
        // TODO: DBPointer — Deprecated
        // db_pointer : elem_type == 0x0cu8 => DbPointer,
        // TODO: String
        // javascript : elem_type == 0x0du8 => String,
        // TODO: Symbol. Deprecated
        // symbol : elem_type == 0x0eu8 => Symbol,
        // TODO: JavaScript code w/ scope
        // code_with_scope : elem_type == 0x0fu8 => CodeWithScope,
        /// 32-bit integer
        int : elem_type == 0x10u8 => i32le,
        /// Timestamp
        timestamp : elem_type == 0x11u8 => u64le,
        /// 64-bit integer
        long : elem_type == 0x12u8 => i64le,
        // TODO: 128-bit decimal floating point
        // number_decimal : elem_type == 0x13u8 => NumberDecimal,
    },
};

/// BSON Document
///
/// <http://bsonspec.org/spec.html>
Document = struct {
    /// Total number of bytes comprising the document.
    len : i32be,
    /// The elements that make up this document
    fields : [Element; 1u8],
    // /// Terminating byte signalling the end of the file
    // terminator : u8 where u8 == 0x00,
};
