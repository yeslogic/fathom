/// An interned string
type StringId = u16;

struct Module {
    items: Vec<Item>,
}

enum Item {
    Format(Format),
}

struct Format {
    name: StringId,
    fields: Vec<Field>,
}

struct Field {
    name: StringId,
    host_type: HostType,
    read: ReadExpr,
}

#[derive(Clone)]
enum HostType {
    /// A built in type
    Prim(HostPrim),
    /// A custom type that lives in the context
    CustomType(usize),
}

// A host side custom type
enum CustomType {
    Record(Record),
    Enum(Enum),
}

struct Record {
    name: StringId,
    fields: Vec<HostField>,
}

// a match that can yield different types compiles into an enum
struct Enum {
    name: StringId,
    variants: Vec<Variant>,
}

struct Variant {
    name: StringId,
    data: Option<HostType>,
}

struct HostField {
    name: StringId,
    host_type: HostType, // in theory there could also be write: WriteExpr
}

#[derive(Clone)]
enum HostPrim {
    S16,
    U8,
    U16,
    U32,
    Pos,
    // An array with length and element type
    // The length can be another item or a const
    Array(Val, Box<HostType>), // Perhaps array with const length and array with non-const length should be separated
}

#[derive(Clone)]
enum Val {
    /// Item in the context
    Item(usize),
    Const(Const),
}

#[derive(Clone)]
enum Const {
    U8(u8),
    U16(u16),
    U32(u32),
    Pos(u64),
}

struct ReadFn {
    exprs: Vec<ReadExpr>,
}

enum ReadExpr {
    Prim(ReadPrim),
    /// A custom type that lives in the context
    CustomType(usize),
    /// A match expression, cond, branches, default?
    Match(Val, Vec<Branch>),
}

struct Branch {
    pattern: Pattern,
    expr: Box<ReadExpr>,
}

enum Pattern {
    Var(StringId),
    Const(Const),
}

enum ReadPrim {
    S16Be,
    S16Le,
    U8,
    U16Be,
    U16Le,
    U32Be,
    U32Le,
    Array(Val, Box<ReadExpr>),
}

impl Format {
    fn repr(&self) -> CustomType {
        let fields = self
            .fields
            .iter()
            .map(|field| HostField {
                name: field.name,
                host_type: field.host_type.clone(),
            })
            .collect();
        CustomType::Record(Record {
            name: self.name,
            fields,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_f2dot14() {
        // pub struct F2Dot14(u16);
        // impl<'a> ReadFrom<'a> for F2Dot14 {
        //     type ReadType = U16Be;
        //     fn from(value: u16) -> Self {
        //         F2Dot14(value)
        //     }
        // }
        //
        // def f2dot14 : Format = s16be;
        let format = Format {
            name: 1,
            fields: vec![Field {
                name: 1,
                host_type: HostType::Prim(HostPrim::U16),
                read: ReadExpr::Prim(ReadPrim::U16Be),
            }],
        };

        let item = Item::Format(format);
        let module = Module { items: vec![item] };
    }

    #[test]
    fn test_table_record() {
        /*

        def table_record = {
            /// Table identifier.
            table_id <- tag,
            /// CheckSum for this table.
            ///
            /// ## References
            ///
            /// - [Microsoft's OpenType Spec: Calculating Checksums](https://docs.microsoft.com/en-us/typography/opentype/spec/otff#calculating-checksums)
            checksum <- u32be,
            /// Offset from the beginning of the TrueType font file.
            offset <- u32be,
            /// Length of this table.
            length <- u32be,
        };

        pub struct TableRecord {
            pub table_tag: u32,
            pub checksum: u32,
            pub offset: u32,
            pub length: u32,
        }

        impl<'a> ReadFrom<'a> for TableRecord {
            type ReadType = ((U32Be, U32Be), (U32Be, U32Be));
            fn from(((table_tag, checksum), (offset, length)): ((u32, u32), (u32, u32))) -> Self {
                TableRecord {
                    table_tag,
                    checksum,
                    offset,
                    length,
                }
            }
        }
         */

        // In order to be able to generate a ReadFrom impl we need to know/generate the ReadType as well as the host representation of that
        // Each field will eventually need a name too. Although in this case the tuple fields can be accessed by index
        let format = Format {
            name: 2,
            fields: vec![
                Field {
                    name: 1, // table_tag
                    host_type: HostType::Prim(HostPrim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 2, // checksum
                    host_type: HostType::Prim(HostPrim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 3, // offset
                    host_type: HostType::Prim(HostPrim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 3, // length
                    host_type: HostType::Prim(HostPrim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
            ],
        };

        let item = Item::Format(format);
        let module = Module { items: vec![item] };
    }

    #[test]
    fn offset_table() {
        /*

        pub struct OffsetTable<'a> {
            pub sfnt_version: u32,
            pub search_range: u16,
            pub entry_selector: u16,
            pub range_shift: u16,
            pub table_records: ReadArray<'a, TableRecord>,
        }

        impl<'a> ReadBinary<'a> for OffsetTable<'a> {
            type HostType = Self;

            fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
                let sfnt_version = ctxt.read_u32be()?;
                match sfnt_version {
                    TTF_MAGIC | CFF_MAGIC => {
                        let num_tables = ctxt.read_u16be()?;
                        let search_range = ctxt.read_u16be()?;
                        let entry_selector = ctxt.read_u16be()?;
                        let range_shift = ctxt.read_u16be()?;
                        let table_records = ctxt.read_array::<TableRecord>(usize::from(num_tables))?;
                        Ok(OffsetTable {
                            sfnt_version,
                            search_range,
                            entry_selector,
                            range_shift,
                            table_records,
                        })
                    }
                    _ => Err(ParseError::BadVersion),
                }
            }
        }
         */

        // Assume table_record above is in the context at index 0

        let offset_table = Format {
            name: 3,
            fields: vec![
                Field {
                    name: 1, // num_tables
                    host_type: HostType::Prim(HostPrim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 2, // sfnt_version
                    host_type: HostType::Prim(HostPrim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 3, // search_range
                    host_type: HostType::Prim(HostPrim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 4, // entry_selector
                    host_type: HostType::Prim(HostPrim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 5, // range_shift
                    host_type: HostType::Prim(HostPrim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 6, // table_records
                    host_type: HostType::Prim(HostPrim::Array(
                        Val::Item(5 /* num_tables */),
                        // This one is referring to a custom type in the host env
                        // Or is it just referring to the format?
                        Box::new(HostType::CustomType(0)),
                    )),
                    read: ReadExpr::Prim(ReadPrim::Array(
                        Val::Item(5 /* num_tables */),
                        // This one is referring to a custom type in the "format" env
                        Box::new(ReadExpr::CustomType(0)),
                    )),
                },
            ],
        };

        let item = Item::Format(offset_table);
        let module = Module { items: vec![item] };
    }

    #[test]
    fn test_match() {
        /*

        // cut-down kern version 0 sub-table
        def subtable0 = {
            /// The start of this sub-table
            format <- u8,
            data <- match format {
                0 => subtable_format0,
                // 2 => limit16 (u16_sub length 8) (subtable_format2 table_start),
                // Unsupported format, read the raw bytes so that we stay synchronised with other sub-tables
                _ => array16 (u16_sub length 6) u8,
            }
        };

        def subtable_format0 = {
            example <- u16be
        };
         */
        let fixme_u16_sub_length_6 = 0;
        let host_type_8 = CustomType::Enum(Enum {
            name: 3, // Subtable0Data
            variants: vec![
                Variant {
                    name: 4, // how to name variants?
                    data: Some(HostType::CustomType(1 /* subtable_format0 */)),
                },
                Variant {
                    name: 5,
                    data: Some(HostType::Prim(HostPrim::Array(
                        Val::Item(fixme_u16_sub_length_6),
                        Box::new(HostType::Prim(HostPrim::U8)),
                    ))),
                },
            ],
        });
        let format = Format {
            name: 4,
            fields: vec![
                Field {
                    name: 1, // format
                    host_type: HostType::Prim(HostPrim::U8),
                    read: ReadExpr::Prim(ReadPrim::U8),
                },
                Field {
                    name: 2, // data
                    host_type: HostType::CustomType(8 /* host_type_8 in context */),
                    read: ReadExpr::Match(
                        Val::Item(1), /* format */
                        vec![
                            Branch {
                                pattern: Pattern::Const(Const::U8(0)),
                                expr: Box::new(ReadExpr::CustomType(1 /* subtable_format0 */)),
                            },
                            Branch {
                                pattern: Pattern::Var(0 /* _ */),
                                expr: Box::new(ReadExpr::Prim(ReadPrim::Array(
                                    Val::Item(fixme_u16_sub_length_6),
                                    Box::new(ReadExpr::Prim(ReadPrim::U8)),
                                ))),
                            },
                        ],
                    ),
                },
            ],
        };

        let item = Item::Format(format);
        let module = Module { items: vec![item] };
    }
}
