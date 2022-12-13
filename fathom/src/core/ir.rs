/// An interned string
type StringId = u16;

#[derive(Clone)]
pub enum Const {
    U8(u8),
    U16(u16),
    U32(u32),
    Pos(u64),
}

// TODO: better name
mod host {
    use super::{Const, StringId};

    #[derive(Clone)]
    pub enum Type {
        /// A built in type
        Prim(Prim),
        /// A custom type that lives in the context
        CustomType(usize),
    }

    // A host side custom type
    pub enum CustomType {
        Record(Record),
        Enum(Enum),
    }

    pub struct Record {
        pub name: StringId,
        pub fields: Vec<Field>,
    }

    // a match that can yield different types compiles into an enum
    pub struct Enum {
        pub name: StringId,
        pub variants: Vec<Variant>,
    }

    pub struct Variant {
        pub name: StringId,
        pub data: Option<Type>,
    }

    pub struct Field {
        pub name: StringId,
        pub host_type: Type, // in theory there could also be write: WriteExpr
    }

    /// Primitive types
    #[derive(Clone)]
    pub enum Prim {
        S16,
        U8,
        U16,
        U32,
        Pos,
        // An array with length and element type
        // The length can be another item or a const
        Array(Expr, Box<Type>), // Perhaps array with const length and array with non-const length should be separated
    }

    // Primitive functions... is it sensible to seperate these from Prim?
    #[derive(Clone)]
    pub enum PrimFn {
        U16Sub,
    }

    #[derive(Clone)]
    pub enum Expr {
        /// Item in the context
        Item(usize),
        Const(Const),
        // Prim(Prim),
        PrimFn(PrimFn),

        /// A function literal
        Func(Function),
        // TODO: Does there need to be a counterpart to this in the `format` module?
        /// Uncurried function application (fn, arguments)
        App(Box<Expr>, Vec<Expr>),
    }

    #[derive(Clone)]
    pub struct Function {
        /// Argument names
        arguments: Vec<StringId>,
        /// Body expression
        body: Box<Expr>,
    }
}

// TODO: better name
mod format {
    use super::{host, Const, StringId};

    pub struct Module {
        pub definitions: Vec<Def>,
    }

    pub struct Def {
        pub name: StringId,
        pub expr: Item,
    }

    pub enum Item {
        Format(Format),
        /// A let expression
        ///
        /// name, def expression, body expression
        Let(StringId, Box<Item>, Box<Item>),
    }

    pub struct Format {
        pub params: Vec<Param>,
        pub fields: Vec<Field>,
    }

    pub struct Param {
        pub name: StringId,
        pub host_type: host::Type,
    }

    pub struct Field {
        pub name: StringId,
        pub host_type: host::Type,
        pub read: ReadExpr,
    }

    pub struct ReadFn {
        pub exprs: Vec<ReadExpr>,
    }

    pub enum ReadExpr {
        Prim(ReadPrim),
        /// A custom type that lives in the context
        CustomType(usize),
        /// A match expression, cond, branches, default?
        Match(host::Expr, Vec<Branch>),
    }

    pub struct Branch {
        pub pattern: Pattern,
        pub expr: Box<ReadExpr>,
    }

    pub enum Pattern {
        Var(StringId),
        Const(Const),
    }

    pub enum ReadPrim {
        S16Be,
        S16Le,
        U8,
        U16Be,
        U16Le,
        U32Be,
        U32Le,
        Array(host::Expr, Box<ReadExpr>),
        StreamPos,
    }

    // impl Format {
    //     fn repr(&self) -> host::CustomType {
    //         let fields = self
    //             .fields
    //             .iter()
    //             .map(|field| host::Field {
    //                 name: field.name,
    //                 host_type: field.host_type.clone(),
    //             })
    //             .collect();
    //         host::CustomType::Record(host::Record {
    //             name: self.name,
    //             fields,
    //         })
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    use format::*;
    use host::Expr;

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
            params: vec![],
            fields: vec![Field {
                name: 1,
                host_type: host::Type::Prim(host::Prim::U16),
                read: ReadExpr::Prim(ReadPrim::U16Be),
            }],
        };

        let item = Item::Format(format);
        let def = Def {
            name: 1,
            expr: item,
        };
        let module = Module {
            definitions: vec![def],
        };
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
            params: vec![],
            fields: vec![
                Field {
                    name: 1, // table_tag
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 2, // checksum
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 3, // offset
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 3, // length
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
            ],
        };

        let item = Item::Format(format);
        let def = Def {
            name: 2,
            expr: item,
        };
        let module = Module {
            definitions: vec![def],
        };
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
            type host::HostType = Self;

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
            params: vec![],
            fields: vec![
                Field {
                    name: 1, // num_tables
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 2, // sfnt_version
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 3, // search_range
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 4, // entry_selector
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 5, // range_shift
                    host_type: host::Type::Prim(host::Prim::U32),
                    read: ReadExpr::Prim(ReadPrim::U32Be),
                },
                Field {
                    name: 6, // table_records
                    host_type: host::Type::Prim(host::Prim::Array(
                        Expr::Item(5 /* num_tables */),
                        // This one is referring to a custom type in the host env
                        // Or is it just referring to the format?
                        Box::new(host::Type::CustomType(0)),
                    )),
                    read: ReadExpr::Prim(ReadPrim::Array(
                        Expr::Item(5 /* num_tables */),
                        // This one is referring to a custom type in the "format" env
                        Box::new(ReadExpr::CustomType(0)),
                    )),
                },
            ],
        };

        let item = Item::Format(offset_table);
        let def = Def {
            name: 3,
            expr: item,
        };
        let module = Module {
            definitions: vec![def],
        };
    }

    #[test]
    fn test_match_and_fn_app() {
        /*

        // cut-down kern version 0 sub-table
        def subtable0 = {
            length <- u16be,
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
        let u16_sub_length_6 = Expr::App(
            Box::new(Expr::PrimFn(host::PrimFn::U16Sub)),
            vec![Expr::Item(0 /* length */), Expr::Const(Const::U16(6))],
        );
        // represents the item storing the value resulting from evaluating the expression
        let u16_sub_length_6_item = 6;
        let host_type_8 = host::CustomType::Enum(host::Enum {
            name: 3, // Subtable0Data
            variants: vec![
                host::Variant {
                    name: 4, // how to name variants?
                    data: Some(host::Type::CustomType(1 /* subtable_format0 */)),
                },
                host::Variant {
                    name: 5,
                    data: Some(host::Type::Prim(host::Prim::Array(
                        u16_sub_length_6,
                        Box::new(host::Type::Prim(host::Prim::U8)),
                    ))),
                },
            ],
        });
        let format = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: 1, // format
                    host_type: host::Type::Prim(host::Prim::U8),
                    read: ReadExpr::Prim(ReadPrim::U8),
                },
                Field {
                    name: 2, // data
                    host_type: host::Type::CustomType(8 /* host_type_8 in context */),
                    read: ReadExpr::Match(
                        Expr::Item(1), /* format */
                        vec![
                            Branch {
                                pattern: Pattern::Const(Const::U8(0)),
                                expr: Box::new(ReadExpr::CustomType(1 /* subtable_format0 */)),
                            },
                            Branch {
                                pattern: Pattern::Var(0 /* _ */),
                                expr: Box::new(ReadExpr::Prim(ReadPrim::Array(
                                    Expr::Item(u16_sub_length_6_item),
                                    Box::new(ReadExpr::Prim(ReadPrim::U8)),
                                ))),
                            },
                        ],
                    ),
                },
            ],
        };

        let item = Item::Format(format);
        let def = Def {
            name: 4,
            expr: item,
        };
        let module = Module {
            definitions: vec![def],
        };
    }

    #[test]
    fn test_let() {
        /*

        def subtable_format0 = (
            let kerning_pair = {
                left <- u16be,
                right <- u16be,
            };

            {
                num_pairs <- u16be,
                pairs <- array16 num_pairs kerning_pair,
            }
        );
         */

        let kerning_pair = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: 1, // left
                    host_type: host::Type::Prim(host::Prim::U16),
                    read: ReadExpr::Prim(ReadPrim::U16Be),
                },
                Field {
                    name: 2, // right
                    host_type: host::Type::Prim(host::Prim::U16),
                    read: ReadExpr::Prim(ReadPrim::U16Be),
                },
            ],
        };

        let format = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: 3, // num_pairs
                    host_type: host::Type::Prim(host::Prim::U8),
                    read: ReadExpr::Prim(ReadPrim::U8),
                },
                Field {
                    name: 4, // pairs
                    host_type: host::Type::Prim(host::Prim::Array(
                        Expr::Item(3 /* num_pairs */),
                        Box::new(host::Type::CustomType(0)), // kerning_pair
                    )),
                    read: ReadExpr::Prim(ReadPrim::Array(
                        Expr::Item(3 /* num_pairs */),
                        // This one is referring to a custom type in the "format" env
                        Box::new(ReadExpr::CustomType(0)), // kerning_pair
                    )),
                },
            ],
        };

        let item = Item::Let(
            5, /* subtable_format0 */
            Box::new(Item::Format(kerning_pair)),
            Box::new(Item::Format(format)),
        );
        let def = Def {
            name: 5,
            expr: item,
        };
        let module = Module {
            definitions: vec![def],
        };
    }

    #[test]
    fn test_pos() {
        /*
        def kerning_pair = {
            left <- stream_pos,
            right <- u16be,
        };
         */

        let kerning_pair = Format {
            params: vec![],
            fields: vec![
                Field {
                    name: 1, // left
                    host_type: host::Type::Prim(host::Prim::Pos),
                    read: ReadExpr::Prim(ReadPrim::StreamPos),
                },
                Field {
                    name: 2, // right
                    host_type: host::Type::Prim(host::Prim::U16),
                    read: ReadExpr::Prim(ReadPrim::U16Be),
                },
            ],
        };
    }

    #[test]
    fn test_format_params() {
        /*

        def offset16 = fun (base : Pos) => fun (format : Format) => {
            offset <- u16be,
            link <- match offset {
                0 => empty,
                _ => link (pos_add_u16 base offset) format, // TODO: Use an option type?
            },
        };


        def example = fun (table_start : Pos) => {
               /// Offset from beginning of this subtable to right-hand class table.
               right_class_table <- offset16 table_start u32be,
        };
        */

        // How should offset16 be represented

        let format = Format {
            params: vec![Param {name: 3, host_type: host::Type::Prim(host::Prim::Pos)}],
            fields: vec![
                Field {
                    name: 1, // right_class_table
                    host_type: host::Type::Prim(host::Prim::U16),
                    read: ReadExpr::Prim(ReadPrim::U16Be),
                },
            ],
        };


        let item = Item::Format(format);
        let def = Def {
            name: 5,
            expr: item,
        };
        let module = Module {
            definitions: vec![def],
        };
    }
}

