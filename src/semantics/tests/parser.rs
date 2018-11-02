use byteorder::{BigEndian, LittleEndian, WriteBytesExt};
use std::io::{Cursor, Write};
use std::mem;

use semantics::parser::{self, ParseError, Value};
use syntax::Label;

use super::*;

fn label(name: &str) -> Label {
    Label(name.to_owned())
}

#[test]
fn silly_root() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module silly;

        Data : U32 -> Type;

        Data len = Array len U32Be;
        struct Silly {
            len : U16Be,
            data : Data len,
        };
    "#;

    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u16::<BigEndian>(3).unwrap(); // len
        given_bytes.write_u32::<BigEndian>(1).unwrap(); // data[0]
        given_bytes.write_u32::<BigEndian>(3).unwrap(); // data[1]
        given_bytes.write_u32::<BigEndian>(6).unwrap(); // data[2]

        Cursor::new(given_bytes)
    };

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Silly"), &module, &mut given_bytes).unwrap(),
        hashmap!{
            0 => Value::Struct(vec![
                (label("len"), Value::U16(3)),
                (
                    label("data"),
                    Value::Array(vec![Value::U32(1), Value::U32(3), Value::U32(6)]),
                ),
            ]),
        },
    );
}

#[test]
fn missing_root() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module silly;

        Data : U32 -> Type;
        Data len = Array len U32Be;
    "#;

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    let mut given_bytes = Cursor::new(vec![]);

    let parsed_value = parser::parse_module(
        &context,
        &Label("Silly".to_owned()),
        &module,
        &mut given_bytes,
    );

    match parsed_value {
        Ok(_) => panic!("expected error"),
        Err(ParseError::MissingRoot { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
    }
}

#[test]
fn pos() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module pos_test;

        struct PosEntry {
            pad : U32Be,
            end: Pos,
        };

        struct PosTest {
            start : Pos,
            data : Array 3 PosEntry,
            end : Pos,
        };
    "#;

    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u32::<BigEndian>(0).unwrap(); // data[0]
        given_bytes.write_u32::<BigEndian>(0).unwrap(); // data[1]
        given_bytes.write_u32::<BigEndian>(0).unwrap(); // data[2]

        Cursor::new(given_bytes)
    };

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes).unwrap(),
        hashmap!{
            0 => Value::Struct(vec![
                (label("start"), Value::Pos(0)),
                (
                    label("data"),
                    Value::Array(vec![
                        Value::Struct(vec![
                            (label("pad"), Value::U32(0)),
                            (label("end"), Value::Pos(mem::size_of::<u32>() as u64 * 1)),
                        ]),
                        Value::Struct(vec![
                            (label("pad"), Value::U32(0)),
                            (label("end"), Value::Pos(mem::size_of::<u32>() as u64 * 2)),
                        ]),
                        Value::Struct(vec![
                            (label("pad"), Value::U32(0)),
                            (label("end"), Value::Pos(mem::size_of::<u32>() as u64 * 3)),
                        ]),
                    ]),
                ),
                (label("end"), Value::Pos(mem::size_of::<u32>() as u64 * 3),),
            ]),
        },
    );
}

#[test]
fn offset() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module offset_test;

        struct PosTest {
            magic : U32Be,
            data_start : Pos,
            data : Array 3 (Offset16Be data_start U8),
        };
    "#;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u32::<BigEndian>(0x123).unwrap(); // magic
        given_bytes.write_u16::<BigEndian>(mem::size_of::<[u16; 3]>() as u16 + 2).unwrap(); // data[0]
        given_bytes.write_u16::<BigEndian>(mem::size_of::<[u16; 3]>() as u16 + 1).unwrap(); // data[1]
        given_bytes.write_u16::<BigEndian>(mem::size_of::<[u16; 3]>() as u16 + 0).unwrap(); // data[2]
        given_bytes.write_u8(25).unwrap(); // *data[2]
        given_bytes.write_u8(30).unwrap(); // *data[1]
        given_bytes.write_u8(35).unwrap(); // *data[0]

        Cursor::new(given_bytes)
    };

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes).unwrap(),
        hashmap!{
            0 => Value::Struct(vec![
                (label("magic"), Value::U32(0x123)),
                (
                    label("data_start"),
                    Value::Pos(mem::size_of::<u32>() as u64)
                ),
                (
                    label("data"),
                    Value::Array(vec![
                        Value::Pos((mem::size_of::<u32>() + mem::size_of::<[u16; 3]>()) as u64 + 2),
                        Value::Pos((mem::size_of::<u32>() + mem::size_of::<[u16; 3]>()) as u64 + 1),
                        Value::Pos((mem::size_of::<u32>() + mem::size_of::<[u16; 3]>()) as u64 + 0),
                    ]),
                ),
            ]),
            (mem::size_of::<u32>() + mem::size_of::<[u16; 3]>()) as u64 + 0 => Value::U8(25),
            (mem::size_of::<u32>() + mem::size_of::<[u16; 3]>()) as u64 + 1 => Value::U8(30),
            (mem::size_of::<u32>() + mem::size_of::<[u16; 3]>()) as u64 + 2 => Value::U8(35),
        },
    );
}

#[test]
fn offset_same_pos() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module offset_test;

        struct PosTest {
            start : Pos,
            offset1 : Offset16Be start U8,
            offset2 : Offset16Be start U8,
        };
    "#;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u16::<BigEndian>(mem::size_of::<[u16; 2]>() as u16).unwrap(); // offset1
        given_bytes.write_u16::<BigEndian>(mem::size_of::<[u16; 2]>() as u16).unwrap(); // offset2
        given_bytes.write_u8(25).unwrap(); // *offset1, *offset2

        Cursor::new(given_bytes)
    };

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes).unwrap(),
        hashmap!{
            0 => Value::Struct(vec![
                (label("start"), Value::Pos(0)),
                (label("offset1"), Value::Pos(mem::size_of::<[u16; 2]>() as u64)),
                (label("offset2"), Value::Pos(mem::size_of::<[u16; 2]>() as u64)),
            ]),
            mem::size_of::<[u16; 2]>() as u64 => Value::U8(25),
        },
    );
}

#[test]
fn offset_same_pos_different_tys() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module offset_test;

        struct PosTest {
            start : Pos,
            offset1 : Offset16Be start U8,
            offset2 : Offset16Be start S8,
        };
    "#;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u16::<BigEndian>(mem::size_of::<[u16; 2]>() as u16).unwrap(); // offset1
        given_bytes.write_u16::<BigEndian>(mem::size_of::<[u16; 2]>() as u16).unwrap(); // offset2
        given_bytes.write_u8(25).unwrap(); // *offset1, *offset2

        Cursor::new(given_bytes)
    };

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    let parsed_value = parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes);
    match parsed_value {
        Ok(_) => panic!("expected error"),
        Err(ParseError::OffsetPointedToDifferentTypes(_, _)) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
    }
}

#[test]
fn offset_pos() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module offset_test;

        struct PosTest {
            magic : U32Be,

            start : Pos,
            offset0 : U16Be,
            offset1 : U16Be,
            offset2 : U16Be,

            pos0 : OffsetPos start offset0 U8,
            pos1 : OffsetPos start offset1 U8,
            pos2 : OffsetPos start offset2 U8,
        };
    "#;

    let start = mem::size_of::<u32>() as u64;
    let offset0 = mem::size_of::<[u16; 3]>() as u16 + 2;
    let offset1 = mem::size_of::<[u16; 3]>() as u16 + 1;
    let offset2 = mem::size_of::<[u16; 3]>() as u16 + 0;
    let pos0 = start + offset0 as u64;
    let pos1 = start + offset1 as u64;
    let pos2 = start + offset2 as u64;

    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u32::<BigEndian>(0x123).unwrap(); // magic

        given_bytes.write_u16::<BigEndian>(offset0).unwrap(); // offset0
        given_bytes.write_u16::<BigEndian>(offset1).unwrap(); // offset1
        given_bytes.write_u16::<BigEndian>(offset2).unwrap(); // offset2

        given_bytes.write_u8(25).unwrap(); // *(start + offset2)
        given_bytes.write_u8(30).unwrap(); // *(start + offset1)
        given_bytes.write_u8(35).unwrap(); // *(start + offset0)

        Cursor::new(given_bytes)
    };

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes).unwrap(),
        hashmap!{
            0 => Value::Struct(vec![
                (label("magic"), Value::U32(0x123)),

                (label("start"), Value::Pos(start)),
                (label("offset0"), Value::U16(offset0)),
                (label("offset1"), Value::U16(offset1)),
                (label("offset2"), Value::U16(offset2)),

                (label("pos0"), Value::Pos(pos0)),
                (label("pos1"), Value::Pos(pos1)),
                (label("pos2"), Value::Pos(pos2)),
            ]),
            pos2 => Value::U8(25),
            pos1 => Value::U8(30),
            pos0 => Value::U8(35),
        },
    );
}

#[test]
fn offset_pos_same_pos() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module offset_test;

        struct PosTest {
            start : Pos,
            offset0 : U16Be,
            offset1 : U16Be,
            pos0 : OffsetPos start offset0 U8,
            pos1 : OffsetPos start offset1 U8,
        };
    "#;

    let start = 0;
    let offset0 = mem::size_of::<[u16; 2]>() as u16;
    let offset1 = mem::size_of::<[u16; 2]>() as u16;
    let pos0 = start + offset0 as u64;
    let pos1 = start + offset1 as u64;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u16::<BigEndian>(offset0).unwrap(); // offset0
        given_bytes.write_u16::<BigEndian>(offset1).unwrap(); // offset1
        given_bytes.write_u8(25).unwrap(); // *(start + offset0), *(start + offset1)

        Cursor::new(given_bytes)
    };

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes).unwrap(),
        hashmap!{
            0 => Value::Struct(vec![
                (label("start"), Value::Pos(start)),
                (label("offset0"), Value::U16(offset0)),
                (label("offset1"), Value::U16(offset1)),
                (label("pos0"), Value::Pos(pos0)),
                (label("pos1"), Value::Pos(pos1)),
            ]),
            pos0 => Value::U8(25),
        },
    );
}

#[test]
fn offset_pos_same_pos_different_tys() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module offset_test;

        struct PosTest {
            start : Pos,
            offset0 : U16Be,
            offset1 : U16Be,
            pos0 : OffsetPos start offset0 U8,
            pos1 : OffsetPos start offset1 S8,
        };
    "#;

    let offset0 = mem::size_of::<[u16; 2]>() as u16;
    let offset1 = mem::size_of::<[u16; 2]>() as u16;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u16::<BigEndian>(offset0).unwrap(); // offset0
        given_bytes.write_u16::<BigEndian>(offset1).unwrap(); // offset1
        given_bytes.write_u8(25).unwrap(); // *(start + offset0), *(start + offset1)

        Cursor::new(given_bytes)
    };

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    let parsed_value = parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes);
    match parsed_value {
        Ok(_) => panic!("expected error"),
        Err(ParseError::OffsetPointedToDifferentTypes(_, _)) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
    }
}

#[test]
fn reserved() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module reserved_test;

        struct Test {
            reserved : Reserved U32Be,
        };
    "#;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u32::<BigEndian>(42).unwrap(); // reserved

        Cursor::new(given_bytes)
    };

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Test"), &module, &mut given_bytes).unwrap(),
        hashmap!{
            0 => Value::Struct(vec![
                (label("reserved"), Value::Struct(Vec::new())),
            ]),
        },
    );
}

#[test]
fn parse_bitmap_nested() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = include_str!("./fixtures/bitmap_nested.ddl");

    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u32::<BigEndian>(3).unwrap(); // header.width
        given_bytes.write_u32::<BigEndian>(2).unwrap(); // header.height
        given_bytes.write_f32::<BigEndian>(0.00).unwrap(); // data[0][0].r
        given_bytes.write_f32::<BigEndian>(0.01).unwrap(); // data[0][0].g
        given_bytes.write_f32::<BigEndian>(0.02).unwrap(); // data[0][0].b
        given_bytes.write_f32::<BigEndian>(0.10).unwrap(); // data[0][1].r
        given_bytes.write_f32::<BigEndian>(0.11).unwrap(); // data[0][1].g
        given_bytes.write_f32::<BigEndian>(0.12).unwrap(); // data[0][1].b
        given_bytes.write_f32::<BigEndian>(0.20).unwrap(); // data[0][2].r
        given_bytes.write_f32::<BigEndian>(0.21).unwrap(); // data[0][2].g
        given_bytes.write_f32::<BigEndian>(0.22).unwrap(); // data[0][2].b
        given_bytes.write_f32::<BigEndian>(1.00).unwrap(); // data[1][0].r
        given_bytes.write_f32::<BigEndian>(1.01).unwrap(); // data[1][0].g
        given_bytes.write_f32::<BigEndian>(1.02).unwrap(); // data[1][0].b
        given_bytes.write_f32::<BigEndian>(1.10).unwrap(); // data[1][1].r
        given_bytes.write_f32::<BigEndian>(1.11).unwrap(); // data[1][1].g
        given_bytes.write_f32::<BigEndian>(1.12).unwrap(); // data[1][1].b
        given_bytes.write_f32::<BigEndian>(1.20).unwrap(); // data[1][2].r
        given_bytes.write_f32::<BigEndian>(1.21).unwrap(); // data[1][2].g
        given_bytes.write_f32::<BigEndian>(1.22).unwrap(); // data[1][2].b

        Cursor::new(given_bytes)
    };

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Bitmap"), &module, &mut given_bytes).unwrap(),
        hashmap!{
            0 => Value::Struct(vec![
                (
                    label("header"),
                    Value::Struct(vec![
                        (label("width"), Value::U32(3)),
                        (label("height"), Value::U32(2)),
                    ]),
                ),
                (
                    label("data"),
                    Value::Array(vec![
                        Value::Array(vec![
                            Value::Struct(vec![
                                (label("r"), Value::F32(0.00)),
                                (label("g"), Value::F32(0.01)),
                                (label("b"), Value::F32(0.02)),
                            ]),
                            Value::Struct(vec![
                                (label("r"), Value::F32(0.10)),
                                (label("g"), Value::F32(0.11)),
                                (label("b"), Value::F32(0.12)),
                            ]),
                            Value::Struct(vec![
                                (label("r"), Value::F32(0.20)),
                                (label("g"), Value::F32(0.21)),
                                (label("b"), Value::F32(0.22)),
                            ]),
                        ]),
                        Value::Array(vec![
                            Value::Struct(vec![
                                (label("r"), Value::F32(1.00)),
                                (label("g"), Value::F32(1.01)),
                                (label("b"), Value::F32(1.02)),
                            ]),
                            Value::Struct(vec![
                                (label("r"), Value::F32(1.10)),
                                (label("g"), Value::F32(1.11)),
                                (label("b"), Value::F32(1.12)),
                            ]),
                            Value::Struct(vec![
                                (label("r"), Value::F32(1.20)),
                                (label("g"), Value::F32(1.21)),
                                (label("b"), Value::F32(1.22)),
                            ]),
                        ]),
                    ]),
                ),
            ]),
        },
    );
}

#[test]
fn parse_bitmap_flat() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = include_str!("./fixtures/bitmap_flat.ddl");

    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u32::<BigEndian>(3).unwrap(); // header.width
        given_bytes.write_u32::<BigEndian>(2).unwrap(); // header.height
        given_bytes.write_f32::<BigEndian>(0.00).unwrap(); // data[(0 * 3) + 0].r
        given_bytes.write_f32::<BigEndian>(0.01).unwrap(); // data[(0 * 3) + 0].g
        given_bytes.write_f32::<BigEndian>(0.02).unwrap(); // data[(0 * 3) + 0].b
        given_bytes.write_f32::<BigEndian>(0.10).unwrap(); // data[(0 * 3) + 1].r
        given_bytes.write_f32::<BigEndian>(0.11).unwrap(); // data[(0 * 3) + 1].g
        given_bytes.write_f32::<BigEndian>(0.12).unwrap(); // data[(0 * 3) + 1].b
        given_bytes.write_f32::<BigEndian>(0.20).unwrap(); // data[(0 * 3) + 2].r
        given_bytes.write_f32::<BigEndian>(0.21).unwrap(); // data[(0 * 3) + 2].g
        given_bytes.write_f32::<BigEndian>(0.22).unwrap(); // data[(0 * 3) + 2].b
        given_bytes.write_f32::<BigEndian>(1.00).unwrap(); // data[(1 * 3) + 0].r
        given_bytes.write_f32::<BigEndian>(1.01).unwrap(); // data[(1 * 3) + 0].g
        given_bytes.write_f32::<BigEndian>(1.02).unwrap(); // data[(1 * 3) + 0].b
        given_bytes.write_f32::<BigEndian>(1.10).unwrap(); // data[(1 * 3) + 1].r
        given_bytes.write_f32::<BigEndian>(1.11).unwrap(); // data[(1 * 3) + 1].g
        given_bytes.write_f32::<BigEndian>(1.12).unwrap(); // data[(1 * 3) + 1].b
        given_bytes.write_f32::<BigEndian>(1.20).unwrap(); // data[(1 * 3) + 2].r
        given_bytes.write_f32::<BigEndian>(1.21).unwrap(); // data[(1 * 3) + 2].g
        given_bytes.write_f32::<BigEndian>(1.22).unwrap(); // data[(1 * 3) + 2].b

        Cursor::new(given_bytes)
    };

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Bitmap"), &module, &mut given_bytes).unwrap(),
        hashmap!{
            0 => Value::Struct(vec![
                (
                    label("header"),
                    Value::Struct(vec![
                        (label("width"), Value::U32(3)),
                        (label("height"), Value::U32(2)),
                    ]),
                ),
                (
                    label("data"),
                    Value::Array(vec![
                        Value::Struct(vec![
                            (label("r"), Value::F32(0.00)),
                            (label("g"), Value::F32(0.01)),
                            (label("b"), Value::F32(0.02)),
                        ]),
                        Value::Struct(vec![
                            (label("r"), Value::F32(0.10)),
                            (label("g"), Value::F32(0.11)),
                            (label("b"), Value::F32(0.12)),
                        ]),
                        Value::Struct(vec![
                            (label("r"), Value::F32(0.20)),
                            (label("g"), Value::F32(0.21)),
                            (label("b"), Value::F32(0.22)),
                        ]),
                        Value::Struct(vec![
                            (label("r"), Value::F32(1.00)),
                            (label("g"), Value::F32(1.01)),
                            (label("b"), Value::F32(1.02)),
                        ]),
                        Value::Struct(vec![
                            (label("r"), Value::F32(1.10)),
                            (label("g"), Value::F32(1.11)),
                            (label("b"), Value::F32(1.12)),
                        ]),
                        Value::Struct(vec![
                            (label("r"), Value::F32(1.20)),
                            (label("g"), Value::F32(1.21)),
                            (label("b"), Value::F32(1.22)),
                        ]),
                    ]),
                ),
            ]),
        },
    );
}

#[test]
fn gif() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = include_str!("./fixtures/gif.ddl");

    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write(b"GIF").unwrap(); // header.magic
        given_bytes.write(b"87a").unwrap(); // header.version
        given_bytes.write_u16::<LittleEndian>(200).unwrap(); // logical_screen.image_width
        given_bytes.write_u16::<LittleEndian>(300).unwrap(); // logical_screen.image_height
        given_bytes.write_u8(0).unwrap(); // logical_screen.flags
        given_bytes.write_u8(0).unwrap(); // logical_screen.bg_color_index
        given_bytes.write_u8(0).unwrap(); // logical_screen.pixel_aspect_ratio

        Cursor::new(given_bytes)
    };

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Gif"), &module, &mut given_bytes).unwrap(),
        hashmap!{
            0 => Value::Struct(vec![
                (
                    label("header"),
                    Value::Struct(vec![
                        (
                            label("magic"),
                            Value::Array(vec![Value::U8(71), Value::U8(73), Value::U8(70)]), // "GIF"
                        ),
                        (
                            label("version"),
                            Value::Array(vec![Value::U8(56), Value::U8(55), Value::U8(97)]), // "87a"
                        ),
                    ]),
                ),
                (
                    label("logical_screen"),
                    Value::Struct(vec![
                        (label("image_width"), Value::U16(200)),
                        (label("image_height"), Value::U16(300)),
                        (label("flags"), Value::U8(0)),
                        (label("bg_color_index"), Value::U8(0)),
                        (label("pixel_aspect_ratio"), Value::U8(0)),
                    ]),
                ),
            ]),
        },
    );
}

#[test]
fn opentype() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = include_str!("./fixtures/opentype.ddl");

    let raw_module = parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    match check_module(&context, &raw_module) {
        Ok(_module) => {},
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("type error!");
        },
    }
}
