use byteorder::{BigEndian, LittleEndian, WriteBytesExt};
use codespan::CodeMap;
use codespan_reporting::termcolor::{ColorChoice, StandardStream};
use pretty_assertions::assert_eq;
use std::io::{Cursor, Write};
use std::mem;

use ddl::semantics::parser::{self, ParseError, Value};
use ddl::semantics::{self, Context};
use ddl::syntax::translation::{Desugar, DesugarEnv};
use ddl::syntax::Label;

mod support;

fn label(name: &str) -> Label {
    Label(name.to_owned())
}

#[test]
fn silly_root() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Silly"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("len"), Value::int(3)),
                (
                    label("data"),
                    Value::Array(vec![Value::int(1), Value::int(3), Value::int(6)]),
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
        module test;

        Data : U32 -> Type;
        Data len = Array len U32Be;
    "#;

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

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
        module test;

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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("start"), Value::Pos(0)),
                (
                    label("data"),
                    Value::Array(vec![
                        Value::Struct(vec![
                            (label("pad"), Value::int(0)),
                            (label("end"), Value::Pos(mem::size_of::<u32>() as u64 * 1)),
                        ]),
                        Value::Struct(vec![
                            (label("pad"), Value::int(0)),
                            (label("end"), Value::Pos(mem::size_of::<u32>() as u64 * 2)),
                        ]),
                        Value::Struct(vec![
                            (label("pad"), Value::int(0)),
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
        module test;

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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("magic"), Value::int(0x123)),
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
            (mem::size_of::<u32>() + mem::size_of::<[u16; 3]>()) as u64 + 0 => Value::int(25),
            (mem::size_of::<u32>() + mem::size_of::<[u16; 3]>()) as u64 + 1 => Value::int(30),
            (mem::size_of::<u32>() + mem::size_of::<[u16; 3]>()) as u64 + 2 => Value::int(35),
        },
    );
}

#[test]
fn offset_same_pos() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("start"), Value::Pos(0)),
                (label("offset1"), Value::Pos(mem::size_of::<[u16; 2]>() as u64)),
                (label("offset2"), Value::Pos(mem::size_of::<[u16; 2]>() as u64)),
            ]),
            mem::size_of::<[u16; 2]>() as u64 => Value::int(25),
        },
    );
}

#[test]
fn offset_same_pos_different_tys() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    let parsed_value = parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes);
    match parsed_value {
        Ok(_) => panic!("expected error"),
        Err(ParseError::OffsetPointedToDifferentTypes(_, _)) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
    }
}

#[test]
fn link() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

        struct PosTest {
            magic : U32Be,

            start : Pos,
            offset0 : U16Be,
            offset1 : U16Be,
            offset2 : U16Be,

            pos0 : Link start offset0 U8,
            pos1 : Link start offset1 U8,
            pos2 : Link start offset2 U8,
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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("magic"), Value::int(0x123)),

                (label("start"), Value::Pos(start)),
                (label("offset0"), Value::int(offset0)),
                (label("offset1"), Value::int(offset1)),
                (label("offset2"), Value::int(offset2)),

                (label("pos0"), Value::Pos(pos0)),
                (label("pos1"), Value::Pos(pos1)),
                (label("pos2"), Value::Pos(pos2)),
            ]),
            pos2 => Value::int(25),
            pos1 => Value::int(30),
            pos0 => Value::int(35),
        },
    );
}

#[test]
fn link_same_pos() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

        struct PosTest {
            start : Pos,
            offset0 : U16Be,
            offset1 : U16Be,
            pos0 : Link start offset0 U8,
            pos1 : Link start offset1 U8,
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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("start"), Value::Pos(start)),
                (label("offset0"), Value::int(offset0)),
                (label("offset1"), Value::int(offset1)),
                (label("pos0"), Value::Pos(pos0)),
                (label("pos1"), Value::Pos(pos1)),
            ]),
            pos0 => Value::int(25),
        },
    );
}

#[test]
fn link_same_pos_different_tys() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

        struct PosTest {
            start : Pos,
            offset0 : U16Be,
            offset1 : U16Be,
            pos0 : Link start offset0 U8,
            pos1 : Link start offset1 S8,
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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    let parsed_value = parser::parse_module(&context, &label("PosTest"), &module, &mut given_bytes);
    match parsed_value {
        Ok(_) => panic!("expected error"),
        Err(ParseError::OffsetPointedToDifferentTypes(_, _)) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
    }
}

#[test]
fn compute_array() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

        u32_mul : U32 -> U32 -> U32;
        u32_mul = extern "int-mul";

        index : (len : int {0 ..}) (A : Type) -> int {0 ..} -> Array len A -> A;
        index _ _ = extern "array-index";

        struct Test {
            len : U32Be,
            data : Array len U32Be,
            data2 : ComputeArray len U32 (\i => u32_mul (index len U32Be i data) 2),
        };
    "#;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u32::<BigEndian>(2).unwrap(); // len
        given_bytes.write_u32::<BigEndian>(42).unwrap(); // data[0]
        given_bytes.write_u32::<BigEndian>(48).unwrap(); // data[1]

        Cursor::new(given_bytes)
    };

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Test"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("len"), Value::int(2)),
                (label("data"), Value::Array(vec![
                    Value::int(42),
                    Value::int(48),
                ])),
                (label("data2"), Value::Array(vec![
                    Value::int(42 * 2),
                    Value::int(48 * 2),
                ])),
            ]),
        },
    );
}

#[test]
fn compute() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r"
        module test;

        struct Test {
            test : Compute (int {0 ..}) (\_ => 1),
        };
    ";

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let given_bytes = Vec::new();
        Cursor::new(given_bytes)
    };

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Test"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("test"), Value::int(1)),
            ]),
        },
    );
}

#[test]
fn array_operations() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

        Nat : Type;
        Nat = int {0 ..};

        nat_eq : Nat -> Nat -> Bool;
        nat_eq = extern "int-eq";

        nat_mul : Nat -> Nat -> Nat;
        nat_mul = extern "int-mul";

        nat_to_string : Nat -> String;
        nat_to_string = extern "int-to-string";

        array_map : (len : Nat) (A B : Type) -> (A -> B) -> Array len A -> Array len B;
        array_map _ _ _ = extern "array-map";

        array_find_map : (len : Nat) (A B : Type) -> (A -> Option B) -> Array len A -> B;
        array_find_map _ _ _ = extern "array-find-map";

        array_eq : (len : Nat) (A : Type) -> (A -> A -> Bool) -> Array len A -> Array len A -> Bool;
        array_eq _ _ = extern "array-eq";

        struct Test {
            test_map1 : Compute (Array 3 Nat) (\_ => array_map 3 Nat Nat (nat_mul 2) [1, 2, 3]),
            test_map2 : Compute (Array 3 String) (\_ => array_map 3 Nat String nat_to_string [1, 2, 3]),
            test_find_map1 : Compute String (\_ => array_find_map 3 Nat String (\x => if nat_eq x 3 { some String "three" } else { none String }) [1, 2, 3]),
            test_eq1 : Compute Bool (\_ => array_eq 3 Nat nat_eq [1, 2, 3] [1, 2, 3]),
            test_eq2 : Compute Bool (\_ => array_eq 3 Nat nat_eq [1, 2, 3] [1, 5, 3]),
        };
    "#;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let given_bytes = Vec::new();
        Cursor::new(given_bytes)
    };

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Test"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("test_map1"), Value::Array(vec![
                    Value::int(2),
                    Value::int(4),
                    Value::int(6),
                ])),
                (label("test_map2"), Value::Array(vec![
                    Value::String("1".to_owned()),
                    Value::String("2".to_owned()),
                    Value::String("3".to_owned()),
                ])),
                (label("test_find_map1"), Value::String("three".to_owned())),
                (label("test_eq1"), Value::Bool(true)),
                (label("test_eq2"), Value::Bool(false)),
            ]),
        },
    );
}

#[test]
fn reserved() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Test"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("reserved"), Value::Struct(Vec::new())),
            ]),
        },
    );
}

#[test]
fn refinement_ok() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

        nat_eq : int {0 ..} -> int {0 ..} -> Bool;
        nat_eq = extern "int-eq";

        struct Test {
            value : { value : U32Be | nat_eq value 0 },
            data : Array value U8, // test subtyping
        };
    "#;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u32::<BigEndian>(0).unwrap(); // value

        Cursor::new(given_bytes)
    };

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Test"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("value"), Value::int(0)),
                (label("data"), Value::Array(vec![])),
            ]),
        },
    );
}

#[test]
fn refinement_fail() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

        nat_eq : int {0 ..} -> int {0 ..} -> Bool;
        nat_eq = extern "int-eq";

        struct Test {
            value : { value : U32Be | nat_eq value 0 },
            data : Array value U8, // test subtyping
        };
    "#;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u32::<BigEndian>(1).unwrap(); // value

        Cursor::new(given_bytes)
    };

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert!(parser::parse_module(&context, &label("Test"), &module, &mut given_bytes).is_err());
}

#[test]
fn union_ok() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

        nat_eq : int {0 ..} -> int {0 ..} -> Bool;
        nat_eq = extern "int-eq";

        union Test {
            Test1,
            Test2,
        };

        struct Test1 {
            format : { format : U32Be | nat_eq format 1 },
            data : F32Be,
        };

        struct Test2 {
            format : { format : U32Be | nat_eq format 2 },
            data1 : U32Be,
            data2 : U32Be,
        };
    "#;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u32::<BigEndian>(2).unwrap(); // format
        given_bytes.write_u32::<BigEndian>(42).unwrap(); // data1
        given_bytes.write_u32::<BigEndian>(43).unwrap(); // data2

        Cursor::new(given_bytes)
    };

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Test"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("format"), Value::int(2)),
                (label("data1"), Value::int(42)),
                (label("data2"), Value::int(43)),
            ]),
        },
    );
}

#[test]
fn union_fail() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

        nat_eq : int {0 ..} -> int {0 ..} -> Bool;
        nat_eq = extern "int-eq";

        union Test {
            Test1,
            Test2,
        };

        struct Test1 {
            format : { format : U32Be | nat_eq format 1 },
            data : F32Be,
        };

        struct Test2 {
            format : { format : U32Be | nat_eq format 2 },
            data1 : U32Be,
            data2 : U32Be,
        };
    "#;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u32::<BigEndian>(3).unwrap(); // format
        given_bytes.write_u32::<BigEndian>(42).unwrap(); // data1
        given_bytes.write_u32::<BigEndian>(43).unwrap(); // data2

        Cursor::new(given_bytes)
    };

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert!(parser::parse_module(&context, &label("Test"), &module, &mut given_bytes).is_err());
}

#[test]
fn array_index() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

        index : (len : int {0 ..}) (A : Type) -> int {0 ..} -> Array len A -> A;
        index _ _ = extern "array-index";

        struct Test {
            lengths : Array 1 U32Be,
            data : Array (index 1 U32Be 0 lengths) U8,
        };
    "#;

    #[cfg_attr(rustfmt, rustfmt_skip)]
    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u32::<BigEndian>(3).unwrap(); // lengths
        given_bytes.write_u8(42).unwrap(); // data[0]
        given_bytes.write_u8(43).unwrap(); // data[1]
        given_bytes.write_u8(44).unwrap(); // data[2]

        Cursor::new(given_bytes)
    };

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Test"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("lengths"), Value::Array(vec![Value::int(3)])),
                (label("data"), Value::Array(vec![Value::int(42), Value::int(43), Value::int(44)])),
            ]),
        },
    );
}

#[test]
fn intersection_ok() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

        intersection Test {
            init_data : InitData,
            computed_data : ComputedData init_data.tag,
        };

        struct InitData {
            len : U16Be,
            offset : U16Be,
            tag: U16Be,
        };

        struct ComputedData (tag : U16) {
            start : Pos,
            len : U16Be,
            offset : Offset16Be start (Array len (Elem tag)),
            tag: U16Be,
        };

        Elem : U16 -> Type;
        Elem tag = match tag {
            0 => U8,
            1 => U16Be,
            2 => U32Be,
            3 => U64Be,
        };
    "#;

    let start = 0;
    let offset = mem::size_of::<[u16; 3]>() as u16;
    let pos = start + offset as u64;

    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u16::<BigEndian>(3).unwrap(); // len
        given_bytes.write_u16::<BigEndian>(offset).unwrap(); // offset
        given_bytes.write_u16::<BigEndian>(2).unwrap(); // tag
        given_bytes.write_u32::<BigEndian>(25).unwrap(); // *offset[0]
        given_bytes.write_u32::<BigEndian>(123456789).unwrap(); // *offset[1]
        given_bytes.write_u32::<BigEndian>(42).unwrap(); // *offset[2]

        Cursor::new(given_bytes)
    };

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Test"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (label("init_data"), Value::Struct(vec![
                    (label("len"), Value::int(3)),
                    (label("offset"), Value::int(offset)),
                    (label("tag"), Value::int(2)),
                ])),
                (label("computed_data"), Value::Struct(vec![
                    (label("start"), Value::Pos(start)),
                    (label("len"), Value::int(3)),
                    (label("offset"), Value::Pos(pos)),
                    (label("tag"), Value::int(2)),
                ])),
            ]),
            pos => Value::Array(vec![
                Value::int(25),
                Value::int(123456789),
                Value::int(42),
            ]),
        },
    );
}

#[test]
fn intersection_mismatched_sizes() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let given_format = r#"
        module test;

        intersection Test {
            u16 : U16Be,
            f32 : F32Be,
        };
    "#;

    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_f32::<BigEndian>(256.256).unwrap(); // f32

        Cursor::new(given_bytes)
    };

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    let parsed_value = parser::parse_module(&context, &label("Test"), &module, &mut given_bytes);
    match parsed_value {
        Ok(_) => panic!("expected error"),
        Err(ParseError::MismatchedIntersectionSize(2, 4)) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
    }
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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Bitmap"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (
                    label("header"),
                    Value::Struct(vec![
                        (label("width"), Value::int(3)),
                        (label("height"), Value::int(2)),
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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Bitmap"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (
                    label("header"),
                    Value::Struct(vec![
                        (label("width"), Value::int(3)),
                        (label("height"), Value::int(2)),
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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    let module = semantics::check_module(&context, &raw_module).unwrap();

    assert_eq!(
        parser::parse_module(&context, &label("Gif"), &module, &mut given_bytes).unwrap(),
        im::hashmap! {
            0 => Value::Struct(vec![
                (
                    label("header"),
                    Value::Struct(vec![
                        (
                            label("magic"),
                            Value::Array(vec![Value::int(71), Value::int(73), Value::int(70)]), // "GIF"
                        ),
                        (
                            label("version"),
                            Value::Array(vec![Value::int(56), Value::int(55), Value::int(97)]), // "87a"
                        ),
                    ]),
                ),
                (
                    label("logical_screen"),
                    Value::Struct(vec![
                        (label("image_width"), Value::int(200)),
                        (label("image_height"), Value::int(300)),
                        (label("flags"), Value::int(0)),
                        (label("bg_color_index"), Value::int(0)),
                        (label("pixel_aspect_ratio"), Value::int(0)),
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

    let raw_module = support::parse_module(&mut codemap, given_format)
        .desugar(&desugar_env)
        .unwrap();
    match semantics::check_module(&context, &raw_module) {
        Ok(_module) => {},
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("type error!");
        },
    }
}
