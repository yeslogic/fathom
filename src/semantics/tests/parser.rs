use byteorder::{BigEndian, WriteBytesExt};
use num_bigint::BigInt;
use std::io::Cursor;

use semantics::parser::{self, ParseError};
use syntax::{FloatFormat, IntFormat, Label};

use super::*;

#[test]
fn silly_root() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

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

    let raw_module = parse_module(&mut codemap, given_format).desugar(&desugar_env);
    let module = check_module(&tc_env, &raw_module).unwrap();

    let label = |name: &str| Label(name.to_owned());
    let array = |elems: Vec<RcValue>| RcValue::from(Value::Array(elems));
    let struct_ = |fields: Vec<(Label, RcValue)>| RcValue::from(Value::Struct(fields));
    let int = |value: u32| {
        RcValue::from(Value::Literal(Literal::Int(
            BigInt::from(value),
            IntFormat::Dec,
        )))
    };

    assert_term_eq!(
        parser::parse_module(&tc_env, &label("Silly"), &module, &mut given_bytes,).unwrap(),
        struct_(vec![
            (label("len"), int(3)),
            (label("data"), array(vec![int(1), int(3), int(6)])),
        ]),
    );
}

#[test]
fn missing_root() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_format = r#"
        module silly;

        Data : U32 -> Type;
        Data len = Array len U32Be;
    "#;

    let raw_module = parse_module(&mut codemap, given_format).desugar(&desugar_env);
    let module = check_module(&tc_env, &raw_module).unwrap();

    let mut given_bytes = Cursor::new(vec![]);

    let result_term = parser::parse_module(
        &tc_env,
        &Label("Silly".to_owned()),
        &module,
        &mut given_bytes,
    );

    match result_term {
        Ok(_) => panic!("expected error"),
        Err(ParseError::MissingRoot { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
    }
}

#[test]
fn parse_bitmap_nested() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

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

    let raw_module = parse_module(&mut codemap, given_format).desugar(&desugar_env);
    let module = check_module(&tc_env, &raw_module).unwrap();

    let label = |name: &str| Label(name.to_owned());
    let array = |elems: Vec<RcValue>| RcValue::from(Value::Array(elems));
    let struct_ = |fields: Vec<(Label, RcValue)>| RcValue::from(Value::Struct(fields));
    let float = |value: f32| RcValue::from(Value::Literal(Literal::F32(value, FloatFormat::Dec)));
    let int = |value: u32| {
        RcValue::from(Value::Literal(Literal::Int(
            BigInt::from(value),
            IntFormat::Dec,
        )))
    };

    assert_term_eq!(
        parser::parse_module(&tc_env, &label("Bitmap"), &module, &mut given_bytes).unwrap(),
        RcValue::from(Value::Struct(vec![
            (
                label("header"),
                struct_(vec![(label("width"), int(3)), (label("height"), int(2)),]),
            ),
            (
                label("data"),
                array(vec![
                    array(vec![
                        struct_(vec![
                            (label("r"), float(0.00)),
                            (label("g"), float(0.01)),
                            (label("b"), float(0.02)),
                        ]),
                        struct_(vec![
                            (label("r"), float(0.10)),
                            (label("g"), float(0.11)),
                            (label("b"), float(0.12)),
                        ]),
                        struct_(vec![
                            (label("r"), float(0.20)),
                            (label("g"), float(0.21)),
                            (label("b"), float(0.22)),
                        ]),
                    ]),
                    array(vec![
                        struct_(vec![
                            (label("r"), float(1.00)),
                            (label("g"), float(1.01)),
                            (label("b"), float(1.02)),
                        ]),
                        struct_(vec![
                            (label("r"), float(1.10)),
                            (label("g"), float(1.11)),
                            (label("b"), float(1.12)),
                        ]),
                        struct_(vec![
                            (label("r"), float(1.20)),
                            (label("g"), float(1.21)),
                            (label("b"), float(1.22)),
                        ]),
                    ]),
                ]),
            ),
        ])),
    );
}

#[test]
fn parse_bitmap_flat() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_format = include_str!("./fixtures/bitmap_flat.ddl");

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

    let raw_module = parse_module(&mut codemap, given_format).desugar(&desugar_env);
    let module = check_module(&tc_env, &raw_module).unwrap();

    let label = |name: &str| Label(name.to_owned());
    let array = |elems: Vec<RcValue>| RcValue::from(Value::Array(elems));
    let struct_ = |fields: Vec<(Label, RcValue)>| RcValue::from(Value::Struct(fields));
    let float = |value: f32| RcValue::from(Value::Literal(Literal::F32(value, FloatFormat::Dec)));
    let int = |value: u32| {
        RcValue::from(Value::Literal(Literal::Int(
            BigInt::from(value),
            IntFormat::Dec,
        )))
    };

    assert_term_eq!(
        parser::parse_module(&tc_env, &label("Bitmap"), &module, &mut given_bytes).unwrap(),
        RcValue::from(Value::Struct(vec![
            (
                label("header"),
                struct_(vec![(label("width"), int(3)), (label("height"), int(2)),]),
            ),
            (
                label("data"),
                array(vec![
                    struct_(vec![
                        (label("r"), float(0.00)),
                        (label("g"), float(0.01)),
                        (label("b"), float(0.02)),
                    ]),
                    struct_(vec![
                        (label("r"), float(0.10)),
                        (label("g"), float(0.11)),
                        (label("b"), float(0.12)),
                    ]),
                    struct_(vec![
                        (label("r"), float(0.20)),
                        (label("g"), float(0.21)),
                        (label("b"), float(0.22)),
                    ]),
                    struct_(vec![
                        (label("r"), float(1.00)),
                        (label("g"), float(1.01)),
                        (label("b"), float(1.02)),
                    ]),
                    struct_(vec![
                        (label("r"), float(1.10)),
                        (label("g"), float(1.11)),
                        (label("b"), float(1.12)),
                    ]),
                    struct_(vec![
                        (label("r"), float(1.20)),
                        (label("g"), float(1.21)),
                        (label("b"), float(1.22)),
                    ]),
                ]),
            ),
        ])),
    );
}

#[test]
fn opentype() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_format = include_str!("./fixtures/opentype.ddl");

    let raw_module = parse_module(&mut codemap, given_format).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_module) => {},
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("type error!");
        },
    }
}
