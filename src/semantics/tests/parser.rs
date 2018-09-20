use byteorder::{BigEndian, WriteBytesExt};
use num_bigint::BigInt;
use std::io::Cursor;

use semantics::parser::{self, ParseError};
use syntax::Label;

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

    assert_term_eq!(
        parser::parse_module(
            &tc_env,
            &Label("Silly".to_owned()),
            &module,
            &mut given_bytes,
        ).unwrap(),
        RcValue::from(Value::Struct(vec![
            (
                Label("len".to_owned()),
                RcValue::from(Value::Literal(Literal::Int(BigInt::from(3))))
            ),
            (
                Label("data".to_owned()),
                RcValue::from(Value::Array(vec![
                    RcValue::from(Value::Literal(Literal::Int(BigInt::from(1)))),
                    RcValue::from(Value::Literal(Literal::Int(BigInt::from(3)))),
                    RcValue::from(Value::Literal(Literal::Int(BigInt::from(6)))),
                ])),
            ),
        ])),
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
fn opentype() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_format = r#"
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
            body : case version of {
                -- FIXME: 0x00010000 => TtcHeader1;
                -- FIXME: 0x00020000 => TtcHeader2;
                _ => Unknown;
            },
        };

        struct OpenType {
            tag : Tag,
            body : case tag of {
                -- FIXME: 0x00010000
                "OTTO" => OffsetTable;
                "ttcf" => TtcHeader;
                _ => Unknown;
            },
        };
    "#;

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
