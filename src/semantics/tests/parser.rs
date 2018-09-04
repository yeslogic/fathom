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

    let given_format = parse_check_module(&mut codemap, &tc_env, given_format);

    assert_term_eq!(
        parser::parse_module(
            &tc_env,
            &Label(String::from("Silly")),
            &given_format,
            &mut given_bytes,
        ).unwrap(),
        RcValue::from(Value::Struct(vec![
            (
                Label(String::from("len")),
                RcValue::from(Value::Literal(Literal::Int(BigInt::from(3))))
            ),
            (
                Label(String::from("data")),
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

    let given_format = r#"
        module silly;

        Data : U32 -> Type;
        Data len = Array len U32Be;
    "#;

    let given_format = parse_check_module(&mut codemap, &tc_env, given_format);
    let mut given_bytes = Cursor::new(vec![]);

    let result_term = parser::parse_module(
        &tc_env,
        &Label(String::from("Silly")),
        &given_format,
        &mut given_bytes,
    );

    match result_term {
        Ok(_) => panic!("expected error"),
        Err(ParseError::MissingRoot { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
    }
}
