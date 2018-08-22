use byteorder::{BigEndian, WriteBytesExt};
use std::io::Cursor;

use semantics::parser::{self, ParseError};
use syntax::Label;

use super::*;

#[test]
fn silly_format() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let given_format = r#"Struct {
        len : U16Be,
        data : Array len U32Be,
    }"#;

    let mut given_bytes = {
        let mut given_bytes = Vec::new();

        given_bytes.write_u16::<BigEndian>(3).unwrap(); // len
        given_bytes.write_u32::<BigEndian>(1).unwrap(); // data[0]
        given_bytes.write_u32::<BigEndian>(3).unwrap(); // data[1]
        given_bytes.write_u32::<BigEndian>(6).unwrap(); // data[2]

        Cursor::new(given_bytes)
    };

    let expected_term = r#"struct {
        len = 3,
        data = [1, 3, 6],
    } : Struct {
        len : U16,
        data : Array len U32,
    }"#;

    let given_format = parse_nf_term(&mut codemap, &tc_env, given_format);
    let expected_term = parse_nf_term(&mut codemap, &tc_env, expected_term);

    let result_term = parser::parse_term(&tc_env, &given_format, &mut given_bytes).unwrap();

    assert_term_eq!(result_term, expected_term);
}

#[test]
fn silly_root() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let given_format = r#"
        module silly;

        Data : U32 -> Type;
        Silly : Type;

        Data len = Array len U32Be;
        Silly = Struct {
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

    let expected_term = r#"struct {
        len = 3,
        data = [1, 3, 6],
    } : Struct {
        len : U16,
        data : Array len U32,
    }"#;

    let given_format = parse_check_module(&mut codemap, &tc_env, given_format);
    let expected_term = parse_nf_term(&mut codemap, &tc_env, expected_term);

    let result_term = parser::parse_module(
        &tc_env,
        &Label(String::from("Silly")),
        &given_format,
        &mut given_bytes,
    ).unwrap();

    assert_term_eq!(result_term, expected_term);
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
