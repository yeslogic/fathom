use byteorder::{BigEndian, WriteBytesExt};
use std::io::Cursor;

use semantics::parser;

use super::*;

#[test]
fn test_silly_format() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let given_format = r#"Record {
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

    let expected_term = r#"record {
        len = 3,
        data = [1, 3, 6],
    } : Record {
        len : U16,
        data : Array len U32,
    }"#;

    let given_format = parse_normalize(&mut codemap, &context, given_format);
    let expected_term = parse_normalize(&mut codemap, &context, expected_term);

    let result_term = parser::parse(&context, &given_format, &mut given_bytes).unwrap();

    assert_term_eq!(result_term, expected_term);
}
