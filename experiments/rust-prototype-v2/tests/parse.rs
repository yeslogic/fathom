use codespan::{ByteIndex, ByteSpan};
use codespan::{CodeMap, FileName};
use pretty_assertions::assert_eq;

use ddl::syntax::concrete;
use ddl::syntax::parse::{self, ParseError};

#[test]
fn pi_bad_ident() {
    let src = "((x : Type) : Type) -> Type";
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

    let parse_result = parse::term(&filemap);

    assert_eq!(
        parse_result,
        (
            concrete::Term::Error(ByteSpan::new(ByteIndex(1), ByteIndex(28))),
            vec![ParseError::IdentifierExpectedInPiType {
                span: ByteSpan::new(ByteIndex(2), ByteIndex(12)),
            }],
        )
    );
}

#[test]
fn pi_bad_ident_multi() {
    let src = "((x : Type) : Type) (x : Type) -> Type";
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

    let parse_result = parse::term(&filemap);

    assert_eq!(
        parse_result,
        (
            concrete::Term::Error(ByteSpan::new(ByteIndex(1), ByteIndex(39))),
            vec![ParseError::IdentifierExpectedInPiType {
                span: ByteSpan::new(ByteIndex(2), ByteIndex(12)),
            }],
        )
    );
}

#[test]
fn integer_overflow() {
    use num_bigint::BigInt;

    let src = "Type 11111111111111111111";
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

    let parse_result = parse::term(&filemap);

    assert_eq!(
        parse_result,
        (
            concrete::Term::Error(ByteSpan::new(ByteIndex(1), ByteIndex(26))),
            vec![ParseError::IntegerLiteralOverflow {
                span: ByteSpan::new(ByteIndex(6), ByteIndex(26)),
                value: BigInt::from(11111111111111111111_u64),
            }],
        )
    );
}
