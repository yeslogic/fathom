#![cfg(test)]

use ddl_rt::{FormatWriter, ReadError, ReadScope, U32Be, U8};
use ddl_test_util::ddl::ast::core::{self, binary};

#[path = "../../snapshots/alias/pass_format_array.rs"]
pub mod fixture;

ddl_test_util::core_module!(FIXTURE, "../../snapshots/alias/pass_format_array.core.ddl");

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let read_scope = ReadScope::new(writer.buffer());
    let simple_array = read_scope.read::<fixture::SimpleFormatArray>();

    match simple_array {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_test() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U32Be>(1); // SimpleFormatArray::inner[0]
    writer.write::<U32Be>(2); // SimpleFormatArray::inner[1]
    writer.write::<U32Be>(3); // SimpleFormatArray::inner[2]
    writer.write::<U32Be>(4); // SimpleFormatArray::inner[3]
    writer.write::<U32Be>(5); // SimpleFormatArray::inner[4]
    writer.write::<U32Be>(6); // SimpleFormatArray::inner[5]

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let simple_array = read_scope.read::<fixture::SimpleFormatArray>().unwrap();
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    let simple_array_term =
        binary::read::from_module_item(&mut read_context, &FIXTURE, &"SimpleFormatArray").unwrap();
    assert_eq!(simple_array.inner(), &[1, 2, 3, 4, 5, 6]);
    assert_eq!(
        simple_array_term,
        binary::Term::Seq(
            simple_array
                .inner()
                .iter()
                .map(|elem| binary::Term::Int((*elem).into()))
                .collect(),
        ),
    );

    // TODO: Check remaining
}

#[test]
fn invalid_test_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U32Be>(1); // SimpleFormatArray::inner[0]
    writer.write::<U32Be>(2); // SimpleFormatArray::inner[1]
    writer.write::<U32Be>(3); // SimpleFormatArray::inner[2]
    writer.write::<U32Be>(4); // SimpleFormatArray::inner[3]
    writer.write::<U32Be>(5); // SimpleFormatArray::inner[4]
    writer.write::<U8>(42);

    let read_scope = ReadScope::new(writer.buffer());
    let simple_array = read_scope.read::<fixture::SimpleFormatArray>();

    match simple_array {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    };

    // TODO: Check remaining
}

#[test]
fn valid_test_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U32Be>(1); // SimpleFormatArray::inner[0]
    writer.write::<U32Be>(2); // SimpleFormatArray::inner[1]
    writer.write::<U32Be>(3); // SimpleFormatArray::inner[2]
    writer.write::<U32Be>(4); // SimpleFormatArray::inner[3]
    writer.write::<U32Be>(5); // SimpleFormatArray::inner[4]
    writer.write::<U32Be>(6); // SimpleFormatArray::inner[5]
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let simple_array = read_scope.read::<fixture::SimpleFormatArray>().unwrap();
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    let simple_array_term =
        binary::read::from_module_item(&mut read_context, &FIXTURE, &"SimpleFormatArray").unwrap();
    assert_eq!(simple_array.inner(), &[1, 2, 3, 4, 5, 6]);
    assert_eq!(
        simple_array_term,
        binary::Term::Seq(
            simple_array
                .inner()
                .iter()
                .map(|elem| binary::Term::Int((*elem).into()))
                .collect(),
        ),
    );

    // TODO: Check remaining
}
