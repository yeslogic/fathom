#![cfg(test)]

use fathom_runtime::{FormatWriter, ReadError, ReadScope, U32Be, U8};
use fathom_test_util::fathom::lang::core::{self, binary};

fathom_test_util::core_module!(
    FIXTURE,
    "../../snapshots/alias/pass_format_array.core.fathom"
);

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match binary::read::from_module_item(&mut read_context, &FIXTURE, &"SimpleFormatArray") {
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
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    let simple_array =
        binary::read::from_module_item(&mut read_context, &FIXTURE, &"SimpleFormatArray").unwrap();
    assert_eq!(
        simple_array,
        binary::Term::Seq(vec![
            binary::Term::int(1),
            binary::Term::int(2),
            binary::Term::int(3),
            binary::Term::int(4),
            binary::Term::int(5),
            binary::Term::int(6),
        ]),
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

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match binary::read::from_module_item(&mut read_context, &FIXTURE, &"SimpleFormatArray") {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

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
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    let simple_array =
        binary::read::from_module_item(&mut read_context, &FIXTURE, &"SimpleFormatArray").unwrap();
    assert_eq!(
        simple_array,
        binary::Term::Seq(vec![
            binary::Term::int(1),
            binary::Term::int(2),
            binary::Term::int(3),
            binary::Term::int(4),
            binary::Term::int(5),
            binary::Term::int(6),
        ]),
    );

    // TODO: Check remaining
}
