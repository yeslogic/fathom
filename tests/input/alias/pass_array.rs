#![cfg(test)]

use ddl_rt::{FormatWriter, ReadError, ReadScope, U32Be, U8};
use ddl_test_util::ddl::{binary, core};

#[path = "../../snapshots/alias/pass_array.rs"]
pub mod fixture;

ddl_test_util::core_module!(FIXTURE, "../../snapshots/alias/pass_array.core.ddl");

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let read_scope = ReadScope::new(writer.buffer());
    let simple_array = read_scope.read::<fixture::SimpleArray>();

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
    writer.write::<U32Be>(1); // SimpleArray::inner[0]
    writer.write::<U32Be>(2); // SimpleArray::inner[1]
    writer.write::<U32Be>(3); // SimpleArray::inner[2]
    writer.write::<U32Be>(4); // SimpleArray::inner[3]
    writer.write::<U32Be>(5); // SimpleArray::inner[4]
    writer.write::<U32Be>(6); // SimpleArray::inner[5]

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let simple_array = read_scope.read::<fixture::SimpleArray>().unwrap();
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    let simple_array_term =
        binary::read::read_module_item(&mut read_context, &FIXTURE, &"SimpleArray").unwrap();
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
    writer.write::<U32Be>(1); // SimpleArray::inner[0]
    writer.write::<U32Be>(2); // SimpleArray::inner[1]
    writer.write::<U32Be>(3); // SimpleArray::inner[2]
    writer.write::<U32Be>(4); // SimpleArray::inner[3]
    writer.write::<U32Be>(5); // SimpleArray::inner[4]
    writer.write::<U8>(42);

    let read_scope = ReadScope::new(writer.buffer());
    let simple_array = read_scope.read::<fixture::SimpleArray>();

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
    writer.write::<U32Be>(1); // SimpleArray::inner[0]
    writer.write::<U32Be>(2); // SimpleArray::inner[1]
    writer.write::<U32Be>(3); // SimpleArray::inner[2]
    writer.write::<U32Be>(4); // SimpleArray::inner[3]
    writer.write::<U32Be>(5); // SimpleArray::inner[4]
    writer.write::<U32Be>(6); // SimpleArray::inner[5]
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let simple_array = read_scope.read::<fixture::SimpleArray>().unwrap();
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    let simple_array_term =
        binary::read::read_module_item(&mut read_context, &FIXTURE, &"SimpleArray").unwrap();
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
