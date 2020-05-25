#![cfg(test)]

use ddl_rt::{F64Be, FormatWriter, ReadError, ReadScope, U8};
use ddl_test_util::ddl::ast::core::{self, binary};

#[path = "../../snapshots/alias/pass_if_else_format_type_item.rs"]
pub mod fixture;

ddl_test_util::core_module!(
    FIXTURE,
    "../../snapshots/alias/pass_if_else_format_type_item.core.ddl"
);

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let read_scope = ReadScope::new(writer.buffer());
    let singleton = read_scope.read::<fixture::Test>();

    match singleton {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_test() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<F64Be>(23.64e10); // Test::inner

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let singleton = read_scope.read::<fixture::Test>().unwrap();
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    let test = binary::read::read_module_item(&mut read_context, &FIXTURE, &"Test").unwrap();
    assert_eq!(singleton, 23.64e10);
    assert_eq!(test, binary::Term::F64(singleton));

    // TODO: Check remaining
}

#[test]
fn valid_test_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<F64Be>(781.453298); // Test::inner
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let singleton = read_scope.read::<fixture::Test>().unwrap();
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    let test = binary::read::read_module_item(&mut read_context, &FIXTURE, &"Test").unwrap();
    assert_eq!(singleton, 781.453298);
    assert_eq!(test, binary::Term::F64(singleton));

    // TODO: Check remaining
}
