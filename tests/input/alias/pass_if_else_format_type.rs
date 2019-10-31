#![cfg(test)]

use ddl_test_util::ddl::binary;
use ddl_rt::{ReadError, ReadScope, FormatWriter, U8, F64Be};

#[path = "../../snapshots/alias/pass_if_else_format_type.rs"]
mod fixture;

ddl_test_util::core_module!(FIXTURE, "../../snapshots/alias/pass_if_else_format_type.core.ddl");

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let scope = ReadScope::new(writer.buffer());
    let singleton = scope.read::<fixture::Test>();

    match singleton {
        Err(ReadError::Eof(_)) => {},
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)")
    }

    // TODO: Check remaining
}

#[test]
fn valid_test() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<F64Be>(23.64e10); // Test::inner

    let scope = ReadScope::new(writer.buffer());
    let singleton = scope.read::<fixture::Test>().unwrap();

    let test = binary::read::read_module_item(&FIXTURE, &"Test", &mut scope.reader()).unwrap();
    assert_eq!(singleton.inner(), ddl_rt::Either::Left(23.64e10));

    assert_eq!(test, binary::Term::F64(singleton.inner().left().unwrap().into()));

    // TODO: Check remaining
}

#[test]
fn valid_test_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<F64Be>(781.453298); // Test::inner
    writer.write::<U8>(42);

    let scope = ReadScope::new(writer.buffer());
    let singleton = scope.read::<fixture::Test>().unwrap();

    let test = binary::read::read_module_item(&FIXTURE, &"Test", &mut scope.reader()).unwrap();
    assert_eq!(singleton.inner(), ddl_rt::Either::Left(781.453298));

    assert_eq!(test, binary::Term::F64(singleton.inner().left().unwrap().into()));

    // TODO: Check remaining
}
