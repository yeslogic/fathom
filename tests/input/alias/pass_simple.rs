#![cfg(test)]

use ddl_test_util::ddl::binary;
use ddl_rt::{ReadError, ReadScope, FormatWriter, U8};

#[path = "../../snapshots/alias/pass_simple.rs"]
mod fixture;

ddl_test_util::core_module!(FIXTURE, "../../snapshots/alias/pass_simple.core.ddl");

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let scope = ReadScope::new(writer.buffer());
    let singleton = scope.read::<fixture::Byte>();

    match singleton {
        Err(ReadError::Eof(_)) => {},
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)")
    }

    // TODO: Check remaining
}

#[test]
fn valid_singleton() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(31); // Byte

    let scope = ReadScope::new(writer.buffer());

    let inner = scope.read::<fixture::Byte>().unwrap();
    let byte = binary::read::read_module_item(&FIXTURE, &"Byte", &mut scope.reader()).unwrap();

    assert_eq!(inner, 31);
    assert_eq!(byte, binary::Term::Int(inner.into()));

    // TODO: Check remaining
}

#[test]
fn valid_singleton_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(255); // Byte
    writer.write::<U8>(42);

    let scope = ReadScope::new(writer.buffer());

    let inner = scope.read::<fixture::Byte>().unwrap();
    let byte = binary::read::read_module_item(&FIXTURE, &"Byte", &mut scope.reader()).unwrap();

    assert_eq!(inner, 255);
    assert_eq!(byte, binary::Term::Int(inner.into()));

    // TODO: Check remaining
}
