#![cfg(test)]

use ddl_test_util::ddl::binary;
use ddl_rt::{ReadError, ReadScope, FormatWriter, U8};
use std::collections::BTreeMap;
use std::iter::FromIterator;

#[path = "../../snapshots/struct/pass_singleton.rs"]
mod fixture;

ddl_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_singleton.core.ddl");

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
    writer.write::<U8>(31); // Byte::inner

    let scope = ReadScope::new(writer.buffer());
    let singleton = scope.read::<fixture::Byte>().unwrap();

    match binary::read::read_module_item(&FIXTURE, &"Byte", &mut scope.reader()).unwrap() {
        binary::Term::Struct(fields) => {
            assert_eq!(singleton.inner(), 31);

            assert_eq!(fields, BTreeMap::from_iter(vec![
                ("inner".to_owned(), binary::Term::Int(singleton.inner().into())),
            ]));
        }
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_singleton_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(255); // Byte::inner
    writer.write::<U8>(42);

    let scope = ReadScope::new(writer.buffer());
    let singleton = scope.read::<fixture::Byte>().unwrap();

    match binary::read::read_module_item(&FIXTURE, &"Byte", &mut scope.reader()).unwrap() {
        binary::Term::Struct(fields) => {
            assert_eq!(singleton.inner(), 255);

            assert_eq!(fields, BTreeMap::from_iter(vec![
                ("inner".to_owned(), binary::Term::Int(singleton.inner().into())),
            ]));
        }
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}
