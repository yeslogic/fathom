#![cfg(test)]

use fathom_runtime::{FormatWriter, ReadError, ReadScope, U8};
use fathom_test_util::fathom::lang::core::{self, binary};
use std::collections::BTreeMap;
use std::iter::FromIterator;

#[path = "../../snapshots/struct/pass_singleton.rs"]
pub mod fixture;

fathom_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_singleton.core.fathom");

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let scope = ReadScope::new(writer.buffer());
    let singleton = scope.read::<fixture::Byte>();

    match singleton {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_singleton() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(31); // Byte::inner

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let singleton = read_scope.read::<fixture::Byte>().unwrap();
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match binary::read::from_module_item(&mut read_context, &FIXTURE, &"Byte").unwrap() {
        binary::Term::Struct(fields) => {
            assert_eq!(singleton.inner(), 31);

            assert_eq!(
                fields,
                BTreeMap::from_iter(vec![(
                    "inner".to_owned(),
                    binary::Term::Int(singleton.inner().into())
                ),])
            );
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

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let singleton = read_scope.read::<fixture::Byte>().unwrap();
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match binary::read::from_module_item(&mut read_context, &FIXTURE, &"Byte").unwrap() {
        binary::Term::Struct(fields) => {
            assert_eq!(singleton.inner(), 255);

            assert_eq!(
                fields,
                BTreeMap::from_iter(vec![(
                    "inner".to_owned(),
                    binary::Term::Int(singleton.inner().into())
                ),])
            );
        }
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}
