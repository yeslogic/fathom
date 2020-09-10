#![cfg(test)]

use fathom_runtime::{FormatWriter, ReadError, ReadScope, U8};
use fathom_test_util::fathom::lang::core::{self, binary};

#[path = "../../snapshots/alias/pass_simple.rs"]
pub mod fixture;

fathom_test_util::core_module!(FIXTURE, "../../snapshots/alias/pass_simple.core.fathom");

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let read_scope = ReadScope::new(writer.buffer());
    let singleton = read_scope.read::<fixture::Byte>();

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
    writer.write::<U8>(31); // Byte

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let inner = read_scope.read::<fixture::Byte>().unwrap();
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    let byte = binary::read::from_module_item(&mut read_context, &FIXTURE, &"Byte").unwrap();

    assert_eq!(inner, 31);
    assert_eq!(byte, binary::Term::Int(inner.into()));

    // TODO: Check remaining
}

#[test]
fn valid_singleton_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(255); // Byte
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let inner = read_scope.read::<fixture::Byte>().unwrap();
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    let byte = binary::read::from_module_item(&mut read_context, &FIXTURE, &"Byte").unwrap();

    assert_eq!(inner, 255);
    assert_eq!(byte, binary::Term::Int(inner.into()));

    // TODO: Check remaining
}
