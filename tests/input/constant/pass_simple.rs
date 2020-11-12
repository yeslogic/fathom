#![cfg(test)]

use fathom_runtime::{FormatWriter, ReadError, ReadScope, U8};
use fathom_test_util::fathom::lang::core::{self, binary};

fathom_test_util::core_module!(FIXTURE, "../../snapshots/constant/pass_simple.core.fathom");

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match read_context.read_item(&FIXTURE, &"Byte") {
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
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    let byte = read_context.read_item(&FIXTURE, &"Byte").unwrap();

    assert_eq!(byte, binary::Term::int(31));

    // TODO: Check remaining
}

#[test]
fn valid_singleton_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(255); // Byte
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    let byte = read_context.read_item(&FIXTURE, &"Byte").unwrap();

    assert_eq!(byte, binary::Term::int(255));

    // TODO: Check remaining
}
