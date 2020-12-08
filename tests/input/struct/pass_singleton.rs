#![cfg(test)]

use fathom_runtime::{FormatWriter, ReadError, ReadScope, U8};
use fathom_test_util::fathom::lang::core::semantics::Value;
use fathom_test_util::fathom::lang::core::{self, binary};
use std::collections::BTreeMap;
use std::iter::FromIterator;
use std::sync::Arc;

fathom_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_singleton.core.fathom");

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    match read_context.read_item(&mut reader, &"Byte") {
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
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context.read_item(&mut reader, &"Byte").unwrap(),
        Value::StructTerm(BTreeMap::from_iter(vec![(
            "inner".to_owned(),
            Arc::new(Value::int(31)),
        )])),
    );

    // TODO: Check remaining
}

#[test]
fn valid_singleton_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(255); // Byte::inner
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context.read_item(&mut reader, &"Byte").unwrap(),
        Value::StructTerm(BTreeMap::from_iter(vec![(
            "inner".to_owned(),
            Arc::new(Value::int(255)),
        )])),
    );

    // TODO: Check remaining
}
