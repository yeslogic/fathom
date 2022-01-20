#![cfg(test)]

use fathom_runtime::{FormatWriter, ReadError, ReadScope, I8, U8};
use fathom_test_util::fathom::lang::core::semantics::Value;
use fathom_test_util::fathom::lang::core::{self, binary};
use std::collections::BTreeMap;
use std::iter::FromIterator;
use std::sync::Arc;

fathom_test_util::core_module!(FIXTURE, "./snapshots/pass_pair.core.fathom");

#[test]
fn eof_first() {
    let writer = FormatWriter::new(vec![]);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    match read_context.read_item(&mut reader, &"PairFormat") {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn eof_second() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(255); // PairFormat::first

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    match read_context.read_item(&mut reader, &"PairFormat") {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_pair() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(31); // PairFormat::first
    writer.write::<I8>(-30); // PairFormat::second

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context.read_item(&mut reader, &"PairFormat").unwrap(),
        (
            Value::StructTerm(BTreeMap::from_iter(vec![
                ("first".to_owned(), Arc::new(Value::int(31))),
                ("second".to_owned(), Arc::new(Value::int(-30))),
            ])),
            Vec::new(),
        ),
    );

    // TODO: Check remaining
}

#[test]
fn valid_pair_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(255); // PairFormat::first
    writer.write::<I8>(-30); // PairFormat::second
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context.read_item(&mut reader, &"PairFormat").unwrap(),
        (
            Value::StructTerm(BTreeMap::from_iter(vec![
                ("first".to_owned(), Arc::new(Value::int(255))),
                ("second".to_owned(), Arc::new(Value::int(-30))),
            ])),
            Vec::new(),
        ),
    );

    // TODO: Check remaining
}
