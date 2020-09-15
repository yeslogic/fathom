#![cfg(test)]

use fathom_runtime::{FormatWriter, ReadError, ReadScope, I8, U8};
use fathom_test_util::fathom::lang::core::{self, binary};
use std::collections::BTreeMap;
use std::iter::FromIterator;

fathom_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_pair.core.fathom");

#[test]
fn eof_first() {
    let writer = FormatWriter::new(vec![]);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match read_context.read_item(&FIXTURE, &"Pair") {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn eof_second() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(255); // Pair::first

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match read_context.read_item(&FIXTURE, &"Pair") {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_pair() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(31); // Pair::first
    writer.write::<I8>(-30); // Pair::second

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match read_context.read_item(&FIXTURE, &"Pair").unwrap() {
        binary::Term::Struct(fields) => {
            assert_eq!(
                fields,
                BTreeMap::from_iter(vec![
                    ("first".to_owned(), binary::Term::int(31)),
                    ("second".to_owned(), binary::Term::int(-30)),
                ]),
            );
        }
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_pair_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(255); // Pair::first
    writer.write::<I8>(-30); // Pair::second
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match read_context.read_item(&FIXTURE, &"Pair").unwrap() {
        binary::Term::Struct(fields) => {
            assert_eq!(
                fields,
                BTreeMap::from_iter(vec![
                    ("first".to_owned(), binary::Term::int(255)),
                    ("second".to_owned(), binary::Term::int(-30)),
                ]),
            );
        }
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}
