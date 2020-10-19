#![cfg(test)]

use fathom_runtime::{FormatWriter, ReadScope, U8};
use fathom_test_util::fathom::lang::core::{self, binary};
use std::collections::BTreeMap;
use std::iter::FromIterator;

fathom_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_empty.core.fathom");

#[test]
fn valid_empty() {
    let writer = FormatWriter::new(vec![]);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match read_context.read_item(&FIXTURE, "EmptyFormat").unwrap() {
        binary::Term::Struct(fields) => {
            assert_eq!(fields, BTreeMap::from_iter(vec![]));
        }
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_empty_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match read_context.read_item(&FIXTURE, "EmptyFormat").unwrap() {
        binary::Term::Struct(fields) => {
            assert_eq!(fields, BTreeMap::from_iter(vec![]));
        }
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}
