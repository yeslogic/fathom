#![cfg(test)]

use ddl_rt::{FormatWriter, ReadScope, U8};
use ddl_test_util::ddl::{binary, core};
use std::collections::BTreeMap;
use std::iter::FromIterator;

#[path = "../../snapshots/struct/pass_empty.rs"]
mod fixture;

ddl_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_empty.core.ddl");

#[test]
fn valid_empty() {
    let writer = FormatWriter::new(vec![]);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match (
        read_scope.read::<fixture::Empty>().unwrap(),
        binary::read::read_module_item(&mut read_context, &FIXTURE, "Empty").unwrap(),
    ) {
        (fixture::Empty {}, binary::Term::Struct(fields)) => {
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

    match (
        read_scope.read::<fixture::Empty>().unwrap(),
        binary::read::read_module_item(&mut read_context, &FIXTURE, "Empty").unwrap(),
    ) {
        (fixture::Empty {}, binary::Term::Struct(fields)) => {
            assert_eq!(fields, BTreeMap::from_iter(vec![]));
        }
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}
