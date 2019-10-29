#![cfg(test)]

use ddl_test_util::ddl::binary;
use ddl_rt::{ReadScope, FormatWriter, U8};
use std::collections::BTreeMap;
use std::iter::FromIterator;

#[path = "../../snapshots/struct/pass_empty.rs"]
mod fixture;

ddl_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_empty.core.ddl");

#[test]
fn valid_empty() {
    let writer = FormatWriter::new(vec![]);

    let scope = ReadScope::new(writer.buffer());

    match (
        scope.read::<fixture::Empty>().unwrap(),
        binary::read::read_module_item(&FIXTURE, "Empty", &mut scope.reader()).unwrap(),
    ) {
        (fixture::Empty {}, binary::Term::Struct(fields)) => {
            assert_eq!(fields, BTreeMap::from_iter(vec![]));
        },
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_empty_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(42);

    let scope = ReadScope::new(writer.buffer());

    match (
        scope.read::<fixture::Empty>().unwrap(),
        binary::read::read_module_item(&FIXTURE, "Empty", &mut scope.reader()).unwrap(),
    ) {
        (fixture::Empty {}, binary::Term::Struct(fields)) => {
            assert_eq!(fields, BTreeMap::from_iter(vec![]));
        },
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}
