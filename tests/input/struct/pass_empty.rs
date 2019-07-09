#![cfg(test)]

use ddl_test_util::ddl;
use ddl_rt::{ReadScope, WriteCtxt, U8};
use std::collections::BTreeMap;
use std::iter::FromIterator;

#[path = "../../snapshots/struct/pass_empty.rs"]
mod fixture;
ddl_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_empty.core.ddl");

#[test]
fn valid_empty() {
    let ctxt = WriteCtxt::new(vec![]);

    let mut scope = ReadScope::new(ctxt.buffer());
    let fixture::Empty {} = scope.read::<fixture::Empty>().unwrap();

    match ddl::binary::read::read_module_item(&FIXTURE, "Empty", &mut scope.ctxt()).unwrap() {
        ddl::binary::Term::Struct(fields) => {
            assert_eq!(fields, BTreeMap::from_iter(vec![]));
        },
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_empty_trailing() {
    let mut ctxt = WriteCtxt::new(vec![]);
    ctxt.write::<U8>(42);

    let mut scope = ReadScope::new(ctxt.buffer());
    let fixture::Empty {} = scope.read::<fixture::Empty>().unwrap();

    match ddl::binary::read::read_module_item(&FIXTURE, "Empty", &mut scope.ctxt()).unwrap() {
        ddl::binary::Term::Struct(fields) => {
            assert_eq!(fields, BTreeMap::from_iter(vec![]));
        },
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}
