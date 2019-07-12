#![cfg(test)]

use ddl_test_util::ddl::binary;
use ddl_test_util::ddl_rt::{ReadScope, WriteCtxt, U8};
use std::collections::BTreeMap;
use std::iter::FromIterator;

mod fixture {
    use ddl_test_util::ddl_rt;
    include!("../../snapshots/struct/pass_empty.rs");
}

ddl_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_empty.core.ddl");

#[test]
fn valid_empty() {
    let ctxt = WriteCtxt::new(vec![]);

    let scope = ReadScope::new(ctxt.buffer());

    match (
        scope.read::<fixture::Empty>().unwrap(),
        binary::read::read_module_item(&FIXTURE, "Empty", &mut scope.ctxt()).unwrap(),
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
    let mut ctxt = WriteCtxt::new(vec![]);
    ctxt.write::<U8>(42);

    let scope = ReadScope::new(ctxt.buffer());

    match (
        scope.read::<fixture::Empty>().unwrap(),
        binary::read::read_module_item(&FIXTURE, "Empty", &mut scope.ctxt()).unwrap(),
    ) {
        (fixture::Empty {}, binary::Term::Struct(fields)) => {
            assert_eq!(fields, BTreeMap::from_iter(vec![]));
        },
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}
