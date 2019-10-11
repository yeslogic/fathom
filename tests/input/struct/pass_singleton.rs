#![cfg(test)]

use ddl_test_util::ddl::binary;
use ddl_rt::{ReadError, ReadScope, WriteCtxt, U8};
use std::collections::BTreeMap;
use std::iter::FromIterator;

#[path = "../../snapshots/struct/pass_singleton.rs"]
mod fixture;

ddl_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_singleton.core.ddl");

#[test]
fn eof_inner() {
    let ctxt = WriteCtxt::new(vec![]);

    let scope = ReadScope::new(ctxt.buffer());
    let singleton = scope.read::<fixture::Byte>();

    match singleton {
        Err(ReadError::Eof(_)) => {},
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)")
    }

    // TODO: Check remaining
}

#[test]
fn valid_singleton() {
    let mut ctxt = WriteCtxt::new(vec![]);
    ctxt.write::<U8>(31); // Byte::inner

    let scope = ReadScope::new(ctxt.buffer());

    match (
        scope.read::<fixture::Byte>().unwrap(),
        binary::read::read_module_item(&FIXTURE, &"Byte", &mut scope.ctxt()).unwrap(),
    ) {
        (fixture::Byte { inner }, binary::Term::Struct(fields)) => {
            assert_eq!(inner, 31);

            assert_eq!(fields, BTreeMap::from_iter(vec![
                ("inner".to_owned(), binary::Term::Int(inner.into())),
            ]));
        }
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_singleton_trailing() {
    let mut ctxt = WriteCtxt::new(vec![]);
    ctxt.write::<U8>(255); // Byte::inner
    ctxt.write::<U8>(42);

    let scope = ReadScope::new(ctxt.buffer());

    match (
        scope.read::<fixture::Byte>().unwrap(),
        binary::read::read_module_item(&FIXTURE, &"Byte", &mut scope.ctxt()).unwrap(),
    ) {
        (fixture::Byte { inner }, binary::Term::Struct(fields)) => {
            assert_eq!(inner, 255);

            assert_eq!(fields, BTreeMap::from_iter(vec![
                ("inner".to_owned(), binary::Term::Int(inner.into())),
            ]));
        }
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}
