#![cfg(test)]

use ddl_test_util::ddl;
use ddl_rt::{I8, ReadError, ReadScope, WriteCtxt, U8};
use std::collections::BTreeMap;
use std::iter::FromIterator;

#[path = "../../snapshots/struct/pass_pair.rs"]
mod fixture;
ddl_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_pair.core.ddl");

#[test]
fn eof_first() {
    let ctxt = WriteCtxt::new(vec![]);

    let mut scope = ReadScope::new(ctxt.buffer());
    let pair = scope.read::<fixture::Pair>();

    match pair {
        Err(ReadError::Eof(_)) => {},
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)")
    }

    // TODO: Check remaining
}

#[test]
fn eof_second() {
    let mut ctxt = WriteCtxt::new(vec![]);
    ctxt.write::<U8>(255); // Pair::first

    let mut scope = ReadScope::new(ctxt.buffer());
    let pair = scope.read::<fixture::Pair>();

    match pair {
        Err(ReadError::Eof(_)) => {},
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)")
    }

    // TODO: Check remaining
}

#[test]
fn valid_pair() {
    let mut ctxt = WriteCtxt::new(vec![]);
    ctxt.write::<U8>(31); // Pair::first
    ctxt.write::<I8>(-30); // Pair::second

    let mut scope = ReadScope::new(ctxt.buffer());
    let pair = scope.read::<fixture::Pair>().unwrap();

    assert_eq!(pair.first, 31);
    assert_eq!(pair.second, -30);

    match ddl::binary::read::read_module_item(&FIXTURE, &"Pair", &mut scope.ctxt()).unwrap() {
        ddl::binary::Term::Struct(fields) => {
            assert_eq!(fields, BTreeMap::from_iter(vec![
                ("first".to_owned(), ddl::binary::Term::U8(pair.first)),
                ("second".to_owned(), ddl::binary::Term::S8(pair.second)),
            ]));
        },
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_pair_trailing() {
    let mut ctxt = WriteCtxt::new(vec![]);
    ctxt.write::<U8>(255); // Pair::first
    ctxt.write::<I8>(-30); // Pair::second
    ctxt.write::<U8>(42);

    let mut scope = ReadScope::new(ctxt.buffer());
    let pair = scope.read::<fixture::Pair>().unwrap();

    assert_eq!(pair.first, 255);
    assert_eq!(pair.second, -30);

    match ddl::binary::read::read_module_item(&FIXTURE, &"Pair", &mut scope.ctxt()).unwrap() {
        ddl::binary::Term::Struct(fields) => {
            assert_eq!(fields, BTreeMap::from_iter(vec![
                ("first".to_owned(), ddl::binary::Term::U8(pair.first)),
                ("second".to_owned(), ddl::binary::Term::S8(pair.second)),
            ]));
        },
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}
