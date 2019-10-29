#![cfg(test)]

use ddl_test_util::ddl::binary;
use ddl_rt::{I8, ReadError, ReadScope, FormatWriter, U8};
use std::collections::BTreeMap;
use std::iter::FromIterator;

#[path = "../../snapshots/struct/pass_pair.rs"]
mod fixture;

ddl_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_pair.core.ddl");

#[test]
fn eof_first() {
    let writer = FormatWriter::new(vec![]);

    let scope = ReadScope::new(writer.buffer());
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
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(255); // Pair::first

    let scope = ReadScope::new(writer.buffer());
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
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(31); // Pair::first
    writer.write::<I8>(-30); // Pair::second

    let scope = ReadScope::new(writer.buffer());
    let pair = scope.read::<fixture::Pair>().unwrap();

    match binary::read::read_module_item(&FIXTURE, &"Pair", &mut scope.reader()).unwrap() {
        binary::Term::Struct(fields) => {
            assert_eq!(pair.first(), 31);
            assert_eq!(pair.second(), -30);

            assert_eq!(fields, BTreeMap::from_iter(vec![
                ("first".to_owned(), binary::Term::Int(pair.first().into())),
                ("second".to_owned(), binary::Term::Int(pair.second().into())),
            ]));
        },
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

    let scope = ReadScope::new(writer.buffer());
    let pair = scope.read::<fixture::Pair>().unwrap();

    match binary::read::read_module_item(&FIXTURE, &"Pair", &mut scope.reader()).unwrap() {
        binary::Term::Struct(fields) => {
            assert_eq!(pair.first(), 255);
            assert_eq!(pair.second(), -30);

            assert_eq!(fields, BTreeMap::from_iter(vec![
                ("first".to_owned(), binary::Term::Int(pair.first().into())),
                ("second".to_owned(), binary::Term::Int(pair.second().into())),
            ]));
        },
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}
