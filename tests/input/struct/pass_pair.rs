#![cfg(test)]

use ddl_rt::{FormatWriter, ReadError, ReadScope, I8, U8};
use ddl_test_util::ddl::ast::core::{self, binary};
use std::collections::BTreeMap;
use std::iter::FromIterator;

#[path = "../../snapshots/struct/pass_pair.rs"]
pub mod fixture;

ddl_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_pair.core.ddl");

#[test]
fn eof_first() {
    let writer = FormatWriter::new(vec![]);

    let read_scope = ReadScope::new(writer.buffer());
    let pair = read_scope.read::<fixture::Pair>();

    match pair {
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

    let read_scope = ReadScope::new(writer.buffer());
    let pair = read_scope.read::<fixture::Pair>();

    match pair {
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
    let pair = read_scope.read::<fixture::Pair>().unwrap();
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match binary::read::read_module_item(&mut read_context, &FIXTURE, &"Pair").unwrap() {
        binary::Term::Struct(fields) => {
            assert_eq!(pair.first(), 31);
            assert_eq!(pair.second(), -30);

            assert_eq!(
                fields,
                BTreeMap::from_iter(vec![
                    ("first".to_owned(), binary::Term::Int(pair.first().into())),
                    ("second".to_owned(), binary::Term::Int(pair.second().into())),
                ])
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
    let pair = read_scope.read::<fixture::Pair>().unwrap();
    let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

    match binary::read::read_module_item(&mut read_context, &FIXTURE, &"Pair").unwrap() {
        binary::Term::Struct(fields) => {
            assert_eq!(pair.first(), 255);
            assert_eq!(pair.second(), -30);

            assert_eq!(
                fields,
                BTreeMap::from_iter(vec![
                    ("first".to_owned(), binary::Term::Int(pair.first().into())),
                    ("second".to_owned(), binary::Term::Int(pair.second().into())),
                ])
            );
        }
        _ => panic!("struct expected"),
    }

    // TODO: Check remaining
}
