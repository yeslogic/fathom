#![cfg(test)]

use ddl_rt::{I8, ReadError, ReadScope, WriteCtxt, U8};

#[path = "../../snapshots/struct/pass_pair.rs"]
mod pair;

#[test]
fn eof_first() {
    let ctxt = WriteCtxt::new(vec![]);

    let mut scope = ReadScope::new(ctxt.buffer());
    let pair = scope.read::<pair::Pair>();

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
    let pair = scope.read::<pair::Pair>();

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
    let pair = scope.read::<pair::Pair>().unwrap();

    assert_eq!(pair.first, 31);
    assert_eq!(pair.second, -30);

    // TODO: Check remaining
}

#[test]
fn valid_pair_trailing() {
    let mut ctxt = WriteCtxt::new(vec![]);
    ctxt.write::<U8>(255); // Pair::first
    ctxt.write::<I8>(-30); // Pair::second
    ctxt.write::<U8>(42);

    let mut scope = ReadScope::new(ctxt.buffer());
    let pair = scope.read::<pair::Pair>().unwrap();

    assert_eq!(pair.first, 255);
    assert_eq!(pair.second, -30);

    // TODO: Check remaining
}
