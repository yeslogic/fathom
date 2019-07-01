#![cfg(test)]

use ddl_rt::{ReadError, ReadScope, WriteCtxt, U8};

#[path = "../../snapshots/struct/pass_singleton.rs"]
mod singleton;

#[test]
fn eof_inner() {
    let ctxt = WriteCtxt::new(vec![]);

    let mut scope = ReadScope::new(ctxt.buffer());
    let singleton = scope.read::<singleton::Byte>();

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

    let mut scope = ReadScope::new(ctxt.buffer());
    let singleton = scope.read::<singleton::Byte>().unwrap();

    assert_eq!(singleton.inner, 31);

    // TODO: Check remaining
}

#[test]
fn valid_singleton_trailing() {
    let mut ctxt = WriteCtxt::new(vec![]);
    ctxt.write::<U8>(255); // Byte::inner
    ctxt.write::<U8>(42);

    let mut scope = ReadScope::new(ctxt.buffer());
    let singleton = scope.read::<singleton::Byte>().unwrap();

    assert_eq!(singleton.inner, 255);

    // TODO: Check remaining
}
