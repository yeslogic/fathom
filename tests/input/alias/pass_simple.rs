#![cfg(test)]

use ddl_test_util::ddl::binary;
use ddl_test_util::ddl_rt::{ReadError, ReadScope, WriteCtxt, U8};

mod fixture {
    use ddl_test_util::ddl_rt;
    include!("../../snapshots/alias/pass_simple.rs");
}

ddl_test_util::core_module!(FIXTURE, "../../snapshots/alias/pass_simple.core.ddl");

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
    ctxt.write::<U8>(31); // Byte

    let scope = ReadScope::new(ctxt.buffer());

    let inner = scope.read::<fixture::Byte>().unwrap();
    let byte = binary::read::read_module_item(&FIXTURE, &"Byte", &mut scope.ctxt()).unwrap();

    assert_eq!(inner, 31);
    assert_eq!(byte, binary::Term::U8(inner));

    // TODO: Check remaining
}

#[test]
fn valid_singleton_trailing() {
    let mut ctxt = WriteCtxt::new(vec![]);
    ctxt.write::<U8>(255); // Byte
    ctxt.write::<U8>(42);

    let scope = ReadScope::new(ctxt.buffer());

    let inner = scope.read::<fixture::Byte>().unwrap();
    let byte = binary::read::read_module_item(&FIXTURE, &"Byte", &mut scope.ctxt()).unwrap();

    assert_eq!(inner, 255);
    assert_eq!(byte, binary::Term::U8(inner));

    // TODO: Check remaining
}
