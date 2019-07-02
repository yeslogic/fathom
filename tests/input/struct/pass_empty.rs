#![cfg(test)]

use ddl_rt::{ReadScope, WriteCtxt, U8};

#[path = "../../snapshots/struct/pass_empty.rs"]
mod empty;

#[test]
fn valid_empty() {
    let ctxt = WriteCtxt::new(vec![]);

    let mut scope = ReadScope::new(ctxt.buffer());
    let empty::Empty {} = scope.read::<empty::Empty>().unwrap();

    // TODO: Check remaining
}

#[test]
fn valid_empty_trailing() {
    let mut ctxt = WriteCtxt::new(vec![]);
    ctxt.write::<U8>(42);

    let mut scope = ReadScope::new(ctxt.buffer());
    let empty::Empty {} = scope.read::<empty::Empty>().unwrap();

    // TODO: Check remaining
}
