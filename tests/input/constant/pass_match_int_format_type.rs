#![cfg(test)]

use fathom_runtime::{F64Le, FormatWriter, ReadError, ReadScope, U8};
use fathom_test_util::fathom::lang::core::semantics::Value;
use fathom_test_util::fathom::lang::core::{self, binary};

fathom_test_util::core_module!(
    FIXTURE,
    "../../snapshots/constant/pass_match_int_format_type.core.fathom"
);

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    match read_context.read_item(&mut reader, &"Test") {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_test() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<F64Le>(23.64e10); // Test::inner

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context.read_item(&mut reader, &"Test").unwrap(),
        Value::f64(23.64e10),
    );

    // TODO: Check remaining
}

#[test]
fn valid_test_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<F64Le>(781.453298); // Test::inner
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context.read_item(&mut reader, &"Test").unwrap(),
        Value::f64(781.453298),
    );

    // TODO: Check remaining
}
