#![cfg(test)]

use fathom_runtime::{FormatWriter, ReadScope, U8};
use fathom_test_util::fathom::lang::core::semantics::Value;
use fathom_test_util::fathom::lang::core::{self, binary};
use std::collections::BTreeMap;
use std::iter::FromIterator;

fathom_test_util::core_module!(FIXTURE, "../../snapshots/struct/pass_empty.core.fathom");

#[test]
fn valid_empty() {
    let writer = FormatWriter::new(vec![]);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context.read_item(&mut reader, "EmptyFormat").unwrap(),
        (Value::StructTerm(BTreeMap::from_iter(vec![])), Vec::new()),
    );

    // TODO: Check remaining
}

#[test]
fn valid_empty_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context.read_item(&mut reader, "EmptyFormat").unwrap(),
        (Value::StructTerm(BTreeMap::from_iter(vec![])), Vec::new()),
    );

    // TODO: Check remaining
}
