#![cfg(test)]

use fathom_runtime::{FormatWriter, ReadError, ReadScope, U32Be, U8};
use fathom_test_util::fathom::lang::core::semantics::Value;
use fathom_test_util::fathom::lang::core::{self, binary};
use std::collections::BTreeMap;
use std::iter::FromIterator;
use std::sync::Arc;

fathom_test_util::core_module!(
    FIXTURE,
    "../../snapshots/struct/dependent_fields.core.fathom"
);

#[test]
fn eof_len() {
    let writer = FormatWriter::new(vec![]);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals);

    match read_context.read_item(&mut read_scope.reader(), &FIXTURE, &"ArrayFormat") {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn eof_data() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U32Be>(255); // ArrayFormat::len
    writer.write::<U32Be>(1); // ArrayFormat::data[0]

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals);

    match read_context.read_item(&mut read_scope.reader(), &FIXTURE, &"ArrayFormat") {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_array_format() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U32Be>(3); // ArrayFormat::len
    writer.write::<U32Be>(1); // ArrayFormat::data[0]
    writer.write::<U32Be>(2); // ArrayFormat::data[1]
    writer.write::<U32Be>(3); // ArrayFormat::data[2]

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context
            .read_item(&mut read_scope.reader(), &FIXTURE, &"ArrayFormat")
            .unwrap(),
        Value::StructTerm(BTreeMap::from_iter(vec![
            ("len".to_owned(), Arc::new(Value::int(3))),
            (
                "data".to_owned(),
                Arc::new(Value::ArrayTerm(vec![
                    Arc::new(Value::int(1)),
                    Arc::new(Value::int(2)),
                    Arc::new(Value::int(3)),
                ])),
            ),
        ]),),
    );

    // TODO: Check remaining
}

#[test]
fn valid_array_format_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U32Be>(0); // ArrayFormat::len
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let read_scope = ReadScope::new(writer.buffer());
    let mut read_context = binary::read::Context::new(&globals);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context
            .read_item(&mut read_scope.reader(), &FIXTURE, &"ArrayFormat")
            .unwrap(),
        Value::StructTerm(BTreeMap::from_iter(vec![
            ("len".to_owned(), Arc::new(Value::int(0))),
            ("data".to_owned(), Arc::new(Value::ArrayTerm(Vec::new()))),
        ])),
    );

    // TODO: Check remaining
}
