#![cfg(test)]

use fathom_runtime::{FormatWriter, ReadError, ReadScope, U32Be, U8};
use fathom_test_util::fathom::lang::core::semantics::Value;
use fathom_test_util::fathom::lang::core::{self, binary};
use std::sync::Arc;

fathom_test_util::core_module!(FIXTURE, "./snapshots/pass_format_array.core.fathom");

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    match read_context.read_item(&mut reader, &"SimpleFormatArray") {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_test() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U32Be>(1); // SimpleFormatArray::inner[0]
    writer.write::<U32Be>(2); // SimpleFormatArray::inner[1]
    writer.write::<U32Be>(3); // SimpleFormatArray::inner[2]
    writer.write::<U32Be>(4); // SimpleFormatArray::inner[3]
    writer.write::<U32Be>(5); // SimpleFormatArray::inner[4]
    writer.write::<U32Be>(6); // SimpleFormatArray::inner[5]

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context
            .read_item(&mut reader, &"SimpleFormatArray")
            .unwrap(),
        (
            Value::ArrayTerm(vec![
                Arc::new(Value::int(1)),
                Arc::new(Value::int(2)),
                Arc::new(Value::int(3)),
                Arc::new(Value::int(4)),
                Arc::new(Value::int(5)),
                Arc::new(Value::int(6)),
            ]),
            Vec::new(),
        ),
    );

    // TODO: Check remaining
}

#[test]
fn invalid_test_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U32Be>(1); // SimpleFormatArray::inner[0]
    writer.write::<U32Be>(2); // SimpleFormatArray::inner[1]
    writer.write::<U32Be>(3); // SimpleFormatArray::inner[2]
    writer.write::<U32Be>(4); // SimpleFormatArray::inner[3]
    writer.write::<U32Be>(5); // SimpleFormatArray::inner[4]
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    match read_context.read_item(&mut reader, &"SimpleFormatArray") {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_test_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U32Be>(1); // SimpleFormatArray::inner[0]
    writer.write::<U32Be>(2); // SimpleFormatArray::inner[1]
    writer.write::<U32Be>(3); // SimpleFormatArray::inner[2]
    writer.write::<U32Be>(4); // SimpleFormatArray::inner[3]
    writer.write::<U32Be>(5); // SimpleFormatArray::inner[4]
    writer.write::<U32Be>(6); // SimpleFormatArray::inner[5]
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context
            .read_item(&mut reader, &"SimpleFormatArray")
            .unwrap(),
        (
            Value::ArrayTerm(vec![
                Arc::new(Value::int(1)),
                Arc::new(Value::int(2)),
                Arc::new(Value::int(3)),
                Arc::new(Value::int(4)),
                Arc::new(Value::int(5)),
                Arc::new(Value::int(6)),
            ]),
            Vec::new(),
        ),
    );

    // TODO: Check remaining
}
