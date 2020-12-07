#![cfg(test)]

use fathom_runtime::{FormatWriter, ReadError, ReadScope, U16Be, U8};
use fathom_test_util::fathom::lang::core::semantics::Value;
use fathom_test_util::fathom::lang::core::{self, binary};
use std::collections::BTreeMap;
use std::iter::FromIterator;
use std::sync::Arc;

fathom_test_util::core_module!(FIXTURE, "../../snapshots/struct/positions.core.fathom");

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    match read_context.read_item(&mut reader, &"Root") {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_root() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(b'p'); //     0 ..  1:   Root::magic[0]
    writer.write::<U8>(b'o'); //     1 ..  2:   Root::magic[1]
    writer.write::<U8>(b's'); //     2 ..  3:   Root::magic[2]
    writer.write::<U8>(b' '); //     3 ..  4:   Root::magic[3]
    writer.write::<U16Be>(10); //    4 ..  6:   Root::offset1
    writer.write::<U16Be>(15); //    6 ..  8:   Root::offset2
    writer.write::<U8>(0); //        8 ..  9:   ...
    writer.write::<U8>(0); //        9 .. 10:   ...
    writer.write::<U16Be>(1); //    10 .. 12:   Chunk::width
    writer.write::<U16Be>(2); //    12 .. 14:   Chunk::height
    writer.write::<U8>(0); //       14 .. 15:   ...
    writer.write::<U16Be>(3); //    15 .. 17:   Chunk::width
    writer.write::<U16Be>(4); //    17 .. 19:   Chunk::height

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context.read_item(&mut reader, &"Root").unwrap(),
        (
            Value::StructTerm(BTreeMap::from_iter(vec![
                ("start".to_owned(), Arc::new(Value::pos(0))),
                (
                    "magic".to_owned(),
                    Arc::new(Value::ArrayTerm(vec![
                        Arc::new(Value::int(b'p')),
                        Arc::new(Value::int(b'o')),
                        Arc::new(Value::int(b's')),
                        Arc::new(Value::int(b' ')),
                    ])),
                ),
                ("offset1".to_owned(), Arc::new(Value::int(10))),
                ("offset2".to_owned(), Arc::new(Value::int(15))),
                ("position1".to_owned(), Arc::new(Value::pos(10))),
                ("position2".to_owned(), Arc::new(Value::pos(15))),
            ])),
            vec![
                (
                    10,
                    Value::StructTerm(BTreeMap::from_iter(vec![
                        ("start".to_owned(), Arc::new(Value::pos(10))),
                        ("width".to_owned(), Arc::new(Value::int(1))),
                        ("height".to_owned(), Arc::new(Value::int(2))),
                    ])),
                ),
                (
                    15,
                    Value::StructTerm(BTreeMap::from_iter(vec![
                        ("start".to_owned(), Arc::new(Value::pos(15))),
                        ("width".to_owned(), Arc::new(Value::int(3))),
                        ("height".to_owned(), Arc::new(Value::int(4))),
                    ])),
                ),
            ],
        ),
    );

    // TODO: Check remaining
}

#[test]
fn valid_root_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(b'p'); //     0 ..  1:   Root::magic[0]
    writer.write::<U8>(b'o'); //     1 ..  2:   Root::magic[1]
    writer.write::<U8>(b's'); //     2 ..  3:   Root::magic[2]
    writer.write::<U8>(b' '); //     3 ..  4:   Root::magic[3]
    writer.write::<U16Be>(8); //     4 ..  6:   Root::offset1
    writer.write::<U16Be>(12); //    6 ..  8:   Root::offset2
    writer.write::<U16Be>(1); //     8 .. 10:   Chunk::width
    writer.write::<U16Be>(2); //    10 .. 12:   Chunk::height
    writer.write::<U16Be>(3); //    12 .. 14:   Chunk::width
    writer.write::<U16Be>(4); //    14 .. 16:   Chunk::height
    writer.write::<U8>(42);

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    fathom_test_util::assert_is_equal!(
        globals,
        read_context.read_item(&mut reader, &"Root").unwrap(),
        (
            Value::StructTerm(BTreeMap::from_iter(vec![
                ("start".to_owned(), Arc::new(Value::pos(0))),
                (
                    "magic".to_owned(),
                    Arc::new(Value::ArrayTerm(vec![
                        Arc::new(Value::int(b'p')),
                        Arc::new(Value::int(b'o')),
                        Arc::new(Value::int(b's')),
                        Arc::new(Value::int(b' ')),
                    ])),
                ),
                ("offset1".to_owned(), Arc::new(Value::int(8))),
                ("offset2".to_owned(), Arc::new(Value::int(12))),
                ("position1".to_owned(), Arc::new(Value::pos(8))),
                ("position2".to_owned(), Arc::new(Value::pos(12))),
            ])),
            vec![
                (
                    8,
                    Value::StructTerm(BTreeMap::from_iter(vec![
                        ("start".to_owned(), Arc::new(Value::pos(8))),
                        ("width".to_owned(), Arc::new(Value::int(1))),
                        ("height".to_owned(), Arc::new(Value::int(2))),
                    ])),
                ),
                (
                    12,
                    Value::StructTerm(BTreeMap::from_iter(vec![
                        ("start".to_owned(), Arc::new(Value::pos(12))),
                        ("width".to_owned(), Arc::new(Value::int(3))),
                        ("height".to_owned(), Arc::new(Value::int(4))),
                    ])),
                ),
            ],
        ),
    );

    // TODO: Check remaining
}

#[test]
fn failing_linked_format() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(b'p'); //     0 ..  1:   Root::magic[0]
    writer.write::<U8>(b'o'); //     1 ..  2:   Root::magic[1]
    writer.write::<U8>(b's'); //     2 ..  3:   Root::magic[2]
    writer.write::<U8>(b' '); //     3 ..  4:   Root::magic[3]
    writer.write::<U16Be>(8); //     4 ..  6:   Root::offset1
    writer.write::<U16Be>(10); //    6 ..  8:   Root::offset2
    writer.write::<U16Be>(1); //     8 .. 10:   Chunk::width

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    match read_context.read_item(&mut reader, &"Root") {
        Err(ReadError::Eof(_)) => {}
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

#[test]
fn duplicate_position() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<U8>(b'p'); //     0 ..  1:   Root::magic[0]
    writer.write::<U8>(b'o'); //     1 ..  2:   Root::magic[1]
    writer.write::<U8>(b's'); //     2 ..  3:   Root::magic[2]
    writer.write::<U8>(b' '); //     3 ..  4:   Root::magic[3]
    writer.write::<U16Be>(8); //     4 ..  6:   Root::offset1
    writer.write::<U16Be>(8); //     6 ..  8:   Root::offset2
    writer.write::<U16Be>(1); //     8 .. 10:   Chunk::width
    writer.write::<U16Be>(2); //    10 .. 12:   Chunk::widtht

    let globals = core::Globals::default();
    let mut reader = ReadScope::new(writer.buffer()).reader();
    let mut read_context = binary::read::Context::new(&globals, &FIXTURE);

    match read_context.read_item(&mut reader, &"Root") {
        Err(ReadError::DuplicatePosition { offset }) => assert_eq!(offset, 8),
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)"),
    }

    // TODO: Check remaining
}

// TODO: offset already seen
