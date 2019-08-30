extern crate ddl_object_id;
extern crate ddl_util;
extern crate hex;

use ddl_object_id::ObjectId;
use ddl_util::FromBinary;
use hex::FromHex;
use std::io::{self, Cursor};

static OBJECT_ID: &str = "564B86EF165BD87B6E595515";

#[test]
fn test_object_id_from_binary() {
    let buf = Vec::from_hex(OBJECT_ID).unwrap();
    let mut cursor = Cursor::new(&buf);

    let object_id = ObjectId::from_binary(&mut cursor).unwrap();

    // Test fields
    assert_eq!(0x564B86EF, object_id.epoch_time);
    assert_eq!(0x165BD8, object_id.machine_id);
    assert_eq!(0x7B6E, object_id.process_id);
    assert_eq!(0x595515, object_id.counter);

    // Cursor is at EOF
    assert_eq!(cursor.position(), buf.len() as u64);
}

#[test]
fn test_object_id_from_binary_too_small() {
    let buf = Vec::from_hex(&OBJECT_ID[..8]).unwrap();
    let mut cursor = Cursor::new(buf);

    let err = ObjectId::from_binary(&mut cursor).unwrap_err();

    assert_eq!(err.kind(), io::ErrorKind::UnexpectedEof);
}

#[test]
fn test_object_id_from_hex() {
    let found = ObjectId::from_hex(OBJECT_ID).unwrap();

    let expected = {
        let buf = Vec::from_hex(OBJECT_ID).unwrap();
        let mut cursor = Cursor::new(&buf);
        ObjectId::from_binary(&mut cursor).unwrap()
    };

    assert_eq!(found.epoch_time, expected.epoch_time);
    assert_eq!(found.machine_id, expected.machine_id);
    assert_eq!(found.process_id, expected.process_id);
    assert_eq!(found.counter, expected.counter);
}
