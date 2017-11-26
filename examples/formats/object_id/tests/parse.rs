#[macro_use]
extern crate binary_macros;
extern crate ddl_object_id;

use std::io::{self, Cursor};
use ddl_object_id::ObjectId;

static OBJECT_ID: &[u8; 12] = base16!("564B86EF165BD87B6E595515");

#[test]
fn test_object_id_read() {
    let mut cursor = Cursor::new(OBJECT_ID);

    let object_id = ObjectId::read(&mut cursor).unwrap();

    // Test fields
    assert_eq!(0x564B86EF, object_id.epoch_time);
    assert_eq!(0x165BD8, object_id.machine_id);
    assert_eq!(0x7B6E, object_id.process_id);
    assert_eq!(0x595515, object_id.counter);

    // Cursor is at EOF
    assert_eq!(cursor.position(), OBJECT_ID.len() as u64);
}

#[test]
fn test_object_id_read_too_small() {
    let mut cursor = Cursor::new(&OBJECT_ID[..7]);

    let err = ObjectId::read(&mut cursor).unwrap_err();

    assert_eq!(err.kind(), io::ErrorKind::UnexpectedEof);
}
