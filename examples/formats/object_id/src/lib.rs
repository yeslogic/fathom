extern crate hex;

use hex::FromHex;

include!(concat!(env!("OUT_DIR"), "/object_id.rs"));

#[derive(Debug)]
pub enum FromHexError {
    Io(io::Error),
    FromHex(hex::FromHexError),
}

impl From<io::Error> for FromHexError {
    fn from(src: io::Error) -> FromHexError {
        FromHexError::Io(src)
    }
}

impl From<hex::FromHexError> for FromHexError {
    fn from(src: hex::FromHexError) -> FromHexError {
        FromHexError::FromHex(src)
    }
}

impl FromHex for ObjectId {
    type Error = FromHexError;

    fn from_hex<T: AsRef<[u8]>>(src: T) -> Result<ObjectId, FromHexError> {
        let buf = <[u8; 12]>::from_hex(src)?;
        let object_id = ObjectId::read(&mut io::Cursor::new(buf))?;

        Ok(object_id)
    }
}
