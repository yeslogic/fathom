pub mod error;
pub mod prelude;
pub mod read;
mod size;

/// Unsigned 8-bit binary type.
#[derive(Copy, Clone)]
pub enum U8 {}

/// Signed 8-bit binary type.
#[derive(Copy, Clone)]
pub enum I8 {}

/// Unsigned 16-bit big endian binary type.
#[derive(Copy, Clone)]
pub enum U16Be {}

/// Signed 16-bit big endian binary type.
#[derive(Copy, Clone)]
pub enum I16Be {}

/// Unsigned 24-bit (3 bytes) big endian binary type.
#[derive(Copy, Clone)]
pub enum U24Be {}

/// Unsigned 32-bit big endian binary type.
#[derive(Copy, Clone)]
pub enum U32Be {}

/// Signed 32-bit big endian binary type.
#[derive(Copy, Clone)]
pub enum I32Be {}

/// Signed 64-bit binary type.
#[derive(Copy, Clone)]
pub enum I64Be {}
