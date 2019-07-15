//! Binary interpreter for the data description language.
//!
//! This is only a naive implementation, and intended for getting a better idea
//! of whether our compiled back-ends actually meet the specification.

use std::collections::BTreeMap;

pub mod read;

/// Terms that can be produced as a result of reading a binary file, or used as
/// a source from which to write binary data.
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// Unsigned 8-bit integers.
    U8(u8),
    /// Unsigned 16-bit integers.
    U16(u16),
    /// Unsigned 32-bit integers.
    U32(u32),
    /// Unsigned 64-bit integers.
    U64(u64),
    /// Signed, two's complement 8-bit integers.
    S8(i8),
    /// Signed, two's complement 16-bit integers.
    S16(i16),
    /// Signed, two's complement 32-bit integers.
    S32(i32),
    /// Signed, two's complement 64-bit integers.
    S64(i64),
    /// IEEE754 single-precision floating point numbers.
    F32(f32),
    /// IEEE754 double-precision floating point numbers.
    F64(f64),
    /// Structure values
    Struct(BTreeMap<String, Term>),
}
