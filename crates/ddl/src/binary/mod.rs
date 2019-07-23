//! Binary interpreter for the data description language.
//!
//! This is only a naive implementation, and intended for getting a better idea
//! of whether our compiled back-ends actually meet the specification.

use std::collections::BTreeMap;
use num_bigint::BigInt;

pub mod read;

/// Terms that can be produced as a result of reading a binary file, or used as
/// a source from which to write binary data.
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// Integers.
    Int(BigInt),
    /// IEEE754 single-precision floating point numbers.
    F32(f32),
    /// IEEE754 double-precision floating point numbers.
    F64(f64),
    /// Structure values
    Struct(BTreeMap<String, Term>),
}
