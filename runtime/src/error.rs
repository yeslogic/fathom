//! Error types

use std::fmt;

use crate::read::ReadEof;

/// Errors that originate when parsing
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ParseError {
    BadEof,
    BadValue,
    BadVersion,
    BadOffset,
    BadIndex,
    LimitExceeded,
    MissingValue,
    CompressionError,
    NotImplemented,
}

impl From<ReadEof> for ParseError {
    fn from(_error: ReadEof) -> Self {
        ParseError::BadEof
    }
}

impl From<std::num::TryFromIntError> for ParseError {
    fn from(_error: std::num::TryFromIntError) -> Self {
        ParseError::BadValue
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::BadEof => write!(f, "end of data reached unexpectedly"),
            ParseError::BadValue => write!(f, "invalid value"),
            ParseError::BadVersion => write!(f, "unexpected data version"),
            ParseError::BadOffset => write!(f, "invalid data offset"),
            ParseError::BadIndex => write!(f, "invalid data index"),
            ParseError::LimitExceeded => write!(f, "limit exceeded"),
            ParseError::MissingValue => write!(f, "an expected data value was missing"),
            ParseError::CompressionError => write!(f, "compression error"),
            ParseError::NotImplemented => write!(f, "feature not implemented"),
        }
    }
}

impl std::error::Error for ParseError {}
