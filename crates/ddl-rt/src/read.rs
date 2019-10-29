//! Read binary data.

use std::error::Error;
use std::fmt;

use crate::Format;

/// An error produced while reading binary data.
#[derive(Debug)]
pub enum ReadError {
    /// Tried to read a portion of broken DDL.
    InvalidDataDescription,
    /// An end of file error.
    Eof(ReadEofError),
}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReadError::InvalidDataDescription => {
                write!(f, "attempted to read improperly specified data")
            }
            ReadError::Eof(error) => error.fmt(f),
        }
    }
}

impl Error for ReadError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ReadError::InvalidDataDescription => None,
            ReadError::Eof(error) => Some(error),
        }
    }
}

impl From<ReadEofError> for ReadError {
    fn from(error: ReadEofError) -> Self {
        ReadError::Eof(error)
    }
}

/// An end of file error.
#[derive(Copy, Clone, Debug)]
pub struct ReadEofError {}

impl fmt::Display for ReadEofError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "attempted to read beyond the end of the buffer")
    }
}

impl Error for ReadEofError {}

/// A scope into a larger buffer.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ReadScope<'data> {
    base: usize,
    data: &'data [u8],
}

impl<'data> ReadScope<'data> {
    /// Construct a new scope with the given data.
    #[inline]
    pub fn new(data: &'data [u8]) -> ReadScope<'data> {
        ReadScope { base: 0, data }
    }

    /// The buffer that this scope is holding onto.
    #[inline]
    pub fn data(&self) -> &'data [u8] {
        self.data
    }

    /// Construct a new read context in this scope.
    #[inline]
    pub fn reader(&self) -> FormatReader<'data> {
        FormatReader {
            scope: *self,
            offset: 0,
        }
    }

    /// Return a new scope, offset to the desired position in the buffer.
    ///
    /// If we exceed the length of the buffer, and scope pointing to an empty
    /// buffer will be returned.
    #[inline]
    pub fn offset(&self, offset: usize) -> ReadScope<'data> {
        let base = self.base + offset;
        let data = self.data.get(offset..).unwrap_or(&[]);
        ReadScope { base, data }
    }

    /// Read some binary data in the context.
    #[inline]
    pub fn read<T: ReadFormat<'data>>(&self) -> Result<T::Host, ReadError> {
        self.reader().read::<T>()
    }

    /// Read some binary data in the context without bounds checking.
    #[inline]
    pub unsafe fn read_unchecked<T: ReadFormatUnchecked<'data>>(&mut self) -> T::Host {
        self.reader().read_unchecked::<T>()
    }
}

/// These can be created by calling `ReadScope::reader`.
#[derive(Clone)]
pub struct FormatReader<'data> {
    scope: ReadScope<'data>,
    offset: usize,
}

impl<'data> FormatReader<'data> {
    /// Create a new scope at this context's offset.
    #[inline]
    pub fn scope(&self) -> ReadScope<'data> {
        self.scope.offset(self.offset)
    }

    /// Read some binary data in the context.
    #[inline]
    pub fn read<T: ReadFormat<'data>>(&mut self) -> Result<T::Host, ReadError> {
        T::read(self)
    }

    /// Read some binary data in the context without bounds checking.
    #[inline]
    pub unsafe fn read_unchecked<T: ReadFormatUnchecked<'data>>(&mut self) -> T::Host {
        T::read_unchecked(self)
    }

    /// Read an unsigned u8-bit integer without performing a bounds check.
    #[inline]
    pub unsafe fn read_unchecked_u8(&mut self) -> u8 {
        let byte = *self.scope.data.get_unchecked(self.offset);
        self.offset += 1;
        byte
    }

    /// Check that there is enough space left in the buffer for the given number
    /// of bytes to be read.
    #[inline]
    pub fn check_available(&self, bytes: usize) -> Result<(), ReadEofError> {
        match self.offset.checked_add(bytes) {
            Some(end_pos) if end_pos <= self.scope.data.len() => Ok(()),
            Some(_) | None => Err(ReadEofError {}),
        }
    }
}

/// Binary format types that can be read into host data structures without bounds checking.
pub trait ReadFormatUnchecked<'data>: Format
where
    Self::Host: Sized,
{
    /// The number of bytes consumed by `read_unchecked`.
    const SIZE: usize;

    /// Must read exactly `SIZE` bytes.
    /// Unsafe as it avoids per-byte bounds checking.
    unsafe fn read_unchecked(reader: &mut FormatReader<'data>) -> Self::Host;
}

/// Binary format types that can be read into host data structures.
pub trait ReadFormat<'data>: Format
where
    Self::Host: Sized,
{
    /// Read a host value in the context.
    fn read(reader: &mut FormatReader<'data>) -> Result<Self::Host, ReadError>;
}
