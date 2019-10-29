use crate::Format;

/// An in-memory buffer that can be written into.
pub struct FormatWriter {
    buffer: Vec<u8>,
}

impl FormatWriter {
    /// Create a new buffer from an existing set of bytes.
    pub fn new(buffer: Vec<u8>) -> FormatWriter {
        FormatWriter { buffer }
    }

    /// Get the buffer.
    pub fn into_buffer(self) -> Vec<u8> {
        self.buffer
    }

    /// Get the buffer.
    pub fn buffer(&self) -> &[u8] {
        &self.buffer
    }

    /// Write a `u8` to the buffer.
    pub fn write_u8(&mut self, value: u8) {
        self.buffer.push(value);
    }

    pub fn write<T: WriteFormat>(&mut self, value: T::Host) {
        T::write(self, value)
    }
}

/// Binary format types that can be written to a buffer from a host representation.
pub trait WriteFormat: Format {
    /// Write the binary representation of `Self::Host` to `writer`.
    fn write(writer: &mut FormatWriter, value: Self::Host);
}
