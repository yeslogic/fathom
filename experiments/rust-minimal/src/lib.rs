#![doc = include_str!("../README.md")]

// Supporting modules
mod alloc;
pub mod env;

// Intermediate languages
pub mod core;
pub mod surface;

// Top level driver
mod driver;

// Public exports
pub use driver::{Driver, Status};

/// Interned strings.
pub type StringId = string_interner::symbol::SymbolU16;

/// String interner.
pub type StringInterner = string_interner::StringInterner<
    string_interner::backend::BucketBackend<StringId>,
    std::hash::BuildHasherDefault<fxhash::FxHasher32>,
>;

/// File id.
pub type FileId = usize;

/// Byte offsets into source files.
pub type BytePos = usize;

/// Byte ranges in source files.
#[derive(Debug, Copy, Clone)]
pub struct ByteRange {
    start: BytePos,
    end: BytePos,
}

impl ByteRange {
    pub const fn new(start: BytePos, end: BytePos) -> ByteRange {
        ByteRange { start, end }
    }

    pub const fn start(&self) -> BytePos {
        self.start
    }

    pub const fn end(&self) -> BytePos {
        self.end
    }
}

impl Into<std::ops::Range<usize>> for ByteRange {
    fn into(self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}
