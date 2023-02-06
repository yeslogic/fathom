//! Types related to source files.

use std::fmt;
use std::ops::{Deref, DerefMut, Range};

use crate::files::FileId;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    span: Span,
    inner: T,
}

impl<T> Spanned<T> {
    pub fn new(span: Span, inner: T) -> Spanned<T> {
        Spanned { span, inner }
    }

    pub fn empty(inner: T) -> Spanned<T> {
        Spanned {
            span: Span::Empty,
            inner,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    /// Merge the supplied span with the span of `other` and return `other`
    /// wrapped in that span.
    pub fn merge(span: Span, other: Spanned<T>) -> Spanned<T> {
        let Spanned {
            span: other_span,
            inner,
        } = other;
        Spanned {
            span: span.merge(&other_span),
            inner,
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.inner
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Span {
    Range(FileRange),
    Empty,
}

impl Span {
    pub fn merge(&self, other: &Span) -> Span {
        match (self, other) {
            (Span::Range(a), Span::Range(b)) => a.merge(b).map(Span::Range).unwrap_or(Span::Empty),
            (_, _) => Span::Empty,
        }
    }
}

impl From<FileRange> for Span {
    fn from(range: FileRange) -> Span {
        Span::Range(range)
    }
}

impl From<&FileRange> for Span {
    fn from(range: &FileRange) -> Span {
        Span::Range(*range)
    }
}

impl From<Option<FileRange>> for Span {
    fn from(range: Option<FileRange>) -> Span {
        range.map_or(Span::Empty, Span::Range)
    }
}

/// Byte offsets into source files.
pub type BytePos = u32;

/// Byte ranges in source files.
#[derive(Copy, Clone)]
pub struct FileRange {
    file_id: FileId,
    byte_range: ByteRange,
}

impl fmt::Debug for FileRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "FileRange({}, {}..{})",
            self.file_id, self.byte_range.start, self.byte_range.end
        )
    }
}

impl FileRange {
    pub const fn new(file_id: FileId, byte_range: ByteRange) -> FileRange {
        FileRange {
            file_id,
            byte_range,
        }
    }

    pub fn file_id(&self) -> FileId {
        self.file_id
    }

    pub const fn byte_range(&self) -> ByteRange {
        self.byte_range
    }

    pub const fn start(&self) -> BytePos {
        self.byte_range.start
    }

    pub const fn end(&self) -> BytePos {
        self.byte_range.end
    }

    pub fn merge(&self, other: &FileRange) -> Option<FileRange> {
        if self.file_id == other.file_id {
            Some(FileRange::new(
                self.file_id,
                ByteRange::merge(self.byte_range, other.byte_range),
            ))
        } else {
            None
        }
    }
}

impl From<FileRange> for Range<usize> {
    fn from(file_range: FileRange) -> Range<usize> {
        file_range.byte_range.into()
    }
}

#[derive(Copy, Clone)]
pub struct ByteRange {
    start: BytePos,
    end: BytePos,
}

impl fmt::Debug for ByteRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ByteRange({}..{})", self.start, self.end)
    }
}

impl From<(BytePos, BytePos)> for ByteRange {
    fn from((start, end): (BytePos, BytePos)) -> Self {
        Self { start, end }
    }
}

impl ByteRange {
    pub fn new(start: BytePos, end: BytePos) -> ByteRange {
        ByteRange { start, end }
    }

    pub const fn start(&self) -> BytePos {
        self.start
    }

    pub const fn end(&self) -> BytePos {
        self.end
    }

    pub fn merge(self, other: ByteRange) -> ByteRange {
        ByteRange::new(self.start.min(other.start), self.end.max(other.end))
    }
}

impl From<ByteRange> for Range<usize> {
    fn from(range: ByteRange) -> Range<usize> {
        (range.start as usize)..(range.end as usize)
    }
}

/// A smart constructor around `String`, which guarantees its length is <=
/// `u32::MAX`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgramSource(String);

pub const MAX_SOURCE_LEN: usize = u32::MAX as usize;

impl fmt::Display for ProgramSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Deref for ProgramSource {
    type Target = String;

    fn deref(&self) -> &String {
        &self.0
    }
}

impl AsRef<str> for ProgramSource {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl From<ProgramSource> for String {
    fn from(source: ProgramSource) -> String {
        source.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceTooBig {
    pub actual_len: usize,
}

impl TryFrom<String> for ProgramSource {
    type Error = SourceTooBig;

    fn try_from(string: String) -> Result<ProgramSource, SourceTooBig> {
        if string.len() <= MAX_SOURCE_LEN {
            Ok(ProgramSource(string))
        } else {
            Err(SourceTooBig {
                actual_len: string.len(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    /// `ByteRange` is used a lot. Ensure it doesn't grow accidentally.
    fn byte_range_size() {
        assert_eq!(std::mem::size_of::<ByteRange>(), 8);
    }

    #[test]
    /// `FileRange` is used a lot. Ensure it doesn't grow accidentally.
    fn file_range_size() {
        assert_eq!(std::mem::size_of::<FileRange>(), 12);
    }

    #[test]
    /// `Span` is used a lot. Ensure it doesn't grow accidentally.
    fn span_size() {
        assert_eq!(std::mem::size_of::<Span>(), 12);
    }
}
