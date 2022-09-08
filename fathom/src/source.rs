use std::ops::{Deref, DerefMut};

///! Types related to source files.

/// File id.
pub type FileId = usize; // TODO: use wrapper struct

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    span: Span,
    inner: T,
}

impl<T> Spanned<T> {
    pub fn new(span: Span, inner: T) -> Self {
        Spanned { span, inner }
    }

    pub fn empty(inner: T) -> Self {
        Spanned {
            span: Span::Empty,
            inner,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    /// Merge the supplied span with the span of `other` and return `other` wrapped in that span.
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

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Span {
    Range(ByteRange),
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

impl From<ByteRange> for Span {
    fn from(range: ByteRange) -> Self {
        Span::Range(range)
    }
}

impl From<&ByteRange> for Span {
    fn from(range: &ByteRange) -> Self {
        Span::Range(*range)
    }
}

/// Byte offsets into source files.
pub type BytePos = usize;

/// Byte ranges in source files.
#[derive(Debug, Copy, Clone)]
pub struct ByteRange {
    file_id: FileId,
    start: BytePos,
    end: BytePos,
}

impl ByteRange {
    pub const fn new(file_id: FileId, start: BytePos, end: BytePos) -> ByteRange {
        ByteRange {
            file_id,
            start,
            end,
        }
    }

    pub fn file_id(&self) -> FileId {
        self.file_id
    }

    pub const fn start(&self) -> BytePos {
        self.start
    }

    pub const fn end(&self) -> BytePos {
        self.end
    }

    pub fn merge(&self, other: &ByteRange) -> Option<ByteRange> {
        if self.file_id == other.file_id {
            Some(ByteRange::new(
                self.file_id,
                self.start.min(other.start),
                self.end.max(other.end),
            ))
        } else {
            None
        }
    }
}

impl From<ByteRange> for std::ops::Range<usize> {
    fn from(range: ByteRange) -> Self {
        range.start..range.end
    }
}
