//! Types related to source files.

use std::ops::{Deref, DerefMut, Range};

use crate::files::FileId;

// Interned strings.
pub type StringId = string_interner::symbol::SymbolU16;

/// String interner.
pub struct StringInterner {
    alphabetic_names: Vec<StringId>,
    tuple_labels: Vec<StringId>,
    strings: string_interner::StringInterner<
        string_interner::backend::BucketBackend<StringId>,
        std::hash::BuildHasherDefault<fxhash::FxHasher32>,
    >,
}

impl Deref for StringInterner {
    type Target = string_interner::StringInterner<
        string_interner::backend::BucketBackend<StringId>,
        std::hash::BuildHasherDefault<fxhash::FxHasher32>,
    >;

    fn deref(&self) -> &Self::Target {
        &self.strings
    }
}

impl DerefMut for StringInterner {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.strings
    }
}

impl StringInterner {
    /// Construct an empty string interner.
    pub fn new() -> StringInterner {
        StringInterner {
            alphabetic_names: Vec::new(),
            tuple_labels: Vec::new(),
            strings: string_interner::StringInterner::new(),
        }
    }

    /// Allocate and intern all alphabetic names up-to and including `max_index`
    /// if they are not already present.
    pub fn reserve_alphabetic_names(&mut self, max_index: usize) {
        fill_vec(&mut self.alphabetic_names, max_index, |index| {
            self.strings.get_or_intern(alphabetic_name(index))
        })
    }

    /// Retrieve an alphabetic name based on a numeric count. This is useful for
    /// producing human-readable names for unnamed binders.
    ///
    /// ## Example
    ///
    /// ```rust
    /// use fathom::source::StringInterner;
    ///
    /// let mut interner = StringInterner::new();
    /// assert_eq!(interner.get_alphabetic_name(0), interner.get_or_intern("a"));
    /// // ...
    /// assert_eq!(
    ///     interner.get_alphabetic_name(25),
    ///     interner.get_or_intern("z")
    /// );
    /// assert_eq!(
    ///     interner.get_alphabetic_name(26),
    ///     interner.get_or_intern("a1")
    /// );
    /// // ...
    /// assert_eq!(
    ///     interner.get_alphabetic_name(51),
    ///     interner.get_or_intern("z1")
    /// );
    /// assert_eq!(
    ///     interner.get_alphabetic_name(52),
    ///     interner.get_or_intern("a2")
    /// );
    /// // ...
    /// ```
    pub fn get_alphabetic_name(&mut self, index: usize) -> StringId {
        self.reserve_alphabetic_names(index);
        self.alphabetic_names[index]
    }

    /// Allocate and intern all tuple labels up-to and including `max_index`
    /// if they are not already present.
    pub fn reserve_tuple_labels(&mut self, max_index: usize) {
        fill_vec(&mut self.tuple_labels, max_index, |index| {
            self.strings.get_or_intern(format!("_{index}"))
        })
    }

    /// Get or intern a string in the form `_{index}`.
    ///
    /// ## Example
    ///
    /// ```rust
    /// use fathom::source::StringInterner;
    ///
    /// let mut interner = StringInterner::new();
    /// assert_eq!(interner.get_tuple_label(0), interner.get_or_intern("_0"));
    /// assert_eq!(interner.get_tuple_label(1), interner.get_or_intern("_1"));
    /// ```
    pub fn get_tuple_label(&mut self, index: usize) -> StringId {
        self.reserve_tuple_labels(index);
        self.tuple_labels[index]
    }

    /// Get or intern a slice of strings in the form `_{index}` for each index
    /// in `range`.
    pub fn get_tuple_labels(&mut self, range: Range<usize>) -> &[StringId] {
        self.reserve_tuple_labels(range.end.saturating_sub(1));
        &self.tuple_labels[range]
    }

    /// Returns true if `label` refers to a string in the form `_{index}`.
    pub fn is_tuple_label(&mut self, index: usize, label: StringId) -> bool {
        label == self.get_tuple_label(index)
    }

    /// Returns true if `labels` is a sequence of tuple labels: `_0`, `_1`, ...
    pub fn is_tuple_labels(&mut self, labels: &[StringId]) -> bool {
        labels == self.get_tuple_labels(0..labels.len())
    }
}

fn alphabetic_name(index: usize) -> String {
    let base = index / 26;
    let letter = index % 26;
    let letter = (letter as u8 + b'a') as char;
    if base == 0 {
        format!("{letter}")
    } else {
        format!("{letter}{base}")
    }
}

fn fill_vec<T>(vec: &mut Vec<T>, max_index: usize, f: impl FnMut(usize) -> T) {
    vec.extend((vec.len()..=max_index).map(f))
}

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

impl From<Option<ByteRange>> for Span {
    fn from(range: Option<ByteRange>) -> Span {
        range.map_or(Span::Empty, Span::Range)
    }
}

/// Byte offsets into source files.
pub type BytePos = u32;

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

impl From<ByteRange> for Range<usize> {
    fn from(range: ByteRange) -> Self {
        (range.start as usize)..(range.end as usize)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    /// `ByteRange` is used a lot. Ensure it doesn't grow accidentally.
    fn byte_range_size() {
        assert_eq!(std::mem::size_of::<ByteRange>(), 12);
    }

    #[test]
    /// `Span` is used a lot. Ensure it doesn't grow accidentally.
    fn span_size() {
        assert_eq!(std::mem::size_of::<Span>(), 12);
    }
}
