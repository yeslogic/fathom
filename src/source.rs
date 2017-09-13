//! Various source mapping utilities

use std::cmp;
use std::path::{Path, PathBuf};
use std::ops::{Add, Sub};

/// A zero-indexed line offest into a source file
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LineIndex(pub usize);

impl LineIndex {
    /// The 1-indexed line number. Useful for pretty printing source locations.
    ///
    /// ```rust
    /// use ddl::source::{LineIndex, LineNumber};
    ///
    /// assert_eq!(LineIndex(0).number(), LineNumber(1));
    /// assert_eq!(LineIndex(3).number(), LineNumber(4));
    /// ```
    pub fn number(self) -> LineNumber {
        LineNumber(self.0 + 1)
    }

    /// Apply the function `f` to the underlying index and return the wrapped result
    pub fn map<F: FnMut(usize) -> usize>(self, mut f: F) -> LineIndex {
        LineIndex(f(self.0))
    }
}

/// A 1-indexed line number. Useful for pretty printing source locations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LineNumber(pub usize);

impl LineNumber {
    /// Apply the function `f` to the underlying number and return the wrapped result
    pub fn map<F: FnMut(usize) -> usize>(self, mut f: F) -> LineNumber {
        LineNumber(f(self.0))
    }
}

/// A zero-indexed column offest into a source file
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ColumnIndex(pub usize);

impl ColumnIndex {
    /// The 1-indexed column number. Useful for pretty printing source locations.
    ///
    /// ```rust
    /// use ddl::source::{ColumnIndex, ColumnNumber};
    ///
    /// assert_eq!(ColumnIndex(0).number(), ColumnNumber(1));
    /// assert_eq!(ColumnIndex(3).number(), ColumnNumber(4));
    /// ```
    pub fn number(self) -> ColumnNumber {
        ColumnNumber(self.0 + 1)
    }

    /// Apply the function `f` to the underlying index and return the wrapped result
    pub fn map<F: FnMut(usize) -> usize>(self, mut f: F) -> ColumnIndex {
        ColumnIndex(f(self.0))
    }
}

/// A 1-indexed column number. Useful for pretty printing source locations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ColumnNumber(pub usize);

impl ColumnNumber {
    /// Apply the function `f` to the underlying number and return the wrapped result
    pub fn map<F: FnMut(usize) -> usize>(self, mut f: F) -> ColumnNumber {
        ColumnNumber(f(self.0))
    }
}

/// A byte offset in a source file
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct BytePos(pub usize);

impl BytePos {
    /// Apply the function `f` to the underlying position and return the wrapped result
    pub fn map<F: FnMut(usize) -> usize>(self, mut f: F) -> BytePos {
        BytePos(f(self.0))
    }
}

impl Add for BytePos {
    type Output = BytePos;

    fn add(self, rhs: BytePos) -> BytePos {
        BytePos(self.0 + rhs.0)
    }
}

impl Sub for BytePos {
    type Output = BytePos;

    fn sub(self, rhs: BytePos) -> BytePos {
        BytePos(self.0 - rhs.0)
    }
}

/// A unicode character offset in a source file
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct CharPos(pub usize);

impl CharPos {
    /// Apply the function `f` to the underlying position and return the wrapped result
    pub fn map<F: FnMut(usize) -> usize>(self, mut f: F) -> CharPos {
        CharPos(f(self.0))
    }
}

impl Add for CharPos {
    type Output = CharPos;

    fn add(self, rhs: CharPos) -> CharPos {
        CharPos(self.0 + rhs.0)
    }
}

impl Sub for CharPos {
    type Output = CharPos;

    fn sub(self, rhs: CharPos) -> CharPos {
        CharPos(self.0 - rhs.0)
    }
}

/// A region of code in a source file
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct Span {
    lo: BytePos,
    hi: BytePos,
}

impl Span {
    /// Create a new span
    ///
    /// ```rust
    /// use ddl::source::{BytePos, Span};
    ///
    /// let span = Span::new(BytePos(3), BytePos(6));
    /// assert_eq!(span.lo(), BytePos(3));
    /// assert_eq!(span.hi(), BytePos(6));
    /// ```
    ///
    /// `lo` are reordered `hi` to maintain the invariant that `lo <= hi`
    ///
    /// ```rust
    /// use ddl::source::{BytePos, Span};
    ///
    /// let span = Span::new(BytePos(6), BytePos(3));
    /// assert_eq!(span.lo(), BytePos(3));
    /// assert_eq!(span.hi(), BytePos(6));
    /// ```
    pub fn new(lo: BytePos, hi: BytePos) -> Span {
        if lo <= hi {
            Span { lo, hi }
        } else {
            Span { lo: hi, hi: lo }
        }
    }

    /// Get the low byte position
    pub fn lo(self) -> BytePos {
        self.lo
    }

    /// Get the high byte position
    pub fn hi(self) -> BytePos {
        self.hi
    }

    /// Return a new span with the low byte position replaced with the supplied byte position
    ///
    /// ```rust
    /// use ddl::source::{BytePos, Span};
    ///
    /// let span = Span::new(BytePos(3), BytePos(6));
    /// assert_eq!(span.with_lo(BytePos(2)), Span::new(BytePos(2), BytePos(6)));
    /// assert_eq!(span.with_lo(BytePos(5)), Span::new(BytePos(5), BytePos(6)));
    /// assert_eq!(span.with_lo(BytePos(7)), Span::new(BytePos(6), BytePos(7)));
    /// ```
    pub fn with_lo(self, lo: BytePos) -> Span {
        Span::new(lo, self.hi())
    }

    /// Return a new span with the high byte position replaced with the supplied byte position
    ///
    /// ```rust
    /// use ddl::source::{BytePos, Span};
    ///
    /// let span = Span::new(BytePos(3), BytePos(6));
    /// assert_eq!(span.with_hi(BytePos(7)), Span::new(BytePos(3), BytePos(7)));
    /// assert_eq!(span.with_hi(BytePos(5)), Span::new(BytePos(3), BytePos(5)));
    /// assert_eq!(span.with_hi(BytePos(2)), Span::new(BytePos(2), BytePos(3)));
    /// ```
    pub fn with_hi(self, hi: BytePos) -> Span {
        Span::new(self.lo(), hi)
    }

    /// Returns a new span representing just the end-point of this span
    ///
    /// ```rust
    /// use ddl::source::{BytePos, Span};
    ///
    /// let span = Span::new(BytePos(3), BytePos(6));
    /// assert_eq!(span.end_point(), Span::new(BytePos(5), BytePos(6)))
    /// ```
    pub fn end_point(self) -> Span {
        self.with_lo(cmp::max(self.hi() - BytePos(1), self.lo()))
    }

    /// Return true if `self` fully encloses `other`.
    ///
    /// ```rust
    /// use ddl::source::{BytePos, Span};
    ///
    /// let a = Span::new(BytePos(5), BytePos(8));
    ///
    /// assert_eq!(a.contains(a), true);
    /// assert_eq!(a.contains(Span::new(BytePos(6), BytePos(7))), true);
    /// assert_eq!(a.contains(Span::new(BytePos(6), BytePos(10))), false);
    /// assert_eq!(a.contains(Span::new(BytePos(3), BytePos(6))), false);
    /// ```
    pub fn contains(self, other: Span) -> bool {
        self.lo() <= other.lo() && other.hi() <= self.hi()
    }

    /// Return a `Span` that would enclose both `self` and `end`.
    ///
    /// ```plain
    /// self     ~~~~~~~
    /// end                     ~~~~~~~~
    /// returns  ~~~~~~~~~~~~~~~~~~~~~~~
    /// ```
    ///
    /// ```rust
    /// use ddl::source::{BytePos, Span};
    ///
    /// let a = Span::new(BytePos(2), BytePos(5));
    /// let b = Span::new(BytePos(10), BytePos(14));
    ///
    /// assert_eq!(a.to(b), Span::new(BytePos(2), BytePos(14)));
    /// ```
    pub fn to(self, end: Span) -> Span {
        Span::new(cmp::min(self.lo(), end.lo()), cmp::max(self.hi(), end.hi()))
    }

    /// Return a `Span` between the end of `self` to the beginning of `end`.
    ///
    /// ```plain
    /// self     ~~~~~~~
    /// end                     ~~~~~~~~
    /// returns         ~~~~~~~~~
    /// ```
    ///
    /// ```rust
    /// use ddl::source::{BytePos, Span};
    ///
    /// let a = Span::new(BytePos(2), BytePos(5));
    /// let b = Span::new(BytePos(10), BytePos(14));
    ///
    /// assert_eq!(a.between(b), Span::new(BytePos(5), BytePos(10)));
    /// ```
    pub fn between(self, end: Span) -> Span {
        Span::new(self.hi(), end.lo())
    }

    /// Return a `Span` between the beginning of `self` to the beginning of `end`.
    ///
    /// ```plain
    /// self     ~~~~~~~
    /// end                     ~~~~~~~~
    /// returns  ~~~~~~~~~~~~~~~~
    /// ```
    ///
    /// ```rust
    /// use ddl::source::{BytePos, Span};
    ///
    /// let a = Span::new(BytePos(2), BytePos(5));
    /// let b = Span::new(BytePos(10), BytePos(14));
    ///
    /// assert_eq!(a.until(b), Span::new(BytePos(2), BytePos(10)));
    /// ```
    pub fn until(self, end: Span) -> Span {
        Span::new(self.lo(), end.lo())
    }

    /// Wrap a value in the span
    ///
    /// ```rust
    /// use ddl::source::{BytePos, Span, Spanned};
    ///
    /// enum Expr {
    ///     Var(String),
    ///     Abs(String, Spanned<Box<Expr>>),
    ///     App(Spanned<Box<Expr>>, Spanned<Box<Expr>>),
    /// }
    ///
    /// let span = Span::new(BytePos(2), BytePos(5));
    /// let expr = Expr::Var("x".to_owned());
    /// let x: Spanned<Box<Expr>> = span.with(expr);
    /// ```
    pub fn with<T: Into<U>, U>(self, value: T) -> Spanned<U> {
        Spanned {
            span: self,
            value: value.into(),
        }
    }
}

/// A value with an associated `Span`
///
/// Construct a `Spanned<T>` in a fluent way using `Span::with`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Spanned<T> {
    /// Apply the function `f` to the underlying value and return the wrapped result
    pub fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> Spanned<U> {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }
}

/// Some source code
pub struct Source {
    /// The name of the file that the source came from
    name: Option<PathBuf>,
    /// The complete source code
    src: String,
    /// Locations of the line beginnings in the source
    line_offsets: Vec<BytePos>,
    /// The byte offset for the last byte in the file
    end_offset: BytePos,
}

impl Source {
    /// Construct a new sorce code, creating an index of line start locations
    pub fn new(name: Option<PathBuf>, src: String) -> Source {
        use std::iter;

        let mut end_offset = BytePos(0);
        let line_offsets = {
            let input_indices = src.bytes()
                .inspect(|_| end_offset.0 += 1)
                .enumerate()
                .filter(|&(_, b)| b == b'\n')
                .map(|(i, _)| BytePos(i + 1)); // index of first char in the line

            iter::once(BytePos(0)).chain(input_indices).collect()
        };

        Source {
            name,
            src,
            line_offsets,
            end_offset,
        }
    }

    /// The name of the file that the source came from
    pub fn name(&self) -> Option<&Path> {
        self.name.as_ref().map(PathBuf::as_ref)
    }

    /// The underlying source code
    pub fn src(&self) -> &str {
        &self.src
    }

    /// Returns the byte offset to the start of `line`
    ///
    /// ```rust
    /// use ddl::source::{BytePos, LineIndex, Source};
    ///
    /// let source = Source::new(None, "hello!\nhowdy\n\nhi萤\nbloop\n".to_owned());
    ///
    /// assert_eq!(source.line_offset(LineIndex(0)), Some(BytePos(0)));
    /// assert_eq!(source.line_offset(LineIndex(1)), Some(BytePos(7)));
    /// assert_eq!(source.line_offset(LineIndex(2)), Some(BytePos(13)));
    /// assert_eq!(source.line_offset(LineIndex(3)), Some(BytePos(14)));
    /// assert_eq!(source.line_offset(LineIndex(4)), Some(BytePos(20)));
    /// assert_eq!(source.line_offset(LineIndex(5)), Some(BytePos(26)));
    /// assert_eq!(source.line_offset(LineIndex(6)), None);
    /// ```
    pub fn line_offset(&self, index: LineIndex) -> Option<BytePos> {
        self.line_offsets.get(index.0).cloned()
    }

    /// Returns the line and column location of `byte`
    ///
    /// ```rust
    /// use ddl::source::{BytePos, ColumnIndex, LineIndex, Source};
    ///
    /// let source = Source::new(None, "hello!\nhowdy\n\nhi萤\nbloop\n".to_owned());
    ///
    /// assert_eq!(source.location(BytePos(0)), Some((LineIndex(0), ColumnIndex(0))));
    /// assert_eq!(source.location(BytePos(7)), Some((LineIndex(1), ColumnIndex(0))));
    /// assert_eq!(source.location(BytePos(13)), Some((LineIndex(2), ColumnIndex(0))));
    /// assert_eq!(source.location(BytePos(14)), Some((LineIndex(3), ColumnIndex(0))));
    /// assert_eq!(source.location(BytePos(20)), Some((LineIndex(4), ColumnIndex(0))));
    /// assert_eq!(source.location(BytePos(26)), Some((LineIndex(5), ColumnIndex(0))));
    /// assert_eq!(source.location(BytePos(300)), None);
    /// ```
    pub fn location(&self, absolute_offset: BytePos) -> Option<(LineIndex, ColumnIndex)> {
        self.line_index(absolute_offset).and_then(|line_index| {
            self.line_offset(line_index).map(|line_offset| {
                (line_index, ColumnIndex((absolute_offset - line_offset).0))
            })
        })
    }

    /// Returns the line index that the byte offset points to
    ///
    /// ```rust
    /// use ddl::source::{BytePos, LineIndex, Source};
    ///
    /// let source = Source::new(None, "hello!\nhowdy\n\nhi萤\nbloop\n".to_owned());
    ///
    /// assert_eq!(source.line_index(BytePos(0)), Some(LineIndex(0)));
    /// assert_eq!(source.line_index(BytePos(7)), Some(LineIndex(1)));
    /// assert_eq!(source.line_index(BytePos(13)), Some(LineIndex(2)));
    /// assert_eq!(source.line_index(BytePos(14)), Some(LineIndex(3)));
    /// assert_eq!(source.line_index(BytePos(20)), Some(LineIndex(4)));
    /// assert_eq!(source.line_index(BytePos(26)), Some(LineIndex(5)));
    /// assert_eq!(source.line_index(BytePos(300)), None);
    /// ```
    pub fn line_index(&self, absolute_offset: BytePos) -> Option<LineIndex> {
        if absolute_offset <= self.end_offset {
            let num_lines = self.line_offsets.len();

            Some(LineIndex(
                (0..num_lines)
                    .filter(|&i| self.line_offsets[i] > absolute_offset)
                    .map(|i| i - 1)
                    .next()
                    .unwrap_or(num_lines - 1),
            ))
        } else {
            None
        }
    }
}
