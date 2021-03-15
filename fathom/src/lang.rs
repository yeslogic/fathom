//! Intermediate languages of the Fathom compiler.

pub mod surface;
//       🠃
pub mod core;
//       🠃
//      ...

/// File identifier
pub type FileId = usize;

/// Location metadata, for diagnostic reporting purposes.
#[derive(Debug, Copy, Clone)]
pub enum Location {
    /// Generated code.
    Generated,
    /// Ranges in a text file.
    FileRange(FileId, Range),
}

impl Location {
    pub fn generated() -> Location {
        Location::Generated
    }

    pub fn file_range(file_id: FileId, range: impl Into<Range>) -> Location {
        Location::FileRange(file_id, range.into())
    }

    pub fn start(self) -> Location {
        match self {
            Location::Generated => Location::Generated,
            Location::FileRange(file_id, range) => Location::FileRange(file_id, range.start()),
        }
    }

    pub fn end(self) -> Location {
        match self {
            Location::Generated => Location::Generated,
            Location::FileRange(file_id, range) => Location::FileRange(file_id, range.end()),
        }
    }

    pub fn merge(self, other: Location) -> Location {
        match (self, other) {
            (Location::Generated, Location::Generated) => Location::Generated,
            (Location::FileRange(file_id0, range0), Location::FileRange(file_id1, range1)) => {
                assert_eq!(
                    file_id0, file_id1,
                    "tried to merge source locations with different file ids"
                );
                Location::FileRange(file_id0, Range::merge(range0, range1))
            }
            (_, _) => Location::Generated,
        }
    }
}

/// A range of source code.
///
/// This is added to simplify working with ranges, because [`std::ops::Range`]
/// does not implement [`std::ops::Copy`].
#[derive(Debug, Copy, Clone)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

impl Range {
    pub fn merge(self, other: Range) -> Range {
        Range {
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.end),
        }
    }

    pub fn start(self) -> Range {
        Range {
            start: self.start,
            end: self.start,
        }
    }

    pub fn end(self) -> Range {
        Range {
            start: self.end,
            end: self.end,
        }
    }
}

impl Into<std::ops::Range<usize>> for Range {
    fn into(self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

impl From<std::ops::Range<usize>> for Range {
    fn from(src: std::ops::Range<usize>) -> Range {
        Range {
            start: src.start,
            end: src.end,
        }
    }
}

/// Data that covers some range of source code.
#[derive(Debug, Clone)]
pub struct Located<Data> {
    pub location: Location,
    pub data: Data,
}

impl<Data> Located<Data> {
    pub fn new(location: Location, data: Data) -> Located<Data> {
        Located { location, data }
    }

    pub fn generated(data: Data) -> Located<Data> {
        Located::new(Location::generated(), data)
    }
}

impl<Data: PartialEq> PartialEq for Located<Data> {
    /// Ignores source location metadata.
    fn eq(&self, other: &Located<Data>) -> bool {
        self.data == other.data
    }
}
