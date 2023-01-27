//! A reimplementation of `codespan-reporting::files::SimpleFiles` that uses
//! `FileId` as the file id, instead of `usize`.

use std::fmt;
use std::num::NonZeroU32;
use std::ops::Range;

use codespan_reporting::files::{Error, SimpleFile};

/// File id.
// - Use `u32` over `usize` because 4 billion files should be enough for anyone
// - `u16` doesn't save any size in `ByteRange` or `Span` compared to `u32`
// - `NonZeroU32` saves 4 bytes on the size of `Span` compared to `u32`
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FileId(NonZeroU32);

impl fmt::Display for FileId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl TryFrom<u32> for FileId {
    type Error = <NonZeroU32 as TryFrom<u32>>::Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        let id = NonZeroU32::try_from(value)?;
        Ok(Self(id))
    }
}

impl From<FileId> for NonZeroU32 {
    fn from(value: FileId) -> Self {
        value.0
    }
}

impl From<FileId> for u32 {
    fn from(value: FileId) -> Self {
        value.0.get()
    }
}

impl From<FileId> for usize {
    fn from(value: FileId) -> Self {
        value.0.get() as Self
    }
}

pub struct Files<Name, Source> {
    files: Vec<SimpleFile<Name, Source>>,
}

impl<Name, Source> Files<Name, Source>
where
    Name: std::fmt::Display,
    Source: AsRef<str>,
{
    /// Create a new files database.
    pub fn new() -> Self {
        Self { files: Vec::new() }
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again.
    pub fn add(&mut self, name: Name, source: Source) -> FileId {
        self.files.push(SimpleFile::new(name, source));
        let len = u32::try_from(self.files.len())
            .expect("Too many files (maximum amount of files is `u32::MAX`)");
        FileId::try_from(len).unwrap()
    }

    /// Get the file corresponding to the given id.
    pub fn get(&self, file_id: FileId) -> Result<&SimpleFile<Name, Source>, Error> {
        let index = usize::from(file_id) - 1;
        self.files.get(index).ok_or(Error::FileMissing)
    }
}

impl<'a, Name, Source> codespan_reporting::files::Files<'a> for Files<Name, Source>
where
    Name: 'a + std::fmt::Display + Clone,
    Source: 'a + AsRef<str>,
{
    type FileId = FileId;
    type Name = Name;
    type Source = &'a str;

    fn name(&self, file_id: FileId) -> Result<Name, Error> {
        Ok(self.get(file_id)?.name().clone())
    }

    fn source(&self, file_id: FileId) -> Result<&str, Error> {
        Ok(self.get(file_id)?.source().as_ref())
    }

    fn line_index(&self, file_id: FileId, byte_index: usize) -> Result<usize, Error> {
        self.get(file_id)?.line_index((), byte_index)
    }

    fn line_range(&self, file_id: FileId, line_index: usize) -> Result<Range<usize>, Error> {
        self.get(file_id)?.line_range((), line_index)
    }
}
