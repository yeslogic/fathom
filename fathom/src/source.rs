///! Types related to source files.

/// File id.
pub type FileId = usize; // TODO: use wrapper struct

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
}

impl Into<std::ops::Range<usize>> for ByteRange {
    fn into(self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}
