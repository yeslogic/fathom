#![doc = include_str!("../README.md")]

pub mod env;

pub mod core;
pub mod surface;

/// Interned strings.
pub type StringId = string_interner::symbol::SymbolU16;

/// String interner.
pub type StringInterner = string_interner::StringInterner<
    StringId,
    string_interner::backend::BucketBackend<StringId>,
    std::hash::BuildHasherDefault<fxhash::FxHasher32>,
>;

pub type BytePos = usize;

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

/// A helpful type for allocating elements to a slice up to a maximum length.
pub struct SliceBuilder<'a, Elem> {
    next_index: usize,
    elems: &'a mut [Elem],
}

impl<'a, Elem: Clone> SliceBuilder<'a, Elem> {
    fn new(
        scope: &'a scoped_arena::Scope<'a>,
        max_len: usize,
        default: Elem,
    ) -> SliceBuilder<'a, Elem> {
        SliceBuilder::from(scope.to_scope_from_iter(std::iter::repeat(default).take(max_len)))
    }

    pub fn push(&mut self, elem: Elem) {
        self.elems[self.next_index] = elem;
        self.next_index += 1;
    }
}

impl<'a, Elem> From<&'a mut [Elem]> for SliceBuilder<'a, Elem> {
    fn from(elems: &'a mut [Elem]) -> SliceBuilder<'a, Elem> {
        SliceBuilder {
            next_index: 0,
            elems,
        }
    }
}

impl<'a, Elem> Into<&'a mut [Elem]> for SliceBuilder<'a, Elem> {
    fn into(self) -> &'a mut [Elem] {
        &mut self.elems[..self.next_index]
    }
}

impl<'a, Elem> Into<&'a [Elem]> for SliceBuilder<'a, Elem> {
    fn into(self) -> &'a [Elem] {
        &self.elems[..self.next_index]
    }
}
