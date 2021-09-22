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

fn to_scope_while_ok<'arena, T: 'arena, E>(
    scope: &'arena scoped_arena::Scope<'arena>,
    terms: impl IntoIterator<Item = Result<T, E>>,
) -> Result<&'arena mut [T], E> {
    use itertools::Itertools;

    let mut result = Ok(());
    let terms = scope.to_scope_from_iter(
        (terms.into_iter())
            .map(|term| term.map_err(|err| result = Err(err)).ok())
            .while_some(),
    );
    result.map(|()| terms)
}
