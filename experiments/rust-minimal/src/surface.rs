//! Surface language.

use lalrpop_util::lalrpop_mod;
use scoped_arena::Scope;
use std::cell::RefCell;

use crate::source::ByteRange;
use crate::{StringId, StringInterner};

lalrpop_mod!(grammar, "/surface/grammar.rs");
// FIXME: This lexer module should be private! LALRPOP's exports are somewhat broken, however.
//        See: https://github.com/lalrpop/lalrpop/pull/584#issuecomment-856731852
pub(crate) mod lexer;
pub mod pretty;

pub mod distillation;
pub mod elaboration;

// TODO: Convert to an internal error message
pub type ParseError<'source> = lalrpop_util::ParseError<usize, lexer::Token<'source>, ()>;

/// Surface terms.
#[derive(Debug, Clone)]
pub enum Term<'arena, Range> {
    Name(Range, StringId),
    Hole(Range, StringId),
    Placeholder(Range),
    Ann(
        Range,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    Let(
        Range,
        (Range, StringId),
        Option<&'arena Term<'arena, Range>>,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    Universe(Range),
    Arrow(
        Range,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    FunType(
        Range,
        (Range, StringId),
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    FunLiteral(Range, (Range, StringId), &'arena Term<'arena, Range>),
    FunElim(
        Range,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    RecordType(Range, &'arena [((Range, StringId), Term<'arena, Range>)]),
    RecordLiteral(Range, &'arena [((Range, StringId), Term<'arena, Range>)]),
    UnitLiteral(Range),
    RecordElim(Range, &'arena Term<'arena, Range>, (Range, StringId)),
    ArrayLiteral(Range, &'arena [Term<'arena, Range>]),
    NumberLiteral(Range, StringId),
    FormatRecord(Range, &'arena [((Range, StringId), Term<'arena, Range>)]),
    ReportedError(Range),
}

impl<'arena, Range: Clone> Term<'arena, Range> {
    pub fn range(&self) -> Range {
        match self {
            Term::Name(range, _)
            | Term::Hole(range, _)
            | Term::Placeholder(range)
            | Term::Ann(range, _, _)
            | Term::Let(range, _, _, _, _)
            | Term::Universe(range)
            | Term::Arrow(range, _, _)
            | Term::FunType(range, _, _, _)
            | Term::FunLiteral(range, _, _)
            | Term::FunElim(range, _, _)
            | Term::RecordType(range, _)
            | Term::RecordLiteral(range, _)
            | Term::UnitLiteral(range)
            | Term::ArrayLiteral(range, _)
            | Term::RecordElim(range, _, _)
            | Term::NumberLiteral(range, _)
            | Term::FormatRecord(range, _)
            | Term::ReportedError(range) => range.clone(),
        }
    }
}

impl<'arena> Term<'arena, ByteRange> {
    /// Parse a term from the `source` string, interning strings to the
    /// supplied `interner` and allocating nodes to the `arena`.
    pub fn parse<'source>(
        interner: &RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
        source: &'source str,
    ) -> Result<Term<'arena, ByteRange>, ParseError<'source>> {
        grammar::TermParser::new().parse(interner, scope, lexer::tokens(source))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_drop() {
        assert!(!std::mem::needs_drop::<Term<'_, ()>>());
        assert!(!std::mem::needs_drop::<Term<'_, StringId>>());
    }
}
