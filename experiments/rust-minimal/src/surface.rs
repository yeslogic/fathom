//! Surface language.

use lalrpop_util::lalrpop_mod;
use scoped_arena::Scope;
use std::cell::RefCell;

use crate::{ByteRange, StringId, StringInterner};

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
    Hole(Range, Option<StringId>),
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
    FunArrow(
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
    FunIntro(Range, (Range, StringId), &'arena Term<'arena, Range>),
    FunElim(
        Range,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    RecordType(Range, &'arena [((Range, StringId), Term<'arena, Range>)]),
    RecordIntro(Range, &'arena [((Range, StringId), Term<'arena, Range>)]),
    RecordEmpty(Range),
    RecordElim(Range, &'arena Term<'arena, Range>, (Range, StringId)),
    NumberLiteral(Range, StringId),
    ArrayLiteral(Range, &'arena [Term<'arena, Range>]),
    FormatRecord(Range, &'arena [((Range, StringId), Term<'arena, Range>)]),
    ReportedError(Range),
}

impl<'arena, Range: Clone> Term<'arena, Range> {
    pub fn range(&self) -> Range {
        match self {
            Term::Name(range, _)
            | Term::Hole(range, _)
            | Term::Ann(range, _, _)
            | Term::Let(range, _, _, _, _)
            | Term::Universe(range)
            | Term::FunArrow(range, _, _)
            | Term::FunType(range, _, _, _)
            | Term::FunIntro(range, _, _)
            | Term::FunElim(range, _, _)
            | Term::RecordType(range, _)
            | Term::RecordIntro(range, _)
            | Term::RecordEmpty(range)
            | Term::RecordElim(range, _, _)
            | Term::ArrayLiteral(range, _)
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
