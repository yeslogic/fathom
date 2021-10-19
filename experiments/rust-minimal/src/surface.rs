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

    U8Type(Range),  // TODO: Use `Name` variant instead
    U16Type(Range), // TODO: Use `Name` variant instead
    U32Type(Range), // TODO: Use `Name` variant instead
    U64Type(Range), // TODO: Use `Name` variant instead
    S8Type(Range),  // TODO: Use `Name` variant instead
    S16Type(Range), // TODO: Use `Name` variant instead
    S32Type(Range), // TODO: Use `Name` variant instead
    S64Type(Range), // TODO: Use `Name` variant instead
    F32Type(Range), // TODO: Use `Name` variant instead
    F64Type(Range), // TODO: Use `Name` variant instead
    NumberLiteral(Range, StringId),

    // TODO: Array8(Range, &'arena Term<'arena, Range>, &'arena Term<'arena, Range>), // TODO: Use `Name` variant instead
    // TODO: Array16(Range, &'arena Term<'arena, Range>, &'arena Term<'arena, Range>), // TODO: Use `Name` variant instead
    // TODO: Array32(Range, &'arena Term<'arena, Range>, &'arena Term<'arena, Range>), // TODO: Use `Name` variant instead
    // TODO: Array64(Range, &'arena Term<'arena, Range>, &'arena Term<'arena, Range>), // TODO: Use `Name` variant instead
    // TODO: ArrayLiteral(Range, &'arena [Term<'arena, Range>]),

    // TODO: Add a more systematic way of extending the format universe.
    //
    // Perhaps something like:
    //
    // ```fathom
    // primitive format u16be : Format = {
    //     Repr = U16,
    //     name = "u16be",
    // };
    //
    // primitive format array16 (len : U16) (f : Format) : Format = {
    //     Repr = Array16 len (Repr f),
    //     name = "array16",
    // };
    // ```
    FormatType(Range), // TODO: Use `Name` variant instead
    FormatRecord(Range, &'arena [((Range, StringId), Term<'arena, Range>)]),
    FormatFail(Range),  // TODO: Use `Name` variant instead
    FormatU8(Range),    // TODO: Use `Name` variant instead
    FormatU16Be(Range), // TODO: Use `Name` variant instead
    FormatU16Le(Range), // TODO: Use `Name` variant instead
    FormatU32Be(Range), // TODO: Use `Name` variant instead
    FormatU32Le(Range), // TODO: Use `Name` variant instead
    FormatU64Be(Range), // TODO: Use `Name` variant instead
    FormatU64Le(Range), // TODO: Use `Name` variant instead
    FormatS8(Range),    // TODO: Use `Name` variant instead
    FormatS16Be(Range), // TODO: Use `Name` variant instead
    FormatS16Le(Range), // TODO: Use `Name` variant instead
    FormatS32Be(Range), // TODO: Use `Name` variant instead
    FormatS32Le(Range), // TODO: Use `Name` variant instead
    FormatS64Be(Range), // TODO: Use `Name` variant instead
    FormatS64Le(Range), // TODO: Use `Name` variant instead
    FormatF32Be(Range), // TODO: Use `Name` variant instead
    FormatF32Le(Range), // TODO: Use `Name` variant instead
    FormatF64Be(Range), // TODO: Use `Name` variant instead
    FormatF64Le(Range), // TODO: Use `Name` variant instead
    // TODO: FormatArray8(Range, &'arena Term<'arena, Range>, &'arena Term<'arena, Range>), // TODO: Use `Name` variant instead
    // TODO: FormatArray16(Range, &'arena Term<'arena, Range>, &'arena Term<'arena, Range>), // TODO: Use `Name` variant instead
    // TODO: FormatArray32(Range, &'arena Term<'arena, Range>, &'arena Term<'arena, Range>), // TODO: Use `Name` variant instead
    // TODO: FormatArray64(Range, &'arena Term<'arena, Range>, &'arena Term<'arena, Range>), // TODO: Use `Name` variant instead
    FormatRepr(Range, &'arena Term<'arena, Range>), // TODO: Use `Name` variant instead

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
            | Term::U8Type(range)
            | Term::U16Type(range)
            | Term::U32Type(range)
            | Term::U64Type(range)
            | Term::S8Type(range)
            | Term::S16Type(range)
            | Term::S32Type(range)
            | Term::S64Type(range)
            | Term::F32Type(range)
            | Term::F64Type(range)
            | Term::NumberLiteral(range, _)
            | Term::FormatType(range)
            | Term::FormatRecord(range, _)
            | Term::FormatFail(range)
            | Term::FormatU8(range)
            | Term::FormatU16Be(range)
            | Term::FormatU16Le(range)
            | Term::FormatU32Be(range)
            | Term::FormatU32Le(range)
            | Term::FormatU64Be(range)
            | Term::FormatU64Le(range)
            | Term::FormatS8(range)
            | Term::FormatS16Be(range)
            | Term::FormatS16Le(range)
            | Term::FormatS32Be(range)
            | Term::FormatS32Le(range)
            | Term::FormatS64Be(range)
            | Term::FormatS64Le(range)
            | Term::FormatF32Be(range)
            | Term::FormatF32Le(range)
            | Term::FormatF64Be(range)
            | Term::FormatF64Le(range)
            | Term::FormatRepr(range, _)
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
