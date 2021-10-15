//! Surface language.

use lalrpop_util::lalrpop_mod;
use scoped_arena::Scope;

use crate::{BytePos, ByteRange, StringId, StringInterner};

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
pub enum Term<'arena> {
    Name(ByteRange, StringId),
    Hole(ByteRange, Option<StringId>),
    Ann(&'arena Term<'arena>, &'arena Term<'arena>),
    Let(
        BytePos,
        (ByteRange, StringId),
        Option<&'arena Term<'arena>>,
        &'arena Term<'arena>,
        &'arena Term<'arena>,
    ),
    Universe(ByteRange),
    FunArrow(&'arena Term<'arena>, &'arena Term<'arena>),
    FunType(
        BytePos,
        (ByteRange, StringId),
        &'arena Term<'arena>,
        &'arena Term<'arena>,
    ),
    FunIntro(BytePos, (ByteRange, StringId), &'arena Term<'arena>),
    FunElim(&'arena Term<'arena>, &'arena Term<'arena>),
    RecordType(ByteRange, &'arena [((ByteRange, StringId), Term<'arena>)]),
    RecordIntro(ByteRange, &'arena [((ByteRange, StringId), Term<'arena>)]),
    RecordEmpty(ByteRange),
    RecordElim(&'arena Term<'arena>, (ByteRange, StringId)),

    U8Type(ByteRange),  // TODO: Use `Name` variant instead
    U16Type(ByteRange), // TODO: Use `Name` variant instead
    U32Type(ByteRange), // TODO: Use `Name` variant instead
    U64Type(ByteRange), // TODO: Use `Name` variant instead
    S8Type(ByteRange),  // TODO: Use `Name` variant instead
    S16Type(ByteRange), // TODO: Use `Name` variant instead
    S32Type(ByteRange), // TODO: Use `Name` variant instead
    S64Type(ByteRange), // TODO: Use `Name` variant instead
    F32Type(ByteRange), // TODO: Use `Name` variant instead
    F64Type(ByteRange), // TODO: Use `Name` variant instead
    // TODO: NumberLiteral(ByteRange, StringId),

    // TODO: Array8(ByteRange, &'arena Term<'arena>, &'arena Term<'arena>), // TODO: Use `Name` variant instead
    // TODO: Array16(ByteRange, &'arena Term<'arena>, &'arena Term<'arena>), // TODO: Use `Name` variant instead
    // TODO: Array32(ByteRange, &'arena Term<'arena>, &'arena Term<'arena>), // TODO: Use `Name` variant instead
    // TODO: Array64(ByteRange, &'arena Term<'arena>, &'arena Term<'arena>), // TODO: Use `Name` variant instead
    // TODO: ArrayLiteral(ByteRange, &'arena [Term<'arena>]),

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
    FormatType(ByteRange), // TODO: Use `Name` variant instead
    FormatRecord(ByteRange, &'arena [((ByteRange, StringId), Term<'arena>)]),
    FormatFail(ByteRange),  // TODO: Use `Name` variant instead
    FormatU8(ByteRange),    // TODO: Use `Name` variant instead
    FormatU16Be(ByteRange), // TODO: Use `Name` variant instead
    FormatU16Le(ByteRange), // TODO: Use `Name` variant instead
    FormatU32Be(ByteRange), // TODO: Use `Name` variant instead
    FormatU32Le(ByteRange), // TODO: Use `Name` variant instead
    FormatU64Be(ByteRange), // TODO: Use `Name` variant instead
    FormatU64Le(ByteRange), // TODO: Use `Name` variant instead
    FormatS8(ByteRange),    // TODO: Use `Name` variant instead
    FormatS16Be(ByteRange), // TODO: Use `Name` variant instead
    FormatS16Le(ByteRange), // TODO: Use `Name` variant instead
    FormatS32Be(ByteRange), // TODO: Use `Name` variant instead
    FormatS32Le(ByteRange), // TODO: Use `Name` variant instead
    FormatS64Be(ByteRange), // TODO: Use `Name` variant instead
    FormatS64Le(ByteRange), // TODO: Use `Name` variant instead
    FormatF32Be(ByteRange), // TODO: Use `Name` variant instead
    FormatF32Le(ByteRange), // TODO: Use `Name` variant instead
    FormatF64Be(ByteRange), // TODO: Use `Name` variant instead
    FormatF64Le(ByteRange), // TODO: Use `Name` variant instead
    // TODO: FormatArray8(ByteRange, &'arena Term<'arena>, &'arena Term<'arena>), // TODO: Use `Name` variant instead
    // TODO: FormatArray16(ByteRange, &'arena Term<'arena>, &'arena Term<'arena>), // TODO: Use `Name` variant instead
    // TODO: FormatArray32(ByteRange, &'arena Term<'arena>, &'arena Term<'arena>), // TODO: Use `Name` variant instead
    // TODO: FormatArray64(ByteRange, &'arena Term<'arena>, &'arena Term<'arena>), // TODO: Use `Name` variant instead
    FormatRepr(BytePos, &'arena Term<'arena>), // TODO: Use `Name` variant instead

    ReportedError(ByteRange),
}

impl<'arena> Term<'arena> {
    /// Parse a term from the `source` string, interning strings to the
    /// supplied `interner` and allocating nodes to the `arena`.
    pub fn parse<'source>(
        interner: &mut StringInterner,
        scope: &'arena Scope<'arena>,
        source: &'source str,
    ) -> Result<Term<'arena>, ParseError<'source>> {
        grammar::TermParser::new().parse(interner, scope, lexer::tokens(source))
    }

    pub fn range(&self) -> ByteRange {
        ByteRange::new(self.start(), self.end())
    }

    fn start(&self) -> BytePos {
        match self {
            Term::Name(range, _) => range.start(),
            Term::Hole(range, _) => range.start(),
            Term::Ann(expr, _) => expr.start(),
            Term::Let(start, _, _, _, _) => *start,
            Term::Universe(range) => range.start(),
            Term::FunArrow(input_type, _) => input_type.start(),
            Term::FunType(start, _, _, _) => *start,
            Term::FunIntro(start, _, _) => *start,
            Term::FunElim(head_expr, _) => head_expr.start(),
            Term::RecordType(range, _) => range.start(),
            Term::RecordIntro(range, _) => range.start(),
            Term::RecordEmpty(range) => range.start(),
            Term::RecordElim(head_expr, _) => head_expr.start(),

            Term::U8Type(range)
            | Term::U16Type(range)
            | Term::U32Type(range)
            | Term::U64Type(range)
            | Term::S8Type(range)
            | Term::S16Type(range)
            | Term::S32Type(range)
            | Term::S64Type(range)
            | Term::F32Type(range)
            | Term::F64Type(range) => range.start(),

            Term::FormatType(range) => range.start(),
            Term::FormatRecord(range, _) => range.start(),
            Term::FormatFail(range)
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
            | Term::FormatF64Le(range) => range.start(),
            Term::FormatRepr(start, _) => *start,

            Term::ReportedError(range) => range.start(),
        }
    }

    fn end(&self) -> BytePos {
        match self {
            Term::Name(range, _) => range.end(),
            Term::Hole(range, _) => range.end(),
            Term::Ann(expr, _) => expr.end(),
            Term::Let(_, _, _, _, output_expr) => output_expr.end(),
            Term::Universe(range) => range.end(),
            Term::FunArrow(_, output_type) => output_type.end(),
            Term::FunType(_, _, _, output_type) => output_type.end(),
            Term::FunIntro(_, _, output_expr) => output_expr.end(),
            Term::FunElim(_, input_expr) => input_expr.end(),
            Term::RecordType(range, _) => range.end(),
            Term::RecordIntro(range, _) => range.end(),
            Term::RecordEmpty(range) => range.end(),
            Term::RecordElim(_, (range, _)) => range.end(),

            Term::U8Type(range)
            | Term::U16Type(range)
            | Term::U32Type(range)
            | Term::U64Type(range)
            | Term::S8Type(range)
            | Term::S16Type(range)
            | Term::S32Type(range)
            | Term::S64Type(range)
            | Term::F32Type(range)
            | Term::F64Type(range) => range.end(),

            Term::FormatType(range) => range.end(),
            Term::FormatRecord(range, _) => range.start(),
            Term::FormatFail(range)
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
            | Term::FormatF64Le(range) => range.end(),
            Term::FormatRepr(_, expr) => expr.end(),

            Term::ReportedError(range) => range.end(),
        }
    }
}
