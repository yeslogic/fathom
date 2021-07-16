//! Surface language.

use lalrpop_util::lalrpop_mod;

use crate::{BytePos, ByteRange, StringId, StringInterner};

// FIXME: This lexer module should be private! LALRPOP's exports are somewhat broken, however.
//        See: https://github.com/lalrpop/lalrpop/pull/584#issuecomment-856731852
pub(crate) mod lexer;
lalrpop_mod!(grammar, "/surface/grammar.rs");
pub mod distillation;
pub mod elaboration;
pub mod pretty;

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
    FunType(
        BytePos,
        (ByteRange, StringId),
        &'arena Term<'arena>,
        &'arena Term<'arena>,
    ),
    FunIntro(BytePos, (ByteRange, StringId), &'arena Term<'arena>),
    FunElim(&'arena Term<'arena>, &'arena Term<'arena>),
    // RecordType(&'arena [(StringId, &'arena Term<'arena>)])
    // RecordTerm(&'arena [(StringId, &'arena Term<'arena>)])
    // RecordElim(&'arena Term<'arena>, StringId)
    ReportedError(ByteRange),
}

impl<'arena> Term<'arena> {
    /// Parse a term from the `source` string, interning strings to the
    /// supplied `interner` and allocating nodes to the `arena`.
    pub fn parse<'source>(
        interner: &mut StringInterner,
        arena: &'arena Arena<'arena>,
        source: &'source str,
    ) -> Result<Term<'arena>, ParseError<'source>> {
        grammar::TermParser::new().parse(interner, arena, lexer::tokens(source))
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
            Term::FunType(start, _, _, _) => *start,
            Term::FunIntro(start, _, _) => *start,
            Term::FunElim(head_expr, _) => head_expr.start(),
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
            Term::FunType(_, _, _, output_type) => output_type.end(),
            Term::FunIntro(_, _, output_expr) => output_expr.end(),
            Term::FunElim(_, input_expr) => input_expr.end(),
            Term::ReportedError(range) => range.end(),
        }
    }
}

/// Arena for storing data related to [`Term`]s.
pub struct Arena<'arena> {
    terms: typed_arena::Arena<Term<'arena>>,
}

impl<'arena> Arena<'arena> {
    pub fn new() -> Arena<'arena> {
        Arena {
            terms: typed_arena::Arena::new(),
        }
    }

    pub fn alloc_term(&self, term: Term<'arena>) -> &mut Term<'arena> {
        self.terms.alloc(term)
    }
}
