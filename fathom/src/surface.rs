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

/// Surface patterns.
#[derive(Debug, Clone)]
pub enum Pattern<'arena, Range> {
    /// Named patterns.
    Name(Range, StringId),
    /// Placeholder patterns.
    Placeholder(Range),
    /// Annotated patterns.
    Ann(
        Range,
        &'arena Pattern<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    // TODO: Number literal patterns
    // TODO: Record literal patterns
}

/// Surface terms.
#[derive(Debug, Clone)]
pub enum Term<'arena, Range> {
    /// Named patterns.
    Name(Range, StringId),
    /// Hole expressions.
    Hole(Range, StringId),
    /// Placeholder expressions.
    Placeholder(Range),
    /// Annotated expressions.
    Ann(
        Range,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Let expressions.
    Let(
        Range,
        &'arena Pattern<'arena, Range>,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// The type of types.
    Universe(Range),
    /// Arrow types.
    Arrow(
        Range,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Dependent function types.
    FunType(
        Range,
        &'arena Pattern<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Function literals.
    FunLiteral(
        Range,
        &'arena Pattern<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Function eliminations.
    FunElim(
        Range,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Dependent record types.
    RecordType(Range, &'arena [((Range, StringId), Term<'arena, Range>)]),
    /// Record literals.
    RecordLiteral(Range, &'arena [((Range, StringId), Term<'arena, Range>)]),
    /// Unit literals.
    UnitLiteral(Range),
    /// Record eliminations.
    RecordElim(Range, &'arena Term<'arena, Range>, (Range, StringId)),
    /// Array literals.
    ArrayLiteral(Range, &'arena [Term<'arena, Range>]),
    /// String literal.
    StringLiteral(Range, StringId),
    /// Number literals.
    NumberLiteral(Range, StringId),
    /// Record format.
    FormatRecord(Range, &'arena [((Range, StringId), Term<'arena, Range>)]),
    /// Reported error sentinel.
    ReportedError(Range),
}

impl<'arena, Range: Clone> Term<'arena, Range> {
    /// Get the source range of the term.
    pub fn range(&self) -> Range {
        match self {
            Term::Name(range, _)
            | Term::Hole(range, _)
            | Term::Placeholder(range)
            | Term::Ann(range, _, _)
            | Term::Let(range, _, _, _)
            | Term::Universe(range)
            | Term::Arrow(range, _, _)
            | Term::FunType(range, _, _)
            | Term::FunLiteral(range, _, _)
            | Term::FunElim(range, _, _)
            | Term::RecordType(range, _)
            | Term::RecordLiteral(range, _)
            | Term::UnitLiteral(range)
            | Term::RecordElim(range, _, _)
            | Term::ArrayLiteral(range, _)
            | Term::StringLiteral(range, _)
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
        assert!(!std::mem::needs_drop::<Pattern<StringId>>());
    }
}
