//! Surface language.

use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::lalrpop_mod;
use scoped_arena::Scope;
use std::cell::RefCell;

use crate::source::{ByteRange, FileId};
use crate::{StringId, StringInterner};

lalrpop_mod!(grammar, "/surface/grammar.rs");
// FIXME: This lexer module should be private! LALRPOP's exports are somewhat broken, however.
//        See: https://github.com/lalrpop/lalrpop/pull/584#issuecomment-856731852
pub(crate) mod lexer;
pub mod pretty;

pub mod distillation;
pub mod elaboration;

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
    /// String literal patterns
    StringLiteral(Range, StringId),
    /// Number literal patterns
    NumberLiteral(Range, StringId),
    // TODO: Record literal patterns
    // RecordLiteral(Range, &'arena [((ByteRange, StringId), Pattern<'arena, Range>)]),
}

impl<'arena, Range: Clone> Pattern<'arena, Range> {
    fn range(&self) -> Range {
        match self {
            Pattern::Name(range, _)
            | Pattern::Placeholder(range)
            | Pattern::Ann(range, _, _)
            | Pattern::StringLiteral(range, _)
            | Pattern::NumberLiteral(range, _) => range.clone(),
        }
    }
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
    /// Match expressions
    Match(
        Range,
        &'arena Term<'arena, Range>,
        &'arena [(Pattern<'arena, Range>, Term<'arena, Range>)],
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
    /// Overlap format.
    FormatOverlap(Range, &'arena [((Range, StringId), Term<'arena, Range>)]),
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
            | Term::Match(range, _, _)
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
            | Term::FormatOverlap(range, _)
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
    ) -> Result<Term<'arena, ByteRange>, ParseMessage> {
        use lalrpop_util::ParseError;

        grammar::TermParser::new()
            .parse(interner, scope, lexer::tokens(source))
            .map_err(|err| match err {
                ParseError::InvalidToken { location } => ParseMessage::InvalidToken {
                    range: ByteRange::new(location, location),
                },
                ParseError::UnrecognizedEOF { location, expected } => {
                    ParseMessage::UnrecognizedEof {
                        range: ByteRange::new(location, location),
                        expected, // TODO: convert to descriptions?
                    }
                }
                ParseError::UnrecognizedToken {
                    token: (start, token, end),
                    expected,
                } => ParseMessage::UnrecognizedToken {
                    range: ByteRange::new(start, end),
                    token: token.description(),
                    expected,
                },
                ParseError::ExtraToken {
                    token: (start, token, end),
                } => ParseMessage::ExtraToken {
                    range: ByteRange::new(start, end),
                    token: token.description(),
                },
                ParseError::User { error } => ParseMessage::Lexer(error),
            })
    }
}

/// Messages produced during parsing
#[derive(Clone, Debug)]
pub enum ParseMessage {
    Lexer(lexer::Error),
    InvalidToken {
        range: ByteRange,
    },
    UnrecognizedEof {
        range: ByteRange,
        expected: Vec<String>,
    },
    UnrecognizedToken {
        range: ByteRange,
        token: &'static str,
        expected: Vec<String>,
    },
    ExtraToken {
        range: ByteRange,
        token: &'static str,
    },
}

impl ParseMessage {
    pub fn to_diagnostic(&self, file_id: FileId) -> Diagnostic<FileId> {
        match self {
            ParseMessage::Lexer(error) => error.to_diagnostic(file_id),
            ParseMessage::InvalidToken { range } => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![Label::primary(file_id, *range)]),
            ParseMessage::UnrecognizedEof { range, expected } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_labels(vec![
                    Label::primary(file_id, *range).with_message("unexpected end of file")
                ])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseMessage::UnrecognizedToken {
                range,
                token,
                expected,
            } => Diagnostic::error()
                .with_message(format!("unexpected token {}", token))
                .with_labels(vec![
                    Label::primary(file_id, *range).with_message("unexpected token")
                ])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseMessage::ExtraToken { range, token } => Diagnostic::error()
                .with_message(format!("extra token {}", token))
                .with_labels(vec![
                    Label::primary(file_id, *range).with_message("extra token")
                ]),
        }
    }
}

fn format_expected(expected: &[impl std::fmt::Display]) -> Option<String> {
    use itertools::Itertools;

    expected.split_last().map(|items| match items {
        (last, []) => format!("expected {}", last),
        (last, expected) => format!("expected {} or {}", expected.iter().format(", "), last),
    })
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
