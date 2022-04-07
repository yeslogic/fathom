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
pub enum Pattern<Range> {
    /// Named patterns, eg. `x`, `true`, `false`
    Name(Range, StringId),
    /// Placeholder patterns, eg. `_`
    Placeholder(Range),
    /// String literal patterns, eg. `"htmx"`
    ///
    /// As with [term literals][Term::StringLiteral], these will be parsed fully
    /// during [elaboration].
    StringLiteral(Range, StringId),
    /// Number literal patterns, eg. `1`, `0x00FF`
    ///
    /// As with [term literals][Term::NumberLiteral], these will be parsed fully
    /// during [elaboration].
    NumberLiteral(Range, StringId),
    // TODO: Record literal patterns
    // RecordLiteral(Range, &'arena [((ByteRange, StringId), Pattern<'arena, Range>)]),
}

impl<Range: Clone> Pattern<Range> {
    fn range(&self) -> Range {
        match self {
            Pattern::Name(range, _)
            | Pattern::Placeholder(range)
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
        Pattern<Range>,
        Option<&'arena Term<'arena, Range>>,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Match expressions
    Match(
        Range,
        &'arena Term<'arena, Range>,
        &'arena [(Pattern<Range>, Term<'arena, Range>)],
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
        Pattern<Range>,
        Option<&'arena Term<'arena, Range>>,
        &'arena Term<'arena, Range>,
    ),
    /// Function literals.
    FunLiteral(
        Range,
        Pattern<Range>,
        Option<&'arena Term<'arena, Range>>,
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
    ///
    /// These are stored as strings, and will be parsed during [elaboration]
    /// once the target type is known.
    StringLiteral(Range, StringId),
    /// Number literals.
    ///
    /// These are stored as strings, and will be parsed during [elaboration]
    /// once the target type is known.
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
            | Term::Let(range, _, _, _, _)
            | Term::Match(range, _, _)
            | Term::Universe(range)
            | Term::Arrow(range, _, _)
            | Term::FunType(range, _, _, _)
            | Term::FunLiteral(range, _, _, _)
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
    ) -> (Term<'arena, ByteRange>, Vec<ParseMessage>) {
        let mut messages = Vec::new();

        let term = grammar::TermParser::new()
            .parse(interner, scope, &mut messages, lexer::tokens(source))
            .unwrap_or_else(|error| {
                let message = ParseMessage::from(error);
                let range = message.range();
                messages.push(message);
                Term::ReportedError(range)
            });

        (term, messages)
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
    pub fn range(&self) -> ByteRange {
        match self {
            ParseMessage::Lexer(error) => error.range(),
            ParseMessage::InvalidToken { range }
            | ParseMessage::UnrecognizedEof { range, .. }
            | ParseMessage::UnrecognizedToken { range, .. }
            | ParseMessage::ExtraToken { range, .. } => *range,
        }
    }

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

type LalrpopParseError<'source> =
    lalrpop_util::ParseError<usize, lexer::Token<'source>, lexer::Error>;

impl From<LalrpopParseError<'_>> for ParseMessage {
    fn from(error: LalrpopParseError<'_>) -> ParseMessage {
        match error {
            LalrpopParseError::InvalidToken { location } => ParseMessage::InvalidToken {
                range: ByteRange::new(location, location),
            },
            LalrpopParseError::UnrecognizedEOF { location, expected } => {
                ParseMessage::UnrecognizedEof {
                    range: ByteRange::new(location, location),
                    expected, // TODO: convert to descriptions?
                }
            }
            LalrpopParseError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => ParseMessage::UnrecognizedToken {
                range: ByteRange::new(start, end),
                token: token.description(),
                expected,
            },
            LalrpopParseError::ExtraToken {
                token: (start, token, end),
            } => ParseMessage::ExtraToken {
                range: ByteRange::new(start, end),
                token: token.description(),
            },
            LalrpopParseError::User { error } => ParseMessage::Lexer(error),
        }
    }
}

type LalrpopErrorRecovery<'source> =
    lalrpop_util::ErrorRecovery<usize, lexer::Token<'source>, lexer::Error>;

impl From<LalrpopErrorRecovery<'_>> for ParseMessage {
    fn from(error: LalrpopErrorRecovery<'_>) -> ParseMessage {
        ParseMessage::from(error.error) // TODO: Use dropped tokens?
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
