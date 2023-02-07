//! Surface language.

use std::cell::RefCell;
use std::fmt;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::lalrpop_mod;
use scoped_arena::Scope;

use crate::core::Plicity;
use crate::files::FileId;
use crate::source::{BytePos, ByteRange, FileRange, ProgramSource, StringId, StringInterner};

lalrpop_mod!(
    #[allow(clippy::all)]
    grammar,
    "/surface/grammar.rs"
);
pub mod lexer;
pub mod pretty;

pub mod distillation;
pub mod elaboration;

/// Modules, consisting of a sequence of top-level items.
#[derive(Debug, Clone)]
pub struct Module<'arena, Range> {
    items: &'arena [Item<'arena, Range>],
}

impl<'arena> Module<'arena, ByteRange> {
    /// Parse a term from the `source` string, interning strings to the
    /// supplied `interner` and allocating nodes to the `arena`.
    pub fn parse(
        interner: &RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
        source: &ProgramSource,
    ) -> (Module<'arena, ByteRange>, Vec<ParseMessage>) {
        let mut messages = Vec::new();

        let tokens = lexer::tokens(source);
        let term = grammar::ModuleParser::new()
            .parse(interner, scope, &mut messages, tokens)
            .unwrap_or_else(|error| {
                messages.push(ParseMessage::from_lalrpop(error));
                Module { items: &[] }
            });

        (term, messages)
    }
}

/// Top-level items.
#[derive(Debug, Clone)]
pub enum Item<'arena, Range> {
    /// Top-level definitions
    Def(ItemDef<'arena, Range>),
    /// Reported error sentinel
    ReportedError(Range),
}

/// Top-level definitions
#[derive(Debug, Clone)]
pub struct ItemDef<'arena, Range> {
    /// The full range of the definition
    range: Range,
    /// The label that identifies this definition
    label: (Range, StringId),
    /// Parameter patterns
    params: &'arena [Param<'arena, Range>],
    /// An optional type annotation for the defined expression
    r#type: Option<&'arena Term<'arena, Range>>,
    /// The defined expression
    expr: &'arena Term<'arena, Range>,
}

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
    /// Boolean literal patterns
    BooleanLiteral(Range, bool),
    // TODO: Record literal patterns
    // RecordLiteral(Range, &'arena [((Range, StringId), Pattern<'arena, Range>)]),
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp<Range> {
    Add(Range),
    Sub(Range),
    Mul(Range),
    Div(Range),
    Eq(Range),
    Neq(Range),
    Lt(Range),
    Lte(Range),
    Gt(Range),
    Gte(Range),
}

impl<Range> BinOp<Range> {
    fn range(&self) -> Range
    where
        Range: Clone,
    {
        match self {
            BinOp::Add(range)
            | BinOp::Sub(range)
            | BinOp::Mul(range)
            | BinOp::Div(range)
            | BinOp::Eq(range)
            | BinOp::Neq(range)
            | BinOp::Lt(range)
            | BinOp::Lte(range)
            | BinOp::Gt(range)
            | BinOp::Gte(range) => range.clone(),
        }
    }

    fn as_str(&self) -> &'static str {
        match self {
            BinOp::Add(_) => "+",
            BinOp::Sub(_) => "-",
            BinOp::Mul(_) => "*",
            BinOp::Div(_) => "/",
            BinOp::Eq(_) => "==",
            BinOp::Neq(_) => "!=",
            BinOp::Lt(_) => "<",
            BinOp::Lte(_) => "<=",
            BinOp::Gt(_) => ">",
            BinOp::Gte(_) => ">=",
        }
    }

    fn map_range<T>(self, f: impl Fn(Range) -> T) -> BinOp<T> {
        match self {
            BinOp::Add(range) => BinOp::Add(f(range)),
            BinOp::Sub(range) => BinOp::Sub(f(range)),
            BinOp::Mul(range) => BinOp::Mul(f(range)),
            BinOp::Div(range) => BinOp::Div(f(range)),
            BinOp::Eq(range) => BinOp::Eq(f(range)),
            BinOp::Neq(range) => BinOp::Neq(f(range)),
            BinOp::Lt(range) => BinOp::Lt(f(range)),
            BinOp::Lte(range) => BinOp::Lte(f(range)),
            BinOp::Gt(range) => BinOp::Gt(f(range)),
            BinOp::Gte(range) => BinOp::Gte(f(range)),
        }
    }
}

impl<Range> fmt::Display for BinOp<Range> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl<Range: Clone> Pattern<Range> {
    pub fn range(&self) -> Range {
        match self {
            Pattern::Name(range, _)
            | Pattern::Placeholder(range)
            | Pattern::StringLiteral(range, _)
            | Pattern::NumberLiteral(range, _)
            | Pattern::BooleanLiteral(range, _) => range.clone(),
        }
    }
}

impl<Range> Pattern<Range> {
    pub fn name(&self) -> Option<StringId> {
        match self {
            Pattern::Name(_, name) => Some(*name),
            _ => None,
        }
    }
}

/// Surface terms.
#[derive(Debug, Clone)]
pub enum Term<'arena, Range> {
    /// Parenthesized term
    Paren(Range, &'arena Term<'arena, Range>),
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
        &'arena LetDef<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Letrec expressions.
    Letrec(
        Range,
        &'arena [LetDef<'arena, Range>],
        &'arena Term<'arena, Range>,
    ),
    /// If expressions
    If(
        Range,
        &'arena Term<'arena, Range>,
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
        Plicity,
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Dependent function types.
    FunType(
        Range,
        &'arena [Param<'arena, Range>],
        &'arena Term<'arena, Range>,
    ),
    /// Function literals.
    FunLiteral(
        Range,
        &'arena [Param<'arena, Range>],
        &'arena Term<'arena, Range>,
    ),
    /// Applications.
    App(
        Range,
        &'arena Term<'arena, Range>,
        &'arena [Arg<'arena, Range>],
    ),
    /// Dependent record types.
    RecordType(Range, &'arena [TypeField<'arena, Range>]),
    /// Record literals.
    RecordLiteral(Range, &'arena [ExprField<'arena, Range>]),
    Tuple(Range, &'arena [Term<'arena, Range>]),
    /// Projections.
    Proj(
        Range,
        &'arena Term<'arena, Range>,
        &'arena [(Range, StringId)],
    ),
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
    /// Boolean literals.
    BooleanLiteral(Range, bool),
    /// Record format.
    FormatRecord(Range, &'arena [FormatField<'arena, Range>]),
    /// Overlap format.
    FormatOverlap(Range, &'arena [FormatField<'arena, Range>]),
    /// Conditional format.
    FormatCond(
        Range,
        (Range, StringId),
        &'arena Term<'arena, Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Binary operator expressions.
    BinOp(
        Range,
        &'arena Term<'arena, Range>,
        BinOp<Range>,
        &'arena Term<'arena, Range>,
    ),
    /// Reported error sentinel.
    ReportedError(Range),
}

#[derive(Debug, Clone)]
pub struct LetDef<'arena, Range> {
    pub pattern: Pattern<Range>,
    pub r#type: Option<Term<'arena, Range>>,
    pub expr: Term<'arena, Range>,
}

impl<'arena, Range: Clone> Term<'arena, Range> {
    /// Get the source range of the term.
    pub fn range(&self) -> Range {
        match self {
            Term::Paren(range, _)
            | Term::Name(range, _)
            | Term::Hole(range, _)
            | Term::Placeholder(range)
            | Term::Ann(range, _, _)
            | Term::Let(range, ..)
            | Term::Letrec(range, ..)
            | Term::If(range, _, _, _)
            | Term::Match(range, _, _)
            | Term::Universe(range)
            | Term::Arrow(range, ..)
            | Term::FunType(range, _, _)
            | Term::FunLiteral(range, _, _)
            | Term::App(range, _, _)
            | Term::RecordType(range, _)
            | Term::RecordLiteral(range, _)
            | Term::Tuple(range, _)
            | Term::Proj(range, _, _)
            | Term::ArrayLiteral(range, _)
            | Term::StringLiteral(range, _)
            | Term::NumberLiteral(range, _)
            | Term::BooleanLiteral(range, _)
            | Term::FormatRecord(range, _)
            | Term::FormatCond(range, _, _, _)
            | Term::FormatOverlap(range, _)
            | Term::BinOp(range, _, _, _)
            | Term::ReportedError(range) => range.clone(),
        }
    }
}

impl<'arena> Term<'arena, FileRange> {
    /// Parse a term from the `source` string, interning strings to the
    /// supplied `interner` and allocating nodes to the `arena`.
    pub fn parse(
        interner: &RefCell<StringInterner>,
        scope: &'arena Scope<'arena>,
        source: &ProgramSource,
    ) -> (Term<'arena, ByteRange>, Vec<ParseMessage>) {
        let mut messages = Vec::new();

        let tokens = lexer::tokens(source);
        let term = grammar::TermParser::new()
            .parse(interner, scope, &mut messages, tokens)
            .unwrap_or_else(|error| {
                let message = ParseMessage::from_lalrpop(error);
                let range = message.range();
                messages.push(message);
                Term::ReportedError(range)
            });

        (term, messages)
    }
}

#[derive(Debug, Clone)]
pub struct Param<'arena, Range> {
    pub plicity: Plicity,
    pub pattern: Pattern<Range>,
    pub r#type: Option<Term<'arena, Range>>,
}

#[derive(Debug, Clone)]
pub struct Arg<'arena, Range> {
    pub plicity: Plicity,
    pub term: Term<'arena, Range>,
}

/// A field declaration in a record and offset format
#[derive(Debug, Clone)]
pub enum FormatField<'arena, Range> {
    /// Regular format field
    Format {
        /// Label identifying the field
        label: (Range, StringId),
        /// The format that this field will be parsed with
        format: Term<'arena, Range>,
        /// An optional predicate that refines the format field
        pred: Option<Term<'arena, Range>>,
    },
    /// Computed format field
    Computed {
        /// Label identifying the field
        label: (Range, StringId),
        /// Optional type annotation
        r#type: Option<Term<'arena, Range>>,
        /// The expression that this field compute
        expr: Term<'arena, Range>,
    },
}

/// A field declaration in a record type
#[derive(Debug, Clone)]
pub struct TypeField<'arena, Range> {
    /// Label identifying the field
    label: (Range, StringId),
    /// The type that is expected for this field
    r#type: Term<'arena, Range>,
}

/// A field definition in a record literal
#[derive(Debug, Clone)]
pub struct ExprField<'arena, Range> {
    /// Label identifying the field
    label: (Range, StringId),
    /// The expression that this field will store.
    /// If it is `None`, it is the same as `label.1`
    expr: Option<Term<'arena, Range>>,
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

    fn from_lalrpop(error: LalrpopParseError<'_>) -> ParseMessage {
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

    fn from_lalrpop_recovery(error: LalrpopErrorRecovery<'_>) -> ParseMessage {
        // TODO: make use of use `error.dropped_tokens` in error reporting?
        ParseMessage::from_lalrpop(error.error)
    }

    pub fn to_diagnostic(&self, file_id: FileId) -> Diagnostic<FileId> {
        let primary_label = |range: &ByteRange| Label::primary(file_id, *range);

        match self {
            ParseMessage::Lexer(error) => error.to_diagnostic(file_id),
            ParseMessage::InvalidToken { range } => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![primary_label(range)]),
            ParseMessage::UnrecognizedEof { range, expected } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_labels(vec![
                    primary_label(range).with_message("unexpected end of file")
                ])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseMessage::UnrecognizedToken {
                range,
                token,
                expected,
            } => Diagnostic::error()
                .with_message(format!("unexpected token {token}"))
                .with_labels(vec![primary_label(range).with_message("unexpected token")])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseMessage::ExtraToken { range, token } => Diagnostic::error()
                .with_message(format!("extra token {token}"))
                .with_labels(vec![primary_label(range).with_message("extra token")]),
        }
    }
}

type LalrpopParseError<'source> =
    lalrpop_util::ParseError<BytePos, lexer::Token<'source>, lexer::Error>;

type LalrpopErrorRecovery<'source> =
    lalrpop_util::ErrorRecovery<BytePos, lexer::Token<'source>, lexer::Error>;

fn format_expected(expected: &[impl std::fmt::Display]) -> Option<String> {
    use itertools::Itertools;

    expected.split_last().map(|items| match items {
        (last, []) => format!("expected {last}"),
        (last, expected) => format!("expected {} or {}", expected.iter().format(", "), last),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::ByteRange;

    #[test]
    fn no_drop() {
        assert!(!std::mem::needs_drop::<Term<'_, ()>>());
        assert!(!std::mem::needs_drop::<Term<'_, StringId>>());
        assert!(!std::mem::needs_drop::<Pattern<StringId>>());
    }

    #[test]
    #[cfg(target_pointer_width = "64")]
    fn term_size() {
        assert_eq!(std::mem::size_of::<Term<()>>(), 32);
        assert_eq!(std::mem::size_of::<Term<ByteRange>>(), 40);
    }

    #[test]
    #[cfg(target_pointer_width = "64")]
    fn pattern_size() {
        assert_eq!(std::mem::size_of::<Pattern<()>>(), 8);
        assert_eq!(std::mem::size_of::<Pattern<ByteRange>>(), 16);
    }
}
