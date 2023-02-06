//! Surface language.

use std::fmt;
use std::ops::Deref;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::lalrpop_mod;
use scoped_arena::Scope;

use crate::core::Plicity;
use crate::files::FileId;
use crate::source::{BytePos, ByteRange, FileRange, ProgramSource};
use crate::symbol::Symbol;

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
        scope: &'arena Scope<'arena>,
        source: &ProgramSource,
    ) -> (Module<'arena, ByteRange>, Vec<ParseMessage>) {
        let mut messages = Vec::new();

        let tokens = lexer::tokens(source);
        let term = grammar::ModuleParser::new()
            .parse(scope, &mut messages, tokens)
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
    label: (Range, Symbol),
    /// Parameter patterns
    params: &'arena [Param<'arena, Range>],
    /// An optional type annotation for the defined expression
    r#type: Option<Term<'arena, Range>>,
    /// The defined expression
    expr: Term<'arena, Range>,
}

/// Surface patterns.
#[derive(Debug, Clone)]
pub enum Pattern<Range> {
    /// Named patterns, eg. `x`, `true`, `false`
    Name(Range, Symbol),
    /// Placeholder patterns, eg. `_`
    Placeholder(Range),
    /// String literal patterns, eg. `"htmx"`
    ///
    /// As with [term literals][Term::StringLiteral], these will be parsed fully
    /// during [elaboration].
    StringLiteral(Range, Symbol),
    /// Number literal patterns, eg. `1`, `0x00FF`
    ///
    /// As with [term literals][Term::NumberLiteral], these will be parsed fully
    /// during [elaboration].
    NumberLiteral(Range, Symbol),
    /// Boolean literal patterns
    BooleanLiteral(Range, bool),
    // TODO: Record literal patterns
    // RecordLiteral(Range, &'arena [((Range, Symbol), Pattern<'arena, Range>)]),
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

/// Surface terms.
#[derive(Debug, Clone)]
pub enum Term<'arena, Range> {
    /// Parenthesized term
    Paren(Range, &'arena Term<'arena, Range>),
    /// Named patterns.
    Name(Range, Symbol),
    /// Hole expressions.
    Hole(Range, Symbol),
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
        &'arena [(Range, Symbol)],
    ),
    /// Array literals.
    ArrayLiteral(Range, &'arena [Term<'arena, Range>]),
    /// String literal.
    ///
    /// These are stored as strings, and will be parsed during [elaboration]
    /// once the target type is known.
    StringLiteral(Range, Symbol),
    /// Number literals.
    ///
    /// These are stored as strings, and will be parsed during [elaboration]
    /// once the target type is known.
    NumberLiteral(Range, Symbol),
    /// Boolean literals.
    BooleanLiteral(Range, bool),
    /// Record format.
    FormatRecord(Range, &'arena [FormatField<'arena, Range>]),
    /// Overlap format.
    FormatOverlap(Range, &'arena [FormatField<'arena, Range>]),
    /// Conditional format.
    FormatCond(
        Range,
        (Range, Symbol),
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

impl<'arena, Range> Term<'arena, Range> {
    /// Apply `f` to all the child terms of self.
    /// Useful for implementing functions that need to behave differently on a
    /// few specific node types, but otherwise just recurse through the tree
    /// normally
    pub fn walk(&self, mut f: impl FnMut(&Self)) {
        let mut recur2 = |term1, term2| {
            f(term1);
            f(term2)
        };

        match self {
            Term::Name(_, _)
            | Term::Hole(_, _)
            | Term::Placeholder(_)
            | Term::Universe(_)
            | Term::StringLiteral(_, _)
            | Term::NumberLiteral(_, _)
            | Term::BooleanLiteral(_, _)
            | Term::ReportedError(_) => {}

            Term::Paren(_, term) => f(term),
            Term::Ann(_, term, r#type) => recur2(term, r#type),
            Term::Let(_, def, body) => {
                if let Some(r#type) = def.r#type.as_ref() {
                    f(r#type);
                }
                f(&def.expr);
                f(body);
            }
            Term::If(_, cond, then, r#else) => {
                f(cond);
                f(then);
                f(r#else)
            }
            Term::Match(_, scrut, branches) => {
                f(scrut);
                branches.iter().for_each(|(_, term)| f(term));
            }
            Term::Arrow(_, _, input, output) => recur2(input, output),
            Term::FunType(_, params, body) | Term::FunLiteral(_, params, body) => {
                params.iter().for_each(|param| {
                    if let Some(r#type) = param.r#type.as_ref() {
                        f(r#type)
                    }
                });
                f(body)
            }
            Term::App(_, head, args) => {
                f(head);
                args.iter().for_each(|arg| f(&arg.term));
            }
            Term::RecordType(_, fields) => fields.iter().for_each(|field| f(&field.r#type)),
            Term::RecordLiteral(_, field) => field.iter().for_each(|field| {
                if let Some(term) = field.expr.as_ref() {
                    f(term)
                }
            }),
            Term::Proj(_, head, _) => f(head),
            Term::Tuple(_, terms) | Term::ArrayLiteral(_, terms) => terms.iter().for_each(f),
            Term::FormatRecord(_, fields) | Term::FormatOverlap(_, fields) => {
                fields.iter().for_each(|field| match field {
                    FormatField::Format { format, pred, .. } => {
                        f(format);
                        if let Some(pred) = pred {
                            f(pred)
                        }
                    }
                    FormatField::Computed { r#type, expr, .. } => {
                        if let Some(r#type) = r#type {
                            f(r#type);
                        }
                        f(expr);
                    }
                })
            }
            Term::FormatCond(_, _, format, pred) => recur2(format, pred),
            Term::BinOp(_, lhs, _, rhs) => recur2(lhs, rhs),
        }
    }
}

impl<'arena> Term<'arena, FileRange> {
    /// Parse a term from the `source` string, interning strings to the
    /// supplied `interner` and allocating nodes to the `arena`.
    pub fn parse(
        scope: &'arena Scope<'arena>,
        source: &ProgramSource,
    ) -> (Term<'arena, ByteRange>, Vec<ParseMessage>) {
        let mut messages = Vec::new();

        let tokens = lexer::tokens(source);
        let term = grammar::TermParser::new()
            .parse(scope, &mut messages, tokens)
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
pub struct LetDef<'arena, Range> {
    pub pattern: Pattern<Range>,
    pub r#type: Option<Term<'arena, Range>>,
    pub expr: Term<'arena, Range>,
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
        label: (Range, Symbol),
        /// The format that this field will be parsed with
        format: Term<'arena, Range>,
        /// An optional predicate that refines the format field
        pred: Option<Term<'arena, Range>>,
    },
    /// Computed format field
    Computed {
        /// Label identifying the field
        label: (Range, Symbol),
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
    label: (Range, Symbol),
    /// The type that is expected for this field
    r#type: Term<'arena, Range>,
}

/// A field definition in a record literal
#[derive(Debug, Clone)]
pub struct ExprField<'arena, Range> {
    /// Label identifying the field
    label: (Range, Symbol),
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

pub struct Builder<'arena> {
    scope: &'arena Scope<'arena>,
}

impl<'arena> Deref for Builder<'arena> {
    type Target = Scope<'arena>;

    fn deref(&self) -> &Self::Target {
        self.scope
    }
}

impl<'arena> Builder<'arena> {
    pub fn new(scope: &'arena Scope<'arena>) -> Self {
        Self { scope }
    }

    pub fn into_scope(self) -> &'arena Scope<'arena> {
        self.scope
    }

    pub fn paren<Range>(
        &self,
        range: impl Into<Range>,
        term: Term<'arena, Range>,
    ) -> Term<'arena, Range> {
        Term::Paren(range.into(), self.scope.to_scope(term))
    }

    pub fn ann<Range>(
        &self,
        range: impl Into<Range>,
        term: Term<'arena, Range>,
        r#type: Term<'arena, Range>,
    ) -> Term<'arena, Range> {
        Term::Ann(
            range.into(),
            self.scope.to_scope(term),
            self.scope.to_scope(r#type),
        )
    }

    pub fn r#let<Range>(
        &self,
        range: impl Into<Range>,
        def: LetDef<'arena, Range>,
        body: Term<'arena, Range>,
    ) -> Term<'arena, Range> {
        Term::Let(
            range.into(),
            self.scope.to_scope(def),
            self.scope.to_scope(body),
        )
    }

    pub fn if_then_else<Range>(
        &self,
        range: impl Into<Range>,
        cond: Term<'arena, Range>,
        then: Term<'arena, Range>,
        r#else: Term<'arena, Range>,
    ) -> Term<'arena, Range> {
        Term::If(
            range.into(),
            self.scope.to_scope(cond),
            self.scope.to_scope(then),
            self.scope.to_scope(r#else),
        )
    }

    pub fn r#match<Range>(
        &self,
        range: impl Into<Range>,
        scrut: Term<'arena, Range>,
        branches: &'arena [(Pattern<Range>, Term<'arena, Range>)],
    ) -> Term<'arena, Range> {
        Term::Match(range.into(), self.scope.to_scope(scrut), branches)
    }

    pub fn arrow<Range>(
        &self,
        range: impl Into<Range>,
        plicity: Plicity,
        r#type: Term<'arena, Range>,
        body: Term<'arena, Range>,
    ) -> Term<'arena, Range> {
        Term::Arrow(
            range.into(),
            plicity,
            self.scope.to_scope(r#type),
            self.scope.to_scope(body),
        )
    }

    pub fn fun_type<Range>(
        &self,
        range: impl Into<Range>,
        params: &'arena [Param<'arena, Range>],
        body: Term<'arena, Range>,
    ) -> Term<'arena, Range> {
        Term::FunType(range.into(), params, self.scope.to_scope(body))
    }

    pub fn fun_lit<Range>(
        &self,
        range: impl Into<Range>,
        params: &'arena [Param<'arena, Range>],
        body: Term<'arena, Range>,
    ) -> Term<'arena, Range> {
        Term::FunLiteral(range.into(), params, self.scope.to_scope(body))
    }

    pub fn record_type<Range>(
        &self,
        range: impl Into<Range>,
        fields: &'arena [TypeField<'arena, Range>],
    ) -> Term<'arena, Range> {
        Term::RecordType(range.into(), fields)
    }

    pub fn record_lit<Range>(
        &self,
        range: impl Into<Range>,
        fields: &'arena [ExprField<'arena, Range>],
    ) -> Term<'arena, Range> {
        Term::RecordLiteral(range.into(), fields)
    }

    pub fn tuple<Range>(
        &self,
        range: impl Into<Range>,
        terms: &'arena [Term<'arena, Range>],
    ) -> Term<'arena, Range> {
        Term::Tuple(range.into(), terms)
    }

    pub fn fun_app<Range>(
        &self,
        range: impl Into<Range>,
        fun: Term<'arena, Range>,
        args: &'arena [Arg<'arena, Range>],
    ) -> Term<'arena, Range> {
        Term::App(range.into(), self.scope.to_scope(fun), args)
    }

    pub fn record_proj<Range>(
        &self,
        range: impl Into<Range>,
        head: Term<'arena, Range>,
        labels: &'arena [(Range, Symbol)],
    ) -> Term<'arena, Range> {
        Term::Proj(range.into(), self.scope.to_scope(head), labels)
    }

    pub fn array_lit<Range>(
        &self,
        range: impl Into<Range>,
        terms: &'arena [Term<'arena, Range>],
    ) -> Term<'arena, Range> {
        Term::ArrayLiteral(range.into(), terms)
    }

    pub fn format_record<Range>(
        &self,
        range: impl Into<Range>,
        fields: &'arena [FormatField<'arena, Range>],
    ) -> Term<'arena, Range> {
        Term::FormatRecord(range.into(), fields)
    }

    pub fn format_overlap<Range>(
        &self,
        range: impl Into<Range>,
        fields: &'arena [FormatField<'arena, Range>],
    ) -> Term<'arena, Range> {
        Term::FormatOverlap(range.into(), fields)
    }

    pub fn format_cond<Range>(
        &self,
        range: impl Into<Range>,
        label: (Range, Symbol),
        format: Term<'arena, Range>,
        pred: Term<'arena, Range>,
    ) -> Term<'arena, Range> {
        Term::FormatCond(
            range.into(),
            label,
            self.scope.to_scope(format),
            self.scope.to_scope(pred),
        )
    }

    pub fn binop<Range>(
        &self,
        range: impl Into<Range>,
        lhs: Term<'arena, Range>,
        op: BinOp<Range>,
        rhs: Term<'arena, Range>,
    ) -> Term<'arena, Range> {
        Term::BinOp(
            range.into(),
            self.scope.to_scope(lhs),
            op,
            self.scope.to_scope(rhs),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_drop() {
        assert!(!std::mem::needs_drop::<Term<'_, ()>>());
        assert!(!std::mem::needs_drop::<Term<'_, Symbol>>());
        assert!(!std::mem::needs_drop::<Pattern<Symbol>>());
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
