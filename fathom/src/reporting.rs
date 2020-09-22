//! Diagnostic messages used in the Fathom compiler.
//!
//! These can be converted to [`Diagnostic`]s in order to present them to the user.
//!
//! [`Diagnostic`]: codespan_reporting::diagnostics::Diagnostic

#![allow(clippy::useless_format)]

use codespan_reporting::diagnostic::{Diagnostic, Label};
use pretty::DocAllocator;
use std::ops::Range;

use crate::lang::{core, surface};
use crate::literal;

/// Global diagnostic messages
#[derive(Debug, Clone)]
pub enum Message {
    NotYetImplemented {
        file_id: usize,
        range: Range<usize>,
        feature_name: &'static str,
    },
    Lexer(LexerMessage),
    LiteralParse(LiteralParseMessage),
    Parse(ParseMessage),
    CoreTyping(CoreTypingMessage),
    SurfaceToCore(SurfaceToCoreMessage),
}

impl From<LexerMessage> for Message {
    fn from(error: LexerMessage) -> Self {
        Message::Lexer(error)
    }
}

impl From<ParseMessage> for Message {
    fn from(error: ParseMessage) -> Self {
        Message::Parse(error)
    }
}

impl From<LiteralParseMessage> for Message {
    fn from(message: LiteralParseMessage) -> Self {
        Message::LiteralParse(message)
    }
}

impl From<CoreTypingMessage> for Message {
    fn from(message: CoreTypingMessage) -> Self {
        Message::CoreTyping(message)
    }
}

impl From<SurfaceToCoreMessage> for Message {
    fn from(message: SurfaceToCoreMessage) -> Self {
        Message::SurfaceToCore(message)
    }
}

impl Message {
    pub fn from_lalrpop<T: std::fmt::Display>(
        file_id: usize,
        error: lalrpop_util::ParseError<usize, T, LexerMessage>,
    ) -> Message {
        use lalrpop_util::ParseError::*;

        match error {
            InvalidToken { location } => Message::from(LexerMessage::InvalidToken {
                file_id,
                range: location..location,
            }),
            UnrecognizedEOF { location, expected } => {
                Message::from(ParseMessage::UnrecognizedEof {
                    file_id,
                    range: location..location,
                    expected,
                })
            }
            UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Message::from(ParseMessage::UnrecognizedToken {
                file_id,
                range: start..end,
                token: token.to_string(),
                expected,
            }),
            ExtraToken {
                token: (start, token, end),
            } => Message::from(ParseMessage::ExtraToken {
                file_id,
                range: start..end,
                token: token.to_string(),
            }),
            User { error } => Message::from(error),
        }
    }

    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<usize>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        match self {
            Message::NotYetImplemented {
                file_id,
                range,
                feature_name,
            } => Diagnostic::bug()
                .with_message(format!("not yet implemented: {}", feature_name))
                .with_labels(vec![Label::primary(*file_id, range.clone())
                    .with_message("relies on an unimplemented language feature")]),
            Message::Lexer(message) => message.to_diagnostic(),
            Message::Parse(message) => message.to_diagnostic(),
            Message::LiteralParse(message) => message.to_diagnostic(),
            Message::CoreTyping(message) => message.to_diagnostic(pretty_alloc),
            Message::SurfaceToCore(message) => message.to_diagnostic(pretty_alloc),
        }
    }
}

/// Messages produced during lexing
#[derive(Debug, Clone)]
pub enum LexerMessage {
    InvalidToken {
        file_id: usize,
        range: Range<usize>,
    },
    UnexpectedChar {
        file_id: usize,
        start: usize,
        found: char,
        expected: &'static [&'static str],
    },
    UnexpectedEof {
        file_id: usize,
        eof: usize,
        expected: &'static [&'static str],
    },
}

impl LexerMessage {
    pub fn to_diagnostic(&self) -> Diagnostic<usize> {
        match self {
            LexerMessage::InvalidToken { file_id, range } => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![Label::primary(*file_id, range.clone())]),
            LexerMessage::UnexpectedChar {
                file_id,
                start,
                found,
                expected,
            } => {
                let end = start + found.len_utf8();
                let range = *start..end;

                Diagnostic::error()
                    .with_message(format!("unexpected character `{}`", found))
                    .with_labels(vec![
                        Label::primary(*file_id, range).with_message("unexpected character")
                    ])
                    .with_notes(
                        format_expected(expected).map_or(Vec::new(), |message| vec![message]),
                    )
            }
            LexerMessage::UnexpectedEof {
                file_id,
                eof,
                expected,
            } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_labels(vec![
                    Label::primary(*file_id, *eof..*eof).with_message("unexpected end of file")
                ])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
        }
    }
}

/// Messages produced during parsing
#[derive(Clone, Debug)]
pub enum ParseMessage {
    UnrecognizedEof {
        file_id: usize,
        range: Range<usize>,
        expected: Vec<String>,
    },
    UnrecognizedToken {
        file_id: usize,
        range: Range<usize>,
        token: String,
        expected: Vec<String>,
    },
    ExtraToken {
        file_id: usize,
        range: Range<usize>,
        token: String,
    },
}

impl ParseMessage {
    pub fn to_diagnostic(&self) -> Diagnostic<usize> {
        match self {
            ParseMessage::UnrecognizedEof {
                file_id,
                range,
                expected,
            } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_labels(vec![
                    Label::primary(*file_id, range.clone()).with_message("unexpected end of file")
                ])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseMessage::UnrecognizedToken {
                file_id,
                range,
                token,
                expected,
            } => Diagnostic::error()
                .with_message(format!("unexpected token {}", token))
                .with_labels(vec![
                    Label::primary(*file_id, range.clone()).with_message("unexpected token")
                ])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseMessage::ExtraToken {
                file_id,
                range,
                token,
            } => Diagnostic::error()
                .with_message(format!("extra token {}", token))
                .with_labels(vec![
                    Label::primary(*file_id, range.clone()).with_message("extra token")
                ]),
        }
    }
}

fn format_expected(expected: &[impl std::fmt::Display]) -> Option<String> {
    use itertools::Itertools;

    expected.split_last().map(|items| match items {
        // TODO: Improve token formatting
        (last, []) => format!("expected {}", last),
        (last, expected) => format!("expected {} or {}", expected.iter().format(", "), last),
    })
}

#[derive(Clone, Debug)]
pub enum LiteralParseMessage {
    ExpectedRadixOrDecimalDigit(usize, Range<usize>),
    ExpectedStartOfNumericLiteral(usize, Range<usize>),
    ExpectedDigit(usize, Range<usize>, literal::Base),
    ExpectedDigitOrSeparator(usize, Range<usize>, literal::Base),
    ExpectedDigitSeparatorOrExp(usize, Range<usize>, literal::Base),
    ExpectedDigitSeparatorFracOrExp(usize, Range<usize>, literal::Base),
    FloatLiteralExponentNotSupported(usize, Range<usize>),
    UnsupportedFloatLiteralBase(usize, Range<usize>, literal::Base),
    UnexpectedEndOfLiteral(usize, Range<usize>),
}

impl LiteralParseMessage {
    pub fn to_diagnostic(&self) -> Diagnostic<usize> {
        match self {
            LiteralParseMessage::ExpectedRadixOrDecimalDigit(file_id, range) => Diagnostic::error()
                .with_message("expected a radix or decimal digit")
                .with_labels(vec![Label::primary(*file_id, range.clone())]),
            LiteralParseMessage::ExpectedStartOfNumericLiteral(file_id, range) => {
                Diagnostic::error()
                    .with_message("expected the start of a numeric literal")
                    .with_labels(vec![Label::primary(*file_id, range.clone())])
            }
            LiteralParseMessage::ExpectedDigit(file_id, range, base) => Diagnostic::error()
                .with_message(format!("expected a base {} digit", base.to_u8()))
                .with_labels(vec![Label::primary(*file_id, range.clone())]),
            LiteralParseMessage::ExpectedDigitOrSeparator(file_id, range, base) => {
                Diagnostic::error()
                    .with_message(format!(
                        "expected a base {} digit or digit separator",
                        base.to_u8(),
                    ))
                    .with_labels(vec![Label::primary(*file_id, range.clone())])
            }
            LiteralParseMessage::ExpectedDigitSeparatorOrExp(file_id, range, base) => {
                Diagnostic::error()
                    .with_message(format!(
                        "expected a base {} digit, digit separator, or exponent",
                        base.to_u8(),
                    ))
                    .with_labels(vec![Label::primary(*file_id, range.clone())])
            }
            LiteralParseMessage::ExpectedDigitSeparatorFracOrExp(file_id, range, base) => {
                Diagnostic::error()
                    .with_message(format!(
                        "expected a base {} digit, digit separator, period, or exponent",
                        base.to_u8(),
                    ))
                    .with_labels(vec![Label::primary(*file_id, range.clone())])
            }
            LiteralParseMessage::FloatLiteralExponentNotSupported(file_id, range) => {
                Diagnostic::error()
                    .with_message("exponents are not yet supported for float literals")
                    .with_labels(vec![Label::primary(*file_id, range.clone())])
            }
            LiteralParseMessage::UnsupportedFloatLiteralBase(file_id, range, base) => {
                Diagnostic::error()
                    .with_message(format!(
                        "base {} float literals are not yet supported",
                        base.to_u8(),
                    ))
                    .with_labels(vec![Label::primary(*file_id, range.clone())])
                    .with_notes(vec![
                        "only base 10 float literals are currently supported".to_owned()
                    ])
            }
            LiteralParseMessage::UnexpectedEndOfLiteral(file_id, range) => Diagnostic::error()
                .with_message("unexpected end of literal")
                .with_labels(vec![Label::primary(*file_id, range.clone())]),
        }
    }
}

/// Messages produced from [`lang::core::typing`]
///
/// [`lang::core::typing`]: crate::lang::core::typing
#[derive(Debug, Clone)]
pub enum CoreTypingMessage {
    GlobalNameNotFound {
        file_id: usize,
        name: String,
        name_range: Range<usize>,
    },
    ItemNameNotFound {
        file_id: usize,
        name: String,
        name_range: Range<usize>,
    },
    FieldRedeclaration {
        file_id: usize,
        field_name: String,
        record_range: Range<usize>,
    },
    ItemRedefinition {
        file_id: usize,
        name: String,
        found_range: Range<usize>,
        original_range: Range<usize>,
    },
    TypeMismatch {
        file_id: usize,
        term_range: Range<usize>,
        expected_type: core::Term,
        found_type: core::Term,
    },
    UniverseMismatch {
        file_id: usize,
        term_range: Range<usize>,
        found_type: core::Term,
    },
    TermHasNoType {
        file_id: usize,
        term_range: Range<usize>,
    },
    NotAFunction {
        file_id: usize,
        head_range: Range<usize>,
        head_type: core::Term,
        argument_range: Range<usize>,
    },
    AmbiguousIntElim {
        file_id: usize,
        term_range: Range<usize>,
    },
}

impl CoreTypingMessage {
    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<usize>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        let to_doc = |term| crate::pass::core_to_pretty::from_term(pretty_alloc, term).1;

        match self {
            CoreTypingMessage::GlobalNameNotFound {
                file_id,
                name,
                name_range,
            } => Diagnostic::bug()
                .with_message(format!("global `{}` is not defined", name))
                .with_labels(vec![Label::primary(*file_id, name_range.clone())
                    .with_message("global is not defined")]),
            CoreTypingMessage::ItemNameNotFound {
                file_id,
                name,
                name_range,
            } => Diagnostic::bug()
                .with_message(format!("cannot find item `{}` in this scope", name))
                .with_labels(vec![Label::primary(*file_id, name_range.clone())
                    .with_message("item not found in this scope")]),
            CoreTypingMessage::FieldRedeclaration {
                file_id,
                field_name,
                record_range,
            } => Diagnostic::bug()
                .with_message(format!("field `{}` is already declared", field_name))
                .with_labels(vec![Label::primary(*file_id, record_range.clone())
                    .with_message(format!("field `{}` declared twice", field_name))])
                .with_notes(vec![format!(
                    "`{}` must be defined only per struct",
                    field_name,
                )]),
            CoreTypingMessage::ItemRedefinition {
                file_id,
                name,
                found_range,
                original_range,
            } => Diagnostic::bug()
                .with_message(format!("the name `{}` is defined multiple times", name))
                .with_labels(vec![
                    Label::primary(*file_id, found_range.clone()).with_message("redefined here"),
                    Label::secondary(*file_id, original_range.clone())
                        .with_message("previous definition here"),
                ])
                .with_notes(vec![format!(
                    "`{}` must be defined only once in this module",
                    name,
                )]),
            CoreTypingMessage::TypeMismatch {
                file_id,
                term_range,
                expected_type,
                found_type,
            } => {
                let expected_type = to_doc(expected_type);
                let found_type = to_doc(found_type);

                Diagnostic::bug()
                    .with_message("type mismatch")
                    .with_labels(vec![Label::primary(*file_id, term_range.clone())
                        .with_message(format!(
                            "expected `{}`, found `{}`",
                            expected_type.pretty(std::usize::MAX),
                            found_type.pretty(std::usize::MAX),
                        ))])
                    .with_notes(vec![[
                        format!("expected `{}`", expected_type.pretty(std::usize::MAX)),
                        format!("   found `{}`", found_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            CoreTypingMessage::UniverseMismatch {
                file_id,
                term_range,
                found_type,
            } => {
                let found_type = to_doc(found_type);

                Diagnostic::bug()
                    .with_message("universe mismatch")
                    .with_labels(vec![Label::primary(*file_id, term_range.clone())
                        .with_message(format!(
                            "expected a universe, found `{}`",
                            found_type.pretty(std::usize::MAX),
                        ))])
                    .with_notes(vec![[
                        format!("expected a universe"),
                        format!("   found `{}`", found_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            CoreTypingMessage::TermHasNoType {
                file_id,
                term_range,
            } => Diagnostic::bug()
                .with_message("term has no type")
                .with_labels(vec![Label::primary(*file_id, term_range.clone())
                    .with_message("cannot synthesize type")])
                .with_notes(vec![format!("term has no type")]),
            CoreTypingMessage::NotAFunction {
                file_id,
                head_range,
                head_type,
                argument_range,
            } => {
                let head_type = to_doc(head_type);

                Diagnostic::bug()
                    .with_message(format!(
                        "applied something that is not a function to an argument"
                    ))
                    .with_labels(vec![
                        Label::primary(*file_id, head_range.clone()).with_message(format!(
                            "expected a function, found `{}`",
                            head_type.pretty(std::usize::MAX),
                        )),
                        Label::secondary(*file_id, argument_range.clone())
                            .with_message("applied to this argument"),
                    ])
                    .with_notes(vec![[
                        format!("expected a function"),
                        format!("   found `{}`", head_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            CoreTypingMessage::AmbiguousIntElim {
                file_id,
                term_range,
            } => Diagnostic::bug()
                .with_message("ambiguous integer elimination")
                .with_labels(vec![Label::primary(*file_id, term_range.clone())
                    .with_message("type annotation required")]),
        }
    }
}

/// Messages produced from [`pass::surface_to_core`]
///
/// [`pass::surface_to_core`]: crate::pass::surface_to_core
#[derive(Debug, Clone)]
pub enum SurfaceToCoreMessage {
    FieldRedeclaration {
        file_id: usize,
        name: String,
        found_range: Range<usize>,
        original_range: Range<usize>,
    },
    ItemRedefinition {
        file_id: usize,
        name: String,
        found_range: Range<usize>,
        original_range: Range<usize>,
    },
    TypeMismatch {
        file_id: usize,
        term_range: Range<usize>,
        expected_type: surface::Term,
        found_type: surface::Term,
    },
    UniverseMismatch {
        file_id: usize,
        term_range: Range<usize>,
        found_type: surface::Term,
    },
    TermHasNoType {
        file_id: usize,
        term_range: Range<usize>,
    },
    NotAFunction {
        file_id: usize,
        head_range: Range<usize>,
        head_type: surface::Term,
        argument_range: Range<usize>,
    },
    AmbiguousMatchExpression {
        file_id: usize,
        term_range: Range<usize>,
    },
    VarNameNotFound {
        file_id: usize,
        name: String,
        name_range: Range<usize>,
    },
    NumericLiteralNotSupported {
        file_id: usize,
        literal_range: Range<usize>,
        expected_type: surface::Term,
    },
    AmbiguousNumericLiteral {
        file_id: usize,
        literal_range: Range<usize>,
    },
    UnsupportedPatternType {
        file_id: usize,
        scrutinee_range: Range<usize>,
        found_type: surface::Term,
    },
    NoDefaultPattern {
        file_id: usize,
        match_range: Range<usize>,
    },
    UnreachablePattern {
        file_id: usize,
        pattern_range: Range<usize>,
    },
}

impl SurfaceToCoreMessage {
    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<usize>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        let to_doc = |term| crate::pass::surface_to_pretty::from_term(pretty_alloc, term).1;

        match self {
            SurfaceToCoreMessage::FieldRedeclaration {
                file_id,
                name,
                found_range,
                original_range,
            } => Diagnostic::error()
                .with_message(format!("field `{}` is already declared", name))
                .with_labels(vec![
                    Label::primary(*file_id, found_range.clone())
                        .with_message("field already declared"),
                    Label::secondary(*file_id, original_range.clone())
                        .with_message("previous field declaration here"),
                ])
                .with_notes(vec![format!("`{}` must be defined only per struct", name)]),
            SurfaceToCoreMessage::ItemRedefinition {
                file_id,
                name,
                found_range,
                original_range,
            } => Diagnostic::error()
                .with_message(format!("the name `{}` is defined multiple times", name))
                .with_labels(vec![
                    Label::primary(*file_id, found_range.clone()).with_message("redefined here"),
                    Label::secondary(*file_id, original_range.clone())
                        .with_message("previous definition here"),
                ])
                .with_notes(vec![format!(
                    "`{}` must be defined only once in this module",
                    name,
                )]),
            SurfaceToCoreMessage::TypeMismatch {
                file_id,
                term_range,
                expected_type,
                found_type,
            } => {
                let expected_type = to_doc(expected_type);
                let found_type = to_doc(found_type);

                Diagnostic::error()
                    .with_message("type mismatch")
                    .with_labels(vec![Label::primary(*file_id, term_range.clone())
                        .with_message(format!(
                            "expected `{}`, found `{}`",
                            expected_type.pretty(std::usize::MAX),
                            found_type.pretty(std::usize::MAX),
                        ))])
                    .with_notes(vec![[
                        format!("expected `{}`", expected_type.pretty(std::usize::MAX)),
                        format!("   found `{}`", found_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            SurfaceToCoreMessage::UniverseMismatch {
                file_id,
                term_range,
                found_type,
            } => {
                let found_type = to_doc(found_type);

                Diagnostic::error()
                    .with_message("universe mismatch")
                    .with_labels(vec![Label::primary(*file_id, term_range.clone())
                        .with_message(format!(
                            "expected a universe, found `{}`",
                            found_type.pretty(std::usize::MAX),
                        ))])
                    .with_notes(vec![[
                        format!("expected a universe"),
                        format!("   found `{}`", found_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            SurfaceToCoreMessage::TermHasNoType {
                file_id,
                term_range,
            } => Diagnostic::error()
                .with_message("term has no type")
                .with_labels(vec![Label::primary(*file_id, term_range.clone())
                    .with_message("cannot synthesize type")])
                .with_notes(vec![format!("term has no type")]),
            SurfaceToCoreMessage::NotAFunction {
                file_id,
                head_range,
                head_type,
                argument_range,
            } => {
                let head_type = to_doc(head_type);

                Diagnostic::error()
                    .with_message(format!(
                        "applied something that is not a function to an argument"
                    ))
                    .with_labels(vec![
                        Label::primary(*file_id, head_range.clone()).with_message(format!(
                            "expected a function, found `{}`",
                            head_type.pretty(std::usize::MAX),
                        )),
                        Label::secondary(*file_id, argument_range.clone())
                            .with_message("applied to this argument"),
                    ])
                    .with_notes(vec![[
                        format!("expected a function"),
                        format!("   found `{}`", head_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            SurfaceToCoreMessage::AmbiguousMatchExpression {
                file_id,
                term_range,
            } => Diagnostic::error()
                .with_message("ambiguous match expression")
                .with_labels(vec![Label::primary(*file_id, term_range.clone())
                    .with_message("type annotation required")]),
            SurfaceToCoreMessage::VarNameNotFound {
                file_id,
                name,
                name_range,
            } => Diagnostic::error()
                .with_message(format!("cannot find `{}` in this scope", name))
                .with_labels(vec![Label::primary(*file_id, name_range.clone())
                    .with_message("not found in this scope")]),
            SurfaceToCoreMessage::NumericLiteralNotSupported {
                file_id,
                literal_range,
                expected_type,
            } => {
                let expected_type = to_doc(expected_type);

                Diagnostic::error()
                    .with_message(format!(
                        "cannot construct a `{}` from a numeric literal",
                        expected_type.pretty(std::usize::MAX),
                    ))
                    .with_labels(vec![Label::primary(*file_id, literal_range.clone())
                        .with_message(format!(
                            "numeric literals not supported for type `{}`",
                            expected_type.pretty(std::usize::MAX),
                        ))])
            }
            SurfaceToCoreMessage::AmbiguousNumericLiteral {
                file_id,
                literal_range,
            } => Diagnostic::error()
                .with_message("ambiguous numeric literal")
                .with_labels(vec![Label::primary(*file_id, literal_range.clone())
                    .with_message("type annotation required")]),
            SurfaceToCoreMessage::UnsupportedPatternType {
                file_id,
                scrutinee_range,
                found_type,
            } => {
                let found_type = to_doc(found_type);

                Diagnostic::error()
                    .with_message(format!(
                        "unsupported pattern type: `{}`",
                        found_type.pretty(std::usize::MAX)
                    ))
                    .with_labels(vec![Label::primary(*file_id, scrutinee_range.clone())
                        .with_message(format!("unsupported pattern type"))])
                    .with_notes(vec![
                        format!(
                            "unsupported pattern type: `{}`",
                            found_type.pretty(std::usize::MAX)
                        ),
                        "can only currently match against terms of type `Bool` or `Int`".to_owned(),
                    ])
            }
            SurfaceToCoreMessage::NoDefaultPattern {
                file_id,
                match_range,
            } => Diagnostic::error()
                .with_message("non-exhaustive patterns")
                .with_labels(vec![Label::primary(*file_id, match_range.clone())
                    .with_message("missing default pattern")]),
            SurfaceToCoreMessage::UnreachablePattern {
                file_id,
                pattern_range,
            } => Diagnostic::warning()
                .with_message("unreachable pattern")
                .with_labels(vec![Label::primary(*file_id, pattern_range.clone())
                    .with_message("unreachable pattern")]),
        }
    }
}
