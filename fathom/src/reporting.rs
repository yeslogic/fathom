//! Diagnostic messages used in the Fathom compiler.
//!
//! These can be converted to [`Diagnostic`]s in order to present them to the user.
//!
//! [`Diagnostic`]: codespan_reporting::diagnostics::Diagnostic

#![allow(clippy::useless_format)]

use codespan_reporting::diagnostic::{Diagnostic, Label};
use itertools::Itertools;
use pretty::DocAllocator;
use std::path::PathBuf;

use crate::lang::{core, surface, FileId, Located, Location};
use crate::literal;

macro_rules! label {
    ($style:ident($location:expr) $(= $message:expr)? $(,)?) => {
        match $location {
            Location::Generated => None,
            Location::FileRange(file_id, range) => {
                Some(Label::$style(*file_id, *range)$(.with_message($message))?)
            },
        }
    };
}

macro_rules! labels {
    (($style:ident($location:expr) $(= $message:expr)?) $(,)?) => {
        label!($style($location) $(= $message)?)
            .collect::<Vec<Label<FileId>>>()
    };
    ($($style:ident($location:expr) $(= $message:expr)?),* $(,)?) => {
        std::iter::empty()
            $(.chain(label!($style($location) $(= $message)?)))*
            .collect::<Vec<Label<FileId>>>()
    };
}

/// Global diagnostic messages
#[derive(Debug, Clone)]
pub enum Message {
    NotYetImplemented {
        location: Location,
        feature_name: &'static str,
    },
    ReadFile {
        path: PathBuf,
        error: String,
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
        file_id: FileId,
        error: lalrpop_util::ParseError<usize, T, LexerMessage>,
    ) -> Message {
        use lalrpop_util::ParseError::*;

        match error {
            InvalidToken { location } => Message::from(LexerMessage::InvalidToken {
                location: Location::file_range(file_id, location..location),
            }),
            UnrecognizedEOF { location, expected } => {
                Message::from(ParseMessage::UnrecognizedEof {
                    location: Location::file_range(file_id, location..location),
                    expected,
                })
            }
            UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Message::from(ParseMessage::UnrecognizedToken {
                location: Location::file_range(file_id, start..end),
                token: token.to_string(),
                expected,
            }),
            ExtraToken {
                token: (start, token, end),
            } => Message::from(ParseMessage::ExtraToken {
                location: Location::file_range(file_id, start..end),
                token: token.to_string(),
            }),
            User { error } => Message::from(error),
        }
    }

    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<FileId>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        match self {
            Message::NotYetImplemented {
                location,
                feature_name,
            } => Diagnostic::bug()
                .with_message(format!("not yet implemented: {}", feature_name))
                .with_labels(labels![
                    primary(location) = "relies on an unimplemented language feature",
                ]),
            Message::ReadFile { path, error } => Diagnostic::error()
                .with_message(format!("failed to read file `{}`", path.display()))
                // TODO: add user-friendly suggestions
                .with_notes(vec![format!("{}", error.to_lowercase())]),
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
    InvalidToken { location: Location },
}

impl LexerMessage {
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        match self {
            LexerMessage::InvalidToken { location } => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(labels![primary(location)]),
        }
    }
}

/// Messages produced during parsing
#[derive(Clone, Debug)]
pub enum ParseMessage {
    UnrecognizedEof {
        location: Location,
        expected: Vec<String>,
    },
    UnrecognizedToken {
        location: Location,
        token: String,
        expected: Vec<String>,
    },
    ExtraToken {
        location: Location,
        token: String,
    },
}

impl ParseMessage {
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        match self {
            ParseMessage::UnrecognizedEof { location, expected } => Diagnostic::error()
                .with_message("unexpected end of file")
                .with_labels(labels![primary(location) = "unexpected end of file"])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseMessage::UnrecognizedToken {
                location,
                token,
                expected,
            } => Diagnostic::error()
                .with_message(format!("unexpected token {}", token))
                .with_labels(labels![primary(location) = "unexpected token"])
                .with_notes(format_expected(expected).map_or(Vec::new(), |message| vec![message])),
            ParseMessage::ExtraToken { location, token } => Diagnostic::error()
                .with_message(format!("extra token {}", token))
                .with_labels(labels![primary(location) = "extra token"]),
        }
    }
}

fn format_expected(expected: &[impl std::fmt::Display]) -> Option<String> {
    expected.split_last().map(|items| match items {
        // TODO: Improve token formatting
        (last, []) => format!("expected {}", last),
        (last, expected) => format!("expected {} or {}", expected.iter().format(", "), last),
    })
}

#[derive(Clone, Debug)]
pub enum LiteralParseMessage {
    ExpectedRadixOrDecimalDigit(Location),
    ExpectedStartOfNumericLiteral(Location),
    ExpectedDigit(Location, literal::Base),
    ExpectedDigitOrSeparator(Location, literal::Base),
    ExpectedDigitSeparatorOrExp(Location, literal::Base),
    ExpectedDigitSeparatorFracOrExp(Location, literal::Base),
    FloatLiteralExponentNotSupported(Location),
    UnsupportedFloatLiteralBase(Location, literal::Base),
    UnexpectedEndOfLiteral(Location),
}

impl LiteralParseMessage {
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        match self {
            LiteralParseMessage::ExpectedRadixOrDecimalDigit(location) => Diagnostic::error()
                .with_message("expected a radix or decimal digit")
                .with_labels(labels![primary(location)]),
            LiteralParseMessage::ExpectedStartOfNumericLiteral(location) => Diagnostic::error()
                .with_message("expected the start of a numeric literal")
                .with_labels(labels![primary(location)]),
            LiteralParseMessage::ExpectedDigit(location, base) => Diagnostic::error()
                .with_message(format!("expected a base {} digit", base.to_u8()))
                .with_labels(labels![primary(location)]),
            LiteralParseMessage::ExpectedDigitOrSeparator(location, base) => Diagnostic::error()
                .with_message(format!(
                    "expected a base {} digit or digit separator",
                    base.to_u8(),
                ))
                .with_labels(labels![primary(location)]),
            LiteralParseMessage::ExpectedDigitSeparatorOrExp(location, base) => Diagnostic::error()
                .with_message(format!(
                    "expected a base {} digit, digit separator, or exponent",
                    base.to_u8(),
                ))
                .with_labels(labels![primary(location)]),
            LiteralParseMessage::ExpectedDigitSeparatorFracOrExp(location, base) => {
                Diagnostic::error()
                    .with_message(format!(
                        "expected a base {} digit, digit separator, period, or exponent",
                        base.to_u8(),
                    ))
                    .with_labels(labels![primary(location)])
            }
            LiteralParseMessage::FloatLiteralExponentNotSupported(location) => Diagnostic::error()
                .with_message("exponents are not yet supported for float literals")
                .with_labels(labels![primary(location)]),
            LiteralParseMessage::UnsupportedFloatLiteralBase(location, base) => Diagnostic::error()
                .with_message(format!(
                    "base {} float literals are not yet supported",
                    base.to_u8(),
                ))
                .with_labels(labels![primary(location)])
                .with_notes(vec![
                    "only base 10 float literals are currently supported".to_owned()
                ]),
            LiteralParseMessage::UnexpectedEndOfLiteral(location) => Diagnostic::error()
                .with_message("unexpected end of literal")
                .with_labels(labels![primary(location)]),
        }
    }
}

/// Messages produced from [`lang::core::typing`]
///
/// [`lang::core::typing`]: crate::lang::core::typing
#[derive(Debug, Clone)]
pub enum CoreTypingMessage {
    GlobalNameNotFound {
        global_name: String,
        global_name_location: Location,
    },
    ItemNameNotFound {
        item_name: String,
        item_name_location: Location,
    },
    LocalIndexNotFound {
        local_index: core::LocalIndex,
        local_index_location: Location,
    },
    FieldRedeclaration {
        field_name: String,
        record_location: Location,
    },
    ItemRedefinition {
        name: String,
        found_location: Location,
        original_location: Location,
    },
    TypeMismatch {
        term_location: Location,
        expected_type: core::Term,
        found_type: core::Term,
    },
    UniverseMismatch {
        term_location: Location,
        found_type: core::Term,
    },
    TermHasNoType {
        term_location: Location,
    },
    NotAFunction {
        head_location: Location,
        head_type: core::Term,
        argument_location: Location,
    },
    FieldNotFound {
        head_location: Location,
        head_type: core::Term,
        label: String,
    },
    AmbiguousTerm {
        term_location: Location,
    },
    UnexpectedArrayTerm {
        term_location: Location,
        expected_type: core::Term,
    },
    DuplicateStructFields {
        duplicate_labels: Vec<Located<String>>,
    },
    MissingStructFields {
        term_location: Location,
        missing_labels: Vec<Located<String>>,
    },
    UnexpectedStructFields {
        term_location: Location,
        unexpected_labels: Vec<Located<String>>,
    },
    UnexpectedStructTerm {
        term_location: Location,
        expected_type: core::Term,
    },
}

impl CoreTypingMessage {
    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<FileId>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        let to_doc = |term| crate::pass::core_to_pretty::from_term(pretty_alloc, term).1;

        match self {
            CoreTypingMessage::GlobalNameNotFound {
                global_name,
                global_name_location,
            } => Diagnostic::bug()
                .with_message(format!("global `{}` is not defined", global_name))
                .with_labels(labels![
                    primary(global_name_location) = "global is not defined",
                ]),
            CoreTypingMessage::ItemNameNotFound {
                item_name,
                item_name_location,
            } => Diagnostic::bug()
                .with_message(format!("cannot find item `{}` in this scope", item_name))
                .with_labels(labels![
                    primary(item_name_location) = "item not found in this scope",
                ]),
            CoreTypingMessage::LocalIndexNotFound {
                local_index,
                local_index_location,
            } => Diagnostic::bug()
                .with_message(format!(
                    "cannot find local `{}` in this scope",
                    local_index.to_usize(),
                ))
                .with_labels(labels![
                    primary(local_index_location) = "local not found in this scope",
                ]),
            CoreTypingMessage::FieldRedeclaration {
                field_name,
                record_location,
            } => Diagnostic::bug()
                .with_message(format!("field `{}` is already declared", field_name))
                .with_labels(labels![
                    primary(record_location) = format!("field `{}` declared twice", field_name),
                ])
                .with_notes(vec![format!(
                    "`{}` must be defined only per struct",
                    field_name,
                )]),
            CoreTypingMessage::ItemRedefinition {
                name,
                found_location,
                original_location,
            } => Diagnostic::bug()
                .with_message(format!("the name `{}` is defined multiple times", name))
                .with_labels(labels![
                    primary(found_location) = "redefined here",
                    secondary(original_location) = "previous definition here",
                ])
                .with_notes(vec![format!(
                    "`{}` must be defined only once in this module",
                    name,
                )]),
            CoreTypingMessage::TypeMismatch {
                term_location,
                expected_type,
                found_type,
            } => {
                let expected_type = to_doc(expected_type);
                let found_type = to_doc(found_type);

                Diagnostic::bug()
                    .with_message("type mismatch")
                    .with_labels(labels![
                        primary(term_location) = format!(
                            "expected `{}`, found `{}`",
                            expected_type.pretty(std::usize::MAX),
                            found_type.pretty(std::usize::MAX),
                        ),
                    ])
                    .with_notes(vec![[
                        format!("expected `{}`", expected_type.pretty(std::usize::MAX)),
                        format!("   found `{}`", found_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            CoreTypingMessage::UniverseMismatch {
                term_location,
                found_type,
            } => {
                let found_type = to_doc(found_type);

                Diagnostic::bug()
                    .with_message("universe mismatch")
                    .with_labels(labels![
                        primary(term_location) = format!(
                            "expected a universe, found `{}`",
                            found_type.pretty(std::usize::MAX),
                        ),
                    ])
                    .with_notes(vec![[
                        format!("expected a universe"),
                        format!("   found `{}`", found_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            CoreTypingMessage::TermHasNoType { term_location } => Diagnostic::bug()
                .with_message("term has no type")
                .with_labels(labels![primary(term_location) = "cannot synthesize type"])
                .with_notes(vec![format!("term has no type")]),
            CoreTypingMessage::NotAFunction {
                head_location,
                head_type,
                argument_location,
            } => {
                let head_type = to_doc(head_type);

                Diagnostic::bug()
                    .with_message(format!(
                        "applied something that is not a function to an argument"
                    ))
                    .with_labels(labels![
                        primary(head_location) = (format!(
                            "expected a function, found `{}`",
                            head_type.pretty(std::usize::MAX),
                        )),
                        secondary(argument_location) = "applied to this argument",
                    ])
                    .with_notes(vec![[
                        format!("expected a function"),
                        format!("   found `{}`", head_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            CoreTypingMessage::FieldNotFound {
                head_location,
                head_type,
                label,
            } => {
                let head_type = to_doc(head_type);

                Diagnostic::bug()
                    .with_message(format!(
                        "could not find field `{}` on type `{}`",
                        &label,
                        head_type.pretty(std::usize::MAX),
                    ))
                    .with_labels(labels![
                        primary(head_location) = "field not found in this term",
                    ])
            }
            CoreTypingMessage::AmbiguousTerm { term_location } => Diagnostic::bug()
                .with_message("ambiguous term")
                .with_labels(labels![primary(term_location) = "type annotation required"]),
            CoreTypingMessage::UnexpectedArrayTerm {
                term_location,
                expected_type,
            } => {
                let expected_type = to_doc(expected_type);

                Diagnostic::bug()
                    .with_message("unexpected array term")
                    .with_labels(labels![
                        primary(term_location) = format!(
                            "expected `{}`, found array term",
                            expected_type.pretty(std::usize::MAX),
                        ),
                    ])
            }
            CoreTypingMessage::DuplicateStructFields { duplicate_labels } => Diagnostic::error()
                .with_message("duplicate fields found in struct")
                .with_labels(
                    duplicate_labels
                        .iter()
                        .flat_map(|label| {
                            label!(primary(&label.location) = "field already defined")
                        })
                        .collect(),
                ),
            CoreTypingMessage::MissingStructFields {
                term_location,
                missing_labels,
            } => Diagnostic::bug()
                .with_message("missing fields for struct")
                .with_labels(
                    std::iter::empty()
                        .chain(label!(
                            primary(term_location) = format!(
                                "missing fields {}",
                                missing_labels.iter().map(|label| &label.data).format(", ")
                            ),
                        ))
                        .chain(missing_labels.iter().flat_map(|label| {
                            label!(secondary(&label.location) = "field defined on struct here")
                        }))
                        .collect(),
                ),
            CoreTypingMessage::UnexpectedStructFields {
                term_location,
                unexpected_labels,
            } => Diagnostic::bug()
                .with_message("unexpected fields found in struct")
                .with_labels(
                    unexpected_labels
                        .iter()
                        .flat_map(|label| label!(primary(&label.location) = "unexpected field"))
                        .chain(label!(
                            secondary(term_location) = "struct instantiated here"
                        ))
                        .collect(),
                ),
            CoreTypingMessage::UnexpectedStructTerm {
                term_location,
                expected_type,
            } => {
                let expected_type = to_doc(expected_type);

                Diagnostic::bug()
                    .with_message("unexpected struct term")
                    .with_labels(labels![
                        primary(term_location) = format!(
                            "expected `{}`, found struct",
                            expected_type.pretty(std::usize::MAX),
                        ),
                    ])
            }
        }
    }
}

/// Messages produced from [`pass::surface_to_core`]
///
/// [`pass::surface_to_core`]: crate::pass::surface_to_core
#[derive(Debug, Clone)]
pub enum SurfaceToCoreMessage {
    MissingStructAnnotation {
        name: String,
        name_location: Location,
    },
    InvalidStructAnnotation {
        name: String,
        ann_type: surface::Term,
        ann_location: Location,
    },
    FieldRedeclaration {
        name: String,
        found_location: Location,
        original_location: Location,
    },
    ItemRedefinition {
        name: String,
        found_location: Location,
        original_location: Location,
    },
    TypeMismatch {
        term_location: Location,
        expected_type: surface::Term,
        found_type: surface::Term,
    },
    UniverseMismatch {
        term_location: Location,
        found_type: surface::Term,
    },
    TermHasNoType {
        term_location: Location,
    },
    NotAFunction {
        head_location: Location,
        head_type: surface::Term,
        argument_location: Location,
    },
    FieldNotFound {
        head_location: Location,
        head_type: surface::Term,
        label: Located<String>,
    },
    AmbiguousMatchExpression {
        term_location: Location,
    },
    VarNameNotFound {
        name: String,
        name_location: Location,
    },
    MismatchedArrayLength {
        term_location: Location,
        found_len: usize,
        expected_len: surface::Term,
    },
    UnexpectedSequenceTerm {
        term_location: Location,
        expected_type: surface::Term,
    },
    NumericLiteralNotSupported {
        literal_location: Location,
        expected_type: surface::Term,
    },
    AmbiguousSequenceTerm {
        location: Location,
    },
    AmbiguousNumericLiteral {
        literal_location: Location,
    },
    AmbiguousStructTerm {
        term_location: Location,
    },
    UnsupportedPatternType {
        scrutinee_location: Location,
        found_type: surface::Term,
    },
    NoDefaultPattern {
        match_location: Location,
    },
    UnreachablePattern {
        pattern_location: Location,
    },
    DuplicateStructFields {
        duplicate_labels: Vec<Located<String>>,
    },
    MissingStructFields {
        term_location: Location,
        missing_labels: Vec<Located<String>>,
    },
    UnexpectedStructFields {
        term_location: Location,
        unexpected_labels: Vec<Located<String>>,
    },
    UnexpectedStructTerm {
        term_location: Location,
        expected_type: surface::Term,
    },
}

impl SurfaceToCoreMessage {
    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<FileId>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        let to_doc = |term| crate::pass::surface_to_pretty::from_term(pretty_alloc, term).1;

        match self {
            SurfaceToCoreMessage::MissingStructAnnotation {
                name,
                name_location,
            } => Diagnostic::error()
                .with_message(format!("missing type annotation for struct `{}`", name))
                .with_labels(labels![
                    primary(&name_location.end()) = "annotation expected here",
                ]),
            SurfaceToCoreMessage::InvalidStructAnnotation {
                name,
                ann_type,
                ann_location,
            } => {
                let ann_type = to_doc(ann_type);

                Diagnostic::error()
                    .with_message(format!("invalid type annotation for struct `{}`", name))
                    .with_labels(labels![
                        primary(ann_location) = format!(
                            "expected `Type` or `Format`, found `{}`",
                            ann_type.pretty(std::usize::MAX),
                        ),
                    ])
                    .with_notes(vec![[
                        format!("expected `Type` or `Format`"),
                        format!("   found `{}`", ann_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            SurfaceToCoreMessage::FieldRedeclaration {
                name,
                found_location,
                original_location,
            } => Diagnostic::error()
                .with_message(format!("field `{}` is already declared", name))
                .with_labels(labels![
                    primary(found_location) = "field already declared",
                    secondary(original_location) = "previous field declaration here",
                ])
                .with_notes(vec![format!("`{}` must be defined only per struct", name)]),
            SurfaceToCoreMessage::ItemRedefinition {
                name,
                found_location,
                original_location,
            } => Diagnostic::error()
                .with_message(format!("the name `{}` is defined multiple times", name))
                .with_labels(labels![
                    primary(found_location) = ("redefined here"),
                    secondary(original_location) = "previous definition here",
                ])
                .with_notes(vec![format!(
                    "`{}` must be defined only once in this module",
                    name,
                )]),
            SurfaceToCoreMessage::TypeMismatch {
                term_location,
                expected_type,
                found_type,
            } => {
                let expected_type = to_doc(expected_type);
                let found_type = to_doc(found_type);

                Diagnostic::error()
                    .with_message("type mismatch")
                    .with_labels(labels![
                        primary(term_location) = format!(
                            "expected `{}`, found `{}`",
                            expected_type.pretty(std::usize::MAX),
                            found_type.pretty(std::usize::MAX),
                        ),
                    ])
                    .with_notes(vec![[
                        format!("expected `{}`", expected_type.pretty(std::usize::MAX)),
                        format!("   found `{}`", found_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            SurfaceToCoreMessage::UniverseMismatch {
                term_location,
                found_type,
            } => {
                let found_type = to_doc(found_type);

                Diagnostic::error()
                    .with_message("universe mismatch")
                    .with_labels(labels![
                        primary(term_location) = format!(
                            "expected a universe, found `{}`",
                            found_type.pretty(std::usize::MAX),
                        ),
                    ])
                    .with_notes(vec![[
                        format!("expected a universe"),
                        format!("   found `{}`", found_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            SurfaceToCoreMessage::TermHasNoType { term_location } => Diagnostic::error()
                .with_message("term has no type")
                .with_labels(labels![primary(term_location) = "cannot synthesize type"])
                .with_notes(vec![format!("term has no type")]),
            SurfaceToCoreMessage::NotAFunction {
                head_location,
                head_type,
                argument_location,
            } => {
                let head_type = to_doc(head_type);

                Diagnostic::error()
                    .with_message(format!(
                        "applied something that is not a function to an argument"
                    ))
                    .with_labels(labels![
                        primary(head_location) = (format!(
                            "expected a function, found `{}`",
                            head_type.pretty(std::usize::MAX),
                        )),
                        secondary(argument_location) = "applied to this argument",
                    ])
                    .with_notes(vec![[
                        format!("expected a function"),
                        format!("   found `{}`", head_type.pretty(std::usize::MAX)),
                    ]
                    .join("\n")])
            }
            SurfaceToCoreMessage::FieldNotFound {
                head_location,
                head_type,
                label,
            } => {
                let head_type = to_doc(head_type);

                Diagnostic::error()
                    .with_message(format!(
                        "could not find field `{}` on type `{}`",
                        &label.data,
                        head_type.pretty(std::usize::MAX),
                    ))
                    .with_labels(labels![
                        primary(&label.location) = "non-existent field",
                        secondary(head_location) = "field not found in this term",
                    ])
            }
            SurfaceToCoreMessage::AmbiguousMatchExpression { term_location } => Diagnostic::error()
                .with_message("ambiguous match expression")
                .with_labels(labels![primary(term_location) = "type annotation required"]),
            SurfaceToCoreMessage::VarNameNotFound {
                name,
                name_location,
            } => Diagnostic::error()
                .with_message(format!("cannot find `{}` in this scope", name))
                .with_labels(labels![primary(name_location) = "not found in this scope"]),
            SurfaceToCoreMessage::MismatchedArrayLength {
                term_location,
                found_len,
                expected_len,
            } => Diagnostic::error()
                .with_message("mismatched array length")
                .with_labels(labels![
                    primary(term_location) = format!(
                        "expected `{}` elements, found `{}` elements",
                        to_doc(&expected_len).pretty(std::usize::MAX),
                        found_len,
                    ),
                ]),
            SurfaceToCoreMessage::UnexpectedSequenceTerm {
                term_location,
                expected_type,
            } => {
                let expected_type = to_doc(expected_type);

                Diagnostic::error()
                    .with_message("unexpected sequence term")
                    .with_labels(labels![
                        primary(term_location) = format!(
                            "expected `{}`, found sequence term",
                            expected_type.pretty(std::usize::MAX),
                        ),
                    ])
            }
            SurfaceToCoreMessage::NumericLiteralNotSupported {
                literal_location,
                expected_type,
            } => {
                let expected_type = to_doc(expected_type);

                Diagnostic::error()
                    .with_message(format!(
                        "cannot construct a `{}` from a numeric literal",
                        expected_type.pretty(std::usize::MAX),
                    ))
                    .with_labels(labels![
                        primary(literal_location) = format!(
                            "numeric literals not supported for type `{}`",
                            expected_type.pretty(std::usize::MAX),
                        ),
                    ])
            }
            SurfaceToCoreMessage::AmbiguousSequenceTerm { location } => Diagnostic::error()
                .with_message("ambiguous sequence term")
                .with_labels(labels![primary(location) = "type annotation required"]),
            SurfaceToCoreMessage::AmbiguousNumericLiteral { literal_location } => {
                Diagnostic::error()
                    .with_message("ambiguous numeric literal")
                    .with_labels(labels![
                        primary(literal_location) = "type annotation required"
                    ])
            }
            SurfaceToCoreMessage::AmbiguousStructTerm { term_location } => Diagnostic::error()
                .with_message("ambiguous struct term")
                .with_labels(labels![primary(term_location) = "type annotation required"]),
            SurfaceToCoreMessage::UnsupportedPatternType {
                scrutinee_location,
                found_type,
            } => {
                let found_type = to_doc(found_type);

                Diagnostic::error()
                    .with_message(format!(
                        "unsupported pattern type: `{}`",
                        found_type.pretty(std::usize::MAX)
                    ))
                    .with_labels(labels![
                        primary(scrutinee_location) = (format!("unsupported pattern type"))
                    ])
                    .with_notes(vec![
                        format!(
                            "unsupported pattern type: `{}`",
                            found_type.pretty(std::usize::MAX)
                        ),
                        "can only currently match against terms of type `Bool` or `Int`".to_owned(),
                    ])
            }
            SurfaceToCoreMessage::NoDefaultPattern { match_location } => Diagnostic::error()
                .with_message("non-exhaustive patterns")
                .with_labels(labels![primary(match_location) = "missing default pattern"]),
            SurfaceToCoreMessage::UnreachablePattern { pattern_location } => Diagnostic::warning()
                .with_message("unreachable pattern")
                .with_labels(labels![primary(pattern_location) = "unreachable pattern"]),
            SurfaceToCoreMessage::DuplicateStructFields { duplicate_labels } => Diagnostic::error()
                .with_message("duplicate fields found in struct")
                .with_labels(
                    duplicate_labels
                        .iter()
                        .flat_map(|label| {
                            label!(primary(&label.location) = "field already defined")
                        })
                        .collect(),
                ),
            SurfaceToCoreMessage::MissingStructFields {
                term_location,
                missing_labels,
            } => Diagnostic::error()
                .with_message("missing fields for struct")
                .with_labels(
                    std::iter::empty()
                        .chain(label!(
                            primary(term_location) = format!(
                                "missing fields {}",
                                missing_labels.iter().map(|label| &label.data).format(", ")
                            ),
                        ))
                        .chain(missing_labels.iter().flat_map(|label| {
                            label!(secondary(&label.location) = "field defined on struct here")
                        }))
                        .collect(),
                ),
            SurfaceToCoreMessage::UnexpectedStructFields {
                term_location,
                unexpected_labels,
            } => Diagnostic::error()
                .with_message("unexpected fields found in struct")
                .with_labels(
                    unexpected_labels
                        .iter()
                        .flat_map(|label| label!(primary(&label.location) = "unexpected field"))
                        .chain(label!(
                            secondary(term_location) = "struct instantiated here"
                        ))
                        .collect(),
                ),
            SurfaceToCoreMessage::UnexpectedStructTerm {
                term_location,
                expected_type,
            } => {
                let expected_type = to_doc(expected_type);

                Diagnostic::error()
                    .with_message("unexpected struct term")
                    .with_labels(labels![
                        primary(term_location) = format!(
                            "expected `{}`, found struct",
                            expected_type.pretty(std::usize::MAX)
                        ),
                    ])
            }
        }
    }
}
