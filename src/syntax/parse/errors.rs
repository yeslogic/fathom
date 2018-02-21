use lalrpop_util::ParseError as LalrpopError;
use codespan::{ByteIndex, ByteSpan, RawIndex};
use codespan_reporting::{Diagnostic, Label, LabelStyle, Severity};
use std::fmt;

use super::{LexerError, Token};

#[derive(Fail, Debug, Clone, PartialEq)]
pub enum ParseError {
    #[fail(display = "{}", _0)]
    Lexer(#[cause] LexerError),

    #[fail(display = "invalid constant suffix: {}", suffix)]
    ConstSuffixInvalid { span: ByteSpan, suffix: String },
    #[fail(display = "missing constant suffix")]
    ConstSuffixMissing { span: ByteSpan },
    #[fail(display = "invalid host type name: {}", name)]
    InvalidHostTypeName { span: ByteSpan, name: String },

    #[fail(display = "Unexpected EOF, expected one of: {}.", expected)]
    UnexpectedEof {
        end: ByteIndex,
        expected: ExpectedTokens,
    },
    #[fail(display = "Unexpected token {}, expected one of: {}.", token, expected)]
    UnexpectedToken {
        span: ByteSpan,
        token: Token<String>,
        expected: ExpectedTokens,
    },
    #[fail(display = "Extra token {}.", token)]
    ExtraToken {
        span: ByteSpan,
        token: Token<String>,
    },
}

/// Flatten away an LALRPOP error, leaving the inner `ParseError` behind
pub fn from_lalrpop<T>(src: &str, err: LalrpopError<ByteIndex, T, ParseError>) -> ParseError
where
    T: Into<Token<String>>,
{
    match err {
        LalrpopError::User { error } => error,
        LalrpopError::InvalidToken { .. } => unreachable!(),
        LalrpopError::UnrecognizedToken {
            token: None,
            expected,
        } => ParseError::UnexpectedEof {
            end: ByteIndex(src.len() as RawIndex),
            expected: ExpectedTokens(expected),
        },
        LalrpopError::UnrecognizedToken {
            token: Some((start, token, end)),
            expected,
        } => ParseError::UnexpectedToken {
            span: ByteSpan::new(start, end),
            token: token.into(),
            expected: ExpectedTokens(expected),
        },
        LalrpopError::ExtraToken {
            token: (start, token, end),
        } => ParseError::ExtraToken {
            span: ByteSpan::new(start, end),
            token: token.into(),
        },
    }
}

impl ParseError {
    /// Convert the error into a diagnostic message
    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            ParseError::Lexer(ref err) => err.to_diagnostic(),

            ParseError::ConstSuffixInvalid { span, ref suffix } => Diagnostic {
                severity: Severity::Error,
                message: format!("invalid constant suffix: {}", suffix),
                labels: vec![
                    Label {
                        message: Some("invalid constant suffix".into()),
                        style: LabelStyle::Primary,
                        span,
                    },
                ],
            },
            ParseError::ConstSuffixMissing { span } => Diagnostic {
                severity: Severity::Error,
                message: format!("missing constant suffix"),
                labels: vec![
                    Label {
                        message: Some("suffix expected here".into()),
                        style: LabelStyle::Primary,
                        span,
                    },
                ],
            },
            ParseError::InvalidHostTypeName { span, ref name } => Diagnostic {
                severity: Severity::Error,
                message: format!("invalid host type name: {}", name),
                labels: vec![
                    Label {
                        message: Some("invalid host type name".into()),
                        style: LabelStyle::Primary,
                        span,
                    },
                ],
            },

            ParseError::UnexpectedToken {
                span,
                ref token,
                ref expected,
            } => Diagnostic {
                severity: Severity::Error,
                message: format!("expected one of {}, found `{}`", expected, token),
                labels: vec![
                    Label {
                        message: Some("unexpected token".into()),
                        style: LabelStyle::Primary,
                        span,
                    },
                ],
            },
            ParseError::UnexpectedEof { end, ref expected } => Diagnostic {
                severity: Severity::Error,
                message: format!("expected one of {}, found `EOF`", expected),
                labels: vec![
                    Label {
                        message: Some("unexpected EOF".into()),
                        style: LabelStyle::Primary,
                        span: ByteSpan::new(end, end),
                    },
                ],
            },
            ParseError::ExtraToken { span, ref token } => Diagnostic {
                severity: Severity::Error,
                message: format!("extra token `{}`", token),
                labels: vec![
                    Label {
                        message: Some("extra token".into()),
                        style: LabelStyle::Primary,
                        span,
                    },
                ],
            },
        }
    }
}

impl From<LexerError> for ParseError {
    fn from(src: LexerError) -> ParseError {
        ParseError::Lexer(src)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpectedTokens(pub Vec<String>);

impl fmt::Display for ExpectedTokens {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, token) in self.0.iter().enumerate() {
            match i {
                0 => write!(f, "{}", token)?,
                i if i < self.0.len() - 1 => write!(f, ", {}", token)?,
                _ => write!(f, ", or {}", token)?,
            }
        }
        Ok(())
    }
}
