use lalrpop_util::ParseError as LalrpopError;
use codespan::{ByteIndex, ByteSpan, FileMap};
use codespan_reporting::{Diagnostic, Label};
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
pub fn from_lalrpop<T>(filemap: &FileMap, err: LalrpopError<ByteIndex, T, ParseError>) -> ParseError
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
            end: filemap.span().end(),
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

            ParseError::ConstSuffixInvalid { span, ref suffix } => {
                Diagnostic::new_error(format!("invalid constant suffix: {}", suffix))
                    .with_label(Label::new_primary(span).with_message("invalid constant suffix"))
            }
            ParseError::ConstSuffixMissing { span } => {
                Diagnostic::new_error(format!("missing constant suffix"))
                    .with_label(Label::new_primary(span).with_message("suffix expected here"))
            }
            ParseError::InvalidHostTypeName { span, ref name } => {
                Diagnostic::new_error(format!("invalid host type name: {}", name))
                    .with_label(Label::new_primary(span).with_message("invalid host type name"))
            }

            ParseError::UnexpectedToken {
                span,
                ref token,
                ref expected,
            } => Diagnostic::new_error(format!("expected one of {}, found `{}`", expected, token))
                .with_label(Label::new_primary(span).with_message("unexpected token")),
            ParseError::UnexpectedEof { end, ref expected } => {
                Diagnostic::new_error(format!("expected one of {}, found `EOF`", expected))
                    .with_label(
                        Label::new_primary(ByteSpan::new(end, end)).with_message("unexpected EOF"),
                    )
            }
            ParseError::ExtraToken { span, ref token } => {
                Diagnostic::new_error(format!("extra token `{}`", token))
                    .with_label(Label::new_primary(span).with_message("extra token"))
            }
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
