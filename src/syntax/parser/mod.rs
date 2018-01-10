use lalrpop_util;

use std::fmt;

use source::BytePos;

pub mod ast;
mod lexer;
mod grammar;

#[cfg(test)]
mod tests;

pub type ParseError = lalrpop_util::ParseError<BytePos, String, GrammarError>;

#[derive(Debug, Fail, Clone, Eq, PartialEq)]
pub enum GrammarError {
    #[fail(display = "{}", _0)] Lexer(lexer::Error),
    #[fail(display = "invalid constant suffix: {}", suffix)] ConstSuffixInvalid { suffix: String },
    #[fail(display = "missing constant suffix")] ConstSuffixMissing,
    #[fail(display = "invalid host type name: {}", name)] InvalidHostTypeName { name: String },
}

impl From<lexer::Error> for GrammarError {
    fn from(src: lexer::Error) -> GrammarError {
        GrammarError::Lexer(src)
    }
}

fn from_lalrpop_err<L, T: fmt::Debug, E>(
    src: lalrpop_util::ParseError<L, T, E>,
) -> lalrpop_util::ParseError<L, String, E> {
    use lalrpop_util::ParseError::*;

    match src {
        InvalidToken { location } => InvalidToken { location },
        UnrecognizedToken { token, expected } => UnrecognizedToken {
            token: token.map(|(lo, token, hi)| (lo, format!("{:?}", token), hi)),
            expected,
        },
        ExtraToken {
            token: (lo, token, hi),
        } => ExtraToken {
            token: (lo, format!("{:?}", token), hi),
        },
        User { error } => User { error },
    }
}
