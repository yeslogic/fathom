use lalrpop_util;

use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

use syntax::ast::{binary, host, Program};
use source::BytePos;

mod lexer;
mod grammar;

#[cfg(test)]
mod tests;

use self::lexer::Lexer;

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

fn parse_ty_const(src: &str) -> Result<host::TypeConst, GrammarError> {
    use syntax::ast::host::{FloatType, IntType, TypeConst};

    match src {
        "unit" => Ok(TypeConst::Unit),
        "bottom" => Ok(TypeConst::Bottom),
        "bool" => Ok(TypeConst::Bool),
        "f32" => Ok(TypeConst::Float(FloatType::F32)),
        "f64" => Ok(TypeConst::Float(FloatType::F64)),
        "i8" => Ok(TypeConst::Int(IntType::i8())),
        "i16" => Ok(TypeConst::Int(IntType::i16())),
        "i32" => Ok(TypeConst::Int(IntType::i32())),
        "i64" => Ok(TypeConst::Int(IntType::i64())),
        "u8" => Ok(TypeConst::Int(IntType::u8())),
        "u16" => Ok(TypeConst::Int(IntType::u16())),
        "u32" => Ok(TypeConst::Int(IntType::u32())),
        "u64" => Ok(TypeConst::Int(IntType::u64())),
        _ => Err(GrammarError::InvalidHostTypeName {
            name: src.to_owned(),
        }),
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

impl FromStr for Program {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<Program, ParseError> {
        grammar::parse_Program(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map_err(from_lalrpop_err)
    }
}

impl FromStr for host::Expr {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<host::Expr, ParseError> {
        grammar::parse_HostExpr(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map(|expr| Rc::try_unwrap(expr).unwrap())
            .map_err(from_lalrpop_err)
    }
}

impl FromStr for host::Type {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<host::Type, ParseError> {
        grammar::parse_HostType(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map(|ty| Rc::try_unwrap(ty).unwrap())
            .map_err(from_lalrpop_err)
    }
}

impl FromStr for binary::Type {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<binary::Type, ParseError> {
        grammar::parse_BinaryType(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map(|ty| Rc::try_unwrap(ty).unwrap())
            .map_err(from_lalrpop_err)
    }
}
