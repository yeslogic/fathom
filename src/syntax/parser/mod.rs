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

fn parse_int_suffix(src: &str) -> Result<host::IntSuffix, GrammarError> {
    use syntax::ast::host::{IntSuffix, SignedType, UnsignedType};

    match src {
        "i8" => Ok(IntSuffix::Signed(SignedType::I8)),
        "i16" => Ok(IntSuffix::Signed(SignedType::I16)),
        "i24" => Ok(IntSuffix::Signed(SignedType::I24)),
        "i32" => Ok(IntSuffix::Signed(SignedType::I32)),
        "i64" => Ok(IntSuffix::Signed(SignedType::I64)),
        "u8" => Ok(IntSuffix::Unsigned(UnsignedType::U8)),
        "u16" => Ok(IntSuffix::Unsigned(UnsignedType::U16)),
        "u24" => Ok(IntSuffix::Unsigned(UnsignedType::U24)),
        "u32" => Ok(IntSuffix::Unsigned(UnsignedType::U32)),
        "u64" => Ok(IntSuffix::Unsigned(UnsignedType::U64)),
        "" => Err(GrammarError::ConstSuffixMissing),
        _ => Err(GrammarError::ConstSuffixInvalid {
            suffix: src.to_owned(),
        }),
    }
}

fn parse_float_suffix(src: &str) -> Result<host::FloatType, GrammarError> {
    use syntax::ast::host::FloatType;

    match src {
        "f32" => Ok(FloatType::F32),
        "f64" => Ok(FloatType::F64),
        "" => Err(GrammarError::ConstSuffixMissing),
        _ => Err(GrammarError::ConstSuffixInvalid {
            suffix: src.to_owned(),
        }),
    }
}

fn parse_ty_const(src: &str) -> Result<host::TypeConst, GrammarError> {
    use syntax::ast::host::{FloatType, SignedType, TypeConst, UnsignedType};

    match src {
        "unit" => Ok(TypeConst::Unit),
        "bool" => Ok(TypeConst::Bool),
        "f32" => Ok(TypeConst::Float(FloatType::F32)),
        "f64" => Ok(TypeConst::Float(FloatType::F64)),
        "i8" => Ok(TypeConst::Signed(SignedType::I8)),
        "i16" => Ok(TypeConst::Signed(SignedType::I16)),
        "i24" => Ok(TypeConst::Signed(SignedType::I24)),
        "i32" => Ok(TypeConst::Signed(SignedType::I32)),
        "i64" => Ok(TypeConst::Signed(SignedType::I64)),
        "u8" => Ok(TypeConst::Unsigned(UnsignedType::U8)),
        "u16" => Ok(TypeConst::Unsigned(UnsignedType::U16)),
        "u24" => Ok(TypeConst::Unsigned(UnsignedType::U24)),
        "u32" => Ok(TypeConst::Unsigned(UnsignedType::U32)),
        "u64" => Ok(TypeConst::Unsigned(UnsignedType::U64)),
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

impl FromStr for Program<String> {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<Program<String>, ParseError> {
        grammar::parse_Program(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map_err(from_lalrpop_err)
    }
}

impl FromStr for host::Expr<String> {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<host::Expr<String>, ParseError> {
        grammar::parse_HostExpr(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map(|expr| Rc::try_unwrap(expr).unwrap())
            .map_err(from_lalrpop_err)
    }
}

impl FromStr for host::Type<String> {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<host::Type<String>, ParseError> {
        grammar::parse_HostType(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map(|ty| Rc::try_unwrap(ty).unwrap())
            .map_err(from_lalrpop_err)
    }
}

impl FromStr for binary::Type<String> {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<binary::Type<String>, ParseError> {
        grammar::parse_BinaryType(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map(|ty| Rc::try_unwrap(ty).unwrap())
            .map_err(from_lalrpop_err)
    }
}
