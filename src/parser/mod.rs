use lalrpop_util;

use std::fmt;
use std::str::FromStr;

use syntax::{binary, host, Program};
use source::BytePos;

mod lexer;
#[allow(unused_extern_crates)]
mod grammar;

use self::lexer::{Error as LexerError, Lexer};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GrammarError<N> {
    Repr(binary::ReprError<N>),
    Lexer(LexerError),
}

impl<N> From<binary::ReprError<N>> for GrammarError<N> {
    fn from(src: binary::ReprError<N>) -> GrammarError<N> {
        GrammarError::Repr(src)
    }
}

impl<N> From<LexerError> for GrammarError<N> {
    fn from(src: LexerError) -> GrammarError<N> {
        GrammarError::Lexer(src)
    }
}

pub type ParseError = lalrpop_util::ParseError<BytePos, String, GrammarError<String>>;

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
        grammar::parse_Expr(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map(|expr| *expr)
            .map_err(from_lalrpop_err)
    }
}

impl FromStr for binary::Type<String> {
    type Err = ParseError;

    fn from_str(src: &str) -> Result<binary::Type<String>, ParseError> {
        grammar::parse_Type(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map(|ty| *ty)
            .map_err(from_lalrpop_err)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_expr_bool_atomic() {
        let src = "
            !((true | (false)))
        ";

        assert_snapshot!(parse_expr_bool_atomic, host::Expr::from_str(src).unwrap());
    }

    #[test]
    fn parse_expr_bool_operators() {
        let src = "
            (true & false) | (true | false)
        ";

        assert_snapshot!(
            parse_expr_bool_operators,
            host::Expr::from_str(src).unwrap()
        );
    }

    #[test]
    fn parse_add_expr() {
        let src = "x + y + z";

        assert_snapshot!(parse_add_expr, host::Expr::from_str(src).unwrap());
    }

    #[test]
    fn parse_sub_expr() {
        let src = "x - y - z";

        assert_snapshot!(parse_sub_expr, host::Expr::from_str(src).unwrap());
    }

    #[test]
    fn parse_add_expr_mixed() {
        let src = "x + y + z - z + x";

        assert_snapshot!(parse_add_expr_mixed, host::Expr::from_str(src).unwrap());
    }

    #[test]
    fn parse_mul_expr() {
        let src = "x * y * z";

        assert_snapshot!(parse_mul_expr, host::Expr::from_str(src).unwrap());
    }

    #[test]
    fn parse_div_expr() {
        let src = "x / y / z";

        assert_snapshot!(parse_div_expr, host::Expr::from_str(src).unwrap());
    }

    #[test]
    fn parse_mul_expr_mixed() {
        let src = "x * y * z / z * x";

        assert_snapshot!(parse_mul_expr_mixed, host::Expr::from_str(src).unwrap());
    }

    #[test]
    fn parse_mixed_arithmetic_expr() {
        let src = "x + y * z / z - x * a";

        assert_snapshot!(
            parse_mixed_arithmetic_expr,
            host::Expr::from_str(src).unwrap()
        );
    }

    #[test]
    fn parse_mixed_arithmetic_expr_parenthesized() {
        let src = "(x + y) * z / (z - x) * a";

        assert_snapshot!(
            parse_mixed_arithmetic_expr_parenthesized,
            host::Expr::from_str(src).unwrap()
        );
    }

    #[test]
    fn parse_proj_expr() {
        let src = "-foo.bar.x";

        assert_snapshot!(parse_proj_expr, host::Expr::from_str(src).unwrap());
    }

    #[test]
    fn parse_subscript_expr() {
        let src = "-foo[23 + (2 + 3)][index]";

        assert_snapshot!(parse_subscript_expr, host::Expr::from_str(src).unwrap());
    }

    #[test]
    fn parse_ty_var() {
        let src = "
            Point
        ";

        assert_snapshot!(parse_ty_var, binary::Type::from_str(src).unwrap());
    }

    #[test]
    fn parse_ty_empty_struct() {
        let src = "struct {}";

        assert_snapshot!(parse_ty_empty_struct, binary::Type::from_str(src).unwrap());
    }

    #[test]
    fn parse_ty_where() {
        let src = "
            struct {
                x: u32 where x => x == 3,
            }
            where x => x == 2
            where x => x == 1
        ";

        assert_snapshot!(parse_ty_where, binary::Type::from_str(src).unwrap());
    }

    #[test]
    fn parse_ty_array_dependent() {
        let src = "
            struct {
                len: u32,
                data1: [f32; len],
                data2: struct {
                    data1: [f32; len],
                    len: u32,
                    padding1: u8,
                    padding2: u8,
                    data2: [f32; len],
                },
            }
        ";

        assert_snapshot!(
            parse_ty_array_dependent,
            binary::Type::from_str(src).unwrap()
        );
    }

    #[test]
    fn parse_simple_definition() {
        let src = "
            Offset32 = u32;
        ";

        assert_snapshot!(parse_simple_definition, Program::from_str(src).unwrap());
    }

    #[test]
    fn parse_array_with_constant_size() {
        let src = "
            Point = [f32; 3];
        ";

        assert_snapshot!(
            parse_array_with_constant_size,
            Program::from_str(src).unwrap()
        );
    }

    #[test]
    fn parse_definition() {
        let src = "
            Point = struct {
                x : u32be,
                y : u32be,
            };

            Array = struct {
                len : u16le,
                data : [Point; len],
            };

            Formats = union {
                struct { format : u16, data: u16 },
                struct { format : u16, point: Point },
                struct { format : u16, array: Array },
            };
        ";

        assert_snapshot!(parse_definition, Program::from_str(src).unwrap());
    }
}
