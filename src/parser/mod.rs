use lalrpop_util;

use syntax::{binary, host, Definition};
use source::BytePos;

mod lexer;
#[allow(unused_extern_crates)]
mod grammar;

use self::lexer::{Error as LexerError, Lexer, Token};

pub type ParseError<'input> = lalrpop_util::ParseError<BytePos, Token<'input>, LexerError>;

pub fn parse<'input>(
    src: &'input str,
) -> Result<
    Vec<Definition<String, binary::SpannedType<String, host::SpannedExpr<String>>>>,
    ParseError<'input>,
> {
    grammar::parse_Definitions(Lexer::new(src))
}

pub fn parse_expr<'input>(
    src: &'input str,
) -> Result<host::SpannedExpr<String>, ParseError<'input>> {
    grammar::parse_Expr(Lexer::new(src))
}

pub fn parse_ty<'input>(
    src: &'input str,
) -> Result<binary::SpannedType<String, host::SpannedExpr<String>>, ParseError<'input>> {
    grammar::parse_Type(Lexer::new(src))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_expr_bool_atomic() {
        let src = "
            !((true | (false)))
        ";

        assert_snapshot!(parse_expr_bool_atomic, parse_expr(src).unwrap());
    }

    #[test]
    fn parse_expr_bool_operators() {
        let src = "
            (true & false) | (true | false)
        ";

        assert_snapshot!(parse_expr_bool_operators, parse_expr(src).unwrap());
    }

    #[test]
    fn parse_add_expr() {
        let src = "x + y + z";

        assert_snapshot!(parse_add_expr, parse_expr(src).unwrap());
    }

    #[test]
    fn parse_sub_expr() {
        let src = "x - y - z";

        assert_snapshot!(parse_sub_expr, parse_expr(src).unwrap());
    }

    #[test]
    fn parse_add_expr_mixed() {
        let src = "x + y + z - z + x";

        assert_snapshot!(parse_add_expr_mixed, parse_expr(src).unwrap());
    }

    #[test]
    fn parse_mul_expr() {
        let src = "x * y * z";

        assert_snapshot!(parse_mul_expr, parse_expr(src).unwrap());
    }

    #[test]
    fn parse_div_expr() {
        let src = "x / y / z";

        assert_snapshot!(parse_div_expr, parse_expr(src).unwrap());
    }

    #[test]
    fn parse_mul_expr_mixed() {
        let src = "x * y * z / z * x";

        assert_snapshot!(parse_mul_expr_mixed, parse_expr(src).unwrap());
    }

    #[test]
    fn parse_mixed_arithmetic_expr() {
        let src = "x + y * z / z - x * a";

        assert_snapshot!(parse_mixed_arithmetic_expr, parse_expr(src).unwrap());
    }

    #[test]
    fn parse_mixed_arithmetic_expr_parenthesized() {
        let src = "(x + y) * z / (z - x) * a";

        assert_snapshot!(
            parse_mixed_arithmetic_expr_parenthesized,
            parse_expr(src).unwrap()
        );
    }

    #[test]
    fn parse_ty_var() {
        let src = "
            Point
        ";

        assert_snapshot!(parse_ty_var, parse_ty(src).unwrap());
    }

    #[test]
    fn parse_ty_empty_struct() {
        let src = "struct {}";

        assert_snapshot!(parse_ty_empty_struct, parse_ty(src).unwrap());
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

        assert_snapshot!(parse_ty_where, parse_ty(src).unwrap());
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

        assert_snapshot!(parse_ty_array_dependent, parse_ty(src).unwrap());
    }

    #[test]
    fn parse_simple_definition() {
        let src = "
            Offset32 = u32;
        ";

        assert_snapshot!(parse_simple_definition, parse(src).unwrap());
    }

    #[test]
    fn parse_array_with_constant_size() {
        let src = "
            Point = [f32; 3];
        ";

        assert_snapshot!(parse_array_with_constant_size, parse(src).unwrap());
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

        assert_snapshot!(parse_definition, parse(src).unwrap());
    }
}
