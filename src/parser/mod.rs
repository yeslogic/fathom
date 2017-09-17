use lalrpop_util;

use ast::{Definition, Expr, Type};
use env::Env;
use source::BytePos;

mod lexer;
#[allow(unused_extern_crates)]
mod grammar;

use self::lexer::{Lexer, Error as LexerError, Token};

pub type ParseError<'input> = lalrpop_util::ParseError<BytePos, Token<'input>, LexerError>;

pub fn parse<'input, 'env>(
    env: &'env Env,
    src: &'input str,
) -> Result<Vec<Definition>, ParseError<'input>> {
    grammar::parse_Definitions(env, Lexer::new(src))
}

pub fn parse_expr<'input, 'env>(
    env: &'env Env,
    src: &'input str,
) -> Result<Expr, ParseError<'input>> {
    grammar::parse_Expr(env, Lexer::new(src))
}

pub fn parse_ty<'input, 'env>(
    env: &'env Env,
    src: &'input str,
) -> Result<Type, ParseError<'input>> {
    grammar::parse_Type(env, Lexer::new(src))
}

#[cfg(test)]
mod tests {
    use ast::*;
    use env::Env;
    use source::BytePos as B;
    use super::*;

    #[test]
    fn parse_ty_ident() {
        let src = "
            Point
        ";

        assert_eq!(
            parse_ty(&Env::default(), src),
            Ok(Type::ident((B(13), B(18)), "Point"))
        );
    }

    #[test]
    fn parse_ty_empty_struct() {
        let src = "struct {}";

        assert_eq!(
            parse_ty(&Env::default(), src),
            Ok(Type::struct_((B(0), B(9)), vec![]))
        );
    }

    #[test]
    fn parse_simple_definition() {
        let src = "
            Offset32 = u32;
        ";

        assert_eq!(
            parse(&Env::default(), src),
            Ok(vec![
                Definition::new(
                    (B(13), B(28)),
                    "Offset32",
                    Type::const_((B(0), B(0)), TypeConst::U32)
                ),
            ])
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

        assert_eq!(
            parse(&Env::default(), src),
            Ok(vec![
                Definition::new(
                    (B(13), B(98)),
                    "Point",
                    Type::struct_(
                        (B(21), B(97)),
                        vec![
                            Field::new(
                                (B(46), B(55)),
                                "x",
                                Type::ident((B(50), B(55)), "u32be")
                            ),
                            Field::new(
                                (B(73), B(82)),
                                "y",
                                Type::ident((B(77), B(82)), "u32be")
                            ),
                        ],
                    )
                ),
                Definition::new(
                    (B(112), B(209)),
                    "Array",
                    Type::struct_(
                        (B(120), B(208)),
                        vec![
                            Field::new(
                                (B(145), B(156)),
                                "len",
                                Type::ident((B(151), B(156)), "u16le")
                            ),
                            Field::new(
                                (B(174), B(193)),
                                "data",
                                Type::array(
                                    (B(181), B(193)),
                                    Type::ident((B(182), B(187)), "Point"),
                                    Expr::var((B(189), B(192)), "len"),
                                )
                            ),
                        ],
                    )
                ),
                Definition::new(
                    (B(223), B(417)),
                    "Formats",
                    Type::union(
                        (B(233), B(416)),
                        vec![
                            Type::struct_(
                                (B(257), B(291)),
                                vec![
                                    Field::new(
                                        (B(266), B(278)),
                                        "format",
                                        Type::const_((B(0), B(0)), TypeConst::U16)
                                    ),
                                    Field::new(
                                        (B(280), B(289)),
                                        "data",
                                        Type::const_((B(0), B(0)), TypeConst::U16)
                                    ),
                                ]
                            ),
                            Type::struct_(
                                (B(309), B(346)),
                                vec![
                                    Field::new(
                                        (B(318), B(330)),
                                        "format",
                                        Type::const_((B(0), B(0)), TypeConst::U16)
                                    ),
                                    Field::new(
                                        (B(332), B(344)),
                                        "point",
                                        Type::ident((B(339), B(344)), "Point")
                                    ),
                                ]
                            ),
                            Type::struct_(
                                (B(364), B(401)),
                                vec![
                                    Field::new(
                                        (B(373), B(385)),
                                        "format",
                                        Type::const_((B(0), B(0)), TypeConst::U16)
                                    ),
                                    Field::new(
                                        (B(387), B(399)),
                                        "array",
                                        Type::ident((B(394), B(399)), "Array")
                                    ),
                                ]
                            ),
                        ],
                    )
                ),
            ])
        );
    }
}
