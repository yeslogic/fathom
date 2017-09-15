mod grammar;

pub use self::grammar::parse_Definitions as parse;
pub use self::grammar::parse_Expr as parse_expr;
pub use self::grammar::parse_Type as parse_ty;

#[cfg(test)]
mod tests {
    use ast::*;
    use env::Env;
    use source::BytePos as B;
    use super::*;

    #[test]
    fn parse_definition() {
        let src = "
            Point = {
                x : u32be,
                y : u32be,
            };

            Array = {
                len : u16le,
                data : [Point; len],
            };

            Formats =
                | { format : u16, data: u16 }
                | { format : u16, point: Point }
                | { format : u16, array: Array };
        ";

        assert_eq!(
            parse(&Env::default(), src),
            Ok(vec![
                Definition::new(
                    (B(13), B(91)),
                    "Point",
                    Type::struct_(
                        (B(21), B(90)),
                        vec![
                            Field::new(
                                (B(39), B(48)),
                                "x",
                                Type::ident((B(43), B(48)), "u32be")
                            ),
                            Field::new(
                                (B(66), B(75)),
                                "y",
                                Type::ident((B(70), B(75)), "u32be")
                            ),
                        ],
                    )
                ),
                Definition::new(
                    (B(105), B(195)),
                    "Array",
                    Type::struct_(
                        (B(113), B(194)),
                        vec![
                            Field::new(
                                (B(131), B(142)),
                                "len",
                                Type::ident((B(137), B(142)), "u16le")
                            ),
                            Field::new(
                                (B(160), B(179)),
                                "data",
                                Type::array(
                                    (B(167), B(179)),
                                    Type::ident((B(168), B(173)), "Point"),
                                    Expr::var((B(175), B(178)), "len"),
                                )
                            ),
                        ],
                    )
                ),
                Definition::new(
                    (B(209), B(363)),
                    "Formats",
                    Type::union(
                        (B(235), B(362)),
                        vec![
                            Type::struct_(
                                (B(237), B(264)),
                                vec![
                                    Field::new(
                                        (B(239), B(251)),
                                        "format",
                                        Type::const_((B(0), B(0)), TypeConst::U16)
                                    ),
                                    Field::new(
                                        (B(253), B(262)),
                                        "data",
                                        Type::const_((B(0), B(0)), TypeConst::U16)
                                    ),
                                ]
                            ),
                            Type::struct_(
                                (B(283), B(313)),
                                vec![
                                    Field::new(
                                        (B(285), B(297)),
                                        "format",
                                        Type::const_((B(0), B(0)), TypeConst::U16)
                                    ),
                                    Field::new(
                                        (B(299), B(311)),
                                        "point",
                                        Type::ident((B(306), B(311)), "Point")
                                    ),
                                ]
                            ),
                            Type::struct_(
                                (B(332), B(362)),
                                vec![
                                    Field::new(
                                        (B(334), B(346)),
                                        "format",
                                        Type::const_((B(0), B(0)), TypeConst::U16)
                                    ),
                                    Field::new(
                                        (B(348), B(360)),
                                        "array",
                                        Type::ident((B(355), B(360)), "Array")
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
