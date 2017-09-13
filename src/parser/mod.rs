mod grammar;

pub use self::grammar::parse_Definitions as parse;

#[cfg(test)]
mod tests {
    use ast::*;
    use source::BytePos as Bp;
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
            parse(src),
            Ok(vec![
                Definition::new(
                    (Bp(13), Bp(91)),
                    "Point",
                    Type::struct_(
                        (Bp(21), Bp(90)),
                        vec![
                            Field::new(
                                (Bp(39), Bp(48)),
                                "x",
                                Type::ident((Bp(43), Bp(48)), "u32be")
                            ),
                            Field::new(
                                (Bp(66), Bp(75)),
                                "y",
                                Type::ident((Bp(70), Bp(75)), "u32be")
                            ),
                        ],
                    )
                ),
                Definition::new(
                    (Bp(105), Bp(195)),
                    "Array",
                    Type::struct_(
                        (Bp(113), Bp(194)),
                        vec![
                            Field::new(
                                (Bp(131), Bp(142)),
                                "len",
                                Type::ident((Bp(137), Bp(142)), "u16le")
                            ),
                            Field::new(
                                (Bp(160), Bp(179)),
                                "data",
                                Type::array(
                                    (Bp(167), Bp(179)),
                                    Type::ident((Bp(168), Bp(173)), "Point"),
                                    IntExpr::var((Bp(175), Bp(178)), "len"),
                                )
                            ),
                        ],
                    )
                ),
                Definition::new(
                    (Bp(209), Bp(363)),
                    "Formats",
                    Type::union(
                        (Bp(235), Bp(362)),
                        vec![
                            Type::struct_(
                                (Bp(237), Bp(264)),
                                vec![
                                    Field::new(
                                        (Bp(239), Bp(251)),
                                        "format",
                                        Type::ident((Bp(248), Bp(251)), "u16")
                                    ),
                                    Field::new(
                                        (Bp(253), Bp(262)),
                                        "data",
                                        Type::ident((Bp(259), Bp(262)), "u16")
                                    ),
                                ]
                            ),
                            Type::struct_(
                                (Bp(283), Bp(313)),
                                vec![
                                    Field::new(
                                        (Bp(285), Bp(297)),
                                        "format",
                                        Type::ident((Bp(294), Bp(297)), "u16")
                                    ),
                                    Field::new(
                                        (Bp(299), Bp(311)),
                                        "point",
                                        Type::ident((Bp(306), Bp(311)), "Point")
                                    ),
                                ]
                            ),
                            Type::struct_(
                                (Bp(332), Bp(362)),
                                vec![
                                    Field::new(
                                        (Bp(334), Bp(346)),
                                        "format",
                                        Type::ident((Bp(343), Bp(346)), "u16")
                                    ),
                                    Field::new(
                                        (Bp(348), Bp(360)),
                                        "array",
                                        Type::ident((Bp(355), Bp(360)), "Array")
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
