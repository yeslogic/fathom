mod grammar;

pub use self::grammar::parse_Definitions as parse;

#[cfg(test)]
mod tests {
    use ast::*;
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
                    "Point",
                    Type::Struct(vec![
                        Field::new("x", Type::ident("u32be")),
                        Field::new("y", Type::ident("u32be")),
                    ])
                ),
                Definition::new(
                    "Array",
                    Type::Struct(vec![
                        Field::new("len", Type::ident("u16le")),
                        Field::new(
                            "data",
                            Type::Array(
                                Box::new(Type::ident("Point")),
                                IntExpr::var("len"),
                            )
                        ),
                    ])
                ),
                Definition::new(
                    "Formats",
                    Type::Union(vec![
                        Type::Struct(vec![
                            Field::new("format", Type::ident("u16")),
                            Field::new("data", Type::ident("u16")),
                        ]),
                        Type::Struct(vec![
                            Field::new("format", Type::ident("u16")),
                            Field::new("point", Type::ident("Point")),
                        ]),
                        Type::Struct(vec![
                            Field::new("format", Type::ident("u16")),
                            Field::new("array", Type::ident("Array")),
                        ]),
                    ])
                ),
            ])
        );
    }
}
