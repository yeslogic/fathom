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
            ])
        );
    }
}
