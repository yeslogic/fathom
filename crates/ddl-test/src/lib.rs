//! Integration tests for the data description language.

#![cfg(test)]
#![warn(rust_2018_idioms)]

mod support;

macro_rules! test {
    ($test_name:ident, $test_path:literal) => {
        #[test]
        fn $test_name() {
            $crate::support::run_test(stringify!($test_name), $test_path);
        }
    };
}

test!(duplicate_definitions, "duplicate_definitions.ddl");
test!(empty, "empty.ddl");
test!(unexpected_token, "unexpected_token.ddl");
test!(unexpected_character, "unexpected_character.ddl");

mod struct_ {
    test!(empty, "struct/empty.ddl");
    test!(empty_doc, "struct/empty_doc.ddl");
    test!(missing_closing_brace, "struct/missing_closing_brace.ddl");
    test!(missing_fields, "struct/missing_fields.ddl");
    test!(missing_name, "struct/missing_name.ddl");
}
