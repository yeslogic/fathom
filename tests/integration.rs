//! Integration tests for the data description language.

#![warn(rust_2018_idioms)]

#[path = "support/mod.rs"]
mod support;

macro_rules! test {
    ($test_name:ident, $test_path:literal) => {
        #[test]
        fn $test_name() {
            $crate::support::run_integration_test(stringify!($test_name), $test_path);
        }
    };
}

// ok
test!(empty, "empty.ddl");
// fail
test!(duplicate_definitions, "duplicate_definitions.ddl");
test!(unexpected_token, "unexpected_token.ddl");
test!(unexpected_character, "unexpected_character.ddl");

mod r#struct {
    // ok
    test!(empty, "struct/empty.ddl");
    test!(empty_doc, "struct/empty_doc.ddl");
    test!(pair, "struct/pair.ddl");
    // fail
    test!(duplicate_fields, "struct/duplicate_fields.ddl");
    test!(missing_closing_brace, "struct/missing_closing_brace.ddl");
    test!(missing_fields, "struct/missing_fields.ddl");
    test!(missing_name, "struct/missing_name.ddl");
    test!(undefined_field, "struct/undefined_field.ddl");
}
