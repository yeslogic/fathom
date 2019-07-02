//! Integration tests for the data description language.

#![warn(rust_2018_idioms)]
#![cfg(test)]

mod support;

macro_rules! test {
    ($test_name:ident, $test_path:literal) => {
        #[test]
        fn $test_name() {
            $crate::support::run_integration_test(stringify!($test_name), $test_path);
        }
    };
}

test!(pass_empty, "pass_empty.ddl");

test!(fail_duplicate_definitions, "fail_duplicate_definitions.ddl");
test!(fail_unexpected_token, "fail_unexpected_token.ddl");
test!(fail_unexpected_character, "fail_unexpected_character.ddl");

#[rustfmt::skip]
mod r#struct {
    test!(pass_empty, "struct/pass_empty.ddl");
    test!(pass_empty_doc, "struct/pass_empty_doc.ddl");
    test!(pass_pair, "struct/pass_pair.ddl");

    test!(fail_duplicate_fields, "struct/fail_duplicate_fields.ddl");
    test!(fail_missing_closing_brace, "struct/fail_missing_closing_brace.ddl");
    test!(fail_missing_fields, "struct/fail_missing_fields.ddl");
    test!(fail_missing_name, "struct/fail_missing_name.ddl");
    test!(fail_undefined_field, "struct/fail_undefined_field.ddl");
}
