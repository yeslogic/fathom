//! Integration tests for Fathom.

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

test!(pass_empty, "pass_empty.fathom");
test!(pass_empty_doc, "pass_empty_doc.fathom");

test!(
    fail_duplicate_definitions,
    "fail_duplicate_definitions.fathom"
);
test!(fail_unexpected_token, "fail_unexpected_token.fathom");
test!(fail_invalid_token, "fail_invalid_token.fathom");

#[rustfmt::skip]
mod alias {
    test!(pass_ann, "alias/pass_ann.fathom");
    test!(pass_format_array, "alias/pass_format_array.fathom");
    test!(pass_alias_term, "alias/pass_alias_term.fathom");
    test!(pass_alias_type, "alias/pass_alias_type.fathom");
    test!(pass_ann_ann, "alias/pass_ann_ann.fathom");
    test!(pass_ann_sugar, "alias/pass_ann_sugar.fathom");
    test!(pass_globals, "alias/pass_globals.fathom");
    test!(pass_if_else_ann_type, "alias/pass_if_else_ann_type.fathom");
    test!(pass_if_else_format_type, "alias/pass_if_else_format_type.fathom");
    test!(pass_if_else_format_type_item, "alias/pass_if_else_format_type_item.fathom");
    test!(pass_if_else_host_type, "alias/pass_if_else_host_type.fathom");
    test!(pass_if_else_host_type_item, "alias/pass_if_else_host_type_item.fathom");
    test!(pass_if_else_if_else_format_type, "alias/pass_if_else_if_else_format_type.fathom");
    test!(pass_if_else_term, "alias/pass_if_else_term.fathom");
    test!(pass_if_else_term_item, "alias/pass_if_else_term_item.fathom");
    test!(pass_literals, "alias/pass_literals.fathom");
    test!(pass_match_int_ann_type, "alias/pass_match_int_ann_type.fathom");
    test!(pass_match_int_format_type, "alias/pass_match_int_format_type.fathom");
    test!(pass_match_int_format_type_item, "alias/pass_match_int_format_type_item.fathom");
    test!(pass_match_int_host_type, "alias/pass_match_int_host_type.fathom");
    test!(pass_match_int_host_type_item, "alias/pass_match_int_host_type_item.fathom");
    test!(pass_match_int_term, "alias/pass_match_int_term.fathom");
    test!(pass_match_int_term_item, "alias/pass_match_int_term_item.fathom");
    test!(pass_match_int_term_unreachable, "alias/pass_match_int_term_unreachable.fathom");
    test!(pass_simple, "alias/pass_simple.fathom");
    test!(pass_simple_doc, "alias/pass_simple_doc.fathom");

    test!(fail_ann_mismatch, "alias/fail_ann_mismatch.fathom");
    test!(fail_format_array_bad_elem, "alias/fail_format_array_bad_elem.fathom");
    test!(fail_format_array_bad_len, "alias/fail_format_array_bad_len.fathom");
    test!(fail_globals, "alias/fail_globals.fathom");
    test!(fail_literals, "alias/fail_literals.fathom");
    test!(fail_type_has_no_type, "alias/fail_type_has_no_type.fathom");
    test!(fail_if_else_term_mismatched_arms, "alias/fail_if_else_term_mismatched_arms.fathom");
    test!(fail_if_else_term_mismatched_condition, "alias/fail_if_else_term_mismatched_condition.fathom");
    test!(fail_match_ambiguous_scrutinee, "alias/fail_match_ambiguous_scrutinee.fathom");
    test!(fail_match_int_missing_default, "alias/fail_match_int_missing_default.fathom");
    test!(fail_match_int_ambiguous, "alias/fail_match_int_ambiguous.fathom");
    test!(fail_match_int_term_mismatched_arms, "alias/fail_match_int_term_mismatched_arms.fathom");
    test!(fail_numeric_literal_ambiguous, "alias/fail_numeric_literal_ambiguous.fathom");
    test!(fail_numeric_literal_not_supported, "alias/fail_numeric_literal_not_supported.fathom");
    test!(fail_unconstrained_int_type, "alias/fail_unconstrained_int_type.fathom");
}

#[rustfmt::skip]
mod r#struct {
    test!(pass_empty, "struct/pass_empty.fathom");
    test!(pass_empty_doc, "struct/pass_empty_doc.fathom");
    test!(pass_if_else_type_item, "struct/pass_if_else_type_item.fathom");
    test!(pass_if_else_type_item_item, "struct/pass_if_else_type_item_item.fathom");
    test!(pass_mixed_format_host, "struct/pass_mixed_format_host.fathom");
    test!(pass_pair, "struct/pass_pair.fathom");
    test!(pass_singleton, "struct/pass_singleton.fathom");
    test!(pass_var, "struct/pass_var.fathom");

    test!(fail_duplicate_fields, "struct/fail_duplicate_fields.fathom");
    test!(fail_field_type_mismatch, "struct/fail_field_type_mismatch.fathom");
    test!(fail_missing_closing_brace, "struct/fail_missing_closing_brace.fathom");
    test!(fail_missing_fields, "struct/fail_missing_fields.fathom");
    test!(fail_missing_name, "struct/fail_missing_name.fathom");
    test!(fail_undefined_field, "struct/fail_undefined_field.fathom");
}
