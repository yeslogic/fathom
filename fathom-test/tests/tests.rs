macro_rules! test {
    ($test_name:ident, $test_path:literal) => {
        #[test]
        fn $test_name() {
            fathom_test::run_integration_test(stringify!($test_name), $test_path);
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
mod constant {
    test!(sequence_term, "constant/sequence_term.fathom");
    test!(repr, "constant/repr.fathom");

    test!(pass_ann, "constant/pass_ann.fathom");
    test!(pass_constant_term, "constant/pass_constant_term.fathom");
    test!(pass_constant_type, "constant/pass_constant_type.fathom");
    test!(pass_ann_ann, "constant/pass_ann_ann.fathom");
    test!(pass_ann_sugar, "constant/pass_ann_sugar.fathom");
    test!(pass_format_array, "constant/pass_format_array.fathom");
    test!(pass_function_types, "constant/pass_function_types.fathom");
    test!(pass_globals, "constant/pass_globals.fathom");
    test!(pass_if_else_ann_type, "constant/pass_if_else_ann_type.fathom");
    test!(pass_if_else_format_type, "constant/pass_if_else_format_type.fathom");
    test!(pass_if_else_format_type_item, "constant/pass_if_else_format_type_item.fathom");
    test!(pass_if_else_host_type, "constant/pass_if_else_host_type.fathom");
    test!(pass_if_else_host_type_item, "constant/pass_if_else_host_type_item.fathom");
    test!(pass_if_else_if_else_format_type, "constant/pass_if_else_if_else_format_type.fathom");
    test!(pass_if_else_term, "constant/pass_if_else_term.fathom");
    test!(pass_if_else_term_item, "constant/pass_if_else_term_item.fathom");
    test!(pass_literals, "constant/pass_literals.fathom");
    test!(pass_match_int_ann_type, "constant/pass_match_int_ann_type.fathom");
    test!(pass_match_int_format_type, "constant/pass_match_int_format_type.fathom");
    test!(pass_match_int_format_type_item, "constant/pass_match_int_format_type_item.fathom");
    test!(pass_match_int_host_type, "constant/pass_match_int_host_type.fathom");
    test!(pass_match_int_host_type_item, "constant/pass_match_int_host_type_item.fathom");
    test!(pass_match_int_term, "constant/pass_match_int_term.fathom");
    test!(pass_match_int_term_item, "constant/pass_match_int_term_item.fathom");
    test!(pass_match_int_term_unreachable, "constant/pass_match_int_term_unreachable.fathom");
    test!(pass_simple, "constant/pass_simple.fathom");
    test!(pass_simple_doc, "constant/pass_simple_doc.fathom");

    test!(fail_ann_mismatch, "constant/fail_ann_mismatch.fathom");
    test!(fail_format_array_bad_elem, "constant/fail_format_array_bad_elem.fathom");
    test!(fail_format_array_bad_len, "constant/fail_format_array_bad_len.fathom");
    test!(fail_literals, "constant/fail_literals.fathom");
    test!(fail_if_else_term_mismatched_arms, "constant/fail_if_else_term_mismatched_arms.fathom");
    test!(fail_if_else_term_mismatched_condition, "constant/fail_if_else_term_mismatched_condition.fathom");
    test!(fail_kind_has_no_type, "constant/fail_kind_has_no_type.fathom");
    test!(fail_match_ambiguous_scrutinee, "constant/fail_match_ambiguous_scrutinee.fathom");
    test!(fail_match_int_missing_default, "constant/fail_match_int_missing_default.fathom");
    test!(fail_match_int_ambiguous, "constant/fail_match_int_ambiguous.fathom");
    test!(fail_match_int_term_mismatched_arms, "constant/fail_match_int_term_mismatched_arms.fathom");
    test!(fail_numeric_literal_ambiguous, "constant/fail_numeric_literal_ambiguous.fathom");
    test!(fail_numeric_literal_not_supported, "constant/fail_numeric_literal_not_supported.fathom");
}

#[rustfmt::skip]
mod r#struct {
    test!(parameters, "struct/parameters.fathom");
    test!(positions, "struct/positions.fathom");
    test!(struct_term, "struct/struct_term.fathom");
    test!(struct_elim, "struct/struct_elim.fathom");
    test!(struct_compute, "struct/struct_compute.fathom");
    test!(dependent_fields, "struct/dependent_fields.fathom");

    test!(pass_empty, "struct/pass_empty.fathom");
    test!(pass_empty_doc, "struct/pass_empty_doc.fathom");
    test!(pass_pair, "struct/pass_pair.fathom");
    test!(pass_singleton, "struct/pass_singleton.fathom");
    test!(pass_var, "struct/pass_var.fathom");

    test!(fail_duplicate_fields, "struct/fail_duplicate_fields.fathom");
    test!(fail_field_type_mismatch, "struct/fail_field_type_mismatch.fathom");
    test!(fail_invalid_ann, "struct/fail_invalid_ann.fathom");
    test!(fail_missing_ann, "struct/fail_missing_ann.fathom");
    test!(fail_missing_closing_brace, "struct/fail_missing_closing_brace.fathom");
    test!(fail_missing_fields, "struct/fail_missing_fields.fathom");
    test!(fail_missing_name, "struct/fail_missing_name.fathom");
    test!(fail_undefined_field, "struct/fail_undefined_field.fathom");
}
