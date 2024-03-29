use codespan::CodeMap;

use ddl::semantics::{self, Context, TypeError};
use ddl::syntax::translation::{Desugar, DesugarEnv};

mod support;

#[test]
fn range_full() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"int {..}";
    let given_expr = r#"5"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn range_from() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"int {0 ..}";
    let given_expr = r#"0"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn range_to() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"int {.. 10}";
    let given_expr = r#"10"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn range_from_to() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"int {0 .. 10}";
    let given_expr = r#"0"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);

    let expected_ty = r"int {0 .. 10}";
    let given_expr = r#"10"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn match_expr() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"String";
    let given_expr = r#"match "helloo" : String {
        "hi" => "haha",
        "hello" => "byee",
        greeting => (extern "string-append" : String -> String -> String) greeting "!!",
    }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn match_expr_bad_literal() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let expected_ty = r"String";
    let given_expr = r#"match "helloo" : String {
        "hi" => "haha",
        1 => "byee",
    }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    let raw_term = support::parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match semantics::check_term(&context, &raw_term, &expected_ty) {
        Err(TypeError::LiteralMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn match_expr_wildcard() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"int {=123}";
    let given_expr = r#"match "helloo" : String {
        _ => 123,
    }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn match_expr_empty() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"String";
    let given_expr = r#"match "helloo" : String {}"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn match_int() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"U32 -> Unit";
    let given_expr = r#"\x : U32 => match x {
        0 => unit,
        1 => unit,
        3 => unit,
        _ => unit,
    }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn match_binary() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"U32Be -> Unit";
    let given_expr = r#"\x : U32Be => match (x : U32) {
        0 => unit,
        1 => unit,
        3 => unit,
        _ => unit,
    }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn match_binary_hex() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"U32Be -> Unit";
    let given_expr = r#"\x : U32Be => match (x : U32) {
        0x00 => unit,
        1 => unit,
        3 => unit,
        _ => unit,
    }"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn array_0_string() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Array 0 String";
    let given_expr = r#"[]"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn array_3_string() {
    let mut codemap = CodeMap::new();
    let context = Context::default();

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello", "hi", "byee"]"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    support::parse_check_term(&mut codemap, &context, given_expr, &expected_ty);
}

#[test]
fn array_len_mismatch() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello", "hi"]"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    let raw_term = support::parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match semantics::check_term(&context, &raw_term, &expected_ty) {
        Err(TypeError::ArrayLengthMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn array_elem_ty_mismatch() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello", "hi", 4]"#;

    let expected_ty = support::parse_nf_term(&mut codemap, &context, expected_ty);
    let raw_term = support::parse_term(&mut codemap, given_expr)
        .desugar(&desugar_env)
        .unwrap();

    match semantics::check_term(&context, &raw_term, &expected_ty) {
        Err(_) => {},
        Ok(term) => panic!("expected error but found: {}", term),
    }
}
