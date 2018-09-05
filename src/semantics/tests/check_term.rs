use super::*;

#[test]
fn range_full() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"{..}";
    let given_expr = r#"5"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn range_from() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"{0 ..}";
    let given_expr = r#"0"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn range_to() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"{.. 10}";
    let given_expr = r#"10"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn range_from_to() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"{0 .. 10}";
    let given_expr = r#"0"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);

    let expected_ty = r"{0 .. 10}";
    let given_expr = r#"10"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn case_expr() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" of {
        "hi" => "haha";
        "hello" => "byee";
        greeting => (extern "string-append" : String -> String -> String) greeting "!!";
    }"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn case_expr_bad_literal() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" of {
        "hi" => "haha";
        1 => "byee";
    }"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    let raw_term = parse_term(&mut codemap, given_expr).desugar(&desugar_env);

    match check_term(&tc_env, &raw_term, &expected_ty) {
        Err(TypeError::LiteralMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn case_expr_wildcard() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"{=123}";
    let given_expr = r#"case "helloo" of {
        _ => 123;
    }"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn case_expr_empty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"String";
    let given_expr = r#"case "helloo" of {}"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn array_0_string() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Array 0 String";
    let given_expr = r#"[]"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn array_3_string() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello", "hi", "byee"]"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    parse_check_term(&mut codemap, &tc_env, given_expr, &expected_ty);
}

#[test]
fn array_len_mismatch() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello", "hi"]"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    let raw_term = parse_term(&mut codemap, given_expr).desugar(&desugar_env);

    match check_term(&tc_env, &raw_term, &expected_ty) {
        Err(TypeError::ArrayLengthMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok(term) => panic!("expected error but found: {}", term),
    }
}

#[test]
fn array_elem_ty_mismatch() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let expected_ty = r"Array 3 String";
    let given_expr = r#"["hello", "hi", 4]"#;

    let expected_ty = parse_nf_term(&mut codemap, &tc_env, expected_ty);
    let raw_term = parse_term(&mut codemap, given_expr).desugar(&desugar_env);

    match check_term(&tc_env, &raw_term, &expected_ty) {
        Err(_) => {},
        Ok(term) => panic!("expected error but found: {}", term),
    }
}
