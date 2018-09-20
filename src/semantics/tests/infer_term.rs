use super::*;

#[test]
fn undefined_name() {
    let tc_env = TcEnv::default();

    let x = FreeVar::fresh_named("x");
    let given_expr = raw::RcTerm::from(raw::Term::Var(ByteSpan::default(), Var::Free(x.clone())));

    assert_eq!(
        infer_term(&tc_env, &given_expr),
        Err(TypeError::UndefinedName {
            span: ByteSpan::default(),
            free_var: x.clone(),
        }),
    );
}

#[test]
fn extern_not_found() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r#"extern "does-not-exist" : U32"#;

    let raw_term = parse_term(&mut codemap, given_expr).desugar(&desugar_env);

    match infer_term(&tc_env, &raw_term) {
        Err(TypeError::UndefinedExternName { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok((term, ty)) => panic!("expected error, found {} : {}", term, ty),
    }
}

#[test]
fn ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type 1";
    let given_expr = r"Type";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn ty_levels() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type 1";
    let given_expr = r"Type 0 : Type 1 : Type 2 : Type 3"; //... Type ∞       ...+:｡(ﾉ･ω･)ﾉﾞ

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn int_literal() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"int {= 23}";
    let given_expr = r#"23"#;

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn ann_ty_id() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type -> Type";
    let given_expr = r"(\a => a) : Type -> Type";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn ann_arrow_ty_id() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(Type -> Type) -> (Type -> Type)";
    let given_expr = r"(\a => a) : (Type -> Type) -> (Type -> Type)";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn ann_id_as_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r"(\a => a) : Type";

    let raw_term = parse_term(&mut codemap, given_expr).desugar(&desugar_env);

    match infer_term(&tc_env, &raw_term) {
        Err(TypeError::UnexpectedFunction { .. }) => {},
        other => panic!("unexpected result: {:#?}", other),
    }
}

#[test]
fn app() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type 1";
    let given_expr = r"(\a : Type 1 => a) Type";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn app_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r"Type Type";

    let raw_term = parse_term(&mut codemap, given_expr).desugar(&desugar_env);

    assert_eq!(
        infer_term(&tc_env, &raw_term),
        Err(TypeError::ArgAppliedToNonFunction {
            fn_span: ByteSpan::new(ByteIndex(1), ByteIndex(5)),
            arg_span: ByteSpan::new(ByteIndex(6), ByteIndex(10)),
            found: Box::new(concrete::Term::Universe(ByteSpan::default(), Some(1))),
        }),
    )
}

#[test]
fn lam() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a : Type) -> Type";
    let given_expr = r"\a : Type => a";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn pi() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type 1";
    let given_expr = r"(a : Type) -> a";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn id() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a : Type) -> a -> a";
    let given_expr = r"\(a : Type) (x : a) => x";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn id_ann() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a : Type) -> a -> a";
    let given_expr = r"(\a (x : a) => x) : (A : Type) -> A -> A";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

// Passing `Type` to the polymorphic identity function should yield the type
// identity function
#[test]
fn id_app_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type -> Type";
    let given_expr = r"(\(a : Type 1) (x : a) => x) Type";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

// Passing `Type` to the `Type` identity function should yield `Type`
#[test]
fn id_app_ty_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type 1";
    let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) Type";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn id_app_ty_arr_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type 1";
    let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) (Type -> Type)";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn id_app_arr_pi_ty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"Type -> Type";
    let given_expr = r"(\(a : Type 1) (x : a) => x) (Type -> Type) (\x => x)";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn apply() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a b : Type) -> (a -> b) -> a -> b";
    let given_expr = r"\(a b : Type) (f : a -> b) (x : a) => f x";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn const_() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a b : Type) -> a -> b -> a";
    let given_expr = r"\(a b : Type) (x : a) (y : b) => x";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn const_flipped() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a b : Type) -> a -> b -> b";
    let given_expr = r"\(a b : Type) (x : a) (y : b) => y";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn flip() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a b c : Type) -> (a -> b -> c) -> (b -> a -> c)";
    let given_expr = r"\(a b c : Type) (f : a -> b -> c) (y : b) (x : a) => f x y";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn compose() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"(a b c : Type) -> (b -> c) -> (a -> b) -> (a -> c)";
    let given_expr = r"\(a b c : Type) (f : b -> c) (g : a -> b) (x : a) => f (g x)";

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn match_expr() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"String";
    let given_expr = r#"match "helloo" : String {
        "hi" => "haha" : String,
        "hello" => "byee" : String,
        greeting => (extern "string-append" : String -> String -> String) greeting "!!",
    }"#;

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn match_expr_bool() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"String";
    let given_expr = r#"match true {
        true => "hello" : String,
        false => "hi" : String,
    }"#;

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn match_expr_bool_bad() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r#"match "hello" : String {
        true => "hello" : String,
        false => "hi" : String,
    }"#;

    let raw_term = parse_term(&mut codemap, given_expr).desugar(&desugar_env);

    match infer_term(&tc_env, &raw_term) {
        Err(TypeError::Mismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok((term, ty)) => panic!("expected error, found {} : {}", term, ty),
    }
}

#[test]
fn match_expr_wildcard() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();

    let expected_ty = r"String";
    let given_expr = r#"match "helloo" : String {
        test => test,
    }"#;

    assert_term_eq!(
        parse_infer_term(&mut codemap, &tc_env, given_expr).1,
        parse_nf_term(&mut codemap, &tc_env, expected_ty),
    );
}

#[test]
fn match_expr_empty() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r#"match "helloo" : String {}"#;

    let raw_term = parse_term(&mut codemap, given_expr).desugar(&desugar_env);

    match infer_term(&tc_env, &raw_term) {
        Err(TypeError::AmbiguousEmptyMatch { .. }) => {},
        other => panic!("unexpected result: {:#?}", other),
    }
}

mod church_encodings {
    use super::*;

    #[test]
    fn and() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let expected_ty = r"Type -> Type -> Type 1";
        let given_expr = r"\(p q : Type) => (c : Type) -> (p -> q -> c) -> c";

        assert_term_eq!(
            parse_infer_term(&mut codemap, &tc_env, given_expr).1,
            parse_nf_term(&mut codemap, &tc_env, expected_ty),
        );
    }

    #[test]
    fn and_intro() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let expected_ty = r"
            (p q : Type) -> p -> q ->
                ((c : Type) -> (p -> q -> c) -> c)
        ";
        let given_expr = r"
            \(p q : Type) (x : p) (y : q) =>
                \c : Type => \f : (p -> q -> c) => f x y
        ";

        assert_term_eq!(
            parse_infer_term(&mut codemap, &tc_env, given_expr).1,
            parse_nf_term(&mut codemap, &tc_env, expected_ty),
        );
    }

    #[test]
    fn and_proj_left() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let expected_ty = r"
            (p q : Type) ->
                ((c : Type) -> (p -> q -> c) -> c) -> p
        ";
        let given_expr = r"
            \(p q : Type) (pq : (c : Type) -> (p -> q -> c) -> c) =>
                pq p (\x y => x)
        ";

        assert_term_eq!(
            parse_infer_term(&mut codemap, &tc_env, given_expr).1,
            parse_nf_term(&mut codemap, &tc_env, expected_ty),
        );
    }

    #[test]
    fn and_proj_right() {
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let expected_ty = r"
            (p q : Type) -> ((c : Type) -> (p -> q -> c) -> c) -> q
        ";
        let given_expr = r"
            \(p q : Type) (pq : (c : Type) -> (p -> q -> c) -> c) =>
                pq q (\x y => y)
        ";

        assert_term_eq!(
            parse_infer_term(&mut codemap, &tc_env, given_expr).1,
            parse_nf_term(&mut codemap, &tc_env, expected_ty),
        );
    }
}

#[test]
fn array_ambiguous() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let given_expr = r#"[1, 2 : S32]"#;

    let raw_term = parse_term(&mut codemap, given_expr).desugar(&desugar_env);

    match infer_term(&tc_env, &raw_term) {
        Err(TypeError::AmbiguousArrayLiteral { .. }) => {},
        Err(err) => panic!("unexpected error: {:?}", err),
        Ok((term, ty)) => panic!("expected error, found {} : {}", term, ty),
    }
}
