extern crate codespan;
extern crate codespan_reporting;
extern crate ddl;
#[macro_use]
extern crate moniker;

use codespan::CodeMap;
use moniker::{Binder, Embed, FreeVar, Scope, Var};

use ddl::semantics::{self, Context};
use ddl::syntax::core::{RcTerm, RcValue, Term, Value};

mod support;

mod nf_term {
    use ddl::syntax::core::{Head, Neutral, RcNeutral};

    use super::*;

    #[test]
    fn var() {
        let context = Context::default();

        let x = FreeVar::fresh_named("x");
        let var = RcTerm::from(Term::Var(Var::Free(x.clone())));

        assert_eq!(
            semantics::nf_term(&context, &var).unwrap(),
            RcValue::from(Value::from(Var::Free(x))),
        );
    }

    #[test]
    fn ty() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        assert_eq!(
            support::parse_nf_term(&mut codemap, &context, r"Type"),
            RcValue::from(Value::universe(0)),
        );
    }

    #[test]
    fn lam() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let x = FreeVar::fresh_named("x");

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, r"\x : Type => x"),
            RcValue::from(Value::Lam(Scope::new(
                (Binder(x.clone()), Embed(RcValue::from(Value::universe(0)))),
                RcValue::from(Value::from(Var::Free(x))),
            ))),
        );
    }

    #[test]
    fn pi() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let x = FreeVar::fresh_named("x");

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, r"(x : Type) -> x"),
            RcValue::from(Value::Pi(Scope::new(
                (Binder(x.clone()), Embed(RcValue::from(Value::universe(0)))),
                RcValue::from(Value::from(Var::Free(x))),
            ))),
        );
    }

    #[test]
    fn lam_app() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r"\(x : Type -> Type) (y : Type) => x y";

        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let ty_arr = RcValue::from(Value::Pi(Scope::new(
            (
                Binder(FreeVar::fresh_unnamed()),
                Embed(RcValue::from(Value::universe(0))),
            ),
            RcValue::from(Value::universe(0)),
        )));

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr,),
            RcValue::from(Value::Lam(Scope::new(
                (Binder(x.clone()), Embed(ty_arr)),
                RcValue::from(Value::Lam(Scope::new(
                    (Binder(y.clone()), Embed(RcValue::from(Value::universe(0)))),
                    RcValue::from(Value::Neutral(
                        RcNeutral::from(Neutral::Head(Head::Var(Var::Free(x)))),
                        vec![RcValue::from(Value::from(Var::Free(y)))],
                    )),
                ))),
            ))),
        );
    }

    #[test]
    fn pi_app() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r"(x : Type -> Type) -> (y : Type) -> x y";

        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let ty_arr = RcValue::from(Value::Pi(Scope::new(
            (
                Binder(FreeVar::fresh_unnamed()),
                Embed(RcValue::from(Value::universe(0))),
            ),
            RcValue::from(Value::universe(0)),
        )));

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            RcValue::from(Value::Pi(Scope::new(
                (Binder(x.clone()), Embed(ty_arr)),
                RcValue::from(Value::Pi(Scope::new(
                    (Binder(y.clone()), Embed(RcValue::from(Value::universe(0)))),
                    RcValue::from(Value::Neutral(
                        RcNeutral::from(Neutral::Head(Head::Var(Var::Free(x)))),
                        vec![RcValue::from(Value::from(Var::Free(y)))],
                    )),
                ))),
            ))),
        );
    }

    // Passing `Type` to the polymorphic identity function should yield the type
    // identity function
    #[test]
    fn id_app_ty() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r"(\(a : Type 1) (x : a) => x) Type";
        let expected_expr = r"\x : Type => x";

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            support::parse_nf_term(&mut codemap, &context, expected_expr),
        );
    }

    // Passing `Type` to the `Type` identity function should yield `Type`
    #[test]
    fn id_app_ty_ty() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) Type";
        let expected_expr = r"Type";

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            support::parse_nf_term(&mut codemap, &context, expected_expr),
        );
    }

    // Passing `Type -> Type` to the `Type` identity function should yield
    // `Type -> Type`
    #[test]
    fn id_app_ty_arr_ty() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r"(\(a : Type 2) (x : a) => x) (Type 1) (Type -> Type)";
        let expected_expr = r"Type -> Type";

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            support::parse_nf_term(&mut codemap, &context, expected_expr),
        );
    }

    // Passing the id function to itself should yield the id function
    #[test]
    fn id_app_id() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r"
            (\(a : Type 1) (x : a) => x)
                ((a : Type) -> a -> a)
                (\(a : Type) (x : a) => x)
        ";
        let expected_expr = r"\(a : Type) (x : a) => x";

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            support::parse_nf_term(&mut codemap, &context, expected_expr),
        );
    }

    // Passing the id function to the 'const' combinator should yield a
    // function that always returns the id function
    #[test]
    fn const_app_id_ty() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r"
            (\(a : Type 1) (b : Type 2) (x : a) (y : b) => x)
                ((a : Type) -> a -> a)
                (Type 1)
                (\(a : Type) (x : a) => x)
                Type
        ";
        let expected_expr = r"\(a : Type) (x : a) => x";

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            support::parse_nf_term(&mut codemap, &context, expected_expr),
        );
    }

    #[test]
    fn horrifying_app_1() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r"(\(t : Type) (f : (a : Type) -> Type) => f t) String (\(a : Type) => a)";
        let expected_expr = r"String";

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            support::parse_nf_term(&mut codemap, &context, expected_expr),
        );
    }

    #[test]
    fn horrifying_app_2() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r#"(\(t: String) (f: String -> String) => f t) "hello""#;
        let expected_expr = r#"\(f : String -> String) => f "hello""#;

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            support::parse_nf_term(&mut codemap, &context, expected_expr),
        );
    }

    #[test]
    fn if_true() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r#"
            if true { "true" } else { "false" } : String
        "#;
        let expected_expr = r#"
            "true" : String
        "#;

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            support::parse_nf_term(&mut codemap, &context, expected_expr),
        );
    }

    #[test]
    fn if_false() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r#"
            if false { "true" } else { "false" } : String
        "#;
        let expected_expr = r#"
            "false" : String
        "#;

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            support::parse_nf_term(&mut codemap, &context, expected_expr),
        );
    }

    #[test]
    fn if_eval_cond() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r#"
            if (match "hi" : String {
                "hi" => true,
                _ => false,
            }) { "true" } else { "false" } : String
        "#;
        let expected_expr = r#"
            "true" : String
        "#;

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            support::parse_nf_term(&mut codemap, &context, expected_expr),
        );
    }

    #[test]
    fn match_expr_bool_true() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r#"
            match true {
                true => "true" : String,
                false => "false" : String,
            }
        "#;
        let expected_expr = r#"
            "true" : String
        "#;

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            support::parse_nf_term(&mut codemap, &context, expected_expr),
        );
    }

    #[test]
    fn match_expr_bool_false() {
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let given_expr = r#"
            match false {
                true => "true" : String,
                false => "false" : String,
            }
        "#;
        let expected_expr = r#"
            "false" : String
        "#;

        assert_term_eq!(
            support::parse_nf_term(&mut codemap, &context, given_expr),
            support::parse_nf_term(&mut codemap, &context, expected_expr),
        );
    }
}
