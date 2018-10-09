use codespan::{CodeMap, FileName};
use codespan_reporting;
use codespan_reporting::termcolor::{ColorChoice, StandardStream};
use goldenfile::Mint;

use std::io::Write;
use syntax::parse;

use super::*;

fn golden(filename: &str, literal: &str) {
    let path = "src/syntax/translation/desugar/goldenfiles";

    let mut mint = Mint::new(path);
    let mut file = mint.new_goldenfile(filename).unwrap();
    let env = DesugarEnv::new(HashMap::new());

    let term = parse_desugar_term(&env, literal);

    write!(file, "{:#?}", term).unwrap();
}

fn parse_module(codemap: &mut CodeMap, src: &str) -> concrete::Module {
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
    let (concrete_module, errors) = parse::module(&filemap);

    if !errors.is_empty() {
        let writer = StandardStream::stdout(ColorChoice::Always);
        for error in errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
        }
        panic!("parse error!")
    }

    concrete_module
}

fn parse_term(codemap: &mut CodeMap, src: &str) -> concrete::Term {
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
    let (concrete_term, errors) = parse::term(&filemap);

    if !errors.is_empty() {
        let writer = StandardStream::stdout(ColorChoice::Always);
        for error in errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
        }
        panic!("parse error!")
    }

    concrete_term
}

fn parse_desugar_term(env: &DesugarEnv, src: &str) -> raw::RcTerm {
    let mut codemap = CodeMap::new();

    match parse_term(&mut codemap, src).desugar(env) {
        Ok(raw_term) => raw_term,
        Err(error) => {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
            panic!("type error!");
        },
    }
}

mod module {
    use super::*;

    #[test]
    fn infer_bare_definition() {
        let mut codemap = CodeMap::new();
        let desugar_env = DesugarEnv::new(hashmap!{
            "true".to_owned() => FreeVar::fresh_named("true"),
        });

        let src = "
            module test;

            foo = true;
        ";

        if let Err(err) = parse_module(&mut codemap, src).desugar(&desugar_env) {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
            panic!("type error!")
        }
    }

    #[test]
    fn forward_declarations() {
        let mut codemap = CodeMap::new();
        let desugar_env = DesugarEnv::new(hashmap!{
            "Bool".to_owned() => FreeVar::fresh_named("Bool"),
            "true".to_owned() => FreeVar::fresh_named("true"),
            "false".to_owned() => FreeVar::fresh_named("false"),
        });

        let src = "
            module test;

            foo : Bool;
            bar : Bool;
            bar = true;
            foo = false;
        ";

        if let Err(err) = parse_module(&mut codemap, src).desugar(&desugar_env) {
            let writer = StandardStream::stdout(ColorChoice::Always);
            codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
            panic!("type error!")
        }
    }

    #[test]
    fn declaration_after_definition() {
        let mut codemap = CodeMap::new();
        let desugar_env = DesugarEnv::new(hashmap!{
            "Bool".to_owned() => FreeVar::fresh_named("Bool"),
            "true".to_owned() => FreeVar::fresh_named("true"),
        });

        let src = "
            module test;

            foo = true;
            foo : Bool;
        ";

        match parse_module(&mut codemap, src).desugar(&desugar_env) {
            Ok(_) => panic!("expected error"),
            Err(DesugarError::DeclarationFollowedDefinition { .. }) => {},
            Err(err) => panic!("unexpected error: {}", err),
        }
    }

    #[test]
    fn duplicate_declarations() {
        let mut codemap = CodeMap::new();
        let desugar_env = DesugarEnv::new(hashmap!{
            "Bool".to_owned() => FreeVar::fresh_named("Bool"),
            "I32".to_owned() => FreeVar::fresh_named("I32"),
        });

        let src = "
            module test;

            foo : Bool;
            foo : I32;
        ";

        match parse_module(&mut codemap, src).desugar(&desugar_env) {
            Ok(_) => panic!("expected error"),
            Err(DesugarError::DuplicateDeclarations { .. }) => {},
            Err(err) => panic!("unexpected error: {}", err),
        }
    }

    #[test]
    fn duplicate_definitions() {
        let mut codemap = CodeMap::new();
        let desugar_env = DesugarEnv::new(hashmap!{
            "Bool".to_owned() => FreeVar::fresh_named("Bool"),
            "I32".to_owned() => FreeVar::fresh_named("I32"),
        });

        let src = "
            module test;

            foo = Bool;
            foo = I32;
        ";

        match parse_module(&mut codemap, src).desugar(&desugar_env) {
            Ok(_) => panic!("expected error"),
            Err(DesugarError::DuplicateDefinitions { .. }) => {},
            Err(err) => panic!("unexpected error: {}", err),
        }
    }

}

mod term {
    use syntax::raw::{RcTerm, Term};
    use syntax::Level;

    use super::*;

    #[test]
    fn var() {
        let env = DesugarEnv::new(HashMap::new());

        match *parse_desugar_term(&env, "or_elim").inner {
            raw::Term::Var(_, Var::Free(ref free_var)) => {
                assert_eq!(free_var.pretty_name, Some("or_elim".to_owned()));
            },
            ref term => panic!("unexpected term: {}", term),
        }
    }

    #[test]
    fn ty() {
        golden("ty", r"Type");
    }

    #[test]
    fn ty_level() {
        golden("ty_level", r"Type 2");
    }

    #[test]
    fn ann() {
        golden("ann", r"Type : Type");
    }

    #[test]
    fn ann_ann_left() {
        golden("ann_ann_left", r"Type : Type : Type");
    }

    #[test]
    fn ann_ann_right() {
        golden("ann_ann_right", r"Type : (Type : Type)");
    }

    #[test]
    fn ann_ann_ann() {
        golden("ann_ann_ann", r"(Type : Type) : (Type : Type)");
    }

    #[test]
    fn lam_ann() {
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse_desugar_term(&env, r"\x : Type -> Type => x"),
            RcTerm::from(Term::Lam(
                ByteSpan::default(),
                Scope::new(
                    (
                        Binder(x.clone()),
                        Embed(RcTerm::from(Term::Pi(
                            ByteSpan::default(),
                            Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(u0())), u0()),
                        ))),
                    ),
                    RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone()))),
                ),
            )),
        );
    }

    #[test]
    fn lam() {
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let var_y = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(y.clone())));
        let hole = || RcTerm::from(Term::Hole(ByteSpan::default()));

        assert_term_eq!(
            parse_desugar_term(&env, r"\x : (\y => y) => x"),
            RcTerm::from(Term::Lam(
                ByteSpan::default(),
                Scope::new(
                    (
                        Binder(x.clone()),
                        Embed(RcTerm::from(Term::Lam(
                            ByteSpan::default(),
                            Scope::new((Binder(y.clone()), Embed(hole())), var_y()),
                        )))
                    ),
                    var_x(),
                ),
            )),
        );
    }

    #[test]
    fn lam_lam_ann() {
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse_desugar_term(&env, r"\(x y : Type) => x"),
            RcTerm::from(Term::Lam(
                ByteSpan::default(),
                Scope::new(
                    (Binder(x.clone()), Embed(u0())),
                    RcTerm::from(Term::Lam(
                        ByteSpan::default(),
                        Scope::new((Binder(y.clone()), Embed(u0())), var_x()),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn arrow() {
        let env = DesugarEnv::new(HashMap::new());

        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse_desugar_term(&env, r"Type -> Type"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(u0())), u0()),
            )),
        );
    }

    #[test]
    fn pi() {
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse_desugar_term(&env, r"(x : Type -> Type) -> x"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new(
                    (
                        Binder(x.clone()),
                        Embed(RcTerm::from(Term::Pi(
                            ByteSpan::default(),
                            Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(u0())), u0()),
                        ))),
                    ),
                    var_x(),
                ),
            )),
        );
    }

    #[test]
    fn pi_pi() {
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse_desugar_term(&env, r"(x y : Type) -> x"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new(
                    (Binder(x.clone()), Embed(u0())),
                    RcTerm::from(Term::Pi(
                        ByteSpan::default(),
                        Scope::new((Binder(y.clone()), Embed(u0())), var_x()),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn pi_arrow() {
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse_desugar_term(&env, r"(x : Type) -> x -> x"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new(
                    (Binder(x.clone()), Embed(u0())),
                    RcTerm::from(Term::Pi(
                        ByteSpan::default(),
                        Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(var_x())), var_x()),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn lam_app() {
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let y = FreeVar::fresh_named("y");
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let var_y = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(y.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse_desugar_term(&env, r"\(x : Type -> Type) (y : Type) => x y"),
            RcTerm::from(Term::Lam(
                ByteSpan::default(),
                Scope::new(
                    (
                        Binder(x.clone()),
                        Embed(RcTerm::from(Term::Pi(
                            ByteSpan::default(),
                            Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(u0())), u0()),
                        ))),
                    ),
                    RcTerm::from(Term::Lam(
                        ByteSpan::default(),
                        Scope::new(
                            (Binder(y.clone()), Embed(u0())),
                            RcTerm::from(Term::App(var_x(), var_y())),
                        ),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn id() {
        let env = DesugarEnv::new(HashMap::new());

        let x = FreeVar::fresh_named("x");
        let a = FreeVar::fresh_named("a");
        let var_x = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(x.clone())));
        let var_a = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(a.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse_desugar_term(&env, r"\(a : Type) (x : a) => x"),
            RcTerm::from(Term::Lam(
                ByteSpan::default(),
                Scope::new(
                    (Binder(a.clone()), Embed(u0())),
                    RcTerm::from(Term::Lam(
                        ByteSpan::default(),
                        Scope::new((Binder(x.clone()), Embed(var_a())), var_x()),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn id_ty() {
        let env = DesugarEnv::new(HashMap::new());

        let a = FreeVar::fresh_named("a");
        let var_a = || RcTerm::from(Term::Var(ByteSpan::default(), Var::Free(a.clone())));
        let u0 = || RcTerm::from(Term::Universe(ByteSpan::default(), Level(0)));

        assert_term_eq!(
            parse_desugar_term(&env, r"(a : Type) -> a -> a"),
            RcTerm::from(Term::Pi(
                ByteSpan::default(),
                Scope::new(
                    (Binder(a.clone()), Embed(u0())),
                    RcTerm::from(Term::Pi(
                        ByteSpan::default(),
                        Scope::new((Binder(FreeVar::fresh_unnamed()), Embed(var_a())), var_a()),
                    )),
                ),
            )),
        );
    }

    mod sugar {
        use super::*;

        #[test]
        fn lam_args() {
            let env = DesugarEnv::new(HashMap::new());

            assert_term_eq!(
                parse_desugar_term(&env, r"\x (y : Type) z => x"),
                parse_desugar_term(&env, r"\x => \y : Type => \z => x"),
            );
        }

        #[test]
        fn lam_args_multi() {
            let env = DesugarEnv::new(HashMap::new());

            assert_term_eq!(
                parse_desugar_term(&env, r"\(x : Type) (y : Type) z => x"),
                parse_desugar_term(&env, r"\(x y : Type) z => x"),
            );
        }

        #[test]
        fn pi_args() {
            let env = DesugarEnv::new(HashMap::new());

            assert_term_eq!(
                parse_desugar_term(&env, r"(a : Type) -> (x y z : a) -> x"),
                parse_desugar_term(&env, r"(a : Type) -> (x : a) -> (y : a) -> (z : a) -> x"),
            );
        }

        #[test]
        fn pi_args_multi() {
            let env = DesugarEnv::new(HashMap::new());

            assert_term_eq!(
                parse_desugar_term(&env, r"(a : Type) (x y z : a) (w : x) -> x"),
                parse_desugar_term(
                    &env,
                    r"(a : Type) -> (x : a) -> (y : a) -> (z : a) -> (w : x) -> x"
                ),
            );
        }

        #[test]
        fn arrow() {
            let env = DesugarEnv::new(HashMap::new());

            assert_term_eq!(
                parse_desugar_term(&env, r"(a : Type) -> a -> a"),
                parse_desugar_term(&env, r"(a : Type) -> (x : a) -> a"),
            )
        }

        #[test]
        fn if_then_else() {
            let env = DesugarEnv::new(hashmap!{
                "true".to_owned() => FreeVar::fresh_named("true"),
                "false".to_owned() => FreeVar::fresh_named("false"),
            });

            assert_term_eq!(
                parse_desugar_term(&env, r#"if true { "true" } else { "false" }"#),
                parse_desugar_term(&env, r#"match true { true => "true", false => "false" }"#),
            )
        }

        #[test]
        fn struct_field_puns() {
            let env = DesugarEnv::new(hashmap!{
                "x".to_owned() => FreeVar::fresh_named("x"),
                "y".to_owned() => FreeVar::fresh_named("y"),
            });

            assert_term_eq!(
                parse_desugar_term(&env, r#"struct { x, y }"#),
                parse_desugar_term(&env, r#"struct { x = x, y = y }"#),
            )
        }
    }
}
