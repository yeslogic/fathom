extern crate codespan;
extern crate codespan_reporting;
extern crate ddl;
extern crate moniker;

use codespan::CodeMap;
use codespan_reporting::termcolor::{ColorChoice, StandardStream};

use ddl::semantics::{self, Context, TypeError};
use ddl::syntax::translation::{Desugar, DesugarEnv};

mod support;

#[test]
fn infer_bare_definition() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = "
        module test;

        foo = true;
    ";

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn forward_declarations() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = "
        module test;

        foo : Bool;
        bar : Bool;
        bar = true;
        foo = false;
    ";

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn forward_declarations_forward_ref() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = "
        module test;

        foo : Bool;
        bar : Bool;
        bar = foo;
        foo = false;
    ";

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn forward_struct_definitions() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = "
        module test;

        struct Bar { foo : Foo };
        struct Foo {};
    ";

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn empty_struct() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = r#"
        module test;

        struct Test {};

        test : (Test : Type);
        test = struct {};
    "#;

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn simple_struct() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = r#"
        module test;

        struct Test {
            t : Type,
            x : String,
        };

        test : (Test : Type 1);
        test = struct {
            t = String,
            x = "hello",
        };
    "#;

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn dependent_struct() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = r#"
        module test;

        struct Test {
            t : Type,
            x : t,
        };

        test : (Test : Type 1);
        test = struct {
            t = String,
            x = "hello",
        };
    "#;

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn dependent_struct_propagate_types() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = r#"
        module test;

        struct Test {
            t : Type,
            x : t,
        };

        test : (Test : Type 1);
        test = struct {
            t = S32,
            x = 1,
        };
    "#;

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn simple_struct_proj() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = r#"
        module test;

        struct Test {
            t : Type,
            x : String,
        };

        test : (Test : Type 1);
        test = struct {
            t = S32,
            x = "hello",
        };

        test_x : String = test.x;
    "#;

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn simple_struct_proj_missing() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = r#"
        module test;

        struct Test {
            x : String,
        };

        test : Test;
        test = struct {
            x = "hello",
        };

        test_bloop = test.bloop;
    "#;

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    match semantics::check_module(&context, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::NoFieldInType { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn dependent_struct_proj_weird1() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = r#"
        module test;

        struct Data {
            t : Type,
            x : t,
        };

        struct Test {
            data : Data,
            f : data.t -> Type,
            test : f data.x,
        };
    "#;

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn dependent_struct_proj_weird2() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = r#"
        module test;

        struct Data {
            n : U16,
            x : Array n S8,
            y : Array n S8,
        };

        struct Test {
            data : Data,
            inner_prod : (len : U16) -> Array len S8 -> Array len S8 -> S32,

            test1 : S32 -> Type,
            test2 : test1 (inner_prod data.n data.x data.y),
        };
    "#;

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn dependent_struct_with_integer() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = r#"
        module test;

        struct Test {
            len : U16Be,
            data : Array len U32Be,
        };
    "#;

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn struct_field_mismatch_lt() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = r#"
        module test;

        struct Test {
            x : String,
            y : String,
        };

        test : Test = struct {
            x = "hello",
        };
    "#;

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    match semantics::check_module(&context, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::StructSizeMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn struct_field_mismatch_gt() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = r#"
        module test;

        struct Test {
            x : String,
        };

        test : Test = struct {
            x = "hello",
            y = "hello",
        };
    "#;

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    match semantics::check_module(&context, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::StructSizeMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn struct_parameterised() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = "
        module test;

        struct Foo (len : U32) (A : Type) {
            data : Array len A,
        };

        Foo3U32 = Foo 3 U32;
        Foo3 = Foo 3;

        foo : Foo3 U32;
        foo = struct {
            data = [1, 2, 3],
        };

        data = foo.data : Array 3 U32;
    ";

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn array_index() {
    let mut codemap = CodeMap::new();
    let context = Context::default();
    let desugar_env = DesugarEnv::new(context.mappings());

    let src = r#"
        module test;

        index : (len : int {0 ..}) (A : Type) -> int {0 ..} -> Array len A -> A;
        index _ _ = extern "array-index";

        struct Test {
            lengths : Array 1 U32Be,
            data : Array (index 1 U32Be 0 lengths) U8,
        };
    "#;

    let raw_module = support::parse_module(&mut codemap, src)
        .desugar(&desugar_env)
        .unwrap();
    if let Err(err) = semantics::check_module(&context, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}
