use super::*;

#[test]
fn infer_bare_definition() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = "
        module test;

        foo = true;
    ";

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn forward_declarations() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = "
        module test;

        foo : Bool;
        bar : Bool;
        bar = true;
        foo = false;
    ";

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn forward_declarations_forward_ref() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = "
        module test;

        foo : Bool;
        bar : Bool;
        bar = foo;
        foo = false;
    ";

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::UndefinedName { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn declaration_after_definition() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = "
        module test;

        foo = true;
        foo : Bool;
    ";

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::DeclarationFollowedDefinition { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn duplicate_declarations() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = "
        module test;

        foo : Bool;
        foo : I32;
    ";

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::DuplicateDeclarations { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn duplicate_definitions() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = "
        module test;

        foo = Bool;
        foo = I32;
    ";

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::DuplicateDefinitions { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn empty_struct() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = r#"
        module test;

        struct Test {};

        test : (Test : Type);
        test = struct {};
    "#;

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn simple_struct() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

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

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn dependent_struct() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

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

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn dependent_struct_propagate_types() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

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

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn simple_struct_proj() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

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

        test-x : String = test.x;
    "#;

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn simple_struct_proj_missing() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = r#"
        module test;

        struct Test {
            x : String,
        };

        test : Test;
        test = struct {
            x = "hello",
        };

        test-bloop = test.bloop;
    "#;

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::NoFieldInType { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn dependent_struct_proj_weird1() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

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

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn dependent_struct_proj_weird2() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = r#"
        module test;

        struct Data {
            n : U16,
            x : Array n S8,
            y : Array n S8,
        };

        struct Test {
            data : Data,
            inner-prod : (len : U16) -> Array len S8 -> Array len S8 -> S32,

            test1 : S32 -> Type,
            test2 : test1 (inner-prod data.n data.x data.y),
        };
    "#;

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn dependent_struct_with_integer() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = r#"
        module test;

        struct Test {
            len : U16Be,
            data : Array len U32Be,
        };
    "#;

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn struct_field_mismatch_lt() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

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

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::StructSizeMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn struct_field_mismatch_gt() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

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

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::StructSizeMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}
