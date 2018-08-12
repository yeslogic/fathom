use super::*;

#[test]
fn infer_bare_definition() {
    let mut codemap = CodeMap::new();

    let src = "
        module test;

        foo = true;
    ";

    let raw_module = parse_module(&mut codemap, src);
    if let Err(err) = check_module(&TcEnv::default(), &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn forward_declarations() {
    let mut codemap = CodeMap::new();
    let writer = StandardStream::stdout(ColorChoice::Always);

    let src = "
        module test;

        foo : Bool;
        bar : Bool;
        bar = true;
        foo = false;
    ";

    let raw_module = parse_module(&mut codemap, src);
    if let Err(err) = check_module(&TcEnv::default(), &raw_module) {
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn forward_declarations_forward_ref() {
    let mut codemap = CodeMap::new();

    let src = "
        module test;

        foo : Bool;
        bar : Bool;
        bar = foo;
        foo = false;
    ";

    let raw_module = parse_module(&mut codemap, src);
    match check_module(&TcEnv::default(), &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::NotYetDefined { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn declaration_after_definition() {
    let mut codemap = CodeMap::new();

    let src = "
        module test;

        foo = true;
        foo : Bool;
    ";

    let raw_module = parse_module(&mut codemap, src);
    match check_module(&TcEnv::default(), &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::DeclarationFollowedDefinition { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn duplicate_declarations() {
    let mut codemap = CodeMap::new();

    let src = "
        module test;

        foo : Bool;
        foo : I32;
    ";

    let raw_module = parse_module(&mut codemap, src);
    match check_module(&TcEnv::default(), &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::DuplicateDeclarations { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn duplicate_definitions() {
    let mut codemap = CodeMap::new();

    let src = "
        module test;

        foo = Bool;
        foo = I32;
    ";

    let raw_module = parse_module(&mut codemap, src);
    match check_module(&TcEnv::default(), &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::DuplicateDefinitions { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}
