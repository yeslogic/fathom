use codespan::Files;
use std::fs;
use std::path::{Path, PathBuf};

mod directives;

lazy_static::lazy_static! {
    static ref TESTS_DIR: PathBuf =
        Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/../../tests"))
            .canonicalize()
            .unwrap();
}

pub fn run_test(_test_name: &str, test_path: &str) {
    let mut files = Files::new();

    let source = fs::read_to_string(&TESTS_DIR.join(test_path)).unwrap();
    let file_id = files.add(test_path, source);

    for token in directives::Lexer::new(&files, file_id) {
        println!("{:?}", token);
    }

    let module = ddl_parse::parse_module(files.source(file_id)).unwrap();
    ddl_compile_rust::compile_module(&module).unwrap();
    ddl_compile_doc::compile_module(&module).unwrap();
}
