use std::path::{Path, PathBuf};
use std::fs;

lazy_static::lazy_static! {
    static ref TESTS_DIR: PathBuf =
        Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/../../tests"))
            .canonicalize()
            .unwrap();
}

pub fn run_test(_test_name: &str, test_path: &str) {
    let source = fs::read_to_string(&TESTS_DIR.join(test_path)).unwrap();
    let module = ddl_parse::parse_module(&source).unwrap();
    ddl_compile_rust::compile_module(&module).unwrap();
    ddl_compile_doc::compile_module(&module).unwrap();
}
