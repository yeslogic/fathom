use codespan::Files;
use codespan_reporting;
use codespan_reporting::termcolor::{ColorChoice, StandardStream};
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
    let reporting_config = codespan_reporting::Config::default();
    let stdout = StandardStream::stdout(ColorChoice::Always);

    let mut files = Files::new();

    let test_path = TESTS_DIR.join(test_path);
    let source = fs::read_to_string(&test_path).unwrap();
    let file_id = files.add(test_path.display().to_string(), source);

    let (directives, diagnostics) = {
        let lexer = directives::Lexer::new(&files, file_id);
        let mut parser = directives::Parser::new(&files, file_id);
        parser.expect_directives(lexer);
        parser.finish()
    };

    if !diagnostics.is_empty() {
        for diagnostic in diagnostics {
            codespan_reporting::emit(&mut stdout.lock(), &reporting_config, &files, &diagnostic)
                .unwrap();
        }
        panic!("failed to parse diagnostics");
    }

    if let Some(parse_status) = directives.parse {
        let module = ddl_parse::parse_module(files.source(file_id)).unwrap();

        if let Some(compile_rust_status) = directives.compile_rust {
            ddl_compile_rust::compile_module(&module).unwrap();
        }

        if let Some(compile_doc_status) = directives.compile_doc {
            ddl_compile_doc::compile_module(&module).unwrap();
        }
    }
}
