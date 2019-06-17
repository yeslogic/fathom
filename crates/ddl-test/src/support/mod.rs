use codespan::{FileId, Files};
use codespan_reporting::termcolor::{BufferWriter, ColorChoice, StandardStream};
use codespan_reporting::{self, Diagnostic, Severity};
use std::fs;
use std::path::{Path, PathBuf};

mod directives;
mod snapshot;

use self::directives::Directives;

lazy_static::lazy_static! {
    static ref TESTS_DIR: PathBuf =
        Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/../../tests"))
            .canonicalize()
            .unwrap();
}

pub fn run_test(test_name: &str, test_path: &str) {
    // Set up output streams

    let reporting_config = codespan_reporting::Config::default();
    let stdout = StandardStream::stdout(ColorChoice::Auto);

    // Set up files

    let mut files = Files::new();
    let test_path = TESTS_DIR.join(test_path);
    let source = fs::read_to_string(&test_path)
        .unwrap_or_else(|error| panic!("error reading `{}`: {}", test_path.display(), error));
    let file_id = files.add(test_path.display().to_string(), source);

    // Extract the directives from the source code

    let mut directives = {
        let (directives, diagnostics) = {
            let lexer = directives::Lexer::new(&files, file_id);
            let mut parser = directives::Parser::new(&files, file_id);
            parser.expect_directives(lexer);
            parser.finish()
        };

        if !diagnostics.is_empty() {
            let writer = &mut stdout.lock();
            for diagnostic in diagnostics {
                codespan_reporting::emit(writer, &reporting_config, &files, &diagnostic).unwrap();
            }

            panic!("failed to parse diagnostics");
        }

        // TODO: Check stage topology?

        directives
    };

    // Run stages

    eprintln!();

    let mut failed_checks = Vec::new();
    let mut unexpected_diagnostics = Vec::new();

    // SKIP
    if let Some(reason) = &directives.skip {
        eprintln!("Skipped: {}", reason);
        return;
    }

    // FIXME: We should check these `_status` things somehow

    // PARSE
    if let Some(_status) = directives.parse {
        let (module, mut diagnostics) = ddl_parse::parse_module(&files, file_id);
        validate_pass(&files, file_id, &mut directives, &mut diagnostics);
        unexpected_diagnostics.extend(diagnostics);

        // COMPILE/RUST
        if let Some(_status) = directives.compile_rust {
            use assert_cmd::prelude::*;
            use predicates::prelude::*;
            use std::process::Command;

            let mut output = Vec::new();
            let mut diagnostics = ddl_compile_rust::compile_module(&mut output, &module).unwrap();

            if let Err(error) = snapshot::compare(&test_path, "rs", &output) {
                failed_checks.push("compile_rust: snapshot");

                eprintln!("Failed COMPILE/RUST snapshot test:");
                eprintln!();
                eprintln!("{}", error);
            }

            validate_pass(&files, file_id, &mut directives, &mut diagnostics);
            unexpected_diagnostics.extend(diagnostics);

            // Test compiled output against rustc
            let temp_dir = assert_fs::TempDir::new().unwrap();
            Command::new("rustc")
                .arg(format!("--out-dir={}", temp_dir.path().display()))
                // just do type checking, skipping codegen
                .arg("--emit=dep-info,metadata")
                .arg("--crate-type=rlib")
                .arg(test_path.with_extension("rs"))
                // FIXME: don't panic on this!
                .assert()
                .code(0)
                .stdout(predicate::str::is_empty())
                .stderr(predicate::str::is_empty())
                .success();
        }

        // COMPILE/DOC
        if let Some(_status) = directives.compile_doc {
            let mut output = Vec::new();
            let mut diagnostics = ddl_compile_doc::compile_module(&mut output, &module).unwrap();

            if let Err(error) = snapshot::compare(&test_path, "md", &output) {
                failed_checks.push("compile_doc: snapshot");

                eprintln!("Failed COMPILE/DOC snapshot test:");
                eprintln!();
                eprintln!("{}", error);
            }

            validate_pass(&files, file_id, &mut directives, &mut diagnostics);
            unexpected_diagnostics.extend(diagnostics);
        }
    }

    // Ensure that no unexpected diagnostics and no expected diagnostics remain

    if !unexpected_diagnostics.is_empty() {
        failed_checks.push("unexpected_diagnostics");

        eprintln!("Unexpected diagnostics found:");
        eprintln!();

        // Use a buffer so that this doesn't get printed interleaved with the
        // test status output.

        let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();
        for diagnostic in &unexpected_diagnostics {
            codespan_reporting::emit(&mut buffer, &reporting_config, &files, diagnostic).unwrap();
        }

        eprintln!("{}", String::from_utf8_lossy(buffer.as_slice()));
    }

    if !directives.expected_diagnostics.is_empty() {
        failed_checks.push("expected_diagnostics");

        eprintln!("Expected diagnostics not found:");
        eprintln!();

        for expected in &directives.expected_diagnostics {
            let severity = match expected.severity {
                Severity::Bug => "bug",
                Severity::Error => "error",
                Severity::Warning => "warning",
                Severity::Note => "note",
                Severity::Help => "help",
            };

            eprintln!(
                "{}:{}: {}: {}",
                test_path.display(),
                expected.line.number(),
                severity,
                expected.pattern,
            );
        }

        eprintln!();
    }

    if !failed_checks.is_empty() {
        eprintln!("failed {} checks:", failed_checks.len());
        for check in failed_checks {
            eprintln!("    {}", check);
        }
        eprintln!();

        panic!("failed {}", test_name);
    }
}

fn validate_pass(
    files: &Files,
    file_id: FileId,
    directives: &mut Directives,
    diagnostics: &mut Vec<Diagnostic>,
) {
    diagnostics.retain(|diagnostic| {
        let start = files
            .location(file_id, diagnostic.primary_label.span.start())
            .unwrap();

        let mut found_match = false;

        directives.expected_diagnostics.retain(|expected| {
            found_match = expected.line == start.line
                && expected.severity == diagnostic.severity
                && expected.pattern.is_match(&diagnostic.message);
            !found_match
        });

        !found_match
    });
}
