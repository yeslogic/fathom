use codespan::Files;
use codespan_reporting::termcolor::{BufferWriter, ColorChoice, StandardStream};
use codespan_reporting::{self, Diagnostic, Severity};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

mod directives;
mod snapshot;

use self::directives::ExpectedDiagnostic;

lazy_static::lazy_static! {
    static ref CARGO_MANIFEST_DIR: PathBuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    static ref INPUT_DIR: PathBuf = CARGO_MANIFEST_DIR.join("tests").join("input");

    static ref CARGO_TARGET_DIR: PathBuf = {
        let output = Command::new(env!("CARGO"))
            .arg("metadata")
            .arg("--no-deps")
            .arg("--format-version=1")
            .output();

        match output {
            Err(error) => panic!("error executing `cargo metadata`: {}", error),
            Ok(output) => match json::parse(&String::from_utf8_lossy(&output.stdout)) {
                Err(error) => panic!("error parsing `cargo metadata`: {}", error),
                Ok(metadata) => PathBuf::from(metadata["target_directory"].as_str().unwrap()),
            }
        }
    };

    static ref CARGO_DEPS_DIR: PathBuf = CARGO_TARGET_DIR.join("debug").join("deps");
    static ref CARGO_INCREMENTAL_DIR: PathBuf = CARGO_TARGET_DIR.join("debug").join("incremental");
    static ref CARGO_DDL_RT_RLIB: PathBuf = CARGO_TARGET_DIR.join("debug").join("libddl_rt.rlib");
}

pub fn run_integration_test(test_name: &str, test_ddl_path: &str) {
    // Set up output streams

    let reporting_config = codespan_reporting::Config::default();
    let stdout = StandardStream::stdout(ColorChoice::Auto);

    // Set up files

    let mut files = Files::new();
    let test_ddl_path = INPUT_DIR.join(test_ddl_path);
    let source = fs::read_to_string(&test_ddl_path)
        .unwrap_or_else(|error| panic!("error reading `{}`: {}", test_ddl_path.display(), error));
    let file_id = files.add(test_ddl_path.display().to_string(), source);

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

        directives
    };

    // Run stages

    eprintln!();

    let mut failed_checks = Vec::new();
    let mut found_diagnostics = Vec::new();

    // SKIP
    if let Some(reason) = &directives.skip {
        eprintln!("Skipped: {}", reason);
        return;
    }

    // PARSE
    let concrete_module = {
        let (concrete_module, diagnostics) = ddl::parse::parse_module(&files, file_id);
        found_diagnostics.extend(diagnostics);
        concrete_module
    };

    // ELABORATE
    let core_module = {
        let (core_module, diagnostics) = ddl::elaborate::elaborate_module(&concrete_module);
        found_diagnostics.extend(diagnostics);

        // The core syntax from the elaborator should always be well-formed!
        let validation_diagnostics = ddl::core::validate::validate_module(&core_module);
        if !validation_diagnostics.is_empty() {
            failed_checks.push("elaborate: validate");

            eprintln!("Failed ELABORATE: validate");
            eprintln!();
            let writer = &mut stdout.lock();
            for diagnostic in validation_diagnostics {
                codespan_reporting::emit(writer, &reporting_config, &files, &diagnostic).unwrap();
            }
        }

        core_module
    };

    // COMPILE/RUST
    compile_rust(
        &test_ddl_path,
        &mut failed_checks,
        &mut found_diagnostics,
        &core_module,
    );

    // COMPILE/DOC
    compile_doc(
        &test_ddl_path,
        &mut failed_checks,
        &mut found_diagnostics,
        &core_module,
    );

    // Ensure that no unexpected diagnostics and no expected diagnostics remain

    retain_unexpected(
        &files,
        &mut found_diagnostics,
        &mut directives.expected_diagnostics,
    );

    if !found_diagnostics.is_empty() {
        failed_checks.push("unexpected_diagnostics");

        eprintln!("Unexpected diagnostics found:");
        eprintln!();

        // Use a buffer so that this doesn't get printed interleaved with the
        // test status output.

        let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();
        for diagnostic in &found_diagnostics {
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
                test_ddl_path.display(),
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

fn compile_rust(
    test_ddl_path: &Path,
    failed_checks: &mut Vec<&str>,
    found_diagnostics: &mut Vec<Diagnostic>,
    core_module: &ddl::core::Module,
) {
    let mut output = Vec::new();
    let diagnostics = ddl::compile::rust::compile_module(&mut output, core_module).unwrap();
    found_diagnostics.extend(diagnostics);

    if let Err(error) = snapshot::compare(test_ddl_path, "rs", &output) {
        failed_checks.push("compile_rust: snapshot");

        eprintln!("Failed COMPILE/RUST: snapshot test");
        eprintln!();
        eprintln!("{}", error);
    } else {
        // Test compiled output against rustc
        let temp_dir = assert_fs::TempDir::new().unwrap();

        Command::new(env!("CARGO"))
            .arg("build")
            .arg("--package=ddl-rt")
            .output()
            .unwrap();

        let output = Command::new("rustc")
            .arg(format!("--out-dir={}", temp_dir.path().display()))
            // just do type checking, skipping codegen
            .arg("--emit=dep-info,metadata")
            .arg("--crate-type=rlib")
            .arg("--edition=2018")
            .arg("-C")
            .arg(format!("incremental={}", CARGO_INCREMENTAL_DIR.display()))
            .arg("-L")
            .arg(format!("dependency={}", CARGO_DEPS_DIR.display()))
            .arg("--extern")
            .arg(format!("ddl_rt={}", CARGO_DDL_RT_RLIB.display()))
            .arg(snapshot::out_path(test_ddl_path, "rs").unwrap())
            .output();

        match output {
            Ok(output) => {
                if !output.status.success() {
                    failed_checks.push("compile_rust: rustc status");

                    eprintln!("Failed COMPILE/RUST: rustc status");
                    eprintln!();
                    eprintln!("Unexpected exist status: {}", output.status);
                    eprintln!();
                }

                if !output.stdout.is_empty() {
                    failed_checks.push("compile_rust: rustc stdout");

                    eprintln!("Failed COMPILE/RUST: rustc stdout");
                    eprintln!();
                    eprintln!("{}", String::from_utf8_lossy(&output.stdout));
                    eprintln!();
                }

                if !output.stderr.is_empty() {
                    failed_checks.push("compile_rust: rustc stderr");

                    eprintln!("Failed COMPILE/RUST: rustc stderr");
                    eprintln!();
                    eprintln!("{}", String::from_utf8_lossy(&output.stderr));
                    eprintln!();
                }
            }
            Err(error) => {
                failed_checks.push("compile_rust: execute rustc");

                eprintln!("Failed COMPILE/RUST:");
                eprintln!();
                eprintln!("{}", error);
                eprintln!();
            }
        }
    }
}

fn compile_doc(
    test_ddl_path: &Path,
    failed_checks: &mut Vec<&str>,
    found_diagnostics: &mut Vec<Diagnostic>,
    core_module: &ddl::core::Module,
) {
    let mut output = Vec::new();
    let diagnostics = ddl::compile::doc::compile_module(&mut output, core_module).unwrap();
    found_diagnostics.extend(diagnostics);

    if let Err(error) = snapshot::compare(test_ddl_path, "md", &output) {
        failed_checks.push("compile_doc: snapshot");

        eprintln!("Failed COMPILE/DOC: snapshot test");
        eprintln!();
        eprintln!("{}", error);
    }
}

fn retain_unexpected(
    files: &Files,
    found_diagnostics: &mut Vec<Diagnostic>,
    expected_diagnostics: &mut Vec<ExpectedDiagnostic>,
) {
    use std::collections::BTreeSet;

    let mut found_removals = BTreeSet::new();
    let mut expected_removals = BTreeSet::new();

    for (found_index, found_diagnostic) in found_diagnostics.iter().enumerate() {
        for (expected_index, expected_diagnostic) in expected_diagnostics.iter().enumerate() {
            if is_expected(files, found_diagnostic, expected_diagnostic) {
                found_removals.insert(found_index);
                expected_removals.insert(expected_index);
            }
        }
    }

    for index in found_removals.into_iter().rev() {
        found_diagnostics.remove(index);
    }

    for index in expected_removals.into_iter().rev() {
        expected_diagnostics.remove(index);
    }
}

fn is_expected(
    files: &Files,
    found_diagnostic: &Diagnostic,
    expected_diagnostic: &ExpectedDiagnostic,
) -> bool {
    found_diagnostic.primary_label.file_id == expected_diagnostic.file_id && {
        let start = found_diagnostic.primary_label.span.start();
        let found_location = files.location(expected_diagnostic.file_id, start).unwrap();
        let found_message = &found_diagnostic.message;

        found_location.line == expected_diagnostic.line
            && found_diagnostic.severity == expected_diagnostic.severity
            && expected_diagnostic.pattern.is_match(found_message)
    }
}
