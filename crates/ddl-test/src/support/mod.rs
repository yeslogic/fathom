use codespan::Files;
use codespan_reporting::diagnostic::{Diagnostic, Severity};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{BufferWriter, ColorChoice, StandardStream};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

mod directives;
mod snapshot;

use self::directives::ExpectedDiagnostic;

lazy_static::lazy_static! {
    static ref CARGO_METADATA: json::JsonValue = {
        let output = Command::new(env!("CARGO"))
            .arg("metadata")
            .arg("--no-deps")
            .arg("--format-version=1")
            .output();

        match output {
            Err(error) => panic!("error executing `cargo metadata`: {}", error),
            Ok(output) => match json::parse(&String::from_utf8_lossy(&output.stdout)) {
                Err(error) => panic!("error parsing `cargo metadata`: {}", error),
                Ok(metadata) => metadata,
            }
        }
    };

    static ref CARGO_TARGET_DIR: PathBuf = PathBuf::from(CARGO_METADATA["target_directory"].as_str().unwrap());
    static ref CARGO_WORKSPACE_ROOT: PathBuf = PathBuf::from(CARGO_METADATA["workspace_root"].as_str().unwrap());
    static ref CARGO_DEPS_DIR: PathBuf = CARGO_TARGET_DIR.join("debug").join("deps");
    static ref CARGO_INCREMENTAL_DIR: PathBuf = CARGO_TARGET_DIR.join("debug").join("incremental");
    static ref CARGO_DDL_RT_RLIB: PathBuf = CARGO_TARGET_DIR.join("debug").join("libddl_rt.rlib");

    static ref INPUT_DIR: PathBuf = CARGO_WORKSPACE_ROOT.join("tests").join("input");
    static ref SNAPSHOTS_DIR: PathBuf = CARGO_WORKSPACE_ROOT.join("tests").join("snapshots");
}

pub fn run_integration_test(test_name: &str, ddl_path: &str) {
    // Set up output streams

    let term_config = term::Config::default();
    let stdout = StandardStream::stdout(ColorChoice::Auto);

    // Set up files

    let mut files = Files::new();
    let input_ddl_path = INPUT_DIR.join(ddl_path);
    let snapshot_filename = SNAPSHOTS_DIR.join(ddl_path).with_extension("");
    let source = fs::read_to_string(&input_ddl_path)
        .unwrap_or_else(|error| panic!("error reading `{}`: {}", input_ddl_path.display(), error));
    let file_id = files.add(input_ddl_path.display().to_string(), source);

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
                term::emit(writer, &term_config, &files, &diagnostic).unwrap();
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

    let concrete_module = {
        let lexer = ddl::lexer::Lexer::new(&files, file_id);
        ddl::concrete::Module::parse(file_id, lexer, &mut |d| found_diagnostics.push(d))
    };

    let core_module = elaborate(
        &files,
        &term_config,
        &mut failed_checks,
        &mut found_diagnostics,
        &concrete_module,
    );

    core_roundtrip(
        &mut files,
        &term_config,
        &snapshot_filename,
        &mut failed_checks,
        &core_module,
    );

    compile_rust(
        &test_name,
        &input_ddl_path,
        &snapshot_filename,
        &mut failed_checks,
        &core_module,
    );

    compile_doc(&snapshot_filename, &mut failed_checks, &core_module);

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
            term::emit(&mut buffer, &term_config, &files, diagnostic).unwrap();
        }

        eprintln_indented(4, "", "---- found diagnostics ----");
        eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
    }

    if !directives.expected_diagnostics.is_empty() {
        failed_checks.push("expected_diagnostics");

        eprintln!("Expected diagnostics not found:");
        eprintln!();

        eprintln_indented(4, "", "---- expected diagnostics ----");
        for expected in &directives.expected_diagnostics {
            let severity = match expected.severity {
                Severity::Bug => "bug",
                Severity::Error => "error",
                Severity::Warning => "warning",
                Severity::Note => "note",
                Severity::Help => "help",
            };

            eprintln!(
                "    | {}:{}: {}: {}",
                input_ddl_path.display(),
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

fn elaborate(
    files: &Files,
    term_config: &codespan_reporting::term::Config,
    failed_checks: &mut Vec<&str>,
    found_diagnostics: &mut Vec<Diagnostic>,
    concrete_module: &ddl::concrete::Module,
) -> ddl::core::Module {
    let core_module =
        ddl::elaborate::elaborate_module(&concrete_module, &mut |d| found_diagnostics.push(d));

    // The core syntax from the elaborator should always be well-formed!
    let mut validation_diagnostics = Vec::new();
    ddl::core::validate::validate_module(&core_module, &mut |d| validation_diagnostics.push(d));
    if !validation_diagnostics.is_empty() {
        failed_checks.push("elaborate: validate");

        let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();
        for diagnostic in &validation_diagnostics {
            term::emit(&mut buffer, &term_config, &files, diagnostic).unwrap();
        }

        eprintln!("  • elaborate: validate");
        eprintln!();
        eprintln_indented(4, "", "---- found diagnostics ----");
        eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
    }

    core_module
}

fn core_roundtrip(
    files: &mut Files,
    term_config: &codespan_reporting::term::Config,
    snapshot_filename: &Path,
    failed_checks: &mut Vec<&str>,
    core_module: &ddl::core::Module,
) {
    let pretty_core = {
        let arena = pretty::Arena::new();
        let pretty::DocBuilder(_, doc) = core_module.doc(&arena);
        doc.pretty(100).to_string()
    };

    let snapshot_core_ddl_path = snapshot_filename.with_extension("core.ddl");
    if let Err(error) = snapshot::compare(&snapshot_core_ddl_path, &pretty_core.as_bytes()) {
        failed_checks.push("core_roundtrip: snapshot");

        eprintln!("  • core_roundtrip: snapshot");
        eprintln!();
        eprintln!();
        eprintln_indented(4, "", "---- snapshot error ----");
        eprintln_indented(4, "", &error.to_string());
    }

    let mut core_parse_diagnostics = Vec::new();
    let core_file_id = files.add(snapshot_core_ddl_path.display().to_string(), pretty_core);
    let parsed_core_module = {
        let lexer = ddl::lexer::Lexer::new(&files, core_file_id);
        ddl::core::Module::parse(core_file_id, lexer, &mut |d| core_parse_diagnostics.push(d))
    };
    if !core_parse_diagnostics.is_empty() {
        failed_checks.push("core_roundtrip: parse core");

        let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();
        for diagnostic in &core_parse_diagnostics {
            term::emit(&mut buffer, &term_config, &files, diagnostic).unwrap();
        }

        eprintln!("  • core_roundtrip: parse core");
        eprintln!();
        eprintln_indented(4, "", "---- found diagnostics ----");
        eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
    }

    if *core_module != parsed_core_module {
        failed_checks.push("core_roundtrip: core != parse(pretty(core))");

        eprintln!("  • core_roundtrip: core != parse(pretty(core))");
    }
}

fn compile_rust(
    test_name: &str,
    input_ddl_path: &Path,
    snapshot_filename: &Path,
    failed_checks: &mut Vec<&str>,
    core_module: &ddl::core::Module,
) {
    let mut output = Vec::new();
    ddl::compile::rust::compile_module(&mut output, core_module).unwrap();
    let snapshot_rs_path = snapshot_filename.with_extension("rs");

    if let Err(error) = snapshot::compare(&snapshot_rs_path, &output) {
        failed_checks.push("compile_rust: snapshot");

        eprintln!("  • compile_rust: snapshot");
        eprintln!();
        eprintln!();
        eprintln_indented(4, "", "---- snapshot error ----");
        eprintln_indented(4, "", &error.to_string());
    } else {
        // Test compiled output against rustc
        let temp_dir = assert_fs::TempDir::new().unwrap();

        Command::new(env!("CARGO"))
            .arg("build")
            .arg("--package=ddl-rt")
            .output()
            .unwrap();

        let (rs_path, is_binary_parse_test) = match &input_ddl_path.with_extension("rs") {
            input_rs_path if input_rs_path.exists() => (input_rs_path.clone(), true),
            _ => (snapshot_rs_path, false),
        };

        let output = Command::new("rustc")
            .arg(format!("--out-dir={}", temp_dir.path().display()))
            .arg(format!("--crate-name={}", test_name))
            .arg("--test")
            .arg("--color=always")
            .arg("--edition=2018")
            .arg("-C")
            .arg(format!("incremental={}", CARGO_INCREMENTAL_DIR.display()))
            .arg("-L")
            .arg(format!("dependency={}", CARGO_DEPS_DIR.display()))
            .arg("--extern")
            .arg(format!("ddl_rt={}", CARGO_DDL_RT_RLIB.display()))
            .arg(&rs_path)
            .output();

        match output {
            Ok(output) => {
                if !output.status.success()
                    || !output.stdout.is_empty()
                    || !output.stderr.is_empty()
                {
                    failed_checks.push("compile_rust: rust compile output");

                    eprintln!("  • compile_rust: rust compile output");
                    eprintln!();
                }

                if !output.status.success() {
                    eprintln_indented(4, "", "---- rustc status ----");
                    eprintln_indented(4, "| ", &output.status.to_string());
                    eprintln!();
                }

                if !output.stdout.is_empty() {
                    eprintln_indented(4, "", "---- rustc stdout ----");
                    eprintln_indented(4, "| ", &String::from_utf8_lossy(&output.stdout));
                    eprintln!();
                }

                if !output.stderr.is_empty() {
                    eprintln_indented(4, "", "---- rustc stderr ----");
                    eprintln_indented(4, "| ", &String::from_utf8_lossy(&output.stderr));
                    eprintln!();
                }
            }
            Err(error) => {
                failed_checks.push("compile_rust: execute rustc");

                eprintln!("  • compile_rust: execute rustc");
                eprintln!();
                eprintln_indented(4, "", "---- rustc error ----");
                eprintln_indented(4, "", &error.to_string());
                eprintln!();
            }
        }

        // Run tests

        if is_binary_parse_test {
            let test_path = temp_dir.path().join(test_name);
            let display_path = test_path.display();
            match Command::new(&test_path).arg("--color=always").output() {
                Ok(output) => {
                    if !output.status.success() {
                        failed_checks.push("compile_rust: rust test output");

                        eprintln!("  • compile_rust: rust test output");
                        eprintln!();
                        eprintln_indented(4, "", &format!("---- {} stdout ----", display_path));
                        eprintln_indented(4, "| ", &String::from_utf8_lossy(&output.stdout));
                        eprintln!();
                        eprintln_indented(4, "", &format!("---- {} stderr ----", display_path));
                        eprintln_indented(4, "| ", &String::from_utf8_lossy(&output.stderr));
                        eprintln!();
                    }
                }
                Err(error) => {
                    failed_checks.push("compile_rust: execute test");

                    eprintln!("  • compile_rust: execute test");
                    eprintln!();
                    eprintln_indented(4, "", &format!("---- {} error ----", display_path));
                    eprintln_indented(4, "| ", &error.to_string());
                    eprintln!();
                }
            }
        }
    }
}

fn compile_doc(
    snapshot_filename: &Path,
    failed_checks: &mut Vec<&str>,
    core_module: &ddl::core::Module,
) {
    let mut output = Vec::new();
    ddl::compile::doc::compile_module(&mut output, core_module).unwrap();

    if let Err(error) = snapshot::compare(&snapshot_filename.with_extension("md"), &output) {
        failed_checks.push("compile_doc: snapshot");

        eprintln!("  • compile_doc: snapshot");
        eprintln!();
        eprintln_indented(4, "", "---- snapshot error ----");
        eprintln_indented(4, "", &error.to_string());
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

fn eprintln_indented(indent: usize, prefix: &str, output: &str) {
    for line in output.lines() {
        eprintln!(
            "{space: >indent$}{prefix}{line}",
            space = "",
            indent = indent,
            prefix = prefix,
            line = line,
        );
    }
}
