use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{Diagnostic, Severity};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{BufferWriter, ColorChoice, StandardStream};
use std::fs;
use std::path::PathBuf;
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
    static ref CARGO_DDL_TEST_UTIL_RLIB: PathBuf = CARGO_TARGET_DIR.join("debug").join("libddl_test_util.rlib");

    static ref INPUT_DIR: PathBuf = CARGO_WORKSPACE_ROOT.join("tests").join("input");
    static ref SNAPSHOTS_DIR: PathBuf = CARGO_WORKSPACE_ROOT.join("tests").join("snapshots");
}

pub fn run_integration_test(test_name: &str, ddl_path: &str) {
    let mut files = Files::new();
    let mut test = Test::setup(&mut files, test_name, ddl_path);

    // Run stages

    eprintln!();

    // SKIP
    if let Some(reason) = &test.directives.skip {
        eprintln!("Skipped: {}", reason);
        return;
    }

    let surface_module = test.parse_surface(&files);
    let core_module = test.elaborate(&files, &surface_module);
    test.roundtrip_delaborate_core(&files, &core_module);
    test.roundtrip_pretty_core(&mut files, &core_module);
    test.compile_rust(&core_module);
    test.compile_doc(&core_module);

    test.finish(&files);
}

struct Test {
    test_name: String,
    term_config: codespan_reporting::term::Config,
    input_ddl_path: PathBuf,
    input_ddl_file_id: FileId,
    snapshot_filename: PathBuf,
    directives: directives::Directives,
    failed_checks: Vec<&'static str>,
    found_diagnostics: Vec<Diagnostic>,
}

impl Test {
    fn setup(files: &mut Files, test_name: &str, ddl_path: &str) -> Test {
        // Set up output streams

        let term_config = term::Config::default();
        let stdout = StandardStream::stdout(ColorChoice::Auto);

        // Set up files

        let input_ddl_path = INPUT_DIR.join(ddl_path);
        let snapshot_filename = SNAPSHOTS_DIR.join(ddl_path).with_extension("");
        let source = fs::read_to_string(&input_ddl_path).unwrap_or_else(|error| {
            panic!("error reading `{}`: {}", input_ddl_path.display(), error)
        });
        let input_ddl_file_id = files.add(input_ddl_path.display().to_string(), source);

        // Extract the directives from the source code

        let directives = {
            let (directives, diagnostics) = {
                let lexer = directives::Lexer::new(&files, input_ddl_file_id);
                let mut parser = directives::Parser::new(&files, input_ddl_file_id);
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

        Test {
            test_name: test_name.to_owned(),
            term_config,
            input_ddl_path,
            input_ddl_file_id,
            snapshot_filename,
            directives,
            failed_checks: Vec::new(),
            found_diagnostics: Vec::new(),
        }
    }

    fn parse_surface(&mut self, files: &Files) -> ddl::surface::Module {
        let keywords = &ddl::lexer::SURFACE_KEYWORDS;
        let lexer = ddl::lexer::Lexer::new(files, self.input_ddl_file_id, keywords);
        ddl::surface::Module::parse(self.input_ddl_file_id, lexer, &mut |d| {
            self.found_diagnostics.push(d)
        })
    }

    fn elaborate(
        &mut self,
        files: &Files,
        surface_module: &ddl::surface::Module,
    ) -> ddl::core::Module {
        let core_module = ddl::surface::elaborate::elaborate_module(&surface_module, &mut |d| {
            self.found_diagnostics.push(d)
        });

        // The core syntax from the elaborator should always be well-formed!
        let mut validation_diagnostics = Vec::new();
        ddl::core::validate::validate_module(&core_module, &mut |d| validation_diagnostics.push(d));
        if !validation_diagnostics.is_empty() {
            self.failed_checks.push("elaborate: validate");

            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();
            for diagnostic in &validation_diagnostics {
                term::emit(&mut buffer, &self.term_config, files, diagnostic).unwrap();
            }

            eprintln!("  • elaborate: validate");
            eprintln!();
            eprintln_indented(4, "", "---- found diagnostics ----");
            eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
            eprintln!();
        }

        core_module
    }

    fn roundtrip_delaborate_core(&mut self, files: &Files, core_module: &ddl::core::Module) {
        let mut elaboration_diagnostics = Vec::new();
        let delaborated_core_module = ddl::surface::elaborate::elaborate_module(
            &ddl::surface::delaborate::delaborate_module(core_module),
            &mut |d| elaboration_diagnostics.push(d),
        );

        if !elaboration_diagnostics.is_empty() {
            self.failed_checks
                .push("roundtrip_delaborate_core: elaborate surface");

            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();
            for diagnostic in &elaboration_diagnostics {
                term::emit(&mut buffer, &self.term_config, files, diagnostic).unwrap();
            }

            eprintln!("  • roundtrip_delaborate_core: elaborate surface");
            eprintln!();
            eprintln_indented(4, "", "---- found diagnostics ----");
            eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
            eprintln!();
        }

        if delaborated_core_module != *core_module {
            let arena = pretty::Arena::new();

            let pretty_core_module = {
                let pretty::DocBuilder(_, doc) = core_module.doc(&arena);
                doc.pretty(100).to_string()
            };
            let pretty_delaborated_core_module = {
                let pretty::DocBuilder(_, doc) = delaborated_core_module.doc(&arena);
                doc.pretty(100).to_string()
            };

            self.failed_checks
                .push("roundtrip_delaborate_core: core != elaborate(delaborate(core))");

            eprintln!("  • roundtrip_delaborate_core: core != elaborate(delaborate(core))");
            eprintln!();
            eprintln_indented(4, "", "---- core ----");
            for line in pretty_core_module.lines() {
                eprintln_indented(4, "| ", line);
            }
            eprintln!();
            eprintln_indented(4, "", "---- elaborate(delaborate(core)) ----");
            for line in pretty_delaborated_core_module.lines() {
                eprintln_indented(4, "| ", line);
            }
            eprintln!();
        }
    }

    fn roundtrip_pretty_core(&mut self, files: &mut Files, core_module: &ddl::core::Module) {
        let arena = pretty::Arena::new();

        let pretty_core_module = {
            let pretty::DocBuilder(_, doc) = core_module.doc(&arena);
            doc.pretty(100).to_string()
        };

        let snapshot_core_ddl_path = self.snapshot_filename.with_extension("core.ddl");
        if let Err(error) =
            snapshot::compare(&snapshot_core_ddl_path, &pretty_core_module.as_bytes())
        {
            self.failed_checks.push("roundtrip_pretty_core: snapshot");

            eprintln!("  • roundtrip_pretty_core: snapshot");
            eprintln!();
            eprintln_indented(4, "", "---- snapshot error ----");
            eprintln_indented(4, "", &error.to_string());
            eprintln!();
        }

        let mut core_parse_diagnostics = Vec::new();
        let core_file_id = files.add(
            snapshot_core_ddl_path.display().to_string(),
            pretty_core_module.clone(),
        );
        let parsed_core_module = {
            let keywords = &ddl::lexer::CORE_KEYWORDS;
            let lexer = ddl::lexer::Lexer::new(files, core_file_id, keywords);
            ddl::core::Module::parse(core_file_id, lexer, &mut |d| core_parse_diagnostics.push(d))
        };
        let pretty_parsed_core_module = {
            let pretty::DocBuilder(_, doc) = parsed_core_module.doc(&arena);
            doc.pretty(100).to_string()
        };

        if !core_parse_diagnostics.is_empty() {
            self.failed_checks.push("roundtrip_pretty_core: parse core");

            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();
            for diagnostic in &core_parse_diagnostics {
                term::emit(&mut buffer, &self.term_config, files, diagnostic).unwrap();
            }

            eprintln!("  • roundtrip_pretty_core: parse core");
            eprintln!();
            eprintln_indented(4, "", "---- found diagnostics ----");
            eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
            eprintln!();
        }

        if *core_module != parsed_core_module {
            self.failed_checks
                .push("roundtrip_pretty_core: core != parse(pretty(core))");

            eprintln!("  • roundtrip_pretty_core: core != parse(pretty(core))");
            eprintln!();
            eprintln_indented(4, "", "---- core ----");
            for line in pretty_core_module.lines() {
                eprintln_indented(4, "| ", line);
            }
            eprintln!();
            eprintln_indented(4, "", "---- parse(pretty(core)) ----");
            for line in pretty_parsed_core_module.lines() {
                eprintln_indented(4, "| ", line);
            }
            eprintln!();
        }
    }

    fn compile_rust(&mut self, core_module: &ddl::core::Module) {
        let mut output = Vec::new();
        let rust_module = ddl::core::compile::rust::compile_module(core_module, &mut |d| {
            self.found_diagnostics.push(d);
        });
        ddl::rust::emit::emit_module(&mut output, &rust_module).unwrap();
        let snapshot_rs_path = self.snapshot_filename.with_extension("rs");

        if let Err(error) = snapshot::compare(&snapshot_rs_path, &output) {
            self.failed_checks.push("compile_rust: snapshot");

            eprintln!("  • compile_rust: snapshot");
            eprintln!();
            eprintln_indented(4, "", "---- snapshot error ----");
            eprintln_indented(4, "", &error.to_string());
            eprintln!();
        } else {
            // Test compiled output against rustc
            let temp_dir = assert_fs::TempDir::new().unwrap();

            let (rs_path, is_binary_parse_test) = match &self.input_ddl_path.with_extension("rs") {
                input_rs_path if input_rs_path.exists() => (input_rs_path.clone(), true),
                _ => (snapshot_rs_path, false),
            };

            let mut rustc_command = Command::new("rustc");

            rustc_command
                .arg(format!("--out-dir={}", temp_dir.path().display()))
                .arg(format!("--crate-name={}", self.test_name))
                .arg("--test")
                .arg("--color=always")
                .arg("--edition=2018")
                // Manually pass shared cargo directories
                .arg("-C")
                .arg(format!("incremental={}", CARGO_INCREMENTAL_DIR.display()))
                .arg("-L")
                .arg(format!("dependency={}", CARGO_DEPS_DIR.display()))
                // Add `ddl-rt` to the dependencies
                .arg("--extern")
                .arg(format!("ddl_rt={}", CARGO_DDL_RT_RLIB.display()));

            // Ensure that ddl-rt is present at `CARGO_DDL_RT_RLIB`
            Command::new(env!("CARGO"))
                .arg("build")
                .arg("--package=ddl-rt")
                .output()
                .unwrap();

            if is_binary_parse_test {
                // Ensure that ddl-test-util is present at `CARGO_DDL_TEST_UTIL_RLIB`
                Command::new(env!("CARGO"))
                    .arg("build")
                    .arg("--package=ddl-test-util")
                    .output()
                    .unwrap();

                // Add `ddl-test-util` to the dependencies
                rustc_command.arg("--extern").arg(format!(
                    "ddl_test_util={}",
                    CARGO_DDL_TEST_UTIL_RLIB.display(),
                ));
            }

            match rustc_command.arg(&rs_path).output() {
                Ok(output) => {
                    if !output.status.success()
                        || !output.stdout.is_empty()
                        || !output.stderr.is_empty()
                    {
                        self.failed_checks.push("compile_rust: rust compile output");

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
                    self.failed_checks.push("compile_rust: execute rustc");

                    eprintln!("  • compile_rust: execute rustc");
                    eprintln!();
                    eprintln_indented(4, "", "---- rustc error ----");
                    eprintln_indented(4, "", &error.to_string());
                    eprintln!();
                }
            }

            // Run binary parse tests
            if is_binary_parse_test {
                let test_path = temp_dir.path().join(&self.test_name);
                let display_path = test_path.display();
                match Command::new(&test_path).arg("--color=always").output() {
                    Ok(output) => {
                        if !output.status.success() {
                            self.failed_checks.push("compile_rust: rust test output");

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
                        self.failed_checks.push("compile_rust: execute test");

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

    fn compile_doc(&mut self, core_module: &ddl::core::Module) {
        let mut output = Vec::new();
        ddl::core::compile::doc::compile_module(&mut output, core_module, &mut |d| {
            self.found_diagnostics.push(d)
        })
        .unwrap();

        if let Err(error) =
            snapshot::compare(&self.snapshot_filename.with_extension("html"), &output)
        {
            self.failed_checks.push("compile_doc: snapshot");

            eprintln!("  • compile_doc: snapshot");
            eprintln!();
            eprintln_indented(4, "", "---- snapshot error ----");
            eprintln_indented(4, "", &error.to_string());
            eprintln!();
        }
    }

    fn finish(mut self, files: &Files) {
        // Ensure that no unexpected diagnostics and no expected diagnostics remain

        retain_unexpected(
            files,
            &mut self.found_diagnostics,
            &mut self.directives.expected_diagnostics,
        );

        if !self.found_diagnostics.is_empty() {
            self.failed_checks.push("unexpected_diagnostics");

            eprintln!("Unexpected diagnostics found:");
            eprintln!();

            // Use a buffer so that this doesn't get printed interleaved with the
            // test status output.

            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();
            for diagnostic in &self.found_diagnostics {
                term::emit(&mut buffer, &self.term_config, files, diagnostic).unwrap();
            }

            eprintln_indented(4, "", "---- found diagnostics ----");
            eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
            eprintln!();
        }

        if !self.directives.expected_diagnostics.is_empty() {
            self.failed_checks.push("expected_diagnostics");

            eprintln!("Expected diagnostics not found:");
            eprintln!();

            eprintln_indented(4, "", "---- expected diagnostics ----");
            for expected in &self.directives.expected_diagnostics {
                let severity = match expected.severity {
                    Severity::Bug => "bug",
                    Severity::Error => "error",
                    Severity::Warning => "warning",
                    Severity::Note => "note",
                    Severity::Help => "help",
                };

                eprintln!(
                    "    | {}:{}: {}: {}",
                    self.input_ddl_path.display(),
                    expected.line.number(),
                    severity,
                    expected.pattern,
                );
            }

            eprintln!();
        }

        if !self.failed_checks.is_empty() {
            eprintln!("failed {} checks:", self.failed_checks.len());
            for check in self.failed_checks {
                eprintln!("    {}", check);
            }
            eprintln!();

            panic!("failed {}", self.test_name);
        }
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
