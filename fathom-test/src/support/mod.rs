use codespan_reporting::diagnostic::{Diagnostic, LabelStyle, Severity};
use codespan_reporting::files::{Files, SimpleFiles};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{BufferWriter, ColorChoice, StandardStream};
use fathom::driver::compiler;
use fathom::pass::{core_to_pretty, core_to_surface, surface_to_core, surface_to_doc};
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
    static ref CARGO_FATHOM_RT_RLIB: PathBuf = CARGO_TARGET_DIR.join("debug").join("libfathom_runtime.rlib");
    static ref CARGO_FATHOM_TEST_UTIL_RLIB: PathBuf = CARGO_TARGET_DIR.join("debug").join("libfathom_test_util.rlib");

    static ref INPUT_DIR: PathBuf = CARGO_WORKSPACE_ROOT.join("tests").join("input");
    static ref SNAPSHOTS_DIR: PathBuf = CARGO_WORKSPACE_ROOT.join("tests").join("snapshots");
    static ref GLOBALS: fathom::lang::core::Globals = fathom::lang::core::Globals::default();

}

pub fn run_integration_test(test_name: &str, fathom_path: &str) {
    let mut test_files = SimpleFiles::new();
    let mut compiler_files = SimpleFiles::new();
    let mut test = Test::setup(&mut test_files, test_name, fathom_path);

    let mut compiler = compiler::Compiler::setup(
        &mut compiler_files,
        test_name,
        INPUT_DIR.join(fathom_path).to_str().unwrap(),
    );

    // Run stages

    eprintln!();

    // SKIP
    if let Some(reason) = &test.directives.skip {
        eprintln!("Skipped: {}", reason);
        return;
    }

    let surface_module = compiler.parse_surface(&compiler_files);
    test.compile_doc(&surface_module);
    let core_module = compiler.elaborate(&compiler_files, &surface_module);
    test.roundtrip_surface_to_core(&test_files, &core_module);
    test.roundtrip_pretty_core(&mut test_files, &core_module);
    test.binary_parse_tests();

    test.finish(&test_files, &compiler.found_messages);
}

struct Test {
    test_name: String,
    term_config: codespan_reporting::term::Config,
    input_fathom_path: PathBuf,
    snapshot_filename: PathBuf,
    directives: directives::Directives,
    failed_checks: Vec<&'static str>,
}

impl Test {
    fn setup(files: &mut SimpleFiles<String, String>, test_name: &str, fathom_path: &str) -> Test {
        // Set up output streams

        let term_config = term::Config::default();
        let stdout = StandardStream::stdout(ColorChoice::Auto);

        // Set up files

        let input_fathom_path = INPUT_DIR.join(fathom_path);
        let snapshot_filename = SNAPSHOTS_DIR.join(fathom_path).with_extension("");
        let source = fs::read_to_string(&input_fathom_path).unwrap_or_else(|error| {
            panic!("error reading `{}`: {}", input_fathom_path.display(), error)
        });
        let input_fathom_file_id = files.add(input_fathom_path.display().to_string(), source);

        // Extract the directives from the source code.  Directives
        // are the annotations on test inputs such as "// ~error:..."
        // to express that the test case expects a particular error.
        let directives = {
            let (directives, diagnostics) = {
                let lexer = directives::Lexer::new(&files, input_fathom_file_id);
                let mut parser = directives::Parser::new(&files, input_fathom_file_id);
                parser.expect_directives(lexer);
                parser.finish()
            };

            if !diagnostics.is_empty() {
                let writer = &mut stdout.lock();
                for diagnostic in diagnostics {
                    term::emit(writer, &term_config, files, &diagnostic).unwrap();
                }

                panic!("failed to parse diagnostics");
            }

            directives
        };

        Test {
            test_name: test_name.to_owned(),
            term_config,
            input_fathom_path,
            snapshot_filename,
            directives,
            failed_checks: Vec::new(),
        }
    }

    fn roundtrip_surface_to_core(
        &mut self,
        files: &SimpleFiles<String, String>,
        core_module: &fathom::lang::core::Module,
    ) {
        let mut context = surface_to_core::Context::new(&GLOBALS);
        let mut core_to_surface_context = core_to_surface::Context::new();
        let surface_module = context.from_module(&core_to_surface_context.from_module(core_module));
        let elaboration_messages = context.drain_messages().collect::<Vec<_>>();

        if !elaboration_messages.is_empty() {
            self.failed_checks
                .push("roundtrip_surface_to_core: elaborate surface");

            let pretty_arena = pretty::Arena::new();
            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();

            for message in &elaboration_messages {
                let diagnostic = message.to_diagnostic(&pretty_arena);
                term::emit(&mut buffer, &self.term_config, files, &diagnostic).unwrap();
            }

            eprintln!("  • roundtrip_surface_to_core: elaborate surface");
            eprintln!();
            eprintln_indented(4, "", "---- found diagnostics ----");
            eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
            eprintln!();
        }

        if surface_module != *core_module {
            let arena = pretty::Arena::new();

            let pretty_core_module = {
                let pretty::DocBuilder(_, doc) = core_to_pretty::from_module(&arena, core_module);
                doc.pretty(100).to_string()
            };
            let pretty_surface_module = {
                let pretty::DocBuilder(_, doc) =
                    core_to_pretty::from_module(&arena, &surface_module);
                doc.pretty(100).to_string()
            };

            self.failed_checks
                .push("roundtrip_surface_to_core: core != surface_to_core(core_to_surface(core))");

            eprintln!(
                "  • roundtrip_surface_to_core: core != surface_to_core(core_to_surface(core))",
            );
            eprintln!();
            eprintln_indented(4, "", "---- core ----");
            for line in pretty_core_module.lines() {
                eprintln_indented(4, "| ", line);
            }
            eprintln!();
            eprintln_indented(4, "", "---- surface_to_core(core_to_surface(core)) ----");
            for line in pretty_surface_module.lines() {
                eprintln_indented(4, "| ", line);
            }
            eprintln!();
        }
    }

    fn roundtrip_pretty_core(
        &mut self,
        files: &mut SimpleFiles<String, String>,
        core_module: &fathom::lang::core::Module,
    ) {
        let arena = pretty::Arena::new();

        let pretty_core_module = {
            let pretty::DocBuilder(_, doc) = core_to_pretty::from_module(&arena, core_module);
            doc.pretty(100).to_string()
        };

        let snapshot_core_fathom_path = self.snapshot_filename.with_extension("core.fathom");
        if let Err(error) =
            snapshot::compare(&snapshot_core_fathom_path, &pretty_core_module.as_bytes())
        {
            self.failed_checks.push("roundtrip_pretty_core: snapshot");

            eprintln!("  • roundtrip_pretty_core: snapshot");
            eprintln!();
            eprintln_indented(4, "", "---- snapshot error ----");
            eprintln_indented(4, "", &error.to_string());
            eprintln!();
        }

        let mut core_parse_messages = Vec::new();
        let core_file_id = files.add(
            snapshot_core_fathom_path.display().to_string(),
            pretty_core_module.clone(),
        );
        let parsed_core_module = {
            let source = files.source(core_file_id).unwrap();
            fathom::lang::core::Module::parse(core_file_id, source, &mut core_parse_messages)
        };
        let pretty_parsed_core_module = {
            let pretty::DocBuilder(_, doc) =
                core_to_pretty::from_module(&arena, &parsed_core_module);
            doc.pretty(100).to_string()
        };

        if !core_parse_messages.is_empty() {
            self.failed_checks.push("roundtrip_pretty_core: parse core");

            let pretty_arena = pretty::Arena::new();
            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();

            for message in &core_parse_messages {
                let diagnostic = message.to_diagnostic(&pretty_arena);
                term::emit(&mut buffer, &self.term_config, files, &diagnostic).unwrap();
            }

            eprintln!("  • roundtrip_pretty_core: parse core");
            eprintln!();
            eprintln_indented(4, "", "---- found diagnostics ----");
            eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
            eprintln!();
        }

        if parsed_core_module != *core_module {
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

    fn binary_parse_tests(&mut self) {
        // Test compiled output against rustc
        let temp_dir = assert_fs::TempDir::new().unwrap();

        let rs_path = match &self.input_fathom_path.with_extension("rs") {
            input_rs_path if input_rs_path.exists() => input_rs_path.clone(),
            _ => return,
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
            // Add `fathom-runtime` to the dependencies
            .arg("--extern")
            .arg(format!("fathom_runtime={}", CARGO_FATHOM_RT_RLIB.display()));

        // Ensure that fathom-runtime is present at `CARGO_FATHOM_RT_RLIB`
        Command::new(env!("CARGO"))
            .arg("build")
            .arg("--package=fathom-runtime")
            .output()
            .unwrap();

        // Ensure that fathom-test-util is present at `CARGO_FATHOM_TEST_UTIL_RLIB`
        Command::new(env!("CARGO"))
            .arg("build")
            .arg("--package=fathom-test-util")
            .output()
            .unwrap();

        // Add `fathom-test-util` to the dependencies
        rustc_command.arg("--extern").arg(format!(
            "fathom_test_util={}",
            CARGO_FATHOM_TEST_UTIL_RLIB.display(),
        ));

        match rustc_command.arg(&rs_path).output() {
            Ok(output) => {
                if !output.status.success()
                    || !output.stdout.is_empty()
                    || !output.stderr.is_empty()
                {
                    self.failed_checks
                        .push("binary_parse_tests: rust compile output");

                    eprintln!("  • binary_parse_tests: rust compile output");
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
                self.failed_checks.push("binary_parse_tests: execute rustc");

                eprintln!("  • binary_parse_tests: execute rustc");
                eprintln!();
                eprintln_indented(4, "", "---- rustc error ----");
                eprintln_indented(4, "", &error.to_string());
                eprintln!();
            }
        }

        let test_path = temp_dir.path().join(&self.test_name);
        let display_path = test_path.display();
        match Command::new(&test_path).arg("--color=always").output() {
            Ok(output) => {
                if !output.status.success() {
                    self.failed_checks
                        .push("binary_parse_tests: rust test output");

                    eprintln!("  • binary_parse_tests: rust test output");
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
                self.failed_checks.push("binary_parse_tests: execute test");

                eprintln!("  • binary_parse_tests: execute test");
                eprintln!();
                eprintln_indented(4, "", &format!("---- {} error ----", display_path));
                eprintln_indented(4, "| ", &error.to_string());
                eprintln!();
            }
        }
    }

    fn compile_doc(&mut self, surface_module: &fathom::lang::surface::Module) {
        let mut output = Vec::new();
        surface_to_doc::Context::new()
            .from_module(&mut output, surface_module)
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

    fn finish(
        mut self,
        files: &SimpleFiles<String, String>,
        found_messages: &Vec<fathom::reporting::Message>,
    ) {
        // Ensure that no unexpected diagnostics and no expected diagnostics remain

        let pretty_arena = pretty::Arena::new();
        let mut found_diagnostics = found_messages
            .iter()
            .map(|message| message.to_diagnostic(&pretty_arena))
            .collect();

        retain_unexpected(
            files,
            &mut found_diagnostics,
            &mut self.directives.expected_diagnostics,
        );

        if !found_diagnostics.is_empty() {
            self.failed_checks.push("unexpected_diagnostics");

            eprintln!("Unexpected diagnostics found:");
            eprintln!();

            // Use a buffer so that this doesn't get printed interleaved with the
            // test status output.

            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();
            for diagnostic in &found_diagnostics {
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
                    self.input_fathom_path.display(),
                    expected.location.line_number,
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
    files: &SimpleFiles<String, String>,
    found_diagnostics: &mut Vec<Diagnostic<usize>>,
    expected_diagnostics: &mut Vec<ExpectedDiagnostic>,
) {
    use std::collections::BTreeSet;

    let mut found_removals = BTreeSet::new();
    let mut expected_removals = BTreeSet::new();

    eprintln!("found_diagnostics: {}", found_diagnostics.len());
    eprintln!("expected_diagnostics: {}", expected_diagnostics.len());

    for (found_index, found_diagnostic) in found_diagnostics.iter().enumerate() {
        for (expected_index, expected_diagnostic) in expected_diagnostics.iter().enumerate() {
            if is_expected(files, found_diagnostic, expected_diagnostic) {
                found_removals.insert(found_index);
                expected_removals.insert(expected_index);
            }
        }
    }

    for index in found_removals.clone().into_iter().rev() {
        found_diagnostics.remove(index);
    }

    for index in expected_removals.clone().into_iter().rev() {
        expected_diagnostics.remove(index);
    }

    eprintln!("found_removals: {}", found_removals.len());
    eprintln!("expected_removals: {}", expected_removals.len());
}

fn is_expected(
    files: &SimpleFiles<String, String>,
    found_diagnostic: &Diagnostic<usize>,
    expected_diagnostic: &ExpectedDiagnostic,
) -> bool {
    // TODO: higher quality diagnostic message matching
    eprintln!("is_expected");
    eprintln!("{:?}", found_diagnostic.labels);
    eprintln!("{:?}", expected_diagnostic);
    found_diagnostic.labels.iter().any(|label| {
        label.style == LabelStyle::Primary && label.file_id == expected_diagnostic.file_id && {
            let found_line_index = files.line_index(label.file_id, label.range.start).unwrap();
            let found_message = &found_diagnostic.message;

            if !(found_line_index == expected_diagnostic.line_index
                && found_diagnostic.severity == expected_diagnostic.severity
                && expected_diagnostic.pattern.is_match(found_message))
            {
                eprintln!("danger, not equal!");
                if found_line_index != expected_diagnostic.line_index {
                    eprintln!("it's the line_index")
                }
                if found_diagnostic.severity != expected_diagnostic.severity {
                    eprintln!("it's the severity")
                }
                if !expected_diagnostic.pattern.is_match(found_message) {
                    eprintln!("it's the pattern!")
                }
            }

            found_line_index == expected_diagnostic.line_index
                && found_diagnostic.severity == expected_diagnostic.severity
                && expected_diagnostic.pattern.is_match(found_message)
        }
    })
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
