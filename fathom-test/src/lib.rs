use codespan_reporting::diagnostic::{Diagnostic, LabelStyle, Severity};
use codespan_reporting::files::{Files, SimpleFiles};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{BufferWriter, ColorChoice};
use fathom::lang::FileId;
use fathom::pass::{core_to_pretty, core_to_surface, surface_to_core, surface_to_doc};
use libtest_mimic::{Outcome, Test};
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{env, fs};
use walkdir::WalkDir;

mod directives;
mod snapshot;

use self::directives::ExpectedDiagnostic;

lazy_static::lazy_static! {
    static ref GLOBALS: fathom::lang::core::Globals = fathom::lang::core::Globals::default();
}

/// Recursively walk over test files under a file path.
pub fn walk_files(root: impl AsRef<Path>) -> impl Iterator<Item = PathBuf> {
    WalkDir::new(root)
        .into_iter()
        .filter_map(|dir_entry| dir_entry.ok())
        .filter(|dir_entry| dir_entry.file_type().is_file())
        .map(|dir_entry| dir_entry.into_path())
}

pub enum TestData {
    Simple(PathBuf),
    Full(PathBuf),
}

pub fn extract_simple_test(path: PathBuf) -> Option<Test<TestData>> {
    if is_fathom_path(&path) {
        Some(Test {
            name: path.display().to_string(),
            kind: String::new(),
            is_ignored: false,
            is_bench: false,
            data: TestData::Simple(path),
        })
    } else {
        None
    }
}

pub fn extract_full_test(path: PathBuf) -> Option<Test<TestData>> {
    if is_fathom_path(&path) {
        Some(Test {
            name: path.display().to_string(),
            kind: String::new(),
            is_ignored: false,
            is_bench: false,
            data: TestData::Full(path),
        })
    } else {
        None
    }
}

pub fn run_test(
    pikelet_bin: &'static str,
) -> impl Fn(&Test<TestData>) -> Outcome + 'static + Send + Sync {
    move |test| match &test.data {
        TestData::Simple(format_file) => run_simple_test(pikelet_bin, test, format_file),
        TestData::Full(format_file) => run_full_test(pikelet_bin, test, format_file),
    }
}

fn run_simple_test(pikelet_bin: &str, test: &Test<TestData>, format_file: &Path) -> Outcome {
    let output = Command::new(pikelet_bin)
        .arg("check")
        .arg("--validate-core")
        .arg(format!("--format-file={}", format_file.display()))
        .output();

    let output = match output {
        Ok(output) => output,
        Err(error) => {
            return Outcome::Failed {
                msg: Some(format!("Error running {}: {}", pikelet_bin, error)),
            };
        }
    };

    if !output.status.success() || !output.stdout.is_empty() || !output.stderr.is_empty() {
        let mut msg = String::new();

        writeln!(msg).unwrap();
        writeln!(msg, "Errors encountered:").unwrap();
        writeln!(msg).unwrap();
        if !output.status.success() {
            writeln!(msg, "  • Unexpected status").unwrap();
        }
        if !output.stdout.is_empty() {
            writeln!(msg, "  • Unexpected stdout").unwrap();
        }
        if !output.stderr.is_empty() {
            writeln!(msg, "  • Unexpected stderr").unwrap();
        }

        if !output.status.success() {
            writeln!(msg).unwrap();
            writeln!(msg, "---- {} status ----", test.name).unwrap();
            writeln!(msg, "{}", output.status).unwrap();
        }
        if !output.stdout.is_empty() {
            writeln!(msg).unwrap();
            writeln!(msg, "---- {} stdout ----", test.name).unwrap();
            msg.push_str(String::from_utf8_lossy(&output.stdout).trim_end());
        }
        if !output.stderr.is_empty() {
            writeln!(msg).unwrap();
            writeln!(msg, "---- {} stderr ----", test.name).unwrap();
            msg.push_str(String::from_utf8_lossy(&output.stderr).trim_end());
        }

        return Outcome::Failed { msg: Some(msg) };
    }

    Outcome::Passed
}

fn run_full_test(_pikelet_bin: &str, test: &Test<TestData>, format_file: &Path) -> Outcome {
    let mut files = SimpleFiles::new();
    let term_config = term::Config::default();

    // Build snapshot file base
    // TODO: reconfigure snapshot directories to make this less cursed?
    let snapshot_file = PathBuf::from("tests/snapshots") // FIXME: blgergh
        .join(format_file.strip_prefix("tests/input").unwrap()) // FIXME: blgergh
        .with_extension("");

    // Load the format file
    let format_file_id = match fs::read_to_string(&format_file) {
        Ok(source) => files.add(format_file.display().to_string(), source),
        Err(error) => {
            let msg = format!("Failed to read `{}`: {}", format_file.display(), error);
            return Outcome::Failed { msg: Some(msg) };
        }
    };

    // Extract the directives from the source code

    let directives = {
        let directive_lexer = directives::Lexer::new(&files, format_file_id);
        let mut directive_parser = directives::Parser::new(&files, format_file_id);
        directive_parser.expect_directives(directive_lexer);
        let (directives, diagnostics) = directive_parser.finish();

        // Check for errors in the directives
        if !diagnostics.is_empty() {
            let mut msg = String::new();

            writeln!(msg).unwrap();
            writeln!(msg, "Failed to parse test directives:").unwrap();
            writeln!(msg).unwrap();

            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();
            for diagnostic in diagnostics {
                term::emit(&mut buffer, &term_config, &files, &diagnostic).unwrap();
            }

            writeln!(msg, "---- {} stderr ----", test.name).unwrap();
            msg.push_str(String::from_utf8_lossy(&buffer.as_slice()).trim_end());

            return Outcome::Failed { msg: Some(msg) };
        }

        directives
    };

    // Check if we should ignore the test
    if let Some(_reason) = directives.skip {
        return Outcome::Ignored;
    }

    // Run test stages

    let mut full_test = FullTest {
        files,
        term_config,
        format_file,
        format_file_id,
        snapshot_file,
        directives,
        failed_checks: Vec::new(),
        found_messages: Vec::new(),
    };

    let surface_module = full_test.parse_surface();
    full_test.compile_doc(&surface_module);
    let core_module = full_test.surface_to_core(&surface_module);
    full_test.roundtrip_surface_to_core(&core_module);
    full_test.roundtrip_core_to_pretty(&core_module);
    full_test.binary_parse_tests();

    full_test.finish()
}

struct FullTest<'a> {
    files: SimpleFiles<String, String>,
    term_config: codespan_reporting::term::Config,
    format_file: &'a Path,
    format_file_id: FileId,
    snapshot_file: PathBuf,
    directives: directives::Directives,
    failed_checks: Vec<&'static str>,
    found_messages: Vec<fathom::reporting::Message>,
}

impl<'a> FullTest<'a> {
    fn parse_surface(&mut self) -> fathom::lang::surface::Module {
        let file_id = self.format_file_id;
        let source = self.files.source(file_id).unwrap();
        fathom::lang::surface::Module::parse(file_id, source, &mut self.found_messages)
    }

    fn surface_to_core(
        &mut self,
        surface_module: &fathom::lang::surface::Module,
    ) -> fathom::lang::core::Module {
        let mut context = surface_to_core::Context::new(&GLOBALS);
        let core_module = context.from_module(&surface_module);
        self.found_messages.extend(context.drain_messages());

        // The core syntax from the elaborator should always be well-formed!
        let mut context = fathom::lang::core::typing::Context::new(&GLOBALS);
        context.is_module(&core_module);
        let validation_messages = context.drain_messages().collect::<Vec<_>>();

        if !validation_messages.is_empty() {
            self.failed_checks.push("surface_to_core: typing");

            let pretty_arena = pretty::Arena::new();
            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();

            for message in &validation_messages {
                let diagnostic = message.to_diagnostic(&pretty_arena);
                term::emit(&mut buffer, &self.term_config, &self.files, &diagnostic).unwrap();
            }

            eprintln!("  • surface_to_core: typing");
            eprintln!();
            eprintln!("    ---- found diagnostics ----");
            eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
            eprintln!();
        }

        core_module
    }

    fn roundtrip_surface_to_core(&mut self, core_module: &fathom::lang::core::Module) {
        let mut context = surface_to_core::Context::new(&GLOBALS);
        let mut core_to_surface_context = core_to_surface::Context::new();
        let surface_module = context.from_module(&core_to_surface_context.from_module(core_module));
        let elaboration_messages = context.drain_messages().collect::<Vec<_>>();

        if !elaboration_messages.is_empty() {
            self.failed_checks
                .push("roundtrip_surface_to_core: surface_to_core");

            let pretty_arena = pretty::Arena::new();
            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();

            for message in &elaboration_messages {
                let diagnostic = message.to_diagnostic(&pretty_arena);
                term::emit(&mut buffer, &self.term_config, &self.files, &diagnostic).unwrap();
            }

            eprintln!("  • roundtrip_surface_to_core: surface_to_core");
            eprintln!();
            eprintln!("    ---- found diagnostics ----");
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
            eprintln!("    ---- core ----");
            for line in pretty_core_module.lines() {
                eprintln_indented(4, "| ", line);
            }
            eprintln!();
            eprintln!("    ---- surface_to_core(core_to_surface(core)) ----");
            for line in pretty_surface_module.lines() {
                eprintln_indented(4, "| ", line);
            }
            eprintln!();
        }
    }

    fn roundtrip_core_to_pretty(&mut self, core_module: &fathom::lang::core::Module) {
        let arena = pretty::Arena::new();

        let pretty_core_module = {
            let pretty::DocBuilder(_, doc) = core_to_pretty::from_module(&arena, core_module);
            doc.pretty(100).to_string()
        };

        let snapshot_core_fathom_path = self.snapshot_file.with_extension("core.fathom");
        if let Err(error) =
            snapshot::compare(&snapshot_core_fathom_path, &pretty_core_module.as_bytes())
        {
            self.failed_checks.push("roundtrip_pretty_core: snapshot");

            eprintln!("  • roundtrip_pretty_core: snapshot");
            eprintln!();
            eprintln!("    ---- snapshot error ----");
            eprintln_indented(4, "", &error.to_string());
            eprintln!();
        }

        let mut core_parse_messages = Vec::new();
        let core_file_id = self.files.add(
            snapshot_core_fathom_path.display().to_string(),
            pretty_core_module.clone(),
        );
        let parsed_core_module = {
            let source = self.files.source(core_file_id).unwrap();
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
                term::emit(&mut buffer, &self.term_config, &self.files, &diagnostic).unwrap();
            }

            eprintln!("  • roundtrip_pretty_core: parse core");
            eprintln!();
            eprintln!("    ---- found diagnostics ----");
            eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
            eprintln!();
        }

        if parsed_core_module != *core_module {
            self.failed_checks
                .push("roundtrip_pretty_core: core != parse(pretty(core))");

            eprintln!("  • roundtrip_pretty_core: core != parse(pretty(core))");
            eprintln!();
            eprintln!("    ---- core ----");
            for line in pretty_core_module.lines() {
                eprintln_indented(4, "| ", line);
            }
            eprintln!();
            eprintln!("    ---- parse(pretty(core)) ----");
            for line in pretty_parsed_core_module.lines() {
                eprintln_indented(4, "| ", line);
            }
            eprintln!();
        }
    }

    fn binary_parse_tests(&mut self) {
        let rust_source_file = self.format_file.with_extension("rs");
        if !rust_source_file.exists() {
            return;
        }

        let dependencies = &["fathom-runtime", "fathom-test-util"];

        // Ensure that dependencies have been built prior to building the test
        for package_name in dependencies {
            Command::new(env!("CARGO"))
                .args(&["build", "--package", package_name])
                .output()
                .unwrap();
        }

        let test_name = "binary_parser_test";
        let target_dir = target_dir();
        let temp_dir = assert_fs::TempDir::new().unwrap();

        // Build the test harness
        let output = Command::new("rustc")
            .arg("--test")
            .arg("--edition=2018")
            .arg(format!("--out-dir={}", temp_dir.display()))
            .arg(format!("--crate-name={}", test_name))
            .args(&["-L", target_dir.join("deps").to_str().unwrap()])
            .args(dependencies.iter().flat_map(|package_name| {
                let name = heck::SnakeCase::to_snake_case(*package_name);
                let rlib = target_dir.join(format!("lib{}.rlib", name));
                vec!["--extern".into(), format!("{}={}", name, rlib.display())]
            }))
            .arg(&rust_source_file)
            .output();

        fn eprintln_output(name: &str, output: &std::process::Output) {
            if !output.status.success() {
                eprintln!("    ---- {} status ----", name);
                eprintln_indented(4, "| ", &output.status.to_string());
                eprintln!();
            }
            if !output.stdout.is_empty() {
                eprintln!("    ---- {} stdout ----", name);
                eprintln_indented(4, "| ", &String::from_utf8_lossy(&output.stdout));
                eprintln!();
            }
            if !output.stderr.is_empty() {
                eprintln!("    ---- {} stderr ----", name);
                eprintln_indented(4, "| ", &String::from_utf8_lossy(&output.stderr));
                eprintln!();
            }
        }

        let output = match output {
            Ok(output) => output,
            Err(error) => {
                self.failed_checks.push("binary_parse_tests: execute rustc");

                eprintln!("  • binary_parse_tests: execute rustc");
                eprintln!();
                eprintln!("    ---- rustc error ----");
                eprintln_indented(4, "", &error.to_string());
                eprintln!();

                return;
            }
        };

        if !output.status.success() || !output.stdout.is_empty() || !output.stderr.is_empty() {
            self.failed_checks
                .push("binary_parse_tests: rust compile output");

            eprintln!("  • binary_parse_tests: rust compile output");
            eprintln!();
            eprintln_output("rustc", &output);

            return;
        }

        // Run the test harness

        let test_exe = temp_dir
            .join(test_name)
            .with_extension(env::consts::EXE_SUFFIX);

        let output = match Command::new(&test_exe).arg("--color=always").output() {
            Ok(output) => output,
            Err(error) => {
                self.failed_checks.push("binary_parse_tests: execute test");

                eprintln!("  • binary_parse_tests: execute test");
                eprintln!();
                eprintln!("    ---- {} error ----", test_exe.display());
                eprintln_indented(4, "| ", &error.to_string());
                eprintln!();

                return;
            }
        };

        if !output.status.success() || !output.stderr.is_empty() {
            self.failed_checks
                .push("binary_parse_tests: rust test output");

            eprintln!("  • binary_parse_tests: rust test output");
            eprintln!();
            eprintln_output(test_exe.to_str().unwrap(), &output);

            return;
        }
    }

    fn compile_doc(&mut self, surface_module: &fathom::lang::surface::Module) {
        let mut output = Vec::new();
        surface_to_doc::Context::new()
            .from_module(&mut output, surface_module)
            .unwrap();

        if let Err(error) = snapshot::compare(&self.snapshot_file.with_extension("html"), &output) {
            self.failed_checks.push("compile_doc: snapshot");

            eprintln!("  • compile_doc: snapshot");
            eprintln!();
            eprintln!("    ---- snapshot error ----");
            eprintln_indented(4, "", &error.to_string());
            eprintln!();
        }
    }

    fn finish(mut self) -> Outcome {
        // Ensure that no unexpected diagnostics and no expected diagnostics remain

        let pretty_arena = pretty::Arena::new();
        let mut found_diagnostics = self
            .found_messages
            .iter()
            .map(|message| message.to_diagnostic(&pretty_arena))
            .collect();

        retain_unexpected(
            &self.files,
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
                term::emit(&mut buffer, &self.term_config, &self.files, diagnostic).unwrap();
            }

            eprintln!("    ---- found diagnostics ----");
            eprintln_indented(4, "| ", &String::from_utf8_lossy(buffer.as_slice()));
            eprintln!();
        }

        if !self.directives.expected_diagnostics.is_empty() {
            self.failed_checks.push("expected_diagnostics");

            eprintln!("Expected diagnostics not found:");
            eprintln!();

            eprintln!("    ---- expected diagnostics ----");
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
                    self.format_file.display(),
                    expected.location.line_number,
                    severity,
                    expected.pattern,
                );
            }

            eprintln!();
        }

        if !self.failed_checks.is_empty() {
            let mut msg = String::new();

            writeln!(msg, "failed {} checks:", self.failed_checks.len()).unwrap();
            for check in self.failed_checks {
                writeln!(msg, "    • {}", check).unwrap();
            }

            return Outcome::Failed { msg: Some(msg) };
        }

        Outcome::Passed
    }
}

/// Returns the target directory for the test binary
// Adapted from: https://github.com/rust-lang/cargo/blob/485670b3983b52289a2f353d589c57fae2f60f82/tests/testsuite/support/mod.rs#L507-L524
fn target_dir() -> PathBuf {
    env::current_exe()
        .map(|mut path| {
            path.pop();
            if path.ends_with("deps") {
                path.pop();
            }
            path
        })
        .unwrap()
}

fn is_fathom_path(path: &Path) -> bool {
    matches!(path.extension(), Some(ext) if ext == "fathom")
}

fn retain_unexpected(
    files: &SimpleFiles<String, String>,
    found_diagnostics: &mut Vec<Diagnostic<FileId>>,
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
    files: &SimpleFiles<String, String>,
    found_diagnostic: &Diagnostic<FileId>,
    expected_diagnostic: &ExpectedDiagnostic,
) -> bool {
    // TODO: higher quality diagnostic message matching
    found_diagnostic.labels.iter().any(|label| {
        label.style == LabelStyle::Primary && label.file_id == expected_diagnostic.file_id && {
            let found_line_index = files.line_index(label.file_id, label.range.start).unwrap();
            let found_message = &found_diagnostic.message;

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
