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
    fathom_exe: &'static str,
) -> impl Fn(&Test<TestData>) -> Outcome + 'static + Send + Sync {
    move |test| match &test.data {
        TestData::Simple(format_file) => run_simple_test(fathom_exe, format_file),
        TestData::Full(format_file) => run_full_test(fathom_exe, format_file),
    }
}

fn run_simple_test(fathom_exe: &str, format_file: &Path) -> Outcome {
    let output = Command::new(fathom_exe)
        .arg("check")
        .arg("--validate-core")
        .arg(format!("--format-file={}", format_file.display()))
        .output();

    let mut failures = Vec::new();

    match output {
        Ok(output) => {
            if !output.status.success() || !output.stdout.is_empty() || !output.stderr.is_empty() {
                failures.push(Failure {
                    name: "unexpected command output",
                    details: process_output_details("fathom", &output),
                });
            }
        }
        Err(error) => {
            failures.push(Failure {
                name: "unexpected command error",
                details: vec![("std::io::Error".to_owned(), error.to_string())],
            });
        }
    }

    check_failures(&failures)
}

fn run_full_test(_fathom_exe: &str, format_file: &Path) -> Outcome {
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
            let msg = format!("failed to read `{}`: {}", format_file.display(), error);
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
            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();
            for diagnostic in diagnostics {
                term::emit(&mut buffer, &term_config, &files, &diagnostic).unwrap();
            }

            return check_failures(&[Failure {
                name: "parse test directives",
                details: vec![(
                    "diagnostics".to_owned(),
                    String::from_utf8_lossy(&buffer.as_slice()).into(),
                )],
            }]);
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
        expected_diagnostics: directives.expected_diagnostics,
        failures: Vec::new(),
        found_messages: Vec::new(),
    };

    let surface_module = full_test.parse_surface();
    full_test.compile_doc(&surface_module);
    let core_module = full_test.surface_to_core(&surface_module);
    full_test.roundtrip_surface_to_core(&core_module);
    full_test.roundtrip_core_to_pretty(&core_module);
    full_test.binary_parse_tests();
    full_test.check_diagnostics();

    // Check test failures

    check_failures(&full_test.failures)
}

struct FullTest<'a> {
    files: SimpleFiles<String, String>,
    term_config: codespan_reporting::term::Config,
    format_file: &'a Path,
    format_file_id: FileId,
    snapshot_file: PathBuf,
    expected_diagnostics: Vec<directives::ExpectedDiagnostic>,
    failures: Vec<Failure>,
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
            let pretty_arena = pretty::Arena::new();
            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();

            for message in &validation_messages {
                let diagnostic = message.to_diagnostic(&pretty_arena);
                term::emit(&mut buffer, &self.term_config, &self.files, &diagnostic).unwrap();
            }

            self.failures.push(Failure {
                name: "surface_to_core: typing",
                details: vec![(
                    "diagnostics".to_owned(),
                    String::from_utf8_lossy(&buffer.as_slice()).into(),
                )],
            });
        }

        core_module
    }

    fn roundtrip_surface_to_core(&mut self, core_module: &fathom::lang::core::Module) {
        let mut context = surface_to_core::Context::new(&GLOBALS);
        let mut core_to_surface_context = core_to_surface::Context::new();
        let surface_module = context.from_module(&core_to_surface_context.from_module(core_module));
        let elaboration_messages = context.drain_messages().collect::<Vec<_>>();

        if !elaboration_messages.is_empty() {
            let pretty_arena = pretty::Arena::new();
            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();

            for message in &elaboration_messages {
                let diagnostic = message.to_diagnostic(&pretty_arena);
                term::emit(&mut buffer, &self.term_config, &self.files, &diagnostic).unwrap();
            }

            self.failures.push(Failure {
                name: "roundtrip_surface_to_core: surface_to_core",
                details: vec![(
                    "diagnostics".to_owned(),
                    String::from_utf8_lossy(&buffer.as_slice()).into(),
                )],
            });
        }

        if surface_module != *core_module {
            let arena = pretty::Arena::new();

            self.failures.push(Failure {
                name: "roundtrip_surface_to_core: core != surface_to_core(core_to_surface(core))",
                details: vec![
                    ("core".to_owned(), {
                        let pretty::DocBuilder(_, doc) =
                            core_to_pretty::from_module(&arena, core_module);
                        doc.pretty(100).to_string()
                    }),
                    ("surface_to_core(core_to_surface(core))".to_owned(), {
                        let pretty::DocBuilder(_, doc) =
                            core_to_pretty::from_module(&arena, &surface_module);
                        doc.pretty(100).to_string()
                    }),
                ],
            });
        }
    }

    fn roundtrip_core_to_pretty(&mut self, core_module: &fathom::lang::core::Module) {
        let arena = pretty::Arena::new();

        let pretty_core_module = {
            let pretty::DocBuilder(_, doc) = core_to_pretty::from_module(&arena, core_module);
            doc.pretty(100).to_string()
        };

        let snapshot_core_file = self.snapshot_file.with_extension("core.fathom");
        if let Err(error) = snapshot::compare(&snapshot_core_file, &pretty_core_module.as_bytes()) {
            self.failures.push(Failure {
                name: "roundtrip_pretty_core: snapshot",
                details: vec![("snapshot error".to_owned(), error.to_string())],
            });
        }

        let mut core_parse_messages = Vec::new();
        let core_file_id = self.files.add(
            snapshot_core_file.to_string_lossy().into(),
            pretty_core_module.clone(),
        );
        let parsed_core_module = {
            let source = self.files.source(core_file_id).unwrap();
            fathom::lang::core::Module::parse(core_file_id, source, &mut core_parse_messages)
        };

        if !core_parse_messages.is_empty() {
            let pretty_arena = pretty::Arena::new();
            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();

            for message in &core_parse_messages {
                let diagnostic = message.to_diagnostic(&pretty_arena);
                term::emit(&mut buffer, &self.term_config, &self.files, &diagnostic).unwrap();
            }

            self.failures.push(Failure {
                name: "roundtrip_pretty_core: parse core",
                details: vec![(
                    "diagnostics".to_owned(),
                    String::from_utf8_lossy(&buffer.as_slice()).into(),
                )],
            });
        }

        if parsed_core_module != *core_module {
            self.failures.push(Failure {
                name: "roundtrip_pretty_core: core != parse(pretty(core))",
                details: vec![
                    ("core".to_owned(), pretty_core_module),
                    ("parse(pretty(core))".to_owned(), {
                        let pretty::DocBuilder(_, doc) =
                            core_to_pretty::from_module(&arena, &parsed_core_module);
                        doc.pretty(100).to_string()
                    }),
                ],
            });
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

        let output = match output {
            Ok(output) => output,
            Err(error) => {
                self.failures.push(Failure {
                    name: "binary_parse_tests: execute rustc",
                    details: vec![("std::io::Error".to_owned(), error.to_string())],
                });
                return;
            }
        };

        if !output.status.success() || !output.stdout.is_empty() || !output.stderr.is_empty() {
            self.failures.push(Failure {
                name: "binary_parse_tests: rust compile output",
                details: process_output_details("rustc", &output),
            });
            return;
        }

        // Run the test harness

        let test_exe = temp_dir
            .join(test_name)
            .with_extension(env::consts::EXE_SUFFIX);

        let output = match Command::new(&test_exe).arg("--color=always").output() {
            Ok(output) => output,
            Err(error) => {
                self.failures.push(Failure {
                    name: "binary_parse_tests: execute test",
                    details: vec![("std::io::Error".to_owned(), error.to_string())],
                });
                return;
            }
        };

        if !output.status.success() || !output.stderr.is_empty() {
            self.failures.push(Failure {
                name: "binary_parse_tests: rust test output",
                details: process_output_details(test_exe.to_str().unwrap(), &output),
            });
            return;
        }
    }

    fn compile_doc(&mut self, surface_module: &fathom::lang::surface::Module) {
        let mut output = Vec::new();
        surface_to_doc::Context::new()
            .from_module(&mut output, surface_module)
            .unwrap();

        if let Err(error) = snapshot::compare(&self.snapshot_file.with_extension("html"), &output) {
            self.failures.push(Failure {
                name: "compile_doc: snapshot",
                details: vec![("snapshot error".to_owned(), error.to_string())],
            });
        }
    }

    fn check_diagnostics(&mut self) {
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
            &mut self.expected_diagnostics,
        );

        if !found_diagnostics.is_empty() {
            let mut buffer = BufferWriter::stderr(ColorChoice::Auto).buffer();
            for diagnostic in &found_diagnostics {
                term::emit(&mut buffer, &self.term_config, &self.files, diagnostic).unwrap();
            }

            self.failures.push(Failure {
                name: "unexpected_diagnostics",
                details: vec![(
                    "diagnostics".to_owned(),
                    String::from_utf8_lossy(&buffer.as_slice()).into(),
                )],
            });
        }

        if !self.expected_diagnostics.is_empty() {
            let mut msg = String::new();
            for expected in &self.expected_diagnostics {
                writeln!(
                    msg,
                    "{}:{}: {}: {}",
                    self.format_file.display(),
                    expected.location.line_number,
                    match expected.severity {
                        Severity::Bug => "bug",
                        Severity::Error => "error",
                        Severity::Warning => "warning",
                        Severity::Note => "note",
                        Severity::Help => "help",
                    },
                    expected.pattern,
                )
                .unwrap();
            }

            self.failures.push(Failure {
                name: "expected_diagnostics",
                details: vec![("expected diagnostics".to_owned(), msg)],
            });
        }
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

fn process_output_details(name: &str, output: &std::process::Output) -> Vec<(String, String)> {
    let mut details = Vec::new();

    if !output.status.success() {
        details.push((format!("{} status", name), output.status.to_string()));
    }
    if !output.stdout.is_empty() {
        let data = String::from_utf8_lossy(&output.stdout).into();
        details.push((format!("{} stdout", name), data));
    }
    if !output.stderr.is_empty() {
        let data = String::from_utf8_lossy(&output.stderr).into();
        details.push((format!("{} stderr", name), data));
    }

    details
}

struct Failure {
    name: &'static str,
    details: Vec<(String, String)>,
}

fn check_failures(failures: &[Failure]) -> Outcome {
    if failures.is_empty() {
        Outcome::Passed
    } else {
        let mut msg = String::new();

        writeln!(msg).unwrap();
        writeln!(msg, "failures:").unwrap();
        writeln!(msg).unwrap();
        for failure in failures {
            writeln!(msg, "    {}:", failure.name).unwrap();
            for (name, data) in &failure.details {
                writeln!(msg, "        ---- {} ----", name).unwrap();
                for line in data.lines() {
                    writeln!(msg, "        {}", line).unwrap();
                }
            }
            writeln!(msg).unwrap();
        }
        writeln!(msg).unwrap();
        writeln!(msg, "    failures:").unwrap();
        for failure in failures {
            writeln!(msg, "        {}", failure.name).unwrap();
        }

        return Outcome::Failed { msg: Some(msg) };
    }
}
