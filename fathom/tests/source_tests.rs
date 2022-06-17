use serde::{Deserialize, Serialize};
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::process::{self};
use std::{env, fs, io};
use walkdir::WalkDir;

fn main() {
    let args = libtest_mimic::Arguments::from_args();

    std::env::set_current_dir("..").unwrap();

    let tests = std::iter::empty()
        .chain(find_source_files("formats").map(extract_module_test))
        .chain(find_source_files("tests").map(extract_term_test))
        .collect();

    libtest_mimic::run_tests(&args, tests, run_test).exit();
}

pub struct TestData {
    input_file: PathBuf,
    mode: TestMode,
}

pub enum TestMode {
    ElabModule,
    ElabTerm,
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "kebab-case")]
struct Config {
    #[serde(default = "DEFAULT_IGNORE")]
    ignore: bool,
    #[serde(default = "DEFAULT_EXIT_CODE")]
    exit_code: i32,
    #[serde(default = "DEFAULT_EXAMPLE_DATA")]
    example_data: Vec<String>,
    #[serde(default = "DEFAULT_EXAMPLE_DATA")]
    example_data_invalid: Vec<String>,
    #[serde(skip)]
    update_snapshots: bool,
    #[serde(default = "DEFAULT_TEST_NORMALISATION")]
    test_normalisation: bool,
}

const DEFAULT_IGNORE: fn() -> bool = || false;
const DEFAULT_EXIT_CODE: fn() -> i32 = || 0;
const DEFAULT_EXAMPLE_DATA: fn() -> Vec<String> = || Vec::new();
const DEFAULT_TEST_NORMALISATION: fn() -> bool = || false;

struct TestFailure {
    name: &'static str,
    details: Vec<(&'static str, String)>,
}

#[derive(Deserialize, Serialize, Debug, Clone, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "kebab-case")]
struct SnapshotData {
    stdout: String,
    stderr: String,
}

#[derive(Debug)]
struct Snapshot {
    path: PathBuf,
    expected: Option<SnapshotData>,
    actual: SnapshotData,
}

#[derive(Eq, PartialEq, Debug)]
enum SnapshotOutcome {
    Equal,
    Different,
    Missing,
}

struct TestCommand<'a> {
    command: Command<'a>,
    config: &'a Config,
    input_file: &'a Path,
}

#[derive(Copy, Clone)]
enum Command<'a> {
    ElabModule,
    ElabTerm,
    Normalise,
    ParseData(&'a Path, ExpectedOutcome),
}

#[derive(Copy, Clone)]
enum ExpectedOutcome {
    Success,
    Failure,
}

impl<'a> Command<'a> {
    pub fn snap_name(&self) -> &'static str {
        match self {
            Command::Normalise => "norm",
            Command::ElabModule | Command::ElabTerm | Command::ParseData(_, _) => "",
        }
    }

    pub(crate) fn expected_outcome(&self) -> ExpectedOutcome {
        match self {
            Command::ParseData(_, outcome) => *outcome,
            Command::ElabModule | Command::ElabTerm | Command::Normalise => {
                ExpectedOutcome::Success
            }
        }
    }
}

/// Recursively walk over test files under a file path.
pub fn find_source_files(root: impl AsRef<Path>) -> impl Iterator<Item = PathBuf> {
    WalkDir::new(root)
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.file_type().is_file())
        .filter(|entry| matches!(entry.path().extension(), Some(ext) if ext == "fathom"))
        .map(|entry| entry.into_path())
}

fn extract_module_test(path: PathBuf) -> libtest_mimic::Test<TestData> {
    libtest_mimic::Test {
        name: path.display().to_string(),
        kind: String::new(),
        is_ignored: false,
        is_bench: false,
        data: TestData {
            input_file: path,
            mode: TestMode::ElabModule,
        },
    }
}

fn extract_term_test(path: PathBuf) -> libtest_mimic::Test<TestData> {
    libtest_mimic::Test {
        name: path.display().to_string(),
        kind: String::new(),
        is_ignored: false,
        is_bench: false,
        data: TestData {
            input_file: path,
            mode: TestMode::ElabTerm,
        },
    }
}

fn run_test(test: &libtest_mimic::Test<TestData>) -> libtest_mimic::Outcome {
    let mut failures = Vec::new();

    let config: Config = {
        use itertools::Itertools;

        const CONFIG_COMMENT_START: &str = "//~";

        let input_source = std::fs::read_to_string(&test.data.input_file).unwrap();
        // Collect the lines with CONFIG_COMMENT_START prefix, stripping the prefix in the process
        let config_source = input_source
            .lines()
            .filter_map(|line| line.split(CONFIG_COMMENT_START).nth(1))
            .join("\n");

        // Parse those lines as TOML
        match toml::from_str::<Config>(&config_source) {
            Ok(mut config) => {
                config.update_snapshots = env::var_os("FATHOM_UPDATE_SNAP").is_some();
                config
            }
            Err(error) => {
                failures.push(TestFailure {
                    name: "config parse error",
                    details: vec![("toml::de::Error", error.to_string())],
                });

                return failures_to_outcome(&failures);
            }
        }
    };

    if config.ignore {
        return libtest_mimic::Outcome::Ignored;
    }

    let command = match test.data.mode {
        TestMode::ElabModule => Command::ElabModule,
        TestMode::ElabTerm => Command::ElabTerm,
    };
    let test_command = TestCommand::new(command, &config, &test.data.input_file);
    match test_command.run() {
        Ok(mut test_failures) => failures.append(&mut test_failures),
        Err(error) => {
            failures.push(TestFailure {
                name: "unexpected test command error",
                details: vec![("std::io::Error", error.to_string())],
            });
        }
    }

    if config.test_normalisation {
        let test_command = TestCommand::new(Command::Normalise, &config, &test.data.input_file);
        match test_command.run() {
            Ok(mut test_failures) => failures.append(&mut test_failures),
            Err(error) => {
                failures.push(TestFailure {
                    name: "unexpected test command error",
                    details: vec![("std::io::Error", error.to_string())],
                });
            }
        }
    }

    let base_dir = test.data.input_file.with_file_name("");
    let example_data = globwalk::GlobWalkerBuilder::from_patterns(&base_dir, &config.example_data)
        .build()
        .unwrap();

    for example_file in example_data.filter_map(Result::ok) {
        let command = Command::ParseData(&test.data.input_file, ExpectedOutcome::Success);
        let test_command = TestCommand::new(command, &config, example_file.path());
        match test_command.run() {
            Ok(mut test_failures) => failures.append(&mut test_failures),
            Err(error) => {
                failures.push(TestFailure {
                    name: "unexpected test command error",
                    details: vec![("std::io::Error", error.to_string())],
                });
            }
        }
    }

    let invalid_example_data =
        globwalk::GlobWalkerBuilder::from_patterns(&base_dir, &config.example_data_invalid)
            .build()
            .unwrap();

    for example_file in invalid_example_data.filter_map(Result::ok) {
        let command = Command::ParseData(&test.data.input_file, ExpectedOutcome::Failure);
        let test_command = TestCommand::new(command, &config, example_file.path());
        match test_command.run() {
            Ok(mut test_failures) => failures.append(&mut test_failures),
            Err(error) => {
                failures.push(TestFailure {
                    name: "unexpected test command error",
                    details: vec![("std::io::Error", error.to_string())],
                });
            }
        }
    }

    failures_to_outcome(&failures)
}

fn failures_to_outcome(failures: &[TestFailure]) -> libtest_mimic::Outcome {
    if failures.is_empty() {
        libtest_mimic::Outcome::Passed
    } else {
        let mut msg = String::new();

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

        return libtest_mimic::Outcome::Failed { msg: Some(msg) };
    }
}

impl<'a> TestCommand<'a> {
    fn new(command: Command<'a>, config: &'a Config, input_file: &'a Path) -> Self {
        TestCommand {
            command,
            config,
            input_file,
        }
    }

    fn run(&self) -> Result<Vec<TestFailure>, io::Error> {
        let mut failures = Vec::new();
        let mut exe = process::Command::from(self.command);
        exe.arg(self.input_file);

        match exe.output() {
            Ok(output) => {
                let mut snapshot = Snapshot::new(self.command, self.input_file, &output)?;

                // Update if requested
                if self.config.update_snapshots && snapshot.outcome() != SnapshotOutcome::Equal {
                    snapshot.update()?;
                }

                match snapshot.outcome() {
                    SnapshotOutcome::Equal => {}
                    SnapshotOutcome::Different => {
                        let mut details =
                            vec![("path", snapshot.path.to_string_lossy().into_owned())];
                        if let Some(diff) = snapshot.stdout_diff() {
                            details.push(("stdout diff", diff));
                        }
                        if let Some(diff) = snapshot.stderr_diff() {
                            details.push(("stderr diff", diff));
                        }

                        failures.push(TestFailure {
                            name: "snapshot mismatch",
                            details,
                        });
                    }
                    SnapshotOutcome::Missing => {
                        let mut details =
                            vec![("path", snapshot.path.to_string_lossy().into_owned())];
                        if !snapshot.stdout().is_empty() {
                            details.push(("stdout", snapshot.stdout().to_string()));
                        }
                        if !snapshot.stderr().is_empty() {
                            details.push(("stderr", snapshot.stderr().to_string()));
                        }

                        failures.push(TestFailure {
                            name: "snapshot missing",
                            details,
                        });
                    }
                }

                if !command_status_matches_expectation(
                    self.command.expected_outcome(),
                    output.status.code(),
                    self.config.exit_code,
                ) {
                    let mut details = Vec::new();

                    // TODO: Improve output
                    details.push(("command", format!("{:?}", exe)));

                    if output.status.code() != Some(self.config.exit_code) {
                        details.push(("status", output.status.to_string()));
                    }
                    if !snapshot.stdout().is_empty() {
                        details.push(("stdout", snapshot.stdout().to_string()));
                    }
                    if !snapshot.stderr().is_empty() {
                        details.push(("stderr", snapshot.stderr().to_string()));
                    }
                    failures.push(TestFailure {
                        name: "unexpected command output",
                        details,
                    });
                }
            }
            Err(error) => {
                failures.push(TestFailure {
                    name: "unexpected command error",
                    details: vec![("std::io::Error", error.to_string())],
                });
            }
        }
        Ok(failures)
    }
}

fn command_status_matches_expectation(
    expected_outcome: ExpectedOutcome,
    exit_code: Option<i32>,
    expected_code: i32,
) -> bool {
    let expected_code = Some(expected_code);
    match expected_outcome {
        ExpectedOutcome::Success if exit_code == expected_code => true,
        ExpectedOutcome::Failure if exit_code != expected_code => true,
        _ => false,
    }
}

impl<'a> From<Command<'a>> for process::Command {
    fn from(command: Command) -> Self {
        let mut exe = process::Command::new(env!("CARGO_BIN_EXE_fathom"));
        match command {
            Command::ElabModule => {
                exe.args(["elab", "--module"]);
            }
            Command::ElabTerm => {
                exe.args(["elab", "--term"]);
            }
            Command::Normalise => {
                exe.args(["norm", "--term"]);
            }
            Command::ParseData(format, _) => {
                exe.args(["data", "--module"]);
                exe.arg(format);
            }
        }
        exe
    }
}

impl Snapshot {
    fn new(
        command: Command,
        test_path: &Path,
        output: &process::Output,
    ) -> Result<Snapshot, io::Error> {
        let mut file_name = test_path.file_stem().unwrap().to_os_string();
        let snap_suffix = command.snap_name();
        if !snap_suffix.is_empty() {
            file_name.push(".");
            file_name.push(command.snap_name());
        }
        file_name.push(".snap");

        let path = test_path.with_file_name(file_name);
        let actual = SnapshotData {
            stdout: String::from_utf8_lossy(&output.stdout).into(),
            stderr: String::from_utf8_lossy(&output.stderr).into(),
        };
        let expected = match fs::read_to_string(&path) {
            Ok(snap) => toml::from_str(&snap)
                .map(Some)
                .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?,
            Err(err) => match err.kind() {
                io::ErrorKind::NotFound => {
                    // Snapshot is missing and needs to be generated
                    None
                }
                _ => return Err(err),
            },
        };
        Ok(Snapshot {
            path,
            expected,
            actual,
        })
    }

    fn stdout(&self) -> &str {
        &self.actual.stdout
    }

    fn stderr(&self) -> &str {
        &self.actual.stderr
    }

    fn update(&mut self) -> Result<(), io::Error> {
        let serialised = toml::to_string_pretty(&self.actual)
            .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
        fs::write(&self.path, serialised)?;
        self.expected = Some(self.actual.clone());
        Ok(())
    }

    fn outcome(&self) -> SnapshotOutcome {
        match self.expected {
            Some(ref expected) => {
                if expected == &self.actual {
                    SnapshotOutcome::Equal
                } else {
                    SnapshotOutcome::Different
                }
            }
            None => SnapshotOutcome::Missing,
        }
    }

    fn stdout_diff(&self) -> Option<String> {
        self.expected
            .as_ref()
            .and_then(|expected| make_diff(&self.actual.stdout, &expected.stdout))
    }

    fn stderr_diff(&self) -> Option<String> {
        self.expected
            .as_ref()
            .and_then(|expected| make_diff(&self.actual.stderr, &expected.stderr))
    }
}

fn make_diff(actual: &str, expected: &str) -> Option<String> {
    let mut diff = String::new();
    let mut left_line_number = 0;
    let mut right_line_number = 0;
    let line_width = (actual.lines().count().max(expected.lines().count()) as f32)
        .log10()
        .ceil() as usize;
    for result in diff::lines(expected, actual).into_iter() {
        match result {
            diff::Result::Left(l) => {
                left_line_number += 1;
                diff.push_str(&diff_line('-', left_line_number, line_width, l));
            }
            diff::Result::Both(_l, _r) => {
                left_line_number += 1;
                right_line_number += 1;
            }
            diff::Result::Right(r) => {
                right_line_number += 1;
                diff.push_str(&diff_line('+', right_line_number, line_width, r));
            }
        }
    }
    if diff.is_empty() {
        None
    } else {
        Some(diff)
    }
}

fn diff_line(sign: char, line_number: usize, line_width: usize, line: &str) -> String {
    format!(
        "{:>width$}| {} {}\n",
        line_number,
        sign,
        line,
        width = line_width
    )
}
