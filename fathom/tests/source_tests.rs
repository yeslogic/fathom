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

    libtest_mimic::run(&args, tests).exit();
}

#[derive(Deserialize, Debug, Copy, Clone)]
#[serde(rename_all = "kebab-case")]
pub enum TestMode {
    Module,
    Term,
}

impl TestMode {
    fn to_command<'a>(self) -> Command<'a> {
        match self {
            TestMode::Module => Command::ElabModule,
            TestMode::Term => Command::ElabTerm,
        }
    }
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "kebab-case")]
struct Config {
    mode: Option<TestMode>,
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
const DEFAULT_EXAMPLE_DATA: fn() -> Vec<String> = Vec::new;
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

fn extract_module_test(path: PathBuf) -> libtest_mimic::Trial {
    extract_test(path, TestMode::Module)
}

fn extract_term_test(path: PathBuf) -> libtest_mimic::Trial {
    extract_test(path, TestMode::Term)
}

fn extract_test(input_file: PathBuf, default_mode: TestMode) -> libtest_mimic::Trial {
    use itertools::Itertools;

    let test_name = input_file.display().to_string();

    const CONFIG_COMMENT_START: &str = "//~";

    let input_source = std::fs::read_to_string(&input_file).unwrap();
    // Collect the lines with CONFIG_COMMENT_START prefix, stripping the prefix in the process
    let config_source = input_source
        .lines()
        .filter_map(|line| line.split(CONFIG_COMMENT_START).nth(1))
        .join("\n");

    // Parse those lines as TOML
    match toml::from_str::<Config>(&config_source) {
        Ok(mut config) => {
            config.update_snapshots = env::var_os("FATHOM_UPDATE_SNAP").is_some();

            if config.ignore {
                libtest_mimic::Trial::test(test_name, || Ok(())).with_ignored_flag(true)
            } else {
                libtest_mimic::Trial::test(test_name, move || {
                    run_test(input_file, default_mode, config)
                })
            }
        }
        Err(error) => libtest_mimic::Trial::test(test_name, move || {
            failures_to_outcome(&[TestFailure {
                name: "config parse error",
                details: vec![("toml::de::Error", error.to_string())],
            }])
        }),
    }
}

fn run_test(
    input_file: PathBuf,
    default_mode: TestMode,
    config: Config,
) -> Result<(), libtest_mimic::Failed> {
    let mut failures = Vec::new();

    let command = match config.mode {
        Some(mode) => mode.to_command(),
        None => default_mode.to_command(),
    };

    let test_command = TestCommand::new(command, &config, &input_file);
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
        let test_command = TestCommand::new(Command::Normalise, &config, &input_file);
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

    let base_dir = input_file.with_file_name("");
    let example_data = globwalk::GlobWalkerBuilder::from_patterns(&base_dir, &config.example_data)
        .build()
        .unwrap();

    for example_file in example_data.filter_map(Result::ok) {
        let command = Command::ParseData(&input_file, ExpectedOutcome::Success);
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
        let command = Command::ParseData(&input_file, ExpectedOutcome::Failure);
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

fn failures_to_outcome(failures: &[TestFailure]) -> Result<(), libtest_mimic::Failed> {
    if failures.is_empty() {
        Ok(())
    } else {
        let mut msg = String::new();

        writeln!(msg).unwrap();
        for failure in failures {
            writeln!(msg, "    {}:", failure.name).unwrap();
            for (name, data) in &failure.details {
                writeln!(msg, "        ---- {name} ----").unwrap();
                for line in data.lines() {
                    writeln!(msg, "        {line}").unwrap();
                }
            }
            writeln!(msg).unwrap();
        }
        writeln!(msg).unwrap();
        writeln!(msg, "    failures:").unwrap();
        for failure in failures {
            writeln!(msg, "        {}", failure.name).unwrap();
        }

        Err(msg.into())
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
        let mut command = process::Command::from(self.command);
        command.arg(self.input_file);

        match command.output() {
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

                    details.push(("command", command_to_string(&command)));

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

fn strip_current_dir(path: &Path) -> &Path {
    path.strip_prefix(std::env::current_dir().unwrap())
        .unwrap_or(path)
}

fn command_to_string(command: &process::Command) -> String {
    use itertools::Itertools;

    format!(
        "{program} {args}",
        program = strip_current_dir(Path::new(command.get_program())).to_string_lossy(),
        args = command
            .get_args()
            .map(|arg| arg.to_string_lossy())
            .format(" "),
    )
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
    format!("{line_number:>line_width$}| {sign} {line}\n")
}
