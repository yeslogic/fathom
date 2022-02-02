use serde::Deserialize;
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use walkdir::WalkDir;

fn main() {
    let args = libtest_mimic::Arguments::from_args();

    std::env::set_current_dir("..").unwrap();

    let tests = std::iter::empty()
        .chain(find_source_files("formats").map(extract_test))
        .chain(find_source_files("tests").map(extract_test))
        .collect();

    libtest_mimic::run_tests(&args, tests, run_test()).exit();
}

pub struct TestData {
    input_file: PathBuf,
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
}

const DEFAULT_IGNORE: fn() -> bool = || false;
const DEFAULT_EXIT_CODE: fn() -> i32 = || 0;
const DEFAULT_EXAMPLE_DATA: fn() -> Vec<String> = || Vec::new();

struct TestFailure {
    name: &'static str,
    details: Vec<(&'static str, String)>,
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

pub fn extract_test(path: PathBuf) -> libtest_mimic::Test<TestData> {
    libtest_mimic::Test {
        name: path.display().to_string(),
        kind: String::new(),
        is_ignored: false,
        is_bench: false,
        data: TestData { input_file: path },
    }
}

pub fn run_test(
) -> impl 'static + Send + Sync + Fn(&libtest_mimic::Test<TestData>) -> libtest_mimic::Outcome {
    move |test| run_test_impl(test)
}

fn run_test_impl(test: &libtest_mimic::Test<TestData>) -> libtest_mimic::Outcome {
    let mut failures = Vec::new();

    let config: Config = {
        use itertools::Itertools;

        const CONFIG_COMMENT_START: &str = "//~";

        let input_source = std::fs::read_to_string(&test.data.input_file).unwrap();
        let config_source = input_source
            .lines()
            .filter_map(|line| line.split(CONFIG_COMMENT_START).nth(1))
            .join("\n");

        match toml::from_str(&config_source) {
            Ok(config) => config,
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

    let mut command = Command::new(env!("CARGO_BIN_EXE_fathom"));
    command.arg("elab");
    command.args(["--term", test.data.input_file.to_string_lossy().as_ref()]);

    match command.output() {
        Ok(output) => {
            if output.status.code() != Some(config.exit_code) {
                let mut details = Vec::new();

                // TODO: Improve output
                details.push(("command", format!("{:?}", command)));

                if output.status.code() != Some(config.exit_code) {
                    details.push(("status", output.status.to_string()));
                }
                if !output.stdout.is_empty() {
                    let data = String::from_utf8_lossy(&output.stdout).into();
                    details.push(("stdout", data));
                }
                if !output.stderr.is_empty() {
                    let data = String::from_utf8_lossy(&output.stderr).into();
                    details.push(("stderr", data));
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

    let base_dir = test.data.input_file.with_file_name("");
    let example_data = globwalk::GlobWalkerBuilder::from_patterns(&base_dir, &config.example_data)
        .build()
        .unwrap();

    for example_file in example_data.filter_map(Result::ok) {
        let mut command = Command::new(env!("CARGO_BIN_EXE_fathom"));
        command.arg("data");
        command.args(["--format", test.data.input_file.to_string_lossy().as_ref()]);
        command.arg(example_file.path());

        match command.output() {
            Ok(output) => {
                if output.status.code() != Some(0) {
                    let mut details = Vec::new();

                    // TODO: Improve output
                    details.push(("command", format!("{:?}", command)));

                    if output.status.code() != Some(0) {
                        details.push(("status", output.status.to_string()));
                    }
                    if !output.stdout.is_empty() {
                        let data = String::from_utf8_lossy(&output.stdout).into();
                        details.push(("stdout", data));
                    }
                    if !output.stderr.is_empty() {
                        let data = String::from_utf8_lossy(&output.stderr).into();
                        details.push(("stderr", data));
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

                return failures_to_outcome(&failures);
            }
        };
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
