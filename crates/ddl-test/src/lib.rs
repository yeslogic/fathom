//! Integration tests for the data description language.

#![cfg(test)]
#![warn(rust_2018_idioms)]

use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::{fs, env};
use std::path::Path;
use std::process::Command;

fn run_test(test_name: &str) {
    let tests_dir = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/../../tests"));
    let tests_dir = tests_dir.strip_prefix(env::current_dir().unwrap()).unwrap_or(tests_dir);
    let temp_dir = assert_fs::TempDir::new().unwrap();

    Command::cargo_bin("ddl-cli")
        .unwrap()
        .arg(format!("--out-dir={}", temp_dir.path().display()))
        .arg(tests_dir.join(test_name).with_extension("ddl"))
        .assert()
        .code(0)
        .stdout(predicate::str::is_empty())
        .stderr(predicate::str::is_empty())
        .success();

    Command::new("rustc")
        .arg(format!("--out-dir={}", temp_dir.path().display()))
        .arg("--emit=dep-info,metadata")
        .arg("--crate-type=rlib")
        .arg(temp_dir.path().join(test_name).with_extension("rs"))
        .assert()
        .code(0)
        .stdout(predicate::str::is_empty())
        .stderr(predicate::str::is_empty())
        .success();
}

macro_rules! test {
    ($test_name:ident) => {
        #[test]
        fn $test_name() {
            run_test(stringify!($test_name));
        }
    };
}

test!(empty);
test!(broken);
