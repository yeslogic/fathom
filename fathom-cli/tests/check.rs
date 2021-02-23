use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn check_missing_format_file() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("fathom")?;

    cmd.args(&["check", "--format-file=../examples/nope.fathom"]);

    cmd.assert()
        .failure()
        .stdout(predicate::str::is_empty())
        .stderr(predicate::str::contains(
            "failed to read file `../examples/nope.fathom`",
        ))
        .stderr(predicate::str::contains(
            "no such file or directory (os error 2)",
        ));

    Ok(())
}

#[test]
fn check_stl() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("fathom")?;

    cmd.args(&["check", "--format-file=../examples/stl.fathom"]);

    cmd.assert()
        .success()
        .stdout(predicate::str::is_empty())
        .stderr(predicate::str::is_empty());

    Ok(())
}

#[test]
fn check_stl_validate_core() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("fathom")?;

    cmd.args(&[
        "check",
        "--validate-core",
        "--format-file=../examples/stl.fathom",
    ]);

    cmd.assert()
        .success()
        .stdout(predicate::str::is_empty())
        .stderr(predicate::str::is_empty());

    Ok(())
}

#[test]
fn check_stl_emit_core() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("fathom")?;

    cmd.args(&[
        "check",
        "--emit-core",
        "--format-file=../examples/stl.fathom",
    ]);

    cmd.assert()
        .success()
        .stdout(predicate::str::starts_with("//! Binary STL File\n"))
        .stdout(predicate::str::contains("Vec3d"))
        .stdout(predicate::str::contains("Triangle"))
        .stdout(predicate::str::contains("Main"))
        .stderr(predicate::str::is_empty());

    Ok(())
}
