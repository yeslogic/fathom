use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn missing_format_file() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("fathom")?;

    cmd.args(&[
        "data",
        "--format-file=../examples/nope.fathom",
        "../examples/data/nope/nope.nope",
    ]);

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
fn stl_cube() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("fathom")?;

    cmd.args(&[
        "data",
        "--format-file=../examples/stl.fathom",
        "../examples/data/stl/cube.stl",
    ]);

    cmd.assert()
        .success()
        .stdout(predicate::str::starts_with(
            "Main = struct {\n    header = [",
        ))
        .stderr(predicate::str::is_empty());

    Ok(())
}

#[test]
fn stl_cube_validate_core() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("fathom")?;

    cmd.args(&[
        "data",
        "--validate-core",
        "--format-file=../examples/stl.fathom",
        "../examples/data/stl/cube.stl",
    ]);

    cmd.assert()
        .success()
        .stdout(predicate::str::starts_with(
            "Main = struct {\n    header = [",
        ))
        .stderr(predicate::str::is_empty());

    Ok(())
}
