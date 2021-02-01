use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn compile_rust_is_not_implemented() -> anyhow::Result<()> {
    let mut cmd = Command::cargo_bin("fathom")?;

    cmd.args(&[
        "compile",
        "--target=rust",
        "--format-file=../examples/stl.fathom",
    ]);

    cmd.assert()
        .failure()
        .stdout(predicate::str::is_empty())
        .stderr(predicate::str::contains("not yet implemented"));

    Ok(())
}
