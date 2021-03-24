use assert_cmd::prelude::*;
use std::io::Write;
use std::process::Command;

#[test]
fn check_examples() -> anyhow::Result<()> {
    let mut failed_examples = Vec::new();

    for entry in std::fs::read_dir("../examples")? {
        let entry = entry?;

        if matches!(entry.path().extension(), Some(ext) if ext == "fathom") {
            let format_path = entry.path().display().to_string();
            let output = Command::cargo_bin("fathom")?
                .args(&[
                    "check",
                    "--validate-core",
                    &format!("--format-file={}", format_path),
                ])
                .output()?;

            if !output.status.success() || !output.stdout.is_empty() || !output.stderr.is_empty() {
                std::io::stdout().write_all(&output.stdout)?;
                std::io::stderr().write_all(&output.stderr)?;
                failed_examples.push(format_path);
            }
        }
    }

    if !failed_examples.is_empty() {
        panic!("errors found in: {:?}", failed_examples);
    }

    Ok(())
}
