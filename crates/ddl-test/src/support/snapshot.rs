use difference::Changeset;
use std::path::{Path, PathBuf};
use std::{fmt, fs, io, str};

pub fn compare(path: &Path, extension: &str, found_bytes: &[u8]) -> Result<(), SnapshotError> {
    use std::env;

    let out_path = path.with_extension(extension);
    let found_str = std::str::from_utf8(found_bytes).map_err(SnapshotError::OutputUtf8)?;

    let is_bless = env::var("DDL_BLESS").is_ok();

    if out_path.exists() {
        let expected_string = read_snapshot(&out_path)?;
        let changeset = Changeset::new(&expected_string, found_str, "\n");

        if !changeset.diffs.is_empty() {
            if is_bless {
                bless_snapshot(out_path, found_str)?;
            } else {
                return Err(SnapshotError::UnexpectedChangesFound(out_path, changeset));
            }
        }
    } else {
        if is_bless {
            bless_snapshot(out_path, found_str)?;
        } else {
            return Err(SnapshotError::ExistingSnapshotNotFound(out_path));
        }
    }

    Ok(())
}

fn read_snapshot(out_path: &Path) -> Result<String, SnapshotError> {
    fs::read_to_string(&out_path)
        .map_err(|error| SnapshotError::ReadSnapshot(out_path.to_owned(), error))
}

fn bless_snapshot(out_path: PathBuf, found_str: &str) -> Result<(), SnapshotError> {
    fs::write(&out_path, found_str).map_err(|error| SnapshotError::WriteSnapshot(out_path, error))
}

pub enum SnapshotError {
    OutputUtf8(str::Utf8Error),
    ReadSnapshot(PathBuf, io::Error),
    WriteSnapshot(PathBuf, io::Error),
    ExistingSnapshotNotFound(PathBuf),
    UnexpectedChangesFound(PathBuf, Changeset),
}

impl fmt::Display for SnapshotError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SnapshotError::OutputUtf8(error) => writeln!(f, "actual output not utf8: {}", error)?,
            SnapshotError::ReadSnapshot(path, error) => {
                writeln!(f, "error reading snapshot `{}`: {}", path.display(), error)?;
            }
            SnapshotError::WriteSnapshot(path, error) => {
                writeln!(f, "error writing snapshot `{}`: {}", path.display(), error)?;
            }
            SnapshotError::ExistingSnapshotNotFound(path) => {
                writeln!(f, "existing snapshot `{}` not found", path.display())?;
                writeln!(f)?;
                writeln!(
                    f,
                    "note: Run with `DDL_BLESS=1` environment variable to regenerate."
                )?;
                writeln!(f)?;
            }
            SnapshotError::UnexpectedChangesFound(path, changeset) => {
                use difference::Difference as Diff;
                writeln!(f, "changes found in snapshot `{}`: ", path.display())?;
                writeln!(f)?;
                for diff in &changeset.diffs {
                    match diff {
                        // TODO: Colored diffs
                        Diff::Same(data) => writeln!(f, "      {}", data.replace('\n', "¶"))?,
                        Diff::Add(data) => writeln!(f, "    + {}", data.replace('\n', "¶"))?,
                        Diff::Rem(data) => writeln!(f, "    - {}", data.replace('\n', "¶"))?,
                    }
                }
                writeln!(f)?;
                writeln!(
                    f,
                    "note: Run with `DDL_BLESS=1` environment variable to regenerate."
                )?;
                writeln!(f)?;
            }
        }
        Ok(())
    }
}
