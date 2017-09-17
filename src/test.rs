//! Test utilities

use difference::Changeset;
use std::fmt;
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::io::{self, Read, Write};

pub fn snapshot_file_name<P: AsRef<Path>>(current_filename: P, unique_name: &str) -> PathBuf {
    let current_dir = current_filename.as_ref().parent().unwrap();
    let current_file_name = current_filename.as_ref().file_name().unwrap();

    let snapshot_file_name = current_dir
        .join("snapshots")
        .join(current_file_name)
        .with_extension(format!("{}.snap", unique_name));

    snapshot_file_name
}

pub fn regenerate_snapshot<P: AsRef<Path>, T: fmt::Debug>(path: P, value: &T) -> io::Result<()> {
    use std::fs::OpenOptions;

    fs::create_dir_all(path.as_ref().parent().unwrap())?;
    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&path)?;

    write_snapshot_data(&mut file, value)
}

pub fn write_snapshot_data<W: Write, T: fmt::Debug>(writer: &mut W, value: &T) -> io::Result<()> {
    writeln!(writer, "{:#?}", value)
}

pub fn snapshot_changes<T: fmt::Debug>(file: &mut File, value: &T) -> Option<Changeset> {
    let mut found = Vec::new();
    write_snapshot_data(&mut found, value).unwrap();
    let found = String::from_utf8(found).unwrap();

    let mut expected = String::new();
    file.read_to_string(&mut expected).unwrap();

    if found == expected {
        None
    } else {
        Some(Changeset::new(&found, &expected, ""))
    }
}

#[macro_export]
macro_rules! assert_snapshot {
    ($name:ident, $value:expr) => {{
        use std::env;
        use std::fs::File;
        use std::io::ErrorKind;

        let value = &$value;
        let file_name = $crate::test::snapshot_file_name(file!(), stringify!($name));

        match env::var("REGENERATE_SNAPSHOTS") {
            Ok(ref val) if val == "1" => {
                $crate::test::regenerate_snapshot(&file_name, value).unwrap();
            }
            Ok(_) | Err(_) => {
                match File::open(&file_name) {
                    Ok(mut file) => {
                        if let Some(changes) = $crate::test::snapshot_changes(&mut file, value) {
                            println!();
                            println!("Snapshots differ:");
                            println!("{}", changes);
                            println!("In file {:?}", file_name);
                            println!();
                            println!("Rerun the failing test with REGENERATE_SNAPSHOTS=1 regenerate the saved snapshot");
                            println!();
                            panic!();
                        }
                    }
                    Err(err) => {
                        match err.kind() {
                            ErrorKind::NotFound => $crate::test::regenerate_snapshot(&file_name, value).unwrap(),
                            _ => panic!("{}", err),
                        }
                    }
                }
            }
        }
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_name() {
        let current_file_name = PathBuf::from("test/path/foo.rs");
        let unique_name = "my_test";

        assert_eq!(
            snapshot_file_name(current_file_name, unique_name),
            PathBuf::from("test/path/snapshots/foo.my_test.snap")
        );
    }
}
