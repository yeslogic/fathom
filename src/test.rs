//! Test utilities

use difference::Changeset;
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::io::{self, Read};

pub fn snapshot_file_name<P: AsRef<Path>>(current_filename: P, unique_name: &str) -> PathBuf {
    let current_dir = current_filename.as_ref().parent().unwrap();
    let current_file_name = current_filename.as_ref().file_name().unwrap();

    let snapshot_file_name = current_dir
        .join("snapshots")
        .join(current_file_name)
        .with_extension(format!("{}.snap", unique_name));

    snapshot_file_name
}

pub fn regenerate_snapshot<P: AsRef<Path>, T, F>(path: P, value: &T, write_fn: F) -> io::Result<()>
where
    F: Fn(&mut File, &T) -> io::Result<()>,
{
    use std::fs::OpenOptions;

    fs::create_dir_all(path.as_ref().parent().unwrap())?;
    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&path)?;

    write_fn(&mut file, value)
}

pub fn snapshot_changes<T, F>(file: &mut File, value: &T, write_fn: F) -> Option<Changeset>
where
    F: Fn(&mut Vec<u8>, &T) -> io::Result<()>,
{
    let mut found = Vec::new();
    write_fn(&mut found, value).unwrap();
    let found = String::from_utf8(found).unwrap();

    let mut expected = String::new();
    file.read_to_string(&mut expected).unwrap();

    if found == expected {
        None
    } else {
        Some(Changeset::new(&found, &expected, "\n"))
    }
}

#[macro_export]
macro_rules! assert_snapshot {
    ($name:ident, $value:expr, $write_fn:expr) => {{
        use std::env;
        use std::fs::File;
        use std::io::ErrorKind;

        let value = &$value;
        let file_name = $crate::test::snapshot_file_name(file!(), stringify!($name));

        match env::var("REGENERATE_SNAPSHOTS") {
            Ok(ref val) if val == "1" => {
                $crate::test::regenerate_snapshot(&file_name, value, $write_fn).unwrap();
            }
            Ok(_) | Err(_) => {
                match File::open(&file_name) {
                    Ok(mut file) => {
                        if let Some(changes) = $crate::test::snapshot_changes(&mut file, value, $write_fn) {
                            println!();
                            println!("Snapshots differ:");
                            println!("{}", changes);
                            println!("In file {:?}", file_name);
                            println!();
                            println!("Rerun the failing test with REGENERATE_SNAPSHOTS=1 \
                                      regenerate the saved snapshot");
                            println!();
                            panic!();
                        }
                    }
                    Err(err) => {
                        match err.kind() {
                            ErrorKind::NotFound => {
                                $crate::test::regenerate_snapshot(&file_name, value, $write_fn).unwrap()
                            }
                            _ => panic!("{}", err),
                        }
                    }
                }
            }
        }
    }};
}

#[macro_export]
macro_rules! assert_debug_snapshot {
    ($name:ident, $value:expr) => {
        assert_snapshot!($name, $value, |writer, value| {
            use std::io::prelude::*;

            writeln!(writer, "{:#?}", value)
        });
    };
}

#[macro_export]
macro_rules! assert_display_snapshot {
    ($name:ident, $value:expr) => {
        assert_snapshot!($name, $value, |writer, value| {
            use std::io::prelude::*;

            writeln!(writer, "{}", value)
        });
    };
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
