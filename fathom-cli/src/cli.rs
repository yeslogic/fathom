use std::path::PathBuf;
use std::vec::Vec;
use structopt::StructOpt;

/// Fathom DDL interpreter to provide tools to inspect and query binary files.
#[derive(StructOpt, Debug)]
pub struct Cli {
    /// The pattern to look for
    pattern: String,
    /// The path to the file(s) to parse
    #[structopt(parse(from_os_str))]
    files: Vec<PathBuf>,
}
