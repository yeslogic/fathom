use std::path::PathBuf;
use std::vec::Vec;
use structopt::StructOpt;

/// Fathom DDL interpreter to provide tools to inspect and query binary files.
#[derive(StructOpt, Debug)]
pub struct Cli {
    /// --format=NAME is an optional argument for specifying the name
    /// of the format description in the installed catalog, for
    /// example "opentype", that will be used to process the files.
    #[structopt(long)]
    format: String,
    /// --format-file=FILE is an optional argument for specifying the
    /// file containing the format description, for example
    /// "path/to/myformat.ddl", that will be used to process the
    /// files.
    #[structopt(long)]
    format_file: PathBuf,
    /// The path to the file(s) to parse
    #[structopt(parse(from_os_str))]
    files: Vec<PathBuf>,
}
