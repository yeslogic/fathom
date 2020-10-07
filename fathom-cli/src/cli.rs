use std::path::PathBuf;
use std::vec::Vec;

use structopt::{clap::ArgGroup, StructOpt};

/// Fathom DDL interpreter to provide tools to inspect and query binary files.
#[derive(StructOpt, Debug)]
#[structopt(group = ArgGroup::with_name("format_choice"))]
pub struct Cli {
    // Format choice, optional:
    /// Optional argument for specifying the name of the format
    /// description in the installed catalog, for example "opentype",
    /// that will be used to process the files.
    #[structopt(long, group = "format_choice")]
    format: Option<String>,
    /// Optional argument for specifying the file containing the
    /// format description, for example "path/to/myformat.ddl", that
    /// will be used to process the files.
    #[structopt(long, group = "format_choice")]
    format_file: Option<PathBuf>,
    // Output options, optional:
    /// Prints a brief summary of each file based on the format
    /// description. This may include printing the type or version or
    /// other important details about the file but may not validate
    /// the entire file contents.
    #[structopt(long)]
    summary: bool,
    /// Prints a textual representation of the complete contents of
    /// each file as described by the format description. This will
    /// also validate the entire file against the format description
    /// and warn of any discrepancies.
    #[structopt(long)]
    dump: bool,
    /// Print the results of applying the specified format query to
    /// each input file.
    #[structopt(long)]
    query: Option<String>,
    /// Read a query from file and print the results of applying the
    /// format query to each input file.
    #[structopt(long)]
    query_file: Option<PathBuf>,
    // The (binary) input files to operate on:
    /// The path to the file(s) to parse
    #[structopt(parse(from_os_str))]
    files: Vec<PathBuf>,
}
