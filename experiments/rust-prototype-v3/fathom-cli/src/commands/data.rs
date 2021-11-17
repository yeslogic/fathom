use codespan_reporting::term::termcolor::BufferedStandardStream;
use std::path::PathBuf;
use structopt::StructOpt;

// TODO: Ideas for extending the data tool:
//
// - queries for looking up data in binary files
// - implement 'interactive' binary file exploration/manipulation
// - dump to different output formats, eg. JSON, YAML, XML, etc. (with references + schemas?)
// - convert JSON, YAML, XML, to binary data
// - GUI-based binary data inspector

#[derive(StructOpt, Debug)]
pub struct Options {
    /// The Fathom format file to use when reading
    #[structopt(long = "format-file", name = "FORMAT-PATH")]
    format_file: PathBuf, // TODO: specify formats by name, eg. 'opentype'
    /// Checks that the core module is well-formed after elaboration.
    #[structopt(long = "validate-core")]
    validate_core: bool,
    /// The item name to begin reading from
    #[structopt(long = "item-name", default_value = "Main")]
    item_name: String,
    /// The binary file to read
    #[structopt(name = "BINARY-PATH", parse(from_os_str))]
    binary_file: PathBuf, // TODO: parse multiple binary files
}

pub fn run(options: &crate::Options, command_options: &Options) -> anyhow::Result<()> {
    let mut driver = fathom::driver::Driver::new();
    driver.set_validate_core(command_options.validate_core);
    driver.set_emit_writer(BufferedStandardStream::stdout(options.color));
    driver.set_diagnostic_writer(BufferedStandardStream::stderr(options.color));

    driver.read_data(
        &command_options.format_file,
        &command_options.item_name,
        &command_options.binary_file,
    )?;

    if !driver.check_diagnostics()? {
        std::process::exit(exitcode::DATAERR);
    } else {
        std::process::exit(exitcode::OK);
    }
}
