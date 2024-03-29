use codespan_reporting::term::termcolor::BufferedStandardStream;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
pub struct Options {
    /// The Fathom format file to check.
    #[structopt(long = "format-file", name = "FORMAT-PATH")]
    format_file: PathBuf, // TODO: specify formats by name, eg. 'opentype'
    /// Print the elaborated core module.
    #[structopt(long = "emit-core")]
    emit_core: bool,
    /// Checks that the core module is well-formed after elaboration.
    #[structopt(long = "validate-core")]
    validate_core: bool,
}

pub fn run(options: &crate::Options, command_options: &Options) -> anyhow::Result<()> {
    let mut driver = fathom::driver::Driver::new();
    driver.set_emit_core(command_options.emit_core);
    driver.set_validate_core(command_options.validate_core);
    driver.set_emit_writer(BufferedStandardStream::stdout(options.color));
    driver.set_diagnostic_writer(BufferedStandardStream::stderr(options.color));

    driver.check(&command_options.format_file)?;

    if !driver.check_diagnostics()? {
        std::process::exit(exitcode::DATAERR);
    } else {
        std::process::exit(exitcode::OK);
    }
}
