use codespan_reporting::term::termcolor::BufferedStandardStream;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
pub struct Options {
    /// The Fathom format file to generate documentation for.
    #[structopt(long = "format-file", name = "FORMAT-PATH")]
    format_file: PathBuf, // TODO: specify formats by name, eg. 'opentype'
                          // TODO: specify output file
}

pub fn run(options: &crate::Options, command_options: &Options) -> anyhow::Result<()> {
    let mut driver = fathom::driver::Driver::new();
    driver.set_emit_writer(BufferedStandardStream::stdout(options.color));
    driver.set_diagnostic_writer(BufferedStandardStream::stderr(options.color));

    // TODO: Write to file
    driver.write_doc(&command_options.format_file)?;

    if !driver.check_diagnostics()? {
        std::process::exit(exitcode::DATAERR);
    } else {
        std::process::exit(exitcode::OK);
    }
}
