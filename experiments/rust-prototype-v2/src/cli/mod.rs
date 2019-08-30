//! The command line interface for the DDL

use codespan_reporting::ColorArg;
use failure::Error;

pub mod check;
pub mod repl;

// TODO: test using https://github.com/killercup/assert_cli

#[derive(Debug, structopt::StructOpt)]
#[structopt(name = "ddl")]
pub struct Opts {
    /// Configure coloring of output
    #[structopt(
        long = "color",
        parse(try_from_str),
        default_value = "auto",
        raw(possible_values = "ColorArg::VARIANTS")
    )]
    pub color: ColorArg,

    /// Subcommand to run
    #[structopt(subcommand)]
    pub command: Command,
}

#[derive(Debug, structopt::StructOpt)]
pub enum Command {
    /// Check the that the given files type check
    #[structopt(name = "check")]
    Check(check::Opts),

    /// A REPL for running expressions
    #[structopt(name = "repl")]
    Repl(repl::Opts),
}

pub fn run(opts: Opts) -> Result<(), Error> {
    let color_choice = opts.color.into();
    match opts.command {
        Command::Check(check_opts) => check::run(color_choice, check_opts),
        Command::Repl(repl_opts) => repl::run(color_choice, &repl_opts),
    }
}
