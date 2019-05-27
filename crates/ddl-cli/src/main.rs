#![warn(rust_2018_idioms)]

use ddl_cli::Options;
use std::error::Error;
use structopt::StructOpt;

fn main() -> Result<(), Box<dyn Error>> {
    ddl_cli::run(Options::from_args())
}
