use ddl::cli::Opts;
use failure::Error;
use structopt::StructOpt;

fn main() -> Result<(), Error> {
    ddl::cli::run(Opts::from_args())
}
