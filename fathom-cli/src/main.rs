use structopt::StructOpt;

fn main() -> anyhow::Result<()> {
    fathom_cli::run(fathom_cli::Options::from_args())
}
