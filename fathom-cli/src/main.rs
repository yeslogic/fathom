use structopt::StructOpt;

mod cli;

fn main() {
    let args = cli::Cli::from_args();
    println!("Hello, world!");

    println!("debug: {:?}", args);
}
