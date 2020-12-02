use fathom::driver;
use structopt::StructOpt;

mod cli;

fn main() {
    let args = cli::Cli::from_args();
    println!("Hello, world!");

    println!("debug: {:?}", args);

    if args.files.len() == 0 {
        // error, please provide an input file
    }

    if args.files.len() != 1 {
        // note that we only will read one input file.
    }

    let file = args.files.first().unwrap();

    driver::compiler::run_compiler(
        &*file.clone().into_os_string().into_string().unwrap(),
        &*file,
    )
}
