use fathom::driver;
use structopt::StructOpt;

use fathom::lang::core;
use fathom::lang::core::binary;
use fathom_runtime::ReadScope;

mod cli;

fn main() -> Result<(), String> {
    let args = cli::Cli::from_args();

    println!("debug: {:?}", args);

    if args.files.len() == 0 {
        return Err("Please provide a binary file to operate on.".to_string());
    }

    let root_query = match args.query {
        Some(query) => query,
        None => return Err("Please provide name of root item with --query.".to_string()),
    };

    let ddl_file = args.format_file;

    for input_file in args.files {
        let core_module = driver::compiler::run_compiler(
            &ddl_file.clone().into_os_string().into_string().unwrap(),
            &ddl_file,
        );

        let globals = core::Globals::default();
        let input_buffer = std::fs::read(&input_file).unwrap();
        let read_scope = ReadScope::new(&input_buffer);
        let mut read_context = binary::read::Context::new(&globals, read_scope.reader());

        match read_context.read_item(&core_module, &root_query) {
            Ok(value) => println!("{:?}", value),
            Err(err) => {
                return Err(format!("{:?}", err));
            }
        }
    }

    Ok(())
}
