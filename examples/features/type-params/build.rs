extern crate ddl;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use ddl::syntax::ast::Module;

fn main() {
    let src = {
        let mut src_file = File::open("src/pair.ddl").unwrap();
        let mut src = String::new();
        src_file.read_to_string(&mut src).unwrap();
        src
    };

    let mut module = Module::from_parse(&ddl::parser::module(&src).unwrap()).unwrap();
    module.substitute(&ddl::syntax::ast::base_defs());
    ddl::check::check_module(&module).unwrap();
    let ir = ddl::ir::ast::Module::from(&module);

    let out_dir = env::var("OUT_DIR").unwrap();
    let mut file = File::create(out_dir + "/pair.rs").unwrap();
    write!(file, "{}", ddl::codegen::LowerModule(&ir)).unwrap();
}
