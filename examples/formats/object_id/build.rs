extern crate ddl;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use ddl::syntax::core::Module;

fn main() {
    let src = {
        let mut src_file = File::open("src/object_id.ddl").unwrap();
        let mut src = String::new();
        src_file.read_to_string(&mut src).unwrap();
        src
    };

    let mut module = Module::from_concrete(&ddl::syntax::parse::module(&src).unwrap()).unwrap();
    module.substitute(&ddl::syntax::core::base_defs());
    ddl::semantics::check_module(&module).unwrap();
    let ir = ddl::compile::ir::Module::from(&module);

    let out_dir = env::var("OUT_DIR").unwrap();
    let mut file = File::create(out_dir + "/object_id.rs").unwrap();
    write!(file, "{}", ddl::compile::codegen::LowerModule(&ir)).unwrap();
}
