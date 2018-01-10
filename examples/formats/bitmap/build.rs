extern crate ddl;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use ddl::syntax::ast::Program;
use ddl::syntax::parser::ast::Program as ParseProgram;

fn main() {
    let src = {
        let mut src_file = File::open("src/bitmap.ddl").unwrap();
        let mut src = String::new();
        src_file.read_to_string(&mut src).unwrap();
        src
    };

    let mut program = Program::from(&ParseProgram::from_str(&src).unwrap());
    program.substitute(&ddl::syntax::ast::base_defs());
    ddl::syntax::check::check_program(&program).unwrap();
    let ir = ddl::ir::ast::Program::from(&program);

    let out_dir = env::var("OUT_DIR").unwrap();
    let mut file = File::create(out_dir + "/bitmap.rs").unwrap();
    write!(file, "{}", ddl::codegen::LowerProgram(&ir)).unwrap();
}
