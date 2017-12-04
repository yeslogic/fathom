extern crate ddl;
extern crate difference;

#[path = "../src/test.rs"]
#[macro_use]
mod test;

use std::str::FromStr;

use ddl::syntax::{ast, check};
use ddl::syntax::ast::Program;

#[test]
fn bitmap() {
    const SRC: &str = include_str!("../examples/formats/bitmap/src/bitmap.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(bitmap_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::ast::Program::from(&program);
    assert_debug_snapshot!(bitmap_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(bitmap_codegen, rust_output);
}

#[test]
fn bson() {
    const SRC: &str = include_str!("../examples/formats/bson/src/bson.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(bson_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::ast::Program::from(&program);
    assert_debug_snapshot!(bson_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(bson_codegen, rust_output);
}

#[test]
fn edid() {
    const SRC: &str = include_str!("../examples/formats/edid/src/edid.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(edid_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::ast::Program::from(&program);
    assert_debug_snapshot!(edid_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(edid_codegen, rust_output);
}

#[test]
fn object_id() {
    const SRC: &str = include_str!("../examples/formats/object_id/src/object_id.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(object_id_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::ast::Program::from(&program);
    assert_debug_snapshot!(object_id_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(object_id_codegen, rust_output);
}

#[test]
fn stl() {
    const SRC: &str = include_str!("../examples/formats/stl/src/stl.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(stl_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::ast::Program::from(&program);
    assert_debug_snapshot!(stl_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(stl_codegen, rust_output);
}
