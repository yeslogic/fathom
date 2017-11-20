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
    const SRC: &str = include_str!("../examples/ddl/bitmap.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(bitmap_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_debug_snapshot!(bitmap_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(bitmap_codegen, rust_output);
}

#[test]
fn bitmap_anon_elem() {
    const SRC: &str = include_str!("../examples/ddl/bitmap_anon_elem.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(bitmap_anon_elem_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_debug_snapshot!(bitmap_anon_elem_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(bitmap_anon_elem_codegen, rust_output);
}

#[test]
#[ignore]
fn cmap() {
    const SRC: &str = include_str!("../examples/ddl/cmap.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(cmap_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_debug_snapshot!(cmap_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(cmap_codegen, rust_output);
}

#[test]
fn edid() {
    const SRC: &str = include_str!("../examples/ddl/edid.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(edid_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_debug_snapshot!(edid_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(edid_codegen, rust_output);
}

#[test]
fn heroes_of_might_and_magic_bmp() {
    const SRC: &str = include_str!("../examples/ddl/heroes_of_might_and_magic_bmp.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(heroes_of_might_and_magic_bmp_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_debug_snapshot!(heroes_of_might_and_magic_bmp_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(heroes_of_might_and_magic_bmp_codegen, rust_output);
}

#[test]
#[ignore]
fn ieee754() {
    const SRC: &str = include_str!("../examples/ddl/ieee754.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(ieee754_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_debug_snapshot!(ieee754_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(ieee754_codegen, rust_output);
}

#[test]
fn object_id() {
    const SRC: &str = include_str!("../examples/ddl/object_id.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(object_id_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_debug_snapshot!(object_id_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(object_id_codegen, rust_output);
}

#[test]
fn stl() {
    const SRC: &str = include_str!("../examples/ddl/stl.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    assert_debug_snapshot!(stl_program, program);

    let base_defs = ast::base_defs();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_debug_snapshot!(stl_ir, ir);

    let rust_output = ddl::codegen::LowerProgram(&ir).to_string();
    assert_display_snapshot!(stl_codegen, rust_output);
}
