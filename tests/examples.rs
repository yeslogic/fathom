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

    let base_defs = ast::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_snapshot!(bitmap_ir, ir);
}

#[test]
fn bitmap_anon_elem() {
    const SRC: &str = include_str!("../examples/ddl/bitmap_anon_elem.ddl");

    let base_defs = ast::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_snapshot!(bitmap_anon_elem_ir, ir);
}

#[test]
#[ignore]
fn cmap() {
    const SRC: &str = include_str!("../examples/ddl/cmap.ddl");

    let base_defs = ast::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_snapshot!(cmap_ir, ir);
}

#[test]
fn edid() {
    const SRC: &str = include_str!("../examples/ddl/edid.ddl");

    let base_defs = ast::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_snapshot!(edid_ir, ir);
}

#[test]
fn heroes_of_might_and_magic_bmp() {
    const SRC: &str = include_str!("../examples/ddl/heroes_of_might_and_magic_bmp.ddl");

    let base_defs = ast::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_snapshot!(heroes_of_might_and_magic_bmp_ir, ir);
}

#[test]
#[ignore]
fn ieee754() {
    const SRC: &str = include_str!("../examples/ddl/ieee754.ddl");

    let base_defs = ast::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_snapshot!(ieee754_ir, ir);
}

#[test]
fn object_id() {
    const SRC: &str = include_str!("../examples/ddl/object_id.ddl");

    let base_defs = ast::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_snapshot!(object_id_ir, ir);
}

#[test]
fn stl() {
    const SRC: &str = include_str!("../examples/ddl/stl.ddl");

    let base_defs = ast::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();

    let ir = ddl::ir::owned::ast::Program::from(&program);
    assert_snapshot!(stl_ir, ir);
}
