extern crate ddl;

use std::str::FromStr;

use ddl::{check, syntax};
use ddl::syntax::Program;

#[test]
#[ignore]
fn cmap() {
    const SRC: &str = include_str!("../examples/ddl/cmap.ddl");

    let base_defs = syntax::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();
}

#[test]
fn edid() {
    const SRC: &str = include_str!("../examples/ddl/edid.ddl");

    let base_defs = syntax::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();
}

#[test]
fn heroes_of_might_and_magic_bmp() {
    const SRC: &str = include_str!("../examples/ddl/heroes_of_might_and_magic_bmp.ddl");

    let base_defs = syntax::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();
}

#[test]
fn ieee754() {
    const SRC: &str = include_str!("../examples/ddl/ieee754.ddl");

    let base_defs = syntax::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();
}

#[test]
fn object_id() {
    const SRC: &str = include_str!("../examples/ddl/object_id.ddl");

    let base_defs = syntax::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();
}

#[test]
fn stl() {
    const SRC: &str = include_str!("../examples/ddl/stl.ddl");

    let base_defs = syntax::base_defs();
    let mut program = Program::from_str(SRC).unwrap();
    program.substitute(&base_defs);

    check::check_program(&program).unwrap();
}
