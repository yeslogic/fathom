extern crate ddl;

use std::str::FromStr;

use ddl::{check, syntax};
use ddl::syntax::Program;

#[test]
#[ignore]
fn cmap() {
    const SRC: &str = include_str!("../examples/ddl/cmap.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    let base_defs = syntax::base_defs();

    for def in &mut program.defs {
        for base_def in &base_defs {
            def.ty.substitute(&base_def.name, &base_def.ty);
        }
    }

    check::check_program(&program).unwrap();
}

#[test]
fn edid() {
    const SRC: &str = include_str!("../examples/ddl/edid.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    let base_defs = syntax::base_defs();

    for def in &mut program.defs {
        for base_def in &base_defs {
            def.ty.substitute(&base_def.name, &base_def.ty);
        }
    }

    check::check_program(&program).unwrap();
}

#[test]
fn heroes_of_might_and_magic_bmp() {
    const SRC: &str = include_str!("../examples/ddl/heroes_of_might_and_magic_bmp.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    let base_defs = syntax::base_defs();

    for def in &mut program.defs {
        for base_def in &base_defs {
            def.ty.substitute(&base_def.name, &base_def.ty);
        }
    }

    check::check_program(&program).unwrap();
}

#[test]
fn object_id() {
    const SRC: &str = include_str!("../examples/ddl/object_id.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    let base_defs = syntax::base_defs();

    for def in &mut program.defs {
        for base_def in &base_defs {
            def.ty.substitute(&base_def.name, &base_def.ty);
        }
    }

    check::check_program(&program).unwrap();
}

#[test]
fn stl() {
    const SRC: &str = include_str!("../examples/ddl/stl.ddl");

    let mut program = Program::from_str(SRC).unwrap();
    let base_defs = syntax::base_defs();

    for def in &mut program.defs {
        for base_def in &base_defs {
            def.ty.substitute(&base_def.name, &base_def.ty);
        }
    }

    check::check_program(&program).unwrap();
}
