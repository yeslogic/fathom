extern crate ddl;

use ddl::{check, parser, syntax};

#[test]
#[ignore]
fn cmap() {
    const SRC: &str = include_str!("../examples/ddl/cmap.ddl");

    let defs = parser::parse(SRC).unwrap();
    check::check_defs(syntax::base_defs().iter().chain(&defs)).unwrap();
}

#[test]
fn edid() {
    const SRC: &str = include_str!("../examples/ddl/edid.ddl");

    let defs = parser::parse(SRC).unwrap();
    check::check_defs(syntax::base_defs().iter().chain(&defs)).unwrap();
}

#[test]
fn heroes_of_might_and_magic_bmp() {
    const SRC: &str = include_str!("../examples/ddl/heroes_of_might_and_magic_bmp.ddl");

    let defs = parser::parse(SRC).unwrap();
    check::check_defs(syntax::base_defs().iter().chain(&defs)).unwrap();
}

#[test]
fn object_id() {
    const SRC: &str = include_str!("../examples/ddl/object_id.ddl");

    let defs = parser::parse(SRC).unwrap();
    check::check_defs(syntax::base_defs().iter().chain(&defs)).unwrap();
}

#[test]
fn stl() {
    const SRC: &str = include_str!("../examples/ddl/stl.ddl");

    let defs = parser::parse(SRC).unwrap();
    check::check_defs(syntax::base_defs().iter().chain(&defs)).unwrap();
}
