extern crate ddl;
extern crate difference;

#[path = "../src/test.rs"]
#[macro_use]
mod test;

use ddl::syntax::check;
use ddl::syntax::ast::Module;
use ddl::parser::ast::Module as ParseModule;

#[test]
fn bitmap() {
    const SRC: &str = include_str!("../examples/formats/bitmap/src/bitmap.ddl");

    let mut module = Module::from_parse(&ParseModule::from_str(SRC).unwrap()).unwrap();
    assert_debug_snapshot!(bitmap_module, module);

    let base_defs = Module::prelude();
    module.substitute(&base_defs);

    check::check_module(&module).unwrap();

    let ir = ddl::ir::ast::Module::from(&module);
    assert_debug_snapshot!(bitmap_ir, ir);

    let rust_output = ddl::codegen::LowerModule(&ir).to_string();
    assert_display_snapshot!(bitmap_codegen, rust_output);
}

#[test]
fn bson() {
    const SRC: &str = include_str!("../examples/formats/bson/src/bson.ddl");

    let mut module = Module::from_parse(&ParseModule::from_str(SRC).unwrap()).unwrap();
    assert_debug_snapshot!(bson_module, module);

    let base_defs = Module::prelude();
    module.substitute(&base_defs);

    check::check_module(&module).unwrap();

    let ir = ddl::ir::ast::Module::from(&module);
    assert_debug_snapshot!(bson_ir, ir);

    let rust_output = ddl::codegen::LowerModule(&ir).to_string();
    assert_display_snapshot!(bson_codegen, rust_output);
}

#[test]
fn edid() {
    const SRC: &str = include_str!("../examples/formats/edid/src/edid.ddl");

    let mut module = Module::from_parse(&ParseModule::from_str(SRC).unwrap()).unwrap();
    assert_debug_snapshot!(edid_module, module);

    let base_defs = Module::prelude();
    module.substitute(&base_defs);

    check::check_module(&module).unwrap();

    let ir = ddl::ir::ast::Module::from(&module);
    assert_debug_snapshot!(edid_ir, ir);

    let rust_output = ddl::codegen::LowerModule(&ir).to_string();
    assert_display_snapshot!(edid_codegen, rust_output);
}

#[test]
fn object_id() {
    const SRC: &str = include_str!("../examples/formats/object_id/src/object_id.ddl");

    let mut module = Module::from_parse(&ParseModule::from_str(SRC).unwrap()).unwrap();
    assert_debug_snapshot!(object_id_module, module);

    let base_defs = Module::prelude();
    module.substitute(&base_defs);

    check::check_module(&module).unwrap();

    let ir = ddl::ir::ast::Module::from(&module);
    assert_debug_snapshot!(object_id_ir, ir);

    let rust_output = ddl::codegen::LowerModule(&ir).to_string();
    assert_display_snapshot!(object_id_codegen, rust_output);
}

#[test]
fn stl() {
    const SRC: &str = include_str!("../examples/formats/stl/src/stl.ddl");

    let mut module = Module::from_parse(&ParseModule::from_str(SRC).unwrap()).unwrap();
    assert_debug_snapshot!(stl_module, module);

    let base_defs = Module::prelude();
    module.substitute(&base_defs);

    check::check_module(&module).unwrap();

    let ir = ddl::ir::ast::Module::from(&module);
    assert_debug_snapshot!(stl_ir, ir);

    let rust_output = ddl::codegen::LowerModule(&ir).to_string();
    assert_display_snapshot!(stl_codegen, rust_output);
}
