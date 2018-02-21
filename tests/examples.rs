extern crate codespan;
extern crate ddl;
extern crate difference;

#[path = "../src/test.rs"]
#[macro_use]
mod test;

use codespan::CodeMap;
use ddl::syntax::core::Module;

#[test]
fn bitmap() {
    let mut codemap = CodeMap::new();
    let filemap = codemap
        .add_filemap_from_disk("examples/formats/bitmap/src/bitmap.ddl")
        .unwrap();

    let module = ddl::syntax::parse::module(&filemap).unwrap();
    let mut module = Module::from_concrete(&module).unwrap();
    assert_debug_snapshot!(bitmap_module, module);

    let base_defs = ddl::syntax::core::base_defs();
    module.substitute(&base_defs);

    ddl::semantics::check_module(&module).unwrap();

    let ir = ddl::compile::ir::Module::from(&module);
    assert_debug_snapshot!(bitmap_ir, ir);

    let rust_output = ddl::compile::codegen::LowerModule(&ir).to_string();
    assert_display_snapshot!(bitmap_codegen, rust_output);
}

#[test]
fn bson() {
    let mut codemap = CodeMap::new();
    let filemap = codemap
        .add_filemap_from_disk("examples/formats/bson/src/bson.ddl")
        .unwrap();

    let module = ddl::syntax::parse::module(&filemap).unwrap();
    let mut module = Module::from_concrete(&module).unwrap();
    assert_debug_snapshot!(bson_module, module);

    let base_defs = ddl::syntax::core::base_defs();
    module.substitute(&base_defs);

    ddl::semantics::check_module(&module).unwrap();

    let ir = ddl::compile::ir::Module::from(&module);
    assert_debug_snapshot!(bson_ir, ir);

    let rust_output = ddl::compile::codegen::LowerModule(&ir).to_string();
    assert_display_snapshot!(bson_codegen, rust_output);
}

#[test]
fn edid() {
    let mut codemap = CodeMap::new();
    let filemap = codemap
        .add_filemap_from_disk("examples/formats/edid/src/edid.ddl")
        .unwrap();

    let module = ddl::syntax::parse::module(&filemap).unwrap();
    let mut module = Module::from_concrete(&module).unwrap();
    assert_debug_snapshot!(edid_module, module);

    let base_defs = ddl::syntax::core::base_defs();
    module.substitute(&base_defs);

    ddl::semantics::check_module(&module).unwrap();

    let ir = ddl::compile::ir::Module::from(&module);
    assert_debug_snapshot!(edid_ir, ir);

    let rust_output = ddl::compile::codegen::LowerModule(&ir).to_string();
    assert_display_snapshot!(edid_codegen, rust_output);
}

#[test]
fn object_id() {
    let mut codemap = CodeMap::new();
    let filemap = codemap
        .add_filemap_from_disk("examples/formats/object_id/src/object_id.ddl")
        .unwrap();

    let module = ddl::syntax::parse::module(&filemap).unwrap();
    let mut module = Module::from_concrete(&module).unwrap();
    assert_debug_snapshot!(object_id_module, module);

    let base_defs = ddl::syntax::core::base_defs();
    module.substitute(&base_defs);

    ddl::semantics::check_module(&module).unwrap();

    let ir = ddl::compile::ir::Module::from(&module);
    assert_debug_snapshot!(object_id_ir, ir);

    let rust_output = ddl::compile::codegen::LowerModule(&ir).to_string();
    assert_display_snapshot!(object_id_codegen, rust_output);
}

#[test]
fn stl() {
    let mut codemap = CodeMap::new();
    let filemap = codemap
        .add_filemap_from_disk("examples/formats/stl/src/stl.ddl")
        .unwrap();

    let module = ddl::syntax::parse::module(&filemap).unwrap();
    let mut module = Module::from_concrete(&module).unwrap();
    assert_debug_snapshot!(stl_module, module);

    let base_defs = ddl::syntax::core::base_defs();
    module.substitute(&base_defs);

    ddl::semantics::check_module(&module).unwrap();

    let ir = ddl::compile::ir::Module::from(&module);
    assert_debug_snapshot!(stl_ir, ir);

    let rust_output = ddl::compile::codegen::LowerModule(&ir).to_string();
    assert_display_snapshot!(stl_codegen, rust_output);
}
