#![warn(rust_2018_idioms)]

use codespan_reporting::Diagnostic;
use ddl_concrete as concrete;
use std::io;
use std::io::prelude::*;

pub fn compile_module(
    writer: &mut impl Write,
    module: &concrete::Module,
) -> io::Result<Vec<Diagnostic>> {
    let diagnostics = Vec::new();

    for item in &module.items {
        match item {
            concrete::Item::Struct(_, name) => {
                writeln!(writer, "pub struct {} {{}}", name)?;
            }
        }
    }

    Ok(diagnostics)
}
