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
            concrete::Item::Struct(_, doc, name) => {
                for doc_line in doc.lines() {
                    match doc_line {
                        line if line.trim().is_empty() => writeln!(writer, "///")?,
                        line => writeln!(writer, "/// {}", line)?,
                    }
                }
                writeln!(writer, "pub struct {} {{}}", name)?;
            }
        }
    }

    Ok(diagnostics)
}
