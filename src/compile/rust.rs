use codespan_reporting::Diagnostic;
use std::io;
use std::io::prelude::*;

use crate::core;

pub fn compile_module(
    writer: &mut impl Write,
    module: &core::Module,
) -> io::Result<Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();

    let pkg_name = env!("CARGO_PKG_NAME");
    let pkg_version = env!("CARGO_PKG_VERSION");

    writeln!(
        writer,
        "// This file is automatically @generated by {} {}",
        pkg_name, pkg_version,
    )?;
    writeln!(writer, "// It is not intended for manual editing.")?;

    for item in &module.items {
        match item {
            core::Item::Struct {
                span: _,
                doc,
                name,
                fields,
            } => {
                writeln!(writer)?;

                for doc_line in doc.lines() {
                    match doc_line {
                        line if line.trim().is_empty() => writeln!(writer, "///")?,
                        line => writeln!(writer, "/// {}", line)?,
                    }
                }

                if fields.is_empty() {
                    writeln!(writer, "pub struct {} {{}}", name)?;
                } else {
                    writeln!(writer, "pub struct {} {{", name)?;
                    for (field_doc, _, field_name, field_ty) in fields {
                        for doc_line in field_doc.lines() {
                            match doc_line {
                                line if line.trim().is_empty() => writeln!(writer, "    ///")?,
                                line => writeln!(writer, "    /// {}", line)?,
                            }
                        }

                        write!(writer, "    pub {}: ", field_name)?;
                        compile_ty(writer, field_ty, &mut diagnostics)?;
                        writeln!(writer, ",")?;
                    }
                    writeln!(writer, "}}")?;
                }
            }
        }
    }

    Ok(diagnostics)
}

fn compile_ty(
    writer: &mut impl Write,
    term: &core::Term,
    _diagnostics: &mut Vec<Diagnostic>,
) -> io::Result<()> {
    match term {
        core::Term::U8(_) => write!(writer, "u8"),
        // TODO: figure out what to do here!
        core::Term::Error(_) => unimplemented!("compile error"),
    }
}
