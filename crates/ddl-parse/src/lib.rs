//! Parser for the data description language.

#![warn(rust_2018_idioms)]

use codespan::{FileId, Files};
use codespan_reporting::{Diagnostic, Label};
use ddl_concrete::Module;

mod lexer;

use self::lexer::Lexer;

pub fn parse_module(files: &Files, file_id: FileId) -> (Module, Vec<Diagnostic>) {
    let mut diagnostics = Vec::new();
    let lexer = Lexer::new(files, file_id);

    for token in lexer {
        match token {
            Ok((start, token, end)) => diagnostics.push(Diagnostic::new_error(
                format!("unexpected token `{}`", token),
                Label::new(file_id, start..end, "unexpected token"),
            )),
            Err(diagnostic) => diagnostics.push(diagnostic),
        }
    }

    (Module {}, diagnostics)
}
