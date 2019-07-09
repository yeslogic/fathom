//! Parser for the data description language.

use codespan::{FileId};
use codespan_reporting::diagnostic::{Diagnostic};

use crate::concrete::Module;
use crate::lexer::Lexer;
use crate::diagnostics;

mod grammar {
    include!(concat!(env!("OUT_DIR"), "/parse/grammar.rs"));
}

pub fn parse_module(
    file_id: FileId,
    lexer: Lexer<'_>,
    report: &mut dyn FnMut(Diagnostic),
) -> Module {
    let module = grammar::ModuleParser::new()
        .parse(file_id, report, lexer)
        .unwrap_or_else(|error| {
            report(diagnostics::parse_error(file_id, error));
            Module {
                file_id,
                items: Vec::new(),
            }
        });

    module
}
