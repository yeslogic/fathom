//! Parser for the data description language.

#![warn(rust_2018_idioms)]

use codespan::{FileId, Files};
use codespan_reporting::Diagnostic;
use ddl_concrete::Module;

pub fn parse_module(files: &Files, file_id: FileId) -> (Module, Vec<Diagnostic>) {
    let _source = files.source(file_id);

    (Module {}, Vec::new())
}
