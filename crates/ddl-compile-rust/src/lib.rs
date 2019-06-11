#![warn(rust_2018_idioms)]

use codespan_reporting::Diagnostic;
use ddl_concrete as concrete;

pub fn compile_module(_module: &concrete::Module) -> ((), Vec<Diagnostic>) {
    ((), Vec::new())
}
