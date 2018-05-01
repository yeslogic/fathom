extern crate codespan;
extern crate codespan_reporting;
#[cfg(test)]
extern crate difference;
#[macro_use]
extern crate failure;
extern crate heck;
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;
extern crate pretty;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
extern crate unicode_xid;

#[cfg(test)]
#[macro_use]
mod test;

pub mod name;
pub mod var;

pub mod compile;
pub mod semantics;
pub mod syntax;

// use codespan::FileMap;
// use codespan_reporting::Diagnostic;

// pub fn load_file(file: &FileMap) -> Result<syntax::core::Module, Diagnostic> {
//     use syntax::core::Module;

//     let mut diagnostics = Vec::new();

//     let module = syntax::parse::module(&file).map_err(|err| err.to_diagnostic())?;
//     let mut module = Module::from_concrete(&module).unwrap();
//     semantics::check_module(&module).map_err(|err| err.to_diagnostic())?;
//     Ok(module)
// }
