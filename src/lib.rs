extern crate codespan;
extern crate codespan_reporting;
#[macro_use]
extern crate failure;
extern crate im;
extern crate lalrpop_util;
#[macro_use]
extern crate nameless;
extern crate pretty;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
extern crate unicode_xid;

pub mod semantics;
pub mod syntax;

#[cfg(feature = "cli")]
extern crate rustyline;
#[cfg(feature = "cli")]
#[macro_use]
extern crate structopt;
#[cfg(feature = "cli")]
extern crate term_size;

#[cfg(feature = "cli")]
pub mod cli;

use codespan::FileMap;
use codespan_reporting::Diagnostic;

use syntax::core::Module;

pub fn load_file(file: &FileMap) -> Result<Module, Vec<Diagnostic>> {
    use syntax::translation::Desugar;

    let mut diagnostics = Vec::new();

    let (module, errors) = syntax::parse::module(&file);
    diagnostics.extend(errors.iter().map(|err| err.to_diagnostic()));

    let module = module.desugar();
    match semantics::check_module(&module) {
        Ok(module) => Ok(module),
        Err(err) => {
            diagnostics.push(err.to_diagnostic());
            Err(diagnostics)
        },
    }
}
