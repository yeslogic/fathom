#[cfg(test)]
extern crate difference;
#[macro_use]
extern crate failure;
extern crate heck;
extern crate lalrpop_util;
extern crate pretty;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
extern crate unicode_xid;

#[cfg(test)]
#[macro_use]
mod test;

pub mod name;
pub mod source;
pub mod var;

pub mod check;
pub mod codegen;
pub mod ir;
pub mod parser;
pub mod syntax;
