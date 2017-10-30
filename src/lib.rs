#[cfg(test)]
extern crate difference;
// extern crate inflector;
extern crate lalrpop_util;
#[macro_use]
extern crate maplit;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
// #[macro_use]
// extern crate quote;
extern crate unicode_xid;

#[cfg(test)]
#[macro_use]
mod test;

pub mod syntax;
// pub mod compilers;
pub mod check;
pub mod name;
pub mod parser;
pub mod source;
