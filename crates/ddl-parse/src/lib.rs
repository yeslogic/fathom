//! Parser for the data description language.

#![warn(rust_2018_idioms)]

use ddl_concrete::Module;

pub fn parse_module(src: &str) -> Result<Module, String> {
    println!("{}", src);
    if src.is_empty() {
        Ok(Module {})
    } else {
        Err(format!("unexpected input: {}", src))
    }
}
