//! Utilities for testing the binary data description language.

#![warn(rust_2018_idioms)]

pub use codespan;
pub use ddl;
pub use lazy_static;

#[macro_export]
macro_rules! core_module {
    ($IDENT:ident, $path:literal) => {
        $crate::lazy_static::lazy_static! {
            static ref $IDENT: $crate::ddl::core::Module = {
                use std::fs;

                let mut files = $crate::codespan::Files::new();
                const SOURCE: &str = include_str!($path);
                let file_id = files.add($path.to_string(), SOURCE);

                let lexer = $crate::ddl::lexer::Lexer::new(&files, file_id);
                $crate::ddl::core::Module::parse(file_id, lexer, &mut |_| {}) // FIXME: Log syntax errors?
            };
        }
    };
}
