//! Utilities for testing Fathom.

#![warn(rust_2018_idioms)]

pub use codespan_reporting;
pub use fathom;
pub use lazy_static;

#[macro_export]
macro_rules! core_module {
    ($IDENT:ident, $path:literal) => {
        $crate::lazy_static::lazy_static! {
            static ref $IDENT: $crate::fathom::lang::core::Module = {
                use std::fs;

                let mut files = $crate::codespan_reporting::files::SimpleFiles::new();
                const SOURCE: &str = include_str!($path);
                let file_id = files.add($path.to_string(), SOURCE.to_string());

                let keywords = &$crate::fathom::lexer::CORE_KEYWORDS;
                let lexer = $crate::fathom::lexer::Lexer::new(&files, file_id, keywords);
                $crate::fathom::lang::core::Module::parse(file_id, lexer, &mut |_| {}) // FIXME: Log syntax errors?
            };
        }
    };
}
