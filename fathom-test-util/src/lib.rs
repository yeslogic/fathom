//! Utilities for testing Fathom.

#![warn(rust_2018_idioms)]

pub use codespan_reporting;
pub use fathom;
pub use lazy_static;

#[macro_export]
macro_rules! assert_is_equal {
    ($globals:expr, $result0:expr, $result1:expr $(,)?) => {{
        let (value0, links0) = $result0;
        let (value1, links1) = $result1;

        let items = std::collections::HashMap::new();

        // TODO: better error reporting?
        assert!($crate::fathom::lang::core::semantics::is_equal(
            &$globals, &items, &value0, &value1,
        ));
        for (offset, offset_value1) in links1 {
            assert!($crate::fathom::lang::core::semantics::is_equal(
                &$globals,
                &items,
                &links0[&offset],
                &offset_value1,
            ));
        }
    }};
}

#[macro_export]
macro_rules! core_module {
    ($IDENT:ident, $path:literal) => {
        $crate::lazy_static::lazy_static! {
            static ref $IDENT: $crate::fathom::lang::core::Module = {
                use std::fs;

                let mut files = $crate::codespan_reporting::files::SimpleFiles::new();
                const SOURCE: &str = include_str!($path);
                let file_id = files.add($path.to_string(), SOURCE.to_string());

                let mut messages = Vec::new();
                $crate::fathom::lang::core::Module::parse(file_id, SOURCE, &mut messages) // FIXME: Log syntax errors?
            };
        }
    };
}
