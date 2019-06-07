//! Integration tests for the data description language.

#![cfg(test)]
#![warn(rust_2018_idioms)]

mod support;

macro_rules! test {
    ($test_name:ident, $test_path:literal) => {
        #[test]
        fn $test_name() {
            support::run_test(stringify!($test_name), $test_path);
        }
    };
}

test!(empty, "empty.ddl");
test!(broken, "broken.ddl");
