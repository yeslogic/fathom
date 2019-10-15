//! Test that a struct with a host type field produces a warning.

struct Test {
    format : U32Be,
    host : Bool, //~ error: type mismatch
}
