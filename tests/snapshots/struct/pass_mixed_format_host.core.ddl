//! Test that a struct with a host type field produces a warning.

struct Test {
    format : item U32Be,
    host : !,
}
