//! Test that a struct with a host type field produces a warning.

extern U32Be : Format;
extern Bool : Type;

struct Test {
    format : U32Be,
    host : Bool, //~ error: type mismatch
}
