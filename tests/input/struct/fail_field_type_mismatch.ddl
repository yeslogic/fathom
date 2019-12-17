struct Foo {
    field_type : Host, //~ error: type mismatch
    field_true : true, //~ error: type mismatch
    field_false : true, //~ error: type mismatch
}
