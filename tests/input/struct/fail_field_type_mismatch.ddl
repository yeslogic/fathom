struct Foo {
    field_type : Type, //~ error: cannot synthesize the type of `Type`
    field_true : true, //~ error: type mismatch
    field_false : true, //~ error: type mismatch
}
