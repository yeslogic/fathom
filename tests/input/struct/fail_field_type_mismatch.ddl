extern Bool : Type;
extern true : Bool;

struct Foo {
    field_type : Type, //~ error: type mismatch
    field_true : true, //~ error: type mismatch
    field_false : true, //~ error: type mismatch
}
