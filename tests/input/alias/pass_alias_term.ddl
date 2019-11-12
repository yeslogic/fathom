/// Test that one can refer to local term aliases in aliases.

extern Bool : Type;
extern true : Bool;

Foo = true;
Bar = Foo;
