/// Test that one can refer to local term aliases in aliases.
extern Bool : Type;

extern true : item Bool;

Foo = item true;

Bar = item Foo;
