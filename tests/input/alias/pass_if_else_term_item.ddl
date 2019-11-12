extern Bool : Type;
extern true : Bool;
extern false : Bool;

foo =
    true;

bar : Bool =
    if foo { true } else { false };

baz = bar;
