extern Bool : Type;
extern true : Bool;
extern false : Bool;

test : Bool =
    if true { true } else { false };
