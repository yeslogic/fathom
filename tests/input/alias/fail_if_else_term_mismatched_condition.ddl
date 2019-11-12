extern Bool : Type;
extern true : Bool;
extern false : Bool;

test : Bool =
    if 33.4 { //~ error: cannot construct a `Bool` from a numeric literal
        true
    } else {
        false
    };
