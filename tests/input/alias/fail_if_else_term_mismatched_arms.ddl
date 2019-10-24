Foo : F32 = 33.4;

test : Bool =
    if true {
        true
    } else {
        Foo //~ error: type mismatch
    };
