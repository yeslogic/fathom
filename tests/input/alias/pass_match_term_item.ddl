foo : Int = 33;

bar : Bool =
    match foo {
        33 => true,
        42 => false,
        _ => false,
    };

baz = bar;
