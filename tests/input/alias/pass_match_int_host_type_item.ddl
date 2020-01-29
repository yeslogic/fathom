foo : Int =
    43;

Test : Host =
    match foo {
        42 => F64,
        _ => F32,
    };
