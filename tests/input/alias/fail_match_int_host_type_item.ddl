foo : Int =
    43;

Test : Type =
    match foo {
        42 => F64, //~ error: attempted to compile a non-format type as a host type
        _ => F32, //~ error: attempted to compile a non-format type as a host type
    };
