test : Bool =
    match 33 : Int {
        33 => true,
        33 => true, //~ warning: unreachable pattern
        42 => false,
        foo => false,
        _ => false, //~ warning: unreachable pattern
    };
