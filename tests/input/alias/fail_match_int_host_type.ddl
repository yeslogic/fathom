Test : Type =
    match 42 : Int {
        0 => F64, //~ error: attempted to compile a non-format type as a host type
        _ => F32, //~ error: attempted to compile a non-format type as a host type
    };
