Test : Format =
    match 0 : Int {
        0 => F64Le,
        _ => F64Be,
    };
