foo =
    true;

Test : Type =
    if foo { F64 } else { F32 }; //~ error: cannot compile type level if expression for non-format types
