foo =
    true;

Test : Host =
    if foo {
        F64 //~ error: attempted to compile a non-format type as a host type
    } else {
        F32 //~ error: attempted to compile a non-format type as a host type
    };
