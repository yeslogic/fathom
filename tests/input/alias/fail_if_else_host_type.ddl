extern Bool : Type;
extern true : Bool;
extern F64 : Type;
extern F32 : Type;

Test : Type =
    if true { F64 } else { F32 }; //~ error: cannot compile type level if expression for non-format types
