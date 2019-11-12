extern Bool : Type;
extern true : Bool;
extern F64 : Type;

Test : if true { F64 } else { Bool } = 0.1;
