extern Bool : Type;

extern true : item Bool;

extern F64 : Type;

extern F32 : Type;

foo = item true;

Test = bool_elim item foo { item F64, item F32 } : Type;
