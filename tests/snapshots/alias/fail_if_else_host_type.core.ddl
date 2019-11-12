extern Bool : Type;

extern true : item Bool;

extern F64 : Type;

extern F32 : Type;

Test = bool_elim item true { item F64, item F32 } : Type;
