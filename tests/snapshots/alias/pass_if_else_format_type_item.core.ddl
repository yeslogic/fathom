extern Bool : Type;

extern true : item Bool;

extern F32Be : Format;

extern F64Be : Format;

foo = item true;

Test = bool_elim item foo { item F64Be, item F32Be } : Format;
