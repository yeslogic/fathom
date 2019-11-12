extern Bool : Type;
extern true : Bool;
extern F32Be : Format;
extern F64Be : Format;

Test : Format =
    if true { F64Be } else { F32Be };
