extern Bool : Type;
extern true : Bool;
extern F32Be : Format;
extern F64Be : Format;

foo =
    true;

Test : Format =
    if foo { F64Be } else { F32Be };
