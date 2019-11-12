extern Bool : Type;
extern true : Bool;
extern false : Bool;
extern F32Be : Format;
extern F64Be : Format;

Test : Format =
    if true {
        if true { F64Be } else { F32Be }
    } else {
        if false { F64Be } else { F32Be }
    };
