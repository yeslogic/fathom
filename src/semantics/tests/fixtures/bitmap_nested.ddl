module bitmap;

struct Bitmap {
    header : Header,
    data : Array header.height (Array header.width (Color F32Be)),
};

struct Header {
    width : U32Be,
    height : U32Be,
};

struct Color (A : Type) {
    r : F32Be,
    g : F32Be,
    b : F32Be,
};
