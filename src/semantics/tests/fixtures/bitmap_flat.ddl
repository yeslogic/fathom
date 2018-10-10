module bitmap;

// FIXME: A tad hacky - add operators and proper bounds handling?
nat_mul : int {0 ..} -> int {0 ..} -> int {0 ..};
nat_mul = extern "int-mul";

struct Bitmap {
    header : Header,
    data : Array (nat_mul header.height header.width) (Color F32Be),
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
