Pixel = struct {
    r : u8,
    g : u8,
    b : u8,
    a : u8,
};

Bitmap = struct {
    extents : struct {
        width : u32be,
        height : u32be,
    },
    data : [Pixel; extents.width * extents.height],
};

Bmp = Bitmap;
