Bitmap = struct {
    extents : struct {
        width : u32be,
        height : u32be,
    },
    data : [
        struct {
            r : u8,
            g : u8,
            b : u8,
            a : u8,
        };
        extents.width * extents.height
    ],
};

Bmp = Bitmap;
