/// http://formats.kaitai.io/heroes_of_might_and_magic_bmp/index.html
HerosOfMightAndMagicBmp = struct {
    magic: u16le,
    width: u16le,
    height: u16le,
    data: [u8; width * height],
};
