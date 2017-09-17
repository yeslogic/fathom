//! endian: le

/// http://formats.kaitai.io/heroes_of_might_and_magic_bmp/index.html
HerosOfMightAndMagicBmp = struct {
    magic: u16,
    width: u16,
    height: u16,
    data: [u8; width * height],
};
