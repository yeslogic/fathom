#[macro_use]
extern crate binary_macros;
extern crate ddl_edid;

use std::io::Cursor;
use ddl_edid::Edid;

#[test]
fn test_read_edid() {
    // Obtained on macOS using `ioreg -l | grep IODisplayEDID | awk '{print $7}'`
    let buf = base16!("00FFFFFFFFFFFF00061019A00000000030160104A5211578026FB1A7554C9E250C505400000001010101010101010101010101010101EF8340A0B0083470302036004BCF1000001A000000FC00436F6C6F72204C43440A20202000000010000000000000000000000000000000000010000000000000000000000000000000BC");

    let edid = Edid::read(&mut Cursor::new(&buf[..])).unwrap();

    assert_eq!(edid.header.magic, [0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00]);
    assert_eq!(edid.header.mfg_bytes, 4102);
    assert_eq!(edid.header.product_code, 40985);
    assert_eq!(edid.header.serial, 0);
    assert_eq!(edid.header.mfg_week, 48);
    assert_eq!(edid.header.mfg_year_mod, 22); // 1990+22 = 2012
    assert_eq!(edid.header.edid_version_major, 1);
    assert_eq!(edid.header.edid_version_minor, 4);

    // TODO: ...

    // panic!("{:#?}", edid);
}
