extern crate ddl_stl;
extern crate ddl_util;

use ddl_stl::Stl;
use ddl_util::FromBinary;
use std::io::Cursor;

#[test]
fn test_stl_from_binary_beethoven() {
    let buf = include_bytes!("fixtures/beethoven.stl");
    let mut cursor = Cursor::new(&buf[..]);

    let stl = Stl::from_binary(&mut cursor).unwrap();

    let header = String::from_utf8(stl.header).unwrap();
    let expected_header = "Binary STL output from Blender: \
                           /Users/zachsmith/Desktop/Downloads/beethoven.stl";
    assert_eq!(header, expected_header);

    // Number of expected triangles is the same as the number actually parsed
    assert_eq!(stl.num_triangles as usize, stl.triangles.len());

    // Cursor is at EOF
    assert_eq!(cursor.position(), buf.len() as u64);
}

#[test]
fn test_stl_from_binary_cube() {
    let buf = include_bytes!("fixtures/cube.stl");
    let mut cursor = Cursor::new(&buf[..]);

    let stl = Stl::from_binary(&mut cursor).unwrap();

    let header = String::from_utf8(stl.header).unwrap();
    let expected_header = "Exported from Blender-2.79";
    assert!(header.starts_with(expected_header));

    // Number of expected triangles is the same as the number actually parsed
    assert_eq!(stl.num_triangles as usize, stl.triangles.len());

    // Cursor is at EOF
    assert_eq!(cursor.position(), buf.len() as u64);
}
