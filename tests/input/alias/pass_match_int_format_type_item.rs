#![cfg(test)]

// use ddl_test_util::ddl::binary;
use ddl_rt::{ReadError, ReadScope, FormatWriter, U8, F64Le};

#[path = "../../snapshots/alias/pass_match_int_format_type_item.rs"]
mod fixture;

ddl_test_util::core_module!(FIXTURE, "../../snapshots/alias/pass_match_int_format_type_item.core.ddl");

#[test]
fn eof_inner() {
    let writer = FormatWriter::new(vec![]);

    let scope = ReadScope::new(writer.buffer());
    let singleton = scope.read::<fixture::Test>();

    match singleton {
        Err(ReadError::Eof(_)) => {},
        Err(err) => panic!("eof error expected, found: {:?}", err),
        Ok(_) => panic!("error expected, found: Ok(_)")
    }

    // TODO: Check remaining
}

#[test]
fn valid_test() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<F64Le>(23.64e10); // Test::inner

    let scope = ReadScope::new(writer.buffer());
    let singleton = scope.read::<fixture::Test>().unwrap();

    // FIXME(#162): Requires us to evaluate items!
    // let test = binary::read::read_module_item(&FIXTURE, &"Test", &mut scope.reader()).unwrap();

    match singleton.inner() {
        fixture::Enum0::Variant0(inner) => {
            assert_eq!(inner, 23.64e10);
            // assert_eq!(test, binary::Term::F64(inner));
        },
        _ => panic!("expected `Enum0::Variant0(_)`"),
    }

    // TODO: Check remaining
}

#[test]
fn valid_test_trailing() {
    let mut writer = FormatWriter::new(vec![]);
    writer.write::<F64Le>(781.453298); // Test::inner
    writer.write::<U8>(42);

    let scope = ReadScope::new(writer.buffer());
    let singleton = scope.read::<fixture::Test>().unwrap();

    // FIXME(#162): Requires us to evaluate items!
    // let test = binary::read::read_module_item(&FIXTURE, &"Test", &mut scope.reader()).unwrap();

    match singleton.inner() {
        fixture::Enum0::Variant0(inner) => {
            assert_eq!(inner, 781.453298);
            // assert_eq!(test, binary::Term::F64(inner));
        },
        _ => panic!("expected `Enum0::Variant0(_)`"),
    }

    // TODO: Check remaining
}
