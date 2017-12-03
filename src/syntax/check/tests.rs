use std::rc::Rc;

use super::*;

mod ty_of {
    use super::*;

    macro_rules! assert_ty_of {
        ($given:expr, Ok($expected:expr)) => {{
            let ctx = Context::new();
            let expr = Rc::new($given.parse().unwrap());
            let expected_ty = Rc::new($expected.parse().unwrap());

            assert_eq!(ty_of(&ctx, &expr), Ok(expected_ty));
        }};
        ($given:expr, Err(_)) => {{
            let ctx = Context::new();
            let expr = Rc::new($given.parse().unwrap());

            assert!(ty_of(&ctx, &expr).is_err());
        }};
    }

    #[test]
    fn var_unbound() {
        assert_ty_of!("foo", Err(_));
    }

    #[test]
    fn const_u8() {
        assert_ty_of!("1u8", Ok("u8"));
    }

    #[test]
    fn neg_i8() {
        assert_ty_of!("-(1i8 + 2i8)", Ok("i8"));
    }

    #[test]
    fn neg_u8() {
        assert_ty_of!("-1u8", Err(_));
    }

    #[test]
    fn neg_bool() {
        assert_ty_of!("-(1u8 == 2u8)", Err(_));
    }

    #[test]
    fn not_u8() {
        assert_ty_of!("!(1u8 + 2u8)", Err(_));
    }

    #[test]
    fn not_bool() {
        assert_ty_of!("!(1u8 == 2u8)", Ok("bool"));
    }

    #[test]
    fn arith_ops() {
        assert_ty_of!("1i8 + (1i8 * -2i8)", Ok("i8"));
    }

    #[test]
    fn cmp_ops_eq_u8() {
        assert_ty_of!("1u8 + (1u8 * 2u8) == 3u8", Ok("bool"));
    }

    #[test]
    fn cmp_ops_ne_u8() {
        assert_ty_of!("1u8 + (1u8 * 2u8) != 3u8", Ok("bool"));
    }

    #[test]
    fn cmp_ops_eq_bool() {
        assert_ty_of!("(1u8 == 1u8) == (3u8 == 3u8)", Ok("bool"));
    }

    #[test]
    fn cmp_ops_ne_bool() {
        assert_ty_of!("(1u8 == 1u8) != (3u8 == 3u8)", Ok("bool"));
    }

    #[test]
    fn rel_ops() {
        assert_ty_of!("(1u8 == 3u8) && (2u8 == 2u8) || (1u8 == 2u8)", Ok("bool"));
    }

    #[test]
    fn cast_bool() {
        assert_ty_of!("(1u8 == 3u8) as u8", Err(_));
    }

    #[test]
    fn cast_u8_to_u32() {
        assert_ty_of!("1u8 as u32", Ok("u32"));
    }

    #[test]
    fn cast_u8_to_f32() {
        assert_ty_of!("1u8 as f32", Ok("f32"));
    }

    #[test]
    fn cast_u8_to_f32_to_u16() {
        assert_ty_of!("1u8 as f32 as u16", Ok("u16"));
    }

    #[test]
    fn cast_u8_to_bool() {
        assert_ty_of!("1u8 as bool", Err(_));
    }
}

mod kind_of {
    use std::str::FromStr;
    use syntax::ast;

    use super::*;

    macro_rules! assert_kind_of {
        ($given:expr, Ok($expected:expr)) => {{
            let ctx = Context::new();
            let mut ty = Rc::new(binary::Type::from_str($given).unwrap());
            Rc::make_mut(&mut ty).substitute(&ast::base_defs());

            assert_eq!(kind_of(&ctx, &ty), Ok($expected));
        }};
        ($given:expr, Err(_)) => {{
            let ctx = Context::new();
            let ty = Rc::new($given.parse().unwrap());

            assert!(kind_of(&ctx, &ty).is_err());
        }};
    }

    #[test]
    fn var_unbound() {
        assert_kind_of!("foo", Err(_));
    }

    #[test]
    fn assert_magic() {
        assert_kind_of!(
            "u64le where magic => magic == 0x00ffffffffffff00u64",
            Ok(binary::Kind::Type)
        );
    }
}
