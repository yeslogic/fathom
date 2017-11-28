use std::rc::Rc;

use super::*;
use super::host::{SignedType as St, TypeConst as Tc, UnsignedType as Ut};

mod ty_of {
    use super::*;
    use self::host::Type;

    #[test]
    fn var_unbound() {
        let ctx = Context::new();
        let src = "foo";
        let expr = Rc::new(src.parse().unwrap());

        assert!(ty_of(&ctx, &expr).is_err());
    }

    #[test]
    fn const_int() {
        let ctx = Context::new();
        let src = "1u8";
        let expr = Rc::new(src.parse().unwrap());
        let expected_ty = Rc::new(Type::Const(Tc::Unsigned(Ut::U8)));

        assert_eq!(ty_of(&ctx, &expr), Ok(expected_ty));
    }

    #[test]
    fn neg_int() {
        let ctx = Context::new();
        let src = "-(1i8 + 2i8)";
        let expr = Rc::new(src.parse().unwrap());
        let expected_ty = Rc::new(Type::Const(Tc::Signed(St::I8)));

        assert_eq!(ty_of(&ctx, &expr), Ok(expected_ty));
    }

    #[test]
    fn neg_bool() {
        let ctx = Context::new();
        let src = "-(1u8 == 2u8)";
        let expr = Rc::new(src.parse().unwrap());

        assert!(ty_of(&ctx, &expr).is_err());
    }

    #[test]
    fn not_int() {
        let ctx = Context::new();
        let src = "!(1u8 + 2u8)";
        let expr = Rc::new(src.parse().unwrap());

        assert!(ty_of(&ctx, &expr).is_err());
    }

    #[test]
    fn not_bool() {
        let ctx = Context::new();
        let src = "!(1u8 == 2u8)";
        let expr = Rc::new(src.parse().unwrap());
        let expected_ty = Rc::new(Type::Const(Tc::Bool));

        assert_eq!(ty_of(&ctx, &expr), Ok(expected_ty));
    }

    #[test]
    fn arith_ops() {
        let ctx = Context::new();
        let src = "1i8 + (1i8 * -2i8)";
        let expr = Rc::new(src.parse().unwrap());
        let expected_ty = Rc::new(Type::Const(Tc::Signed(St::I8)));

        assert_eq!(ty_of(&ctx, &expr), Ok(expected_ty));
    }

    #[test]
    fn cmp_ops_eq_int() {
        let ctx = Context::new();
        let src = "1u8 + (1u8 * 2u8) == 3u8";
        let expr = Rc::new(src.parse().unwrap());
        let expected_ty = Rc::new(Type::Const(Tc::Bool));

        assert_eq!(ty_of(&ctx, &expr), Ok(expected_ty));
    }

    #[test]
    fn cmp_ops_ne_int() {
        let ctx = Context::new();
        let src = "1u8 + (1u8 * 2u8) != 3u8";
        let expr = Rc::new(src.parse().unwrap());
        let expected_ty = Rc::new(Type::Const(Tc::Bool));

        assert_eq!(ty_of(&ctx, &expr), Ok(expected_ty));
    }

    #[test]
    fn cmp_ops_eq_bool() {
        let ctx = Context::new();
        let src = "(1u8 == 1u8) == (3u8 == 3u8)";
        let expr = Rc::new(src.parse().unwrap());
        let expected_ty = Rc::new(Type::Const(Tc::Bool));

        assert_eq!(ty_of(&ctx, &expr), Ok(expected_ty));
    }

    #[test]
    fn cmp_ops_ne_bool() {
        let ctx = Context::new();
        let src = "(1u8 == 1u8) != (3u8 == 3u8)";
        let expr = Rc::new(src.parse().unwrap());
        let expected_ty = Rc::new(Type::Const(Tc::Bool));

        assert_eq!(ty_of(&ctx, &expr), Ok(expected_ty));
    }

    #[test]
    fn rel_ops() {
        let ctx = Context::new();
        let src = "(1u8 == 3u8) & (2u8 == 2u8) | (1u8 == 2u8)";
        let expr = Rc::new(src.parse().unwrap());
        let expected_ty = Rc::new(Type::Const(Tc::Bool));

        assert_eq!(ty_of(&ctx, &expr), Ok(expected_ty));
    }
}

mod kind_of {
    use super::*;

    #[test]
    fn var_unbound() {
        let ctx = Context::new();
        let src = "foo";
        let ty = Rc::new(src.parse().unwrap());

        assert!(kind_of(&ctx, &ty).is_err());
    }
}
