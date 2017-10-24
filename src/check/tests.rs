use parser;
use super::*;

mod ty_of {
    use super::*;
    use self::host::{Type, TypeConst};

    #[test]
    fn const_int() {
        let ctx = Ctx::new();
        let src = "1";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type::Const(TypeConst::Int)));
    }

    #[test]
    fn neg_int() {
        let ctx = Ctx::new();
        let src = "-(1 + 2)";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type::Const(TypeConst::Int)));
    }

    #[test]
    fn neg_bool() {
        let ctx = Ctx::new();
        let src = "-(1 == 2)";
        let expr = parser::parse_expr(src).unwrap();

        assert!(ty_of(&ctx, &expr).is_err());
    }

    #[test]
    fn not_int() {
        let ctx = Ctx::new();
        let src = "!(1 + 2)";
        let expr = parser::parse_expr(src).unwrap();

        assert!(ty_of(&ctx, &expr).is_err());
    }

    #[test]
    fn not_bool() {
        let ctx = Ctx::new();
        let src = "!(1 == 2)";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type::Const(TypeConst::Bool)));
    }

    #[test]
    fn arith_ops() {
        let ctx = Ctx::new();
        let src = "1 + (1 * -2)";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type::Const(TypeConst::Int)));
    }

    #[test]
    fn cmp_ops_eq_int() {
        let ctx = Ctx::new();
        let src = "1 + (1 * 2) == 3";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type::Const(TypeConst::Bool)));
    }

    #[test]
    fn cmp_ops_ne_int() {
        let ctx = Ctx::new();
        let src = "1 + (1 * 2) != 3";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type::Const(TypeConst::Bool)));
    }

    #[test]
    fn cmp_ops_eq_bool() {
        let ctx = Ctx::new();
        let src = "(1 == 1) == (3 == 3)";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type::Const(TypeConst::Bool)));
    }

    #[test]
    fn cmp_ops_ne_bool() {
        let ctx = Ctx::new();
        let src = "(1 == 1) != (3 == 3)";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type::Const(TypeConst::Bool)));
    }

    #[test]
    fn rel_ops() {
        let ctx = Ctx::new();
        let src = "(1 == 3) & (2 == 2) | (1 == 2)";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type::Const(TypeConst::Bool)));
    }
}
