use parser;
use super::*;

type SpannedCtx = Ctx<
    String,
    binary::SpannedType<String, host::SpannedExpr<String>>,
    host::SpannedExpr<String>,
>;

mod ty_of {
    use super::*;
    use self::host::{Type, TypeConst, TypeF};

    #[test]
    fn const_int() {
        let ctx = SpannedCtx::new();
        let src = "1";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type(TypeF::Const(TypeConst::Int))));
    }

    #[test]
    fn neg_int() {
        let ctx = SpannedCtx::new();
        let src = "-(1 + 2)";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type(TypeF::Const(TypeConst::Int))));
    }

    #[test]
    fn neg_bool() {
        let ctx = SpannedCtx::new();
        let src = "-(1 == 2)";
        let expr = parser::parse_expr(src).unwrap();

        assert!(ty_of(&ctx, &expr).is_err());
    }

    #[test]
    fn not_int() {
        let ctx = SpannedCtx::new();
        let src = "!(1 + 2)";
        let expr = parser::parse_expr(src).unwrap();

        assert!(ty_of(&ctx, &expr).is_err());
    }

    #[test]
    fn not_bool() {
        let ctx = SpannedCtx::new();
        let src = "!(1 == 2)";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type(TypeF::Const(TypeConst::Bool))));
    }

    #[test]
    fn arith_ops() {
        let ctx = SpannedCtx::new();
        let src = "1 + (1 * -2)";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type(TypeF::Const(TypeConst::Int))));
    }

    #[test]
    fn cmp_ops_eq_int() {
        let ctx = SpannedCtx::new();
        let src = "1 + (1 * 2) == 3";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type(TypeF::Const(TypeConst::Bool))));
    }

    #[test]
    fn cmp_ops_ne_int() {
        let ctx = SpannedCtx::new();
        let src = "1 + (1 * 2) != 3";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type(TypeF::Const(TypeConst::Bool))));
    }

    #[test]
    fn cmp_ops_eq_bool() {
        let ctx = SpannedCtx::new();
        let src = "(1 == 1) == (3 == 3)";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type(TypeF::Const(TypeConst::Bool))));
    }

    #[test]
    fn cmp_ops_ne_bool() {
        let ctx = SpannedCtx::new();
        let src = "(1 == 1) != (3 == 3)";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type(TypeF::Const(TypeConst::Bool))));
    }

    #[test]
    fn rel_ops() {
        let ctx = SpannedCtx::new();
        let src = "(1 == 3) & (2 == 2) | (1 == 2)";
        let expr = parser::parse_expr(src).unwrap();

        assert_eq!(ty_of(&ctx, &expr), Ok(Type(TypeF::Const(TypeConst::Bool))));
    }
}
