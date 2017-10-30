use super::*;

mod ty_of {
    use super::*;
    use self::host::Type;

    #[test]
    fn var_unbound() {
        let ctx = Context::new();
        let src = "foo";
        let expr = Box::new(src.parse().unwrap());

        assert!(ty_of(&ctx, &expr).is_err());
    }

    #[test]
    fn const_int() {
        let ctx = Context::new();
        let src = "1";
        let expr = Box::new(src.parse().unwrap());

        assert_eq!(ty_of(&ctx, &expr), Ok(Box::new(Type::int())));
    }

    #[test]
    fn neg_int() {
        let ctx = Context::new();
        let src = "-(1 + 2)";
        let expr = Box::new(src.parse().unwrap());

        assert_eq!(ty_of(&ctx, &expr), Ok(Box::new(Type::int())));
    }

    #[test]
    fn neg_bool() {
        let ctx = Context::new();
        let src = "-(1 == 2)";
        let expr = Box::new(src.parse().unwrap());

        assert!(ty_of(&ctx, &expr).is_err());
    }

    #[test]
    fn not_int() {
        let ctx = Context::new();
        let src = "!(1 + 2)";
        let expr = Box::new(src.parse().unwrap());

        assert!(ty_of(&ctx, &expr).is_err());
    }

    #[test]
    fn not_bool() {
        let ctx = Context::new();
        let src = "!(1 == 2)";
        let expr = Box::new(src.parse().unwrap());

        assert_eq!(ty_of(&ctx, &expr), Ok(Box::new(Type::bool())));
    }

    #[test]
    fn arith_ops() {
        let ctx = Context::new();
        let src = "1 + (1 * -2)";
        let expr = Box::new(src.parse().unwrap());

        assert_eq!(ty_of(&ctx, &expr), Ok(Box::new(Type::int())));
    }

    #[test]
    fn cmp_ops_eq_int() {
        let ctx = Context::new();
        let src = "1 + (1 * 2) == 3";
        let expr = Box::new(src.parse().unwrap());

        assert_eq!(ty_of(&ctx, &expr), Ok(Box::new(Type::bool())));
    }

    #[test]
    fn cmp_ops_ne_int() {
        let ctx = Context::new();
        let src = "1 + (1 * 2) != 3";
        let expr = Box::new(src.parse().unwrap());

        assert_eq!(ty_of(&ctx, &expr), Ok(Box::new(Type::bool())));
    }

    #[test]
    fn cmp_ops_eq_bool() {
        let ctx = Context::new();
        let src = "(1 == 1) == (3 == 3)";
        let expr = Box::new(src.parse().unwrap());

        assert_eq!(ty_of(&ctx, &expr), Ok(Box::new(Type::bool())));
    }

    #[test]
    fn cmp_ops_ne_bool() {
        let ctx = Context::new();
        let src = "(1 == 1) != (3 == 3)";
        let expr = Box::new(src.parse().unwrap());

        assert_eq!(ty_of(&ctx, &expr), Ok(Box::new(Type::bool())));
    }

    #[test]
    fn rel_ops() {
        let ctx = Context::new();
        let src = "(1 == 3) & (2 == 2) | (1 == 2)";
        let expr = Box::new(src.parse().unwrap());

        assert_eq!(ty_of(&ctx, &expr), Ok(Box::new(Type::bool())));
    }
}

mod kind_of {
    use super::*;

    #[test]
    fn var_unbound() {
        let ctx = Context::new();
        let src = "foo";
        let ty = Box::new(src.parse().unwrap());

        assert!(kind_of(&ctx, &ty).is_err());
    }
}
