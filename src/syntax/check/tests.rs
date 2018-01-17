use super::*;

mod infer_ty {
    use syntax::ast::{FloatType, RcIExpr, SignedType, Type, TypeConst, UnsignedType};
    use parser::ast::Expr as ParseExpr;

    use super::*;

    macro_rules! assert_infer_ty {
        ($given:expr, Ok($expected:expr)) => {{

            let ctx = Context::new();
            let expr = RcIExpr::from_parse(&ParseExpr::from_str($given).unwrap()).unwrap();
            let expected_ty = Type::Const($expected).into();

            assert_eq!(infer_ty(&ctx, &expr), Ok(expected_ty));
        }};
        ($given:expr, Err(_)) => {{

            let ctx = Context::new();
            let expr = RcIExpr::from_parse(&ParseExpr::from_str($given).unwrap()).unwrap();

            assert!(infer_ty(&ctx, &expr).is_err());
        }};
    }

    #[test]
    fn var_unbound() {
        assert_infer_ty!("foo", Err(_));
    }

    #[test]
    fn const_u8() {
        assert_infer_ty!("1u8", Ok(TypeConst::Unsigned(UnsignedType::U8)));
    }

    #[test]
    fn neg_i8() {
        assert_infer_ty!("-(1i8 + 2i8)", Ok(TypeConst::Signed(SignedType::I8)));
    }

    #[test]
    fn neg_u8() {
        assert_infer_ty!("-1u8", Err(_));
    }

    #[test]
    fn neg_bool() {
        assert_infer_ty!("-(1u8 == 2u8)", Err(_));
    }

    #[test]
    fn not_u8() {
        assert_infer_ty!("!(1u8 + 2u8)", Err(_));
    }

    #[test]
    fn not_bool() {
        assert_infer_ty!("!(1u8 == 2u8)", Ok(TypeConst::Bool));
    }

    #[test]
    fn arith_ops() {
        assert_infer_ty!("1i8 + (1i8 * -2i8)", Ok(TypeConst::Signed(SignedType::I8)));
    }

    #[test]
    fn cmp_ops_eq_u8() {
        assert_infer_ty!("1u8 + (1u8 * 2u8) == 3u8", Ok(TypeConst::Bool));
    }

    #[test]
    fn cmp_ops_ne_u8() {
        assert_infer_ty!("1u8 + (1u8 * 2u8) != 3u8", Ok(TypeConst::Bool));
    }

    #[test]
    fn cmp_ops_eq_bool() {
        assert_infer_ty!("(1u8 == 1u8) == (3u8 == 3u8)", Ok(TypeConst::Bool));
    }

    #[test]
    fn cmp_ops_ne_bool() {
        assert_infer_ty!("(1u8 == 1u8) != (3u8 == 3u8)", Ok(TypeConst::Bool));
    }

    #[test]
    fn rel_ops() {
        assert_infer_ty!(
            "(1u8 == 3u8) && (2u8 == 2u8) || (1u8 == 2u8)",
            Ok(TypeConst::Bool)
        );
    }

    #[test]
    fn cast_bool() {
        assert_infer_ty!("(1u8 == 3u8) as u8", Err(_));
    }

    #[test]
    fn cast_u8_to_u32() {
        assert_infer_ty!("1u8 as u32", Ok(TypeConst::Unsigned(UnsignedType::U32)));
    }

    #[test]
    fn cast_u8_to_f32() {
        assert_infer_ty!("1u8 as f32", Ok(TypeConst::Float(FloatType::F32)));
    }

    #[test]
    fn cast_u8_to_f32_to_u16() {
        assert_infer_ty!(
            "1u8 as f32 as u16",
            Ok(TypeConst::Unsigned(UnsignedType::U16))
        );
    }

    #[test]
    fn cast_u8_to_bool() {
        assert_infer_ty!("1u8 as bool", Err(_));
    }
}

mod infer_kind {
    use syntax::ast::{self, Kind};
    use syntax::ast::RcType;
    use parser::ast::Type as ParseType;

    use super::*;

    macro_rules! assert_infer_kind {
        ($given:expr, Ok($expected:expr)) => {{
            let ctx = Context::new();
            let mut ty = RcType::from_parse(&ParseType::from_str($given).unwrap()).unwrap();
            ty.substitute(&ast::base_defs());

            assert_eq!(infer_kind(&ctx, &ty), Ok($expected));
        }};
        ($given:expr, Err(_)) => {{
            let ctx = Context::new();
            let ty = RcType::from_parse(&ParseType::from_str($given).unwrap()).unwrap();

            assert!(infer_kind(&ctx, &ty).is_err());
        }};
    }

    #[test]
    fn var_unbound() {
        assert_infer_kind!("foo", Err(_));
    }

    #[test]
    fn assert_magic() {
        assert_infer_kind!(
            "u64le where magic => magic == 0x00ffffffffffff00u64",
            Ok(Kind::Binary.into())
        );
    }
}

mod check_module {
    use syntax::ast;
    use parser::ast::Module as ParseModule;

    use super::*;

    #[test]
    fn type_param() {
        let src = "
            Array(T) = struct {
                len : u16le,
                data : [T; len],
            };
        ";

        let mut module = Module::from_parse(&ParseModule::from_str(src).unwrap()).unwrap();
        let base_defs = ast::base_defs();
        module.substitute(&base_defs);

        check_module(&module).unwrap();
    }
}
