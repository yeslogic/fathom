use super::*;

mod ty_of {
    use syntax::ast::host::RcExpr;
    use syntax::ast::host::{FloatType, SignedType, Type, TypeConst, UnsignedType};
    use syntax::parser::ast::host::Expr as ParseExpr;

    use super::*;

    macro_rules! assert_ty_of {
        ($given:expr, Ok($expected:expr)) => {{

            let ctx = Context::new();
            let expr = RcExpr::from(&ParseExpr::from_str($given).unwrap());
            let expected_ty = Type::Const($expected).into();

            assert_eq!(ty_of(&ctx, &expr), Ok(expected_ty));
        }};
        ($given:expr, Err(_)) => {{

            let ctx = Context::new();
            let expr = RcExpr::from(&ParseExpr::from_str($given).unwrap());

            assert!(ty_of(&ctx, &expr).is_err());
        }};
    }

    #[test]
    fn var_unbound() {
        assert_ty_of!("foo", Err(_));
    }

    #[test]
    fn const_u8() {
        assert_ty_of!("1u8", Ok(TypeConst::Unsigned(UnsignedType::U8)));
    }

    #[test]
    fn neg_i8() {
        assert_ty_of!("-(1i8 + 2i8)", Ok(TypeConst::Signed(SignedType::I8)));
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
        assert_ty_of!("!(1u8 == 2u8)", Ok(TypeConst::Bool));
    }

    #[test]
    fn arith_ops() {
        assert_ty_of!("1i8 + (1i8 * -2i8)", Ok(TypeConst::Signed(SignedType::I8)));
    }

    #[test]
    fn cmp_ops_eq_u8() {
        assert_ty_of!("1u8 + (1u8 * 2u8) == 3u8", Ok(TypeConst::Bool));
    }

    #[test]
    fn cmp_ops_ne_u8() {
        assert_ty_of!("1u8 + (1u8 * 2u8) != 3u8", Ok(TypeConst::Bool));
    }

    #[test]
    fn cmp_ops_eq_bool() {
        assert_ty_of!("(1u8 == 1u8) == (3u8 == 3u8)", Ok(TypeConst::Bool));
    }

    #[test]
    fn cmp_ops_ne_bool() {
        assert_ty_of!("(1u8 == 1u8) != (3u8 == 3u8)", Ok(TypeConst::Bool));
    }

    #[test]
    fn rel_ops() {
        assert_ty_of!(
            "(1u8 == 3u8) && (2u8 == 2u8) || (1u8 == 2u8)",
            Ok(TypeConst::Bool)
        );
    }

    #[test]
    fn cast_bool() {
        assert_ty_of!("(1u8 == 3u8) as u8", Err(_));
    }

    #[test]
    fn cast_u8_to_u32() {
        assert_ty_of!("1u8 as u32", Ok(TypeConst::Unsigned(UnsignedType::U32)));
    }

    #[test]
    fn cast_u8_to_f32() {
        assert_ty_of!("1u8 as f32", Ok(TypeConst::Float(FloatType::F32)));
    }

    #[test]
    fn cast_u8_to_f32_to_u16() {
        assert_ty_of!(
            "1u8 as f32 as u16",
            Ok(TypeConst::Unsigned(UnsignedType::U16))
        );
    }

    #[test]
    fn cast_u8_to_bool() {
        assert_ty_of!("1u8 as bool", Err(_));
    }
}

mod kind_of {
    use syntax::ast;
    use syntax::ast::binary::RcType;
    use syntax::parser::ast::binary::Type as ParseType;

    use super::*;

    macro_rules! assert_kind_of {
        ($given:expr, Ok($expected:expr)) => {{
            let ctx = Context::new();
            let mut ty = RcType::from(&ParseType::from_str($given).unwrap());
            ty.substitute(&ast::base_defs());

            assert_eq!(kind_of(&ctx, &ty), Ok($expected));
        }};
        ($given:expr, Err(_)) => {{
            let ctx = Context::new();
            let ty = RcType::from(&ParseType::from_str($given).unwrap());

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

mod check_program {
    use syntax::ast;
    use syntax::parser::ast::Program as ParseProgram;

    use super::*;

    #[test]
    fn type_param() {
        let src = "
            Array(T) = struct {
                len : u16le,
                data : [T; len],
            };
        ";

        let mut program = Program::from(&ParseProgram::from_str(src).unwrap());
        let base_defs = ast::base_defs();
        program.substitute(&base_defs);

        check_program(&program).unwrap();
    }
}
