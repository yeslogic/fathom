use codespan::{CodeMap, FileName};

use syntax::parse;
use syntax::translation::ToCore;

use super::*;

mod infer_ty {
    use syntax::core::{FloatType, SignedType, Type, TypeConst, UnsignedType};

    use super::*;

    macro_rules! assert_infer_ty {
        ($given:expr, Ok($expected:expr)) => {{
            let mut codemap = CodeMap::new();
            let filemap = codemap.add_filemap(FileName::virtual_("test"), $given.into());
            let (expr, errors) = parse::expr(&filemap);
            assert!(errors.is_empty());

            let ctx = Context::new();
            let expr = expr.to_core().unwrap();
            let expected_ty = Type::Const($expected).into();

            assert_eq!(infer_ty(&ctx, &expr), Ok(expected_ty));
        }};
        ($given:expr, Err(_)) => {{
            let mut codemap = CodeMap::new();
            let filemap = codemap.add_filemap(FileName::virtual_("test"), $given.into());
            let (expr, errors) = parse::expr(&filemap);
            assert!(errors.is_empty());

            let ctx = Context::new();
            let expr = expr.to_core().unwrap();

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
    use syntax::core::{self, Kind};

    use super::*;

    macro_rules! assert_infer_kind {
        ($given:expr, Ok($expected:expr)) => {{
            let mut codemap = CodeMap::new();
            let filemap = codemap.add_filemap(FileName::virtual_("test"), $given.into());
            let (ty, errors) = parse::ty(&filemap);
            assert!(errors.is_empty());

            let ctx = Context::new();
            let mut ty = ty.to_core().unwrap();
            ty.substitute(&core::base_defs());

            assert_eq!(infer_kind(&ctx, &ty), Ok($expected));
        }};
        ($given:expr, Err(_)) => {{
            let mut codemap = CodeMap::new();
            let filemap = codemap.add_filemap(FileName::virtual_("test"), $given.into());
            let (ty, errors) = parse::ty(&filemap);
            assert!(errors.is_empty());

            let ctx = Context::new();
            let ty = ty.to_core().unwrap();

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
    use syntax::core;

    use super::*;

    #[test]
    fn type_param() {
        let src = "
            Array(T) = struct {
                len : u16le,
                data : [T; len],
            };
        ";

        let mut codemap = CodeMap::new();
        let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());
        let (module, errors) = parse::module(&filemap);
        assert!(errors.is_empty());

        let mut module = module.to_core().unwrap();
        let base_defs = core::base_defs();
        module.substitute(&base_defs);

        check_module(&module).unwrap();
    }
}
