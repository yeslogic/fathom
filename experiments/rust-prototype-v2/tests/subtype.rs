use codespan::CodeMap;

use ddl::semantics::{self, Context};

mod support;

macro_rules! assert_subtype {
    ($sub_ty_src:expr, $super_ty_src:expr) => {{
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let super_ty = $super_ty_src;
        let sub_ty = $sub_ty_src;

        let super_ty = support::parse_nf_term(&mut codemap, &context, &super_ty);
        let sub_ty = support::parse_nf_term(&mut codemap, &context, &sub_ty);

        assert!(
            semantics::is_subtype(&context, &sub_ty, &super_ty),
            "{} <: {}",
            $sub_ty_src,
            $super_ty_src
        );
    }};
}

macro_rules! assert_not_subtype {
    ($sub_ty_src:expr, $super_ty_src:expr) => {{
        let mut codemap = CodeMap::new();
        let context = Context::default();

        let super_ty = $super_ty_src;
        let sub_ty = $sub_ty_src;

        let super_ty = support::parse_nf_term(&mut codemap, &context, &super_ty);
        let sub_ty = support::parse_nf_term(&mut codemap, &context, &sub_ty);

        assert!(
            !semantics::is_subtype(&context, &sub_ty, &super_ty),
            "{} </: {}",
            $sub_ty_src,
            $super_ty_src
        );
    }};
}

mod range_full {
    use super::*;

    #[test]
    fn range_full() {
        assert_subtype!(r"int {..}", r"int {..}");
    }

    #[test]
    fn range_from() {
        assert_not_subtype!(r"int {..}", r"int {1 ..}");
    }

    #[test]
    fn range_to() {
        assert_not_subtype!(r"int {..}", r"int {.. 1}");
    }

    #[test]
    fn range_from_to() {
        assert_not_subtype!(r"int {..}", r"int {1 .. 100}");
    }

    #[test]
    fn range_from_to_flip() {
        assert_not_subtype!(r"int {..}", r"int {100 .. 1}");
    }
}

mod range_from {
    use super::*;

    #[test]
    fn range_full() {
        assert_subtype!(r"int {1 ..}", r"int {..}");
    }

    #[test]
    fn range_from() {
        assert_subtype!(r"int {1 ..}", r"int {0 ..}");
    }

    #[test]
    fn range_from_exact() {
        assert_subtype!(r"int {1 ..}", r"int {1 ..}");
    }

    #[test]
    fn range_from_disjoint() {
        assert_not_subtype!(r"int {0 ..}", r"int {1 ..}");
    }

    #[test]
    fn range_to() {
        assert_not_subtype!(r"int {1 ..}", r"int {.. 3}");
    }

    #[test]
    fn range_to_disjoint() {
        assert_not_subtype!(r"int {3 ..}", r"int {.. 1}");
    }

    // TODO: range_from_to
}

mod range_to {
    use super::*;

    #[test]
    fn range_full() {
        assert_subtype!(r"int {.. 1}", r"int {..}");
    }

    #[test]
    fn range_from() {
        assert_not_subtype!(r"int {.. 1}", r"int {0 ..}");
    }

    #[test]
    fn range_from_disjoint() {
        assert_not_subtype!(r"int {.. 1}", r"int {2 ..}");
    }

    #[test]
    fn range_to() {
        assert_subtype!(r"int {.. 99}", r"int {.. 100}");
    }

    #[test]
    fn range_to_exact() {
        assert_subtype!(r"int {.. 100}", r"int {.. 100}");
    }

    #[test]
    fn range_to_disjoint() {
        assert_not_subtype!(r"int {.. 100}", r"int {.. 99}");
    }

    // TODO: range_from_to
}

mod range_from_to {
    use super::*;

    #[test]
    fn range_full() {
        assert_subtype!(r"int {1 .. 100}", r"int {..}");
    }

    // TODO: range_from
    // TODO: range_to
    // TODO: range_from_to
}

#[test]
fn range_flipped() {
    assert_not_subtype!(r"int {1 .. 100}", r"int {100 .. 1}");
}

#[test]
fn range_flipped2() {
    assert_subtype!(r"int {100 .. 1}", r"int {1 .. 100}");
}

#[test]
fn test_binary() {
    assert_subtype!(r"U8", r"U8");
    assert_subtype!(r"U16Le", r"U16");
    assert_subtype!(r"U32Le", r"U32");
    assert_subtype!(r"U64Le", r"U64");
    assert_subtype!(r"U16Be", r"U16");
    assert_subtype!(r"U32Be", r"U32");
    assert_subtype!(r"U64Be", r"U64");
    assert_subtype!(r"S8", r"S8");
    assert_subtype!(r"S16Le", r"S16");
    assert_subtype!(r"S32Le", r"S32");
    assert_subtype!(r"S64Le", r"S64");
    assert_subtype!(r"S16Be", r"S16");
    assert_subtype!(r"S32Be", r"S32");
    assert_subtype!(r"S64Be", r"S64");
    assert_subtype!(r"F32Le", r"F32");
    assert_subtype!(r"F64Le", r"F64");
    assert_subtype!(r"F32Be", r"F32");
    assert_subtype!(r"F64Be", r"F64");
}

// TODO: integer types

mod f32le {
    use super::*;

    #[test]
    fn f32() {
        assert_subtype!(r"F32Le", r"F32");
    }

    #[test]
    fn f32le() {
        assert_subtype!(r"F32Le", r"F32Le");
    }
}

mod f32be {
    use super::*;

    #[test]
    fn f32() {
        assert_subtype!(r"F32Be", r"F32");
    }

    #[test]
    fn f32be() {
        assert_subtype!(r"F32Be", r"F32Be");
    }
}

mod f64le {
    use super::*;

    #[test]
    fn f64() {
        assert_subtype!(r"F64Le", r"F64");
    }

    #[test]
    fn f64le() {
        assert_subtype!(r"F64Le", r"F64Le");
    }
}

mod f64be {
    use super::*;

    #[test]
    fn f64() {
        assert_subtype!(r"F64Be", r"F64");
    }

    #[test]
    fn f64be() {
        assert_subtype!(r"F64Be", r"F64Be");
    }
}

mod refinement {
    use super::*;

    #[test]
    fn f64() {
        assert_subtype!(r"{ x : F64Be | false }", r"F64");
    }

    #[test]
    fn f64be() {
        assert_subtype!(r"{ x : F64Be | false }", r"F64Be");
    }
}
