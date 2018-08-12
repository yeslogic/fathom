use super::*;

macro_rules! assert_subtype {
    ($sub_ty_src:expr, $super_ty_src:expr) => {{
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let super_ty = $super_ty_src;
        let sub_ty = $sub_ty_src;

        let super_ty = parse_nf_term(&mut codemap, &tc_env, &super_ty);
        let sub_ty = parse_nf_term(&mut codemap, &tc_env, &sub_ty);

        assert!(
            is_subtype(&sub_ty, &super_ty),
            "{} <: {}",
            $sub_ty_src,
            $super_ty_src
        );
    }};
}

macro_rules! assert_not_subtype {
    ($sub_ty_src:expr, $super_ty_src:expr) => {{
        let mut codemap = CodeMap::new();
        let tc_env = TcEnv::default();

        let super_ty = $super_ty_src;
        let sub_ty = $sub_ty_src;

        let super_ty = parse_nf_term(&mut codemap, &tc_env, &super_ty);
        let sub_ty = parse_nf_term(&mut codemap, &tc_env, &sub_ty);

        assert!(
            !is_subtype(&sub_ty, &super_ty),
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
        assert_subtype!(r"{..}", r"{..}");
    }

    #[test]
    fn range_from() {
        assert_not_subtype!(r"{..}", r"{1 ..}");
    }

    #[test]
    fn range_to() {
        assert_not_subtype!(r"{..}", r"{.. 1}");
    }

    #[test]
    fn range_from_to() {
        assert_not_subtype!(r"{..}", r"{1 .. 100}");
    }

    #[test]
    fn range_from_to_flip() {
        assert_not_subtype!(r"{..}", r"{100 .. 1}");
    }
}

mod range_from {
    use super::*;

    #[test]
    fn range_full() {
        assert_subtype!(r"{1 ..}", r"{..}");
    }

    #[test]
    fn range_from() {
        assert_subtype!(r"{1 ..}", r"{0 ..}");
    }

    #[test]
    fn range_from_exact() {
        assert_subtype!(r"{1 ..}", r"{1 ..}");
    }

    #[test]
    fn range_from_disjoint() {
        assert_not_subtype!(r"{0 ..}", r"{1 ..}");
    }

    #[test]
    fn range_to() {
        assert_not_subtype!(r"{1 ..}", r"{.. 3}");
    }

    #[test]
    fn range_to_disjoint() {
        assert_not_subtype!(r"{3 ..}", r"{.. 1}");
    }

    // TODO: range_from_to
}

mod range_to {
    use super::*;

    #[test]
    fn range_full() {
        assert_subtype!(r"{.. 1}", r"{..}");
    }

    #[test]
    fn range_from() {
        assert_not_subtype!(r"{.. 1}", r"{0 ..}");
    }

    #[test]
    fn range_from_disjoint() {
        assert_not_subtype!(r"{.. 1}", r"{2 ..}");
    }

    #[test]
    fn range_to() {
        assert_subtype!(r"{.. 99}", r"{.. 100}");
    }

    #[test]
    fn range_to_exact() {
        assert_subtype!(r"{.. 100}", r"{.. 100}");
    }

    #[test]
    fn range_to_disjoint() {
        assert_not_subtype!(r"{.. 100}", r"{.. 99}");
    }

    // TODO: range_from_to
}

mod range_from_to {
    use super::*;

    #[test]
    fn range_full() {
        assert_subtype!(r"{1 .. 100}", r"{..}");
    }

    // TODO: range_from
    // TODO: range_to
    // TODO: range_from_to
}

#[test]
fn range_flipped() {
    assert_not_subtype!(r"{1 .. 100}", r"{100 .. 1}");
}

#[test]
fn range_flipped2() {
    assert_subtype!(r"{100 .. 1}", r"{1 .. 100}");
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
