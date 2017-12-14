use super::*;

#[test]
fn parse_expr_bool_atomic() {
    let src = "
        !((true || (false)))
    ";

    assert_debug_snapshot!(parse_expr_bool_atomic, host::Expr::from_str(src).unwrap());
}

#[test]
fn parse_expr_bool_operators() {
    let src = "
        (true && false) || (true || false)
    ";

    assert_debug_snapshot!(
        parse_expr_bool_operators,
        host::Expr::from_str(src).unwrap()
    );
}

#[test]
fn parse_add_expr() {
    let src = "x + y + z";

    assert_debug_snapshot!(parse_add_expr, host::Expr::from_str(src).unwrap());
}

#[test]
fn parse_sub_expr() {
    let src = "x - y - z";

    assert_debug_snapshot!(parse_sub_expr, host::Expr::from_str(src).unwrap());
}

#[test]
fn parse_add_expr_mixed() {
    let src = "x + y + z - z + x";

    assert_debug_snapshot!(parse_add_expr_mixed, host::Expr::from_str(src).unwrap());
}

#[test]
fn parse_mul_expr() {
    let src = "x * y * z";

    assert_debug_snapshot!(parse_mul_expr, host::Expr::from_str(src).unwrap());
}

#[test]
fn parse_div_expr() {
    let src = "x / y / z";

    assert_debug_snapshot!(parse_div_expr, host::Expr::from_str(src).unwrap());
}

#[test]
fn parse_mul_expr_mixed() {
    let src = "x * y * z / z * x";

    assert_debug_snapshot!(parse_mul_expr_mixed, host::Expr::from_str(src).unwrap());
}

#[test]
fn parse_mixed_arithmetic_expr() {
    let src = "x + y * z / z - x * a";

    assert_debug_snapshot!(
        parse_mixed_arithmetic_expr,
        host::Expr::from_str(src).unwrap()
    );
}

#[test]
fn parse_mixed_arithmetic_expr_parenthesized() {
    let src = "(x + y) * z / (z - x) * a";

    assert_debug_snapshot!(
        parse_mixed_arithmetic_expr_parenthesized,
        host::Expr::from_str(src).unwrap()
    );
}

#[test]
fn parse_proj_expr() {
    let src = "-foo.bar.x";

    assert_debug_snapshot!(parse_proj_expr, host::Expr::from_str(src).unwrap());
}

#[test]
fn parse_subscript_expr() {
    let src = "-foo[23 + (2u32 + 3u32)][index]";

    assert_debug_snapshot!(parse_subscript_expr, host::Expr::from_str(src).unwrap());
}

#[test]
fn parse_ty_var() {
    let src = "
        Point
    ";

    assert_debug_snapshot!(parse_ty_var, binary::Type::from_str(src).unwrap());
}

#[test]
fn parse_ty_empty_struct() {
    let src = "struct {}";

    assert_debug_snapshot!(parse_ty_empty_struct, binary::Type::from_str(src).unwrap());
}

#[test]
fn parse_ty_assert() {
    let src = "
        struct {
            x: u32 where x => x == 3u32,
        }
        where x => x == 2
        where x => x == 1u32
    ";

    assert_debug_snapshot!(parse_ty_assert, binary::Type::from_str(src).unwrap());
}

#[test]
fn parse_ty_array_dependent() {
    let src = "
        struct {
            len: u32,
            data1: [f32; len],
            data2: struct {
                data1: [f32; len],
                len: u32,
                /// Doc comment
                padding1: u8,
                padding2: u8,
                data2: [f32; len],
            },
        }
    ";

    assert_debug_snapshot!(
        parse_ty_array_dependent,
        binary::Type::from_str(src).unwrap()
    );
}

#[test]
fn parse_simple_definition() {
    let src = "
        Offset32 = u32;
    ";

    assert_debug_snapshot!(parse_simple_definition, Program::from_str(src).unwrap());
}

#[test]
fn parse_array_with_constant_size() {
    let src = "
        Point = [f32; 3u32];
    ";

    assert_debug_snapshot!(
        parse_array_with_constant_size,
        Program::from_str(src).unwrap()
    );
}

#[test]
fn parse_definition() {
    let src = "
        Point = struct {
            x : u32be,
            y : u32be,
        };

        /// A doc comment
        ///
        /// With some mor text
        Array = struct {
            len : u16le,
            data : [Point; len],
        };

        Formats = struct {
            format : u8,
            data : cond {
                /// A doc comment on a union variant
                u16 : format == 0x00u8 => u16,
                point : format == 0x01u8 => Point,
                /// And another doc comment
                array : format == 0x02u8 => Array,
            },
        };
    ";

    assert_debug_snapshot!(parse_definition, Program::from_str(src).unwrap());
}

#[test]
fn parse_type_params() {
    let src = "
        Pair(T, U) = struct {
            l : T,
            r : U,
        };

        Array(T) = struct {
            len : u16le,
            data : [T; len],
        };

        Data(T, U, V) = struct {
            blah : U,
            data : Array(Pair(T, V)),
        };
    ";

    assert_debug_snapshot!(parse_type_params, Program::from_str(src).unwrap());
}
