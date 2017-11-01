use super::*;
use source::{BytePos as B, Span};
use structural::ast::host::Expr;

#[test]
fn compiles_array() {
    let src = "
        A = [struct {}; 256];
        B = A;
    ";

    let program = src.parse().unwrap();
    let found = compile_program(&program);

    let mut expected = Program::new();
    expected.define_alias(
        "A",
        Type::array(
            Type::path("A::Elem"),
            Expr::int(Span::new(B(25), B(28)), 256),
        ),
    );
    expected.define_alias("B", Type::path("A"));
    expected.define_struct("A::Elem", vec![]);

    assert_eq!(found, expected);
}

#[test]
fn compiles_nested_array() {
    let src = "
        A = [[[struct {}; 256]; 256]; 256];
    ";

    let program = src.parse().unwrap();
    let found = compile_program(&program);

    let mut expected = Program::new();
    expected.define_alias(
        "A",
        Type::array(
            Type::array(
                Type::array(
                    Type::path("A::Elem::Elem::Elem"),
                    Expr::int(Span::new(B(27), B(30)), 256),
                ),
                Expr::int(Span::new(B(33), B(36)), 256),
            ),
            Expr::int(Span::new(B(39), B(42)), 256),
        ),
    );
    expected.define_struct("A::Elem::Elem::Elem", vec![]);

    assert_eq!(found, expected);
}

#[test]
fn compiles_nested_struct() {
    let src = "
        A = struct {};
        B = struct {
            x : struct {},
            y : [struct { data : A }; 256],
        };
    ";

    let program = src.parse().unwrap();
    let found = compile_program(&program);

    let mut expected = Program::new();
    expected.define_struct("A", vec![]);
    expected.define_struct(
        "B",
        vec![
            Field::new("x", Type::path("B::x")),
            Field::new(
                "y",
                Type::array(
                    Type::path("B::y::Elem"),
                    Expr::int(Span::new(B(110), B(113)), 256),
                ),
            ),
        ],
    );
    expected.define_struct("B::x", vec![]);
    expected.define_struct("B::y::Elem", vec![Field::new("data", Type::path("A"))]);

    assert_eq!(found, expected);
}

#[test]
fn compiles_union() {
    let src = "
        A = struct {};
        B = struct {};
        C = union { struct {}, A, B, [A; 256] };
    ";

    let program = src.parse().unwrap();
    let found = compile_program(&program);

    let mut expected = Program::new();
    expected.define_struct("A", vec![]);
    expected.define_struct("B", vec![]);
    expected.define_union(
        "C",
        vec![
            Type::path("C::Variant0"),
            Type::path("A"),
            Type::path("B"),
            Type::array(Type::path("A"), Expr::int(Span::new(B(88), B(91)), 256)),
        ],
    );
    expected.define_struct("C::Variant0", vec![]);

    assert_eq!(found, expected);
}
