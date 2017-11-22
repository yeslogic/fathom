//! Lowering from the surface syntax to the intermediate representation

use std::rc::Rc;

use name::Named;
use syntax;
use syntax::ast::{binary, host, Field};
use ir::ast::{Expr, ParseExpr, Path, Program, RepeatBound, Type};
use ir::ast::{RcExpr, RcParseExpr, RcType};
use var::{BindingIndex as Bi, BoundVar, ScopeIndex as Si, Var};

impl<'a> From<&'a syntax::ast::Program<String>> for Program<String> {
    fn from(src: &'a syntax::ast::Program<String>) -> Program<String> {
        let mut program = Program::new();

        for def in &src.defs {
            // Begin tracking the path of this definition from the root name of the
            // source definition. This will be appended to in order to provide a
            // fully qualified path through the type definitions, eg:
            // `Foo::field::Entry::Variant2::...`
            let path = Path::new(def.name.clone());

            match *def.ty {
                // Structs and unions that are defined at the top level should
                // get the best names, closest to what the author of the data
                // definition intended!
                binary::Type::Struct(_, ref fields) => {
                    let lowered_fields = lower_row(&path, fields, |field_path, ty| {
                        lower_ty(&mut program, &field_path, ty)
                    });
                    let parse_expr = struct_parser(&path, fields);
                    program.define_struct(path, lowered_fields, Some(parse_expr));
                }
                binary::Type::Union(_, ref variants) => {
                    let lowered_variants = lower_row(&path, variants, |variant_path, ty| {
                        lower_ty(&mut program, &variant_path, ty)
                    });
                    let parse_expr = union_parser(&path, variants);
                    program.define_union(path, lowered_variants, Some(parse_expr));
                }
                // Everything else should be an alias
                _ => {
                    let ty = lower_ty(&mut program, &path, &def.ty);
                    program.define_alias(path, ty);
                }
            }
        }

        program
    }
}

/// Lower a row to the nominal IR, performing a lowering function for each value
///
/// # Arguments
///
/// * `path` - path to the parent struct or union
/// * `row` - the row of entries to be lowered
/// * `lower_value` - a function that will be called for each entry's
///    corresponding value, appending the name of the entry to `path`
fn lower_row<T, U, F>(
    path: &Path<String>,
    row: &[Field<String, T>],
    mut lower_value: F,
) -> Vec<Field<String, U>>
where
    F: FnMut(Path<String>, &T) -> U,
{
    row.iter()
        .map(|item| {
            let item_path = path.append_child(item.name.clone());
            let ty = lower_value(item_path, &item.value);

            Field::new(item.name.clone(), ty)
        })
        .collect()
}

/// Lower binary types to the nominal format
///
/// # Arguments
///
/// * `program` - the current program. Sub-structs and unions will mutate the
///   program, creating corresponding top-level definitions
/// * `path` - path to the parent struct or union
/// * `ty` - the type to be lowered
fn lower_ty(
    program: &mut Program<String>,
    path: &Path<String>,
    ty: &binary::RcType<String>,
) -> RcType<String> {
    use name::Named;

    // Mirroring `binary::Type::repr`
    match **ty {
        binary::Type::Var(_, Var::Bound(Named(ref name, _))) => Rc::new(Type::path(name.as_str())),
        binary::Type::Var(_, Var::Free(_)) => unimplemented!(),
        binary::Type::Const(binary::TypeConst::U8) => Rc::new(Type::U8),
        binary::Type::Array(_, ref elem_ty, _) => {
            let elem_path = path.append_child("Elem");
            let elem_ty = lower_ty(program, &elem_path, elem_ty);

            Rc::new(Type::Array(elem_ty))
        }
        binary::Type::Assert(_, ref ty, _) => lower_ty(program, path, ty),
        binary::Type::Interp(_, _, _, ref repr_ty) => lower_repr_ty(path, repr_ty),
        binary::Type::Union(_, ref variants) => {
            let lowered_variants = lower_row(path, variants, |variant_path, ty| {
                lower_ty(program, &variant_path, ty)
            });
            program.define_union(path.clone(), lowered_variants, None);

            Rc::new(Type::path(path.clone()))
        }
        binary::Type::Struct(_, ref fields) => {
            let lowered_fields = lower_row(path, fields, |field_path, ty| {
                lower_ty(program, &field_path, ty)
            });
            program.define_struct(path.clone(), lowered_fields, None);

            Rc::new(Type::path(path.clone()))
        }
        binary::Type::Abs(_, _, _) => unimplemented!(),
        binary::Type::App(_, _, _) => unimplemented!(),
    }
}

/// Lower host types to the nominal format
///
/// # Arguments
///
/// * `program` - the current program. Sub-structs and unions will mutate the
///   program, creating corresponding top-level definitions
/// * `path` - path to the parent struct or union
/// * `ty` - the type to be lowered
fn lower_repr_ty(path: &Path<String>, ty: &host::RcType<String>) -> RcType<String> {
    match **ty {
        host::Type::Var(Var::Bound(Named(ref name, _))) => Rc::new(Type::path(name.as_str())),
        host::Type::Var(Var::Free(_)) => unimplemented!(),
        host::Type::Const(host::TypeConst::U8) => Rc::new(Type::U8),
        host::Type::Const(host::TypeConst::Int) => Rc::new(Type::Int),
        host::Type::Const(host::TypeConst::Bool) => Rc::new(Type::Bool),
        host::Type::Arrow(ref arg_tys, ref ret_ty) => {
            let arg_repr_tys = arg_tys
                .iter()
                .map(|arg_ty| lower_repr_ty(&path, arg_ty))
                .collect();
            let ret_repr_ty = lower_repr_ty(&path, ret_ty);

            Rc::new(Type::Arrow(arg_repr_tys, ret_repr_ty))
        }
        host::Type::Array(ref elem_ty) => {
            let elem_path = path.append_child("Elem");
            let elem_ty = lower_repr_ty(&elem_path, elem_ty);

            Rc::new(Type::Array(elem_ty))
        }
        host::Type::Union(_) | host::Type::Struct(_) => {
            // We expect that the repr type has already had a corresponding type
            // generated for it, so instead we just return the current path.
            Rc::new(Type::path(path.clone()))
        }
        host::Type::Abs(_, _) => unimplemented!(),
        host::Type::App(_, _) => unimplemented!(),
    }
}

/// Lower host expressions to the nominal format
///
/// # Arguments
///
/// * `path` - path to the parent struct or union
/// * `expr` - the expression to be lowered
fn lower_expr(path: &Path<String>, expr: &host::RcExpr<String>) -> RcExpr<String> {
    Rc::new(match **expr {
        host::Expr::Const(_, c) => Expr::Const(c),
        host::Expr::Prim(name, ref ty) => Expr::Prim(name, lower_repr_ty(path, ty)),
        host::Expr::Var(_, ref var) => Expr::Var(var.clone()),
        host::Expr::Unop(_, op, ref expr) => Expr::Unop(op, lower_expr(path, expr)),
        host::Expr::Binop(_, op, ref lhs, ref rhs) => {
            Expr::Binop(op, lower_expr(path, lhs), lower_expr(path, rhs))
        }
        host::Expr::Struct(ref fields) => {
            let lowered_fields = lower_row(
                path,
                fields,
                |field_path, expr| lower_expr(&field_path, &expr),
            );

            Expr::Struct(path.clone(), lowered_fields)
        }
        host::Expr::Proj(_, ref expr, ref field_name) => {
            Expr::Proj(lower_expr(path, expr), field_name.clone())
        }
        host::Expr::Intro(_, _, _, _) => unimplemented!(),
        host::Expr::Subscript(_, _, _) => unimplemented!(),
        host::Expr::Abs(_, _, _) => unimplemented!(),
        host::Expr::App(_, _, _) => unimplemented!(),
    })
}

/// Create a struct parser for the given fields
///
/// # Arguments
///
/// * `path` - path to the parent struct or union
/// * `fields` - the fields to be used in the parser
fn struct_parser(
    path: &Path<String>,
    fields: &[Field<String, binary::RcType<String>>],
) -> RcParseExpr<String> {
    let lower_to_field_parser = |field: &Field<String, binary::RcType<String>>| {
        (field.name.clone(), ty_parser(path, &field.value))
    };
    let lower_to_expr_field = |field: &Field<String, binary::RcType<String>>| {
        Field::new(field.name.clone(), Expr::fvar(field.name.clone()))
    };

    Rc::new(ParseExpr::sequence(
        fields.iter().map(lower_to_field_parser).collect(),
        Expr::struct_(
            path.clone(),
            fields.iter().map(lower_to_expr_field).collect(),
        ),
    ))
}

/// Create a union parser for the given fields
///
/// # Arguments
///
/// * `path` - path to the parent struct or union
/// * `fields` - the fields to be used in the parser
fn union_parser(
    path: &Path<String>,
    variants: &[Field<String, binary::RcType<String>>],
) -> RcParseExpr<String> {
    let lower_variant = |variant: &Field<String, binary::RcType<String>>| {
        let variant_parser = ty_parser(path, &variant.value);
        let variant_expr = Rc::new(Expr::intro(
            path.clone(),
            variant.name.clone(),
            // FIXME: generate fresh name?
            Expr::bvar("x", BoundVar::new(Si(0), Bi(0))),
        ));

        Rc::new(ParseExpr::Sequence(
            vec![Named("x".to_owned(), variant_parser)],
            variant_expr,
        ))
    };

    Rc::new(ParseExpr::choice(
        variants.iter().map(lower_variant).collect(),
    ))
}

/// Create a parser for the given type
///
/// # Arguments
///
/// * `path` - path to the parent struct or union
/// * `ty` - the binary type to use as a basis for the parser
fn ty_parser(path: &Path<String>, ty: &binary::RcType<String>) -> RcParseExpr<String> {
    Rc::new(match **ty {
        binary::Type::Var(_, ref var) => ParseExpr::Var(var.clone()),
        binary::Type::Const(binary::TypeConst::U8) => ParseExpr::U8,
        binary::Type::Array(_, ref elem_ty, ref size_expr) => {
            let elem_parser = ty_parser(&path.append_child("Elem"), elem_ty);
            let size_expr = lower_expr(path, size_expr);
            ParseExpr::repeat(elem_parser, RepeatBound::Exact(size_expr))
        }
        binary::Type::Union(_, ref variants) => return union_parser(path, variants),
        binary::Type::Struct(_, ref fields) => return struct_parser(path, fields),
        binary::Type::Assert(_, ref ty, ref pred_expr) => {
            let ty_parser = ty_parser(path, ty);
            let pred_expr = lower_expr(path, pred_expr);
            ParseExpr::assert(ty_parser, pred_expr)
        }
        binary::Type::Interp(_, ref ty, ref conv_expr, _) => {
            ParseExpr::Apply(lower_expr(path, conv_expr), ty_parser(path, ty))
        }
        binary::Type::Abs(_, _, _) => unimplemented!("Abs: {:?}", ty),
        binary::Type::App(_, _, _) => unimplemented!("App: {:?}", ty),
    })
}
