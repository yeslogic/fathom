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

        for definition in &src.definitions {
            // Begin tracking the path of this definition from the root name of the
            // source definition. This will be appended to in order to provide a
            // fully qualified path through the type definitions, eg:
            // `Foo::field::Entry::Variant2::...`
            let path = Path::new(definition.name.clone());

            match *definition.ty {
                // Structs and unions that are defined at the top level should
                // get the best names, closest to what the author of the data
                // definition intended!
                binary::Type::Struct(_, ref fields) => {
                    let lowered_fields = lower_row(&path, fields, |field_path, ty| {
                        lower_ty(&mut program, &field_path, ty)
                    });
                    let parse_expr = struct_parser(&path, fields);
                    program.define_struct(
                        path,
                        definition.doc.clone(),
                        lowered_fields,
                        Some(parse_expr),
                    );
                }
                binary::Type::Union(_, ref variants) => {
                    let lowered_variants = lower_row(&path, variants, |variant_path, ty| {
                        lower_ty(&mut program, &variant_path, ty)
                    });
                    let parse_expr = union_parser(&path, variants);
                    program.define_union(
                        path,
                        definition.doc.clone(),
                        lowered_variants,
                        Some(parse_expr),
                    );
                }
                // Everything else should be an alias
                _ => {
                    let ty = lower_ty(&mut program, &path, &definition.ty);
                    program.define_alias(path, definition.doc.clone(), ty);
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

            Field {
                doc: item.doc.clone(),
                name: item.name.clone(),
                value: ty,
            }
        })
        .collect()
}

/// Lower a type variable to an IR type
fn lower_ty_var(var: &Var<String>) -> RcType<String> {
    Rc::new(match *var {
        Var::Bound(Named(ref name, _)) => Type::Path(Path::new(name.to_string())),
        Var::Free(_) => unimplemented!(),
    })
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
    // Mirroring `binary::Type::repr`
    Rc::new(match **ty {
        binary::Type::Var(_, ref var) => return lower_ty_var(var),
        binary::Type::Const(ty_const) => Type::Const(ty_const.repr()),
        binary::Type::Array(_, ref elem_ty, _) => {
            let elem_path = path.append_child("Elem");
            let elem_ty = lower_ty(program, &elem_path, elem_ty);

            Type::Array(elem_ty)
        }
        binary::Type::Assert(_, ref ty, _) => return lower_ty(program, path, ty),
        binary::Type::Interp(_, _, _, ref repr_ty) => return lower_repr_ty(path, repr_ty),
        binary::Type::Union(_, ref variants) => {
            let lowered_variants = lower_row(path, variants, |variant_path, ty| {
                lower_ty(program, &variant_path, ty)
            });
            program.define_union(path.clone(), String::new(), lowered_variants, None);

            Type::Path(path.clone())
        }
        binary::Type::Struct(_, ref fields) => {
            let lowered_fields = lower_row(path, fields, |field_path, ty| {
                lower_ty(program, &field_path, ty)
            });
            program.define_struct(path.clone(), String::new(), lowered_fields, None);

            Type::Path(path.clone())
        }
        binary::Type::Abs(_, _, _) => unimplemented!(),
        binary::Type::App(_, _, _) => unimplemented!(),
    })
}

/// Lower host types to the nominal format
///
/// # Arguments
///
/// * `path` - path to the parent struct or union
/// * `ty` - the type to be lowered
fn lower_repr_ty(path: &Path<String>, ty: &host::RcType<String>) -> RcType<String> {
    Rc::new(match **ty {
        host::Type::Var(ref var) => return lower_ty_var(var),
        host::Type::Const(ty_const) => Type::Const(ty_const),
        host::Type::Arrow(ref arg_tys, ref ret_ty) => {
            let arg_repr_tys = arg_tys
                .iter()
                .map(|arg_ty| lower_repr_ty(&path, arg_ty))
                .collect();
            let ret_repr_ty = lower_repr_ty(&path, ret_ty);

            Type::Arrow(arg_repr_tys, ret_repr_ty)
        }
        host::Type::Array(ref elem_ty) => {
            let elem_path = path.append_child("Elem");
            let elem_ty = lower_repr_ty(&elem_path, elem_ty);

            Type::Array(elem_ty)
        }
        host::Type::Union(_) | host::Type::Struct(_) => {
            // We expect that the repr type has already had a corresponding type
            // generated for it, so instead we just return the current path.
            Type::Path(path.clone())
        }
        host::Type::Abs(_, _) => unimplemented!(),
        host::Type::App(_, _) => unimplemented!(),
    })
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
        host::Expr::Cast(_, ref src_expr, ref dst_ty) => {
            Expr::Cast(lower_expr(path, src_expr), lower_repr_ty(path, dst_ty))
        }
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
    use var::ScopeIndex;

    let lower_to_field_parser = |field: &Field<String, binary::RcType<String>>| {
        (
            field.name.clone(),
            ty_parser(&path.append_child(field.name.clone()), &field.value),
        )
    };
    let lower_to_expr_field = |field: &Field<String, binary::RcType<String>>| {
        Field {
            doc: field.doc.clone(),
            name: field.name.clone(),
            value: Rc::new(Expr::Var(Var::free(field.name.clone()))),
        }
    };

    let parse_exprs = fields.iter().map(lower_to_field_parser);
    let expr_fields = fields.iter().map(lower_to_expr_field);

    let mut named_exprs = Vec::with_capacity(fields.len());
    let mut seen_names = Vec::<String>::with_capacity(fields.len());

    for (name, mut parse_expr) in parse_exprs {
        for (scope, name) in seen_names.iter().rev().enumerate() {
            Rc::make_mut(&mut parse_expr)
                .abstract_names_at(&[name.clone()], ScopeIndex(scope as u32));
        }

        seen_names.push(name.clone());
        named_exprs.push(Named(name, parse_expr));
    }

    let mut expr = Expr::Struct(path.clone(), expr_fields.collect());
    for (scope, name) in seen_names.iter().rev().enumerate() {
        expr.abstract_names_at(&[name.clone()], ScopeIndex(scope as u32));
    }

    Rc::new(ParseExpr::Sequence(named_exprs, Rc::new(expr)))
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
        let variant_expr = Rc::new(Expr::Intro(
            path.clone(),
            variant.name.clone(),
            // FIXME: generate fresh name?
            Rc::new(Expr::Var(Var::bound("x", BoundVar::new(Si(0), Bi(0))))),
        ));

        Rc::new(ParseExpr::Sequence(
            vec![Named("x".to_owned(), variant_parser)],
            variant_expr,
        ))
    };

    Rc::new(ParseExpr::Choice(
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
        binary::Type::Const(ty_const) => ParseExpr::Const(ty_const),
        binary::Type::Array(_, ref elem_ty, ref size_expr) => {
            let elem_path = path.append_child("Elem");
            let elem_parser = ty_parser(&elem_path, elem_ty);
            let size_expr = lower_expr(path, size_expr);

            ParseExpr::Repeat(elem_parser, RepeatBound::Exact(size_expr))
        }
        binary::Type::Union(_, ref variants) => return union_parser(path, variants),
        binary::Type::Struct(_, ref fields) => return struct_parser(path, fields),
        binary::Type::Assert(_, ref ty, ref pred_expr) => {
            let ty_parser = ty_parser(path, ty);
            let pred_expr = lower_expr(path, pred_expr);

            ParseExpr::Assert(ty_parser, pred_expr)
        }
        binary::Type::Interp(_, ref ty, ref conv_expr, _) => {
            let fn_expr = lower_expr(path, conv_expr);
            let parser_expr = ty_parser(path, ty);

            ParseExpr::Apply(fn_expr, parser_expr)
        }
        binary::Type::Abs(_, _, _) => unimplemented!("Abs: {:?}", ty),
        binary::Type::App(_, _, _) => unimplemented!("App: {:?}", ty),
    })
}
