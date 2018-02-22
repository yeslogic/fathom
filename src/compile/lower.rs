//! Lowering from the surface syntax to the intermediate representation

use std::rc::Rc;

use compile::ir::{Definition, Expr, Item, Module, ParseExpr, Path, RepeatBound, Type};
use compile::ir::{RcExpr, RcParseExpr, RcType};
use name::{Ident, Name};
use syntax::core::{self, Field};
use var::{BindingIndex as Bi, BoundVar, Named, ScopeIndex as Si, Var};

impl<'a> From<&'a core::Module> for Module {
    fn from(src: &'a core::Module) -> Module {
        let mut module = Module::new();

        for definition in &src.definitions {
            // Begin tracking the path of this definition from the root name of the
            // source definition. This will be appended to in order to provide a
            // fully qualified path through the type definitions, eg:
            // `Foo::field::Entry::Variant2::...`
            let path = Path::new(definition.name.0.clone());

            let definition = match *definition.body_ty.inner {
                core::Type::Lam(_, ref params, ref ty) => Definition {
                    doc: Rc::clone(&definition.doc),
                    params: params.iter().map(|p| p.name.clone()).collect(),
                    item: lower_item(&mut module, &path, ty),
                    path,
                },
                _ => Definition {
                    doc: Rc::clone(&definition.doc),
                    params: vec![],
                    item: lower_item(&mut module, &path, &definition.body_ty),
                    path,
                },
            };

            module.define(definition);
        }

        module
    }
}

fn lower_item(module: &mut Module, path: &Path, ty: &core::RcType) -> Item {
    match *ty.inner {
        // Structs and unions that are defined at the top level should
        // get the best names, closest to what the author of the data
        // definition intended!
        core::Type::Struct(_, ref fields) => Item::Struct(
            lower_row(path, fields, |field_path, ty| {
                lower_ty(module, &field_path, ty)
            }),
            Some(struct_parser(path, fields)),
        ),
        core::Type::Cond(_, ref options) => Item::Union(
            lower_row(path, options, |option_path, &(_, ref ty)| {
                lower_ty(module, &option_path, ty)
            }),
            Some(cond_parser(path, options)),
        ),
        // Everything else should be an alias
        _ => Item::Alias(lower_ty(module, path, ty)),
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
fn lower_row<T, U, F>(path: &Path, row: &[Field<T>], mut lower_value: F) -> Vec<Field<U>>
where
    F: FnMut(Path, &T) -> U,
{
    row.iter()
        .map(|item| {
            let item_path = path.append_child(item.name.0.clone());
            let ty = lower_value(item_path, &item.value);

            Field {
                doc: Rc::clone(&item.doc),
                name: item.name.clone(),
                value: ty,
            }
        })
        .collect()
}

/// Lower a type variable to an IR type
fn lower_ty_var(var: &Var) -> RcType {
    match *var {
        Var::Bound(Named { ref name, .. }) => {
            Type::Path(Path::new(name.to_string()), vec![]).into()
        }
        Var::Free(_) => unimplemented!(),
    }
}

/// Lower binary types to the nominal format
///
/// # Arguments
///
/// * `module` - the current module. Sub-structs and unions will mutate the
///   module, creating corresponding top-level definitions
/// * `path` - path to the parent struct or union
/// * `ty` - the type to be lowered
fn lower_ty(module: &mut Module, path: &Path, ty: &core::RcType) -> RcType {
    use semantics::Repr;

    // Mirroring `core::Type::repr`
    match *ty.inner {
        core::Type::Var(_, ref var) => lower_ty_var(var),
        core::Type::Const(ty_const) => Type::Const(ty_const.repr()).into(),
        core::Type::Lam(_, _, _) => {
            // Due to the way our surface syntax is defined, the only type
            // abstractions we should encounter are those that are defined on
            // top-level definitions. Thes should have already been handled in
            // the `From<&'a core::Module>` impl for `Module`.
            panic!("ICE: encountered unexpected type abstraction: {:?}", ty)
        }
        core::Type::App(_, ref ty, ref param_tys) => {
            let lowered_ty = lower_ty(module, path, ty);

            // Replace empty parameter lists on paths with the supplied parameters
            // TODO: This feels rather hacky! I'm sure it will break in non-trivial cases.
            // surely there is a better way to handle this?
            if let Type::Path(ref path, ref params) = *lowered_ty.inner {
                assert!(params.is_empty(), "ICE: Params not empty: {:?}", params);

                let lowered_params = param_tys
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| lower_ty(module, &path.append_child(format!("Arg{}", i)), ty))
                    .collect::<Vec<_>>();

                return Type::Path(path.clone(), lowered_params).into();
            }

            lowered_ty
        }

        core::Type::Array(_, ref elem_ty, _) => {
            let elem_path = path.append_child("Elem");
            let elem_ty = lower_ty(module, &elem_path, elem_ty);

            Type::Array(elem_ty).into()
        }
        core::Type::Assert(_, ref ty, _) => lower_ty(module, path, ty),
        core::Type::Interp(_, _, _, ref repr_ty) => lower_repr_ty(path, repr_ty),
        core::Type::Cond(_, ref options) => {
            let definition = Definition {
                doc: "".into(),
                path: path.clone(),
                params: vec![],
                item: Item::Union(
                    lower_row(path, options, |option_path, &(_, ref ty)| {
                        lower_ty(module, &option_path, ty)
                    }),
                    None,
                ),
            };

            module.define(definition);
            Type::Path(path.clone(), vec![]).into()
        }
        core::Type::Struct(_, ref fields) => {
            let definition = Definition {
                doc: "".into(),
                path: path.clone(),
                params: vec![],
                item: Item::Struct(
                    lower_row(path, fields, |field_path, ty| {
                        lower_ty(module, &field_path, ty)
                    }),
                    None,
                ),
            };

            module.define(definition);
            Type::Path(path.clone(), vec![]).into()
        }

        _ => unimplemented!(),
    }
}

/// Lower host types to the nominal format
///
/// # Arguments
///
/// * `path` - path to the parent struct or union
/// * `ty` - the type to be lowered
fn lower_repr_ty(path: &Path, ty: &core::RcType) -> RcType {
    match *ty.inner {
        core::Type::HostVar(ref var) => lower_ty_var(var),
        core::Type::Const(ty_const) => Type::Const(ty_const).into(),
        core::Type::Arrow(ref arg_tys, ref ret_ty) => {
            let arg_repr_tys = arg_tys
                .iter()
                .map(|arg_ty| lower_repr_ty(path, arg_ty))
                .collect();
            let ret_repr_ty = lower_repr_ty(path, ret_ty);

            Type::Arrow(arg_repr_tys, ret_repr_ty).into()
        }
        core::Type::Lam(_, _, _) => {
            // Due to the way our surface syntax is defined, the only type
            // abstractions we should encounter are those that are defined on
            // top-level definitions. Thes should have already been handled in
            // the `From<&'a core::Module>` impl for `Module`.
            panic!("ICE: encountered unexpected type abstraction: {:?}", ty)
        }
        core::Type::App(_, ref ty, ref param_tys) => {
            let lowered_ty = lower_repr_ty(path, ty);

            // Replace empty parameter lists on paths with the supplied parameters
            // TODO: This feels rather hacky! I'm sure it will break in non-trivial cases.
            // surely there is a better way to handle this?
            if let Type::Path(ref path, ref params) = *lowered_ty.inner {
                assert!(params.is_empty(), "ICE: Params not empty: {:?}", params);

                let lowered_params = param_tys
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| lower_repr_ty(&path.append_child(format!("Arg{}", i)), ty))
                    .collect::<Vec<_>>();

                return Type::Path(path.clone(), lowered_params).into();
            }

            lowered_ty
        }

        core::Type::HostArray(ref elem_ty) => {
            let elem_path = path.append_child("Elem");
            let elem_ty = lower_repr_ty(&elem_path, elem_ty);

            Type::Array(elem_ty).into()
        }
        core::Type::HostUnion(_) | core::Type::HostStruct(_) => {
            // We expect that the repr type has already had a corresponding type
            // generated for it, so instead we just return the current path.
            Type::Path(path.clone(), vec![]).into()
        }

        _ => unimplemented!(),
    }
}

/// Lower host expressions to the nominal format
///
/// # Arguments
///
/// * `path` - path to the parent struct or union
/// * `expr` - the expression to be lowered
fn lower_cexpr(path: &Path, expr: &core::RcCExpr) -> RcExpr {
    match *expr.inner {
        core::CExpr::Intro(_, _, _) => unimplemented!(),
        core::CExpr::Array(_, ref elems) => {
            Expr::Array(elems.iter().map(|elem| lower_cexpr(path, elem)).collect()).into()
        }
        core::CExpr::Inf(ref iexpr) => lower_iexpr(path, iexpr),
    }
}

fn lower_iexpr(path: &Path, expr: &core::RcIExpr) -> RcExpr {
    match *expr.inner {
        core::IExpr::Ann(_, ref expr, ref ty) => {
            let lowered_expr = lower_cexpr(path, expr);
            let lowered_ty = lower_repr_ty(path, ty);

            Expr::Ann(lowered_expr, lowered_ty).into()
        }
        core::IExpr::Const(_, c) => Expr::Const(c).into(),
        core::IExpr::Var(_, ref var) => Expr::Var(var.clone()).into(),
        core::IExpr::Lam(_, ref params, ref body_expr) => {
            let lowered_params = params
                .iter()
                .map(|param| Named::new(param.name.clone(), lower_repr_ty(path, &param.inner)))
                .collect();

            Expr::Lam(lowered_params, lower_iexpr(path, body_expr)).into()
        }
        core::IExpr::App(_, ref fn_expr, ref arg_exprs) => {
            let lowered_arg_exprs = arg_exprs
                .iter()
                .map(|expr| lower_cexpr(path, expr))
                .collect();

            Expr::App(lower_iexpr(path, fn_expr), lowered_arg_exprs).into()
        }

        core::IExpr::Unop(_, op, ref expr) => Expr::Unop(op, lower_iexpr(path, expr)).into(),
        core::IExpr::Binop(_, op, ref lhs, ref rhs) => {
            Expr::Binop(op, lower_iexpr(path, lhs), lower_iexpr(path, rhs)).into()
        }
        core::IExpr::Struct(ref fields) => {
            let lowered_fields = lower_row(path, fields, |field_path, expr| {
                lower_iexpr(&field_path, expr)
            });

            Expr::Struct(path.clone(), lowered_fields).into()
        }
        core::IExpr::Proj(_, ref expr, ref field_name) => {
            Expr::Proj(lower_iexpr(path, expr), field_name.clone()).into()
        }
        core::IExpr::Subscript(_, _, _) => unimplemented!(),
        core::IExpr::Cast(_, ref src_expr, ref dst_ty) => {
            Expr::Cast(lower_iexpr(path, src_expr), lower_repr_ty(path, dst_ty)).into()
        }
    }
}

/// Create a struct parser for the given fields
///
/// # Arguments
///
/// * `path` - path to the parent struct or union
/// * `fields` - the fields to be used in the parser
fn struct_parser(path: &Path, fields: &[Field<core::RcType>]) -> RcParseExpr {
    use var::ScopeIndex;

    let lower_to_field_parser = |field: &Field<core::RcType>| {
        (
            field.name.clone(),
            ty_parser(&path.append_child(field.name.0.clone()), &field.value),
        )
    };
    let lower_to_expr_field = |field: &Field<core::RcType>| Field {
        doc: Rc::clone(&field.doc),
        name: field.name.clone(),
        value: Expr::Var(Var::free(Name::user(field.name.clone()))).into(),
    };

    let parse_exprs = fields.iter().map(lower_to_field_parser);
    let expr_fields = fields.iter().map(lower_to_expr_field);

    let mut named_exprs = Vec::with_capacity(fields.len());
    let mut seen_names = Vec::<Ident>::with_capacity(fields.len());

    for (name, mut parse_expr) in parse_exprs {
        for (scope, name) in seen_names.iter().rev().enumerate() {
            parse_expr.abstract_names_at(&[Name::user(name.clone())], ScopeIndex(scope as u32));
        }

        seen_names.push(name.clone());
        named_exprs.push(Named::new(name, parse_expr));
    }

    let mut expr: RcExpr = Expr::Struct(path.clone(), expr_fields.collect()).into();
    for (scope, name) in seen_names.iter().rev().enumerate() {
        expr.abstract_names_at(&[Name::user(name.clone())], ScopeIndex(scope as u32));
    }

    ParseExpr::Sequence(named_exprs, expr).into()
}

/// Create a union parser for the given fields
///
/// # Arguments
///
/// * `path` - path to the parent struct or union
/// * `fields` - the fields to be used in the parser
fn cond_parser(path: &Path, options: &[Field<(core::RcCExpr, core::RcType)>]) -> RcParseExpr {
    let lower_option = |option: &Field<(core::RcCExpr, core::RcType)>| {
        let pred_expr = lower_cexpr(path, &option.value.0);
        let variant_parser = ParseExpr::Sequence(
            vec![
                Named::new(Ident::from("x"), ty_parser(path, &option.value.1)),
            ],
            Expr::Intro(
                path.clone(),
                option.name.clone(),
                // FIXME: generate fresh name?
                Expr::Var(Var::bound(Name::user("x"), BoundVar::new(Si(0), Bi(0)))).into(),
            ).into(),
        ).into();

        (pred_expr, variant_parser)
    };

    ParseExpr::Cond(options.iter().map(lower_option).collect()).into()
}

/// Create a parser for the given type
///
/// # Arguments
///
/// * `path` - path to the parent struct or union
/// * `ty` - the binary type to use as a basis for the parser
fn ty_parser(path: &Path, ty: &core::RcType) -> RcParseExpr {
    match *ty.inner {
        core::Type::Var(_, ref var) => ParseExpr::Var(var.clone()).into(),
        core::Type::Const(ty_const) => ParseExpr::Const(ty_const).into(),
        core::Type::Lam(_, _, _) => unimplemented!("Abs: {:?}", ty),
        core::Type::App(_, ref ty, _) => ty_parser(path, ty),

        core::Type::Array(_, ref elem_ty, ref size_expr) => {
            let elem_path = path.append_child("Elem");
            let elem_parser = ty_parser(&elem_path, elem_ty);
            let size_expr = lower_iexpr(path, size_expr);

            ParseExpr::Repeat(elem_parser, RepeatBound::Exact(size_expr)).into()
        }
        core::Type::Cond(_, ref options) => cond_parser(path, options),
        core::Type::Struct(_, ref fields) => struct_parser(path, fields),
        core::Type::Assert(_, ref ty, ref pred_expr) => {
            let ty_parser = ty_parser(path, ty);
            let pred_expr = lower_cexpr(path, pred_expr);

            ParseExpr::Assert(ty_parser, pred_expr).into()
        }
        core::Type::Interp(_, ref ty, ref conv_expr, _) => {
            let fn_expr = lower_cexpr(path, conv_expr);
            let parser_expr = ty_parser(path, ty);

            ParseExpr::Apply(fn_expr, parser_expr).into()
        }

        _ => unimplemented!(),
    }
}
