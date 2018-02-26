use codespan::ByteSpan;

use name::{Ident, Name};
use syntax::concrete;
use syntax::core;
use var::{Named, Var};

pub trait ToCore<T> {
    fn to_core(&self) -> T;
}

impl<'input> ToCore<Result<core::Module, ()>> for concrete::Module<'input> {
    fn to_core(&self) -> Result<core::Module, ()> {
        match *self {
            concrete::Module::Valid(ref definitions) => {
                let definitions = definitions
                    .iter()
                    .map(<_>::to_core)
                    .collect::<Result<_, _>>()?;

                Ok(core::Module::new(definitions))
            }
            concrete::Module::Error(_) => Err(()),
        }
    }
}

impl<'input> ToCore<Result<core::Definition, ()>> for concrete::Definition<'input> {
    fn to_core(&self) -> Result<core::Definition, ()> {
        match *self {
            concrete::Definition::Valid {
                ref doc,
                name,
                span,
                ref param_names,
                ref body_ty,
            } => {
                let body_ty = body_ty.to_core()?;

                Ok(core::Definition {
                    doc: doc.join("\n").into(),
                    name: Ident::from(name),
                    body_ty: match &param_names[..] {
                        names if names.is_empty() => body_ty,
                        names => {
                            let kind = core::RcKind::from(core::Kind::Binary);
                            let names = names
                                .iter()
                                .map(|&name| Named::new(Name::user(name), kind.clone()))
                                .collect::<Vec<_>>();

                            core::RcType::lam(span, names, body_ty)
                        }
                    },
                })
            }
            concrete::Definition::Error(_) => Err(()),
        }
    }
}

impl<'input> ToCore<Result<core::RcType, ()>> for concrete::Type<'input> {
    fn to_core(&self) -> Result<core::RcType, ()> {
        use semantics::Repr; // FIXME: Blegh - kind of cross-cutting concerns here...

        match *self {
            concrete::Type::Var(span, name) => {
                Ok(core::Type::Var(span, Var::free(Name::user(name))).into())
            }
            concrete::Type::App(span, ref fn_ty, ref arg_tys) => {
                let fn_ty = fn_ty.to_core()?;
                let arg_tys = arg_tys
                    .iter()
                    .map(|arg| arg.to_core())
                    .collect::<Result<_, _>>()?;

                Ok(core::Type::App(span, fn_ty, arg_tys).into())
            }

            concrete::Type::Array(span, ref elem_ty, ref size_expr) => {
                let elem_ty = elem_ty.to_core()?;
                let size_expr = size_expr.to_core()?;

                Ok(core::Type::Array(span, elem_ty, size_expr).into())
            }
            concrete::Type::Cond(span, ref options) => {
                let options = options
                    .iter()
                    .map(|variant| {
                        Ok(core::Field {
                            doc: variant.doc.join("\n").into(),
                            name: Ident::from(variant.name),
                            value: (variant.value.0.to_core()?, variant.value.1.to_core()?),
                        })
                    })
                    .collect::<Result<_, _>>()?;

                Ok(core::Type::Cond(span, options).into())
            }
            concrete::Type::Struct(span, ref fields) => {
                let fields = fields
                    .iter()
                    .map(|field| {
                        Ok(core::Field {
                            doc: field.doc.join("\n").into(),
                            name: Ident::from(field.name),
                            value: field.value.to_core()?,
                        })
                    })
                    .collect::<Result<_, _>>()?;

                Ok(core::RcType::struct_(span, fields))
            }
            concrete::Type::Where(span, ref ty, lo2, param_name, ref pred_expr) => {
                let ty = ty.to_core()?;
                let pred_fn = core::RcExpr::lam(
                    ByteSpan::new(lo2, span.end()),
                    vec![Named::new(Name::user(param_name), ty.repr())],
                    pred_expr.to_core()?,
                );

                Ok(core::Type::Assert(span, ty, pred_fn).into())
            }
            concrete::Type::Compute(span, repr_ty, ref expr) => {
                let empty = core::Type::Const(core::TypeConst::Empty).into();
                let repr_ty = core::Type::Const(repr_ty).into();
                let conv_fn = core::RcExpr::lam(
                    span,
                    vec![Named::new(Name::Abstract, core::RcType::repr(&empty))],
                    expr.to_core()?,
                );

                Ok(core::Type::Interp(span, empty.into(), conv_fn, repr_ty).into())
            }
            concrete::Type::Error(_) => Err(()),
        }
    }
}

impl<'input> ToCore<Result<core::RcExpr, ()>> for concrete::Expr<'input> {
    fn to_core(&self) -> Result<core::RcExpr, ()> {
        match *self {
            concrete::Expr::Const(span, c) => Ok(core::Expr::Const(span, c).into()),
            concrete::Expr::Ann(span, ref expr, ty) => {
                let expr = expr.to_core()?;
                let ty = core::Type::Const(ty).into();

                Ok(core::Expr::Ann(span, expr, ty).into())
            }
            concrete::Expr::Var(span, name) => {
                Ok(core::Expr::Var(span, Var::free(Name::user(name))).into())
            }

            concrete::Expr::Unop(span, op, ref expr) => {
                let expr = expr.to_core()?;

                Ok(core::Expr::Unop(span, op, expr).into())
            }
            concrete::Expr::Binop(span, op, ref lhs_expr, ref rhs_expr) => {
                let lhs_expr = lhs_expr.to_core()?;
                let rhs_expr = rhs_expr.to_core()?;

                Ok(core::Expr::Binop(span, op, lhs_expr, rhs_expr).into())
            }
            concrete::Expr::Array(_, _) => Err(()),
            concrete::Expr::Proj(span, ref struct_expr, field_name) => {
                let struct_expr = struct_expr.to_core()?;
                let field_name = String::from(field_name);

                Ok(core::Expr::Proj(span, struct_expr, Ident::from(field_name)).into())
            }
            concrete::Expr::Subscript(span, ref array_expr, ref index_expr) => {
                let array_expr = array_expr.to_core()?;
                let index_expr = index_expr.to_core()?;

                Ok(core::Expr::Subscript(span, array_expr, index_expr).into())
            }
            concrete::Expr::Cast(span, ref expr, ty) => {
                let expr = expr.to_core()?;
                let ty = core::Type::Const(ty).into();

                Ok(core::Expr::Cast(span, expr, ty).into())
            }
            concrete::Expr::Error(_) => Err(()),
        }
    }
}
