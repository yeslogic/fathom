//! Type and kind-checking for our DDL

use std::rc::Rc;

use name::{Ident, Name, Named};
use syntax::ast::{Binop, Field, Kind, Module, RcCExpr, RcIExpr, RcKind, RcType};
use self::context::{Context, Scope};
use var::Var;

mod context;
#[cfg(test)]
mod tests;

// Typing

#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedType {
    Array,
    Arrow,
    Unsigned,
    Signed,
    Numeric,
    Actual(RcType),
}

/// An error that was encountered during type checking
#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    /// A variable of the requested name was not bound in this scope
    UnboundVariable { expr: RcIExpr, name: Name },
    /// Variable bound in the context was not at the value level
    ExprBindingExpected { expr: RcIExpr, found: Scope },
    /// One type was expected, but another was found
    Mismatch {
        expr: RcIExpr,
        found: RcType,
        expected: ExpectedType,
    },
    /// One type was expected, but another was found
    InferenceMismatch {
        expr: RcIExpr,
        found: RcType,
        expected: RcType,
    },
    /// Unexpected operand types in a binary operator expression
    BinaryOperands {
        context: Binop,
        expr: RcIExpr,
        lhs_ty: RcType,
        rhs_ty: RcType,
    },
    /// A field was missing when projecting on a record
    MissingField {
        expr: RcIExpr,
        struct_ty: RcType,
        field_name: Ident,
    },
    /// A variant was missing when introducing on a union
    MissingVariant {
        expr: RcCExpr,
        union_ty: RcType,
        variant_name: Ident,
    },
    /// An invalid type was supplied to the cast expression
    InvalidCastType { expr: RcIExpr, found: RcType },
}

/// Check that an expression has the given type in the context
pub fn check_ty(ctx: &Context, expr: &RcCExpr, expected_ty: &RcType) -> Result<(), TypeError> {
    use syntax::ast::CExpr;
    use syntax::ast::Type;

    match *expr.inner {
        // Variant introduction
        CExpr::Intro(_, ref variant_name, ref expr) => {
            // FIXME: Kindcheck union_ty
            match expected_ty.lookup_host_variant(variant_name).cloned() {
                Some(variant_ty) => {
                    check_ty(ctx, expr, &variant_ty)?;
                    Ok(())
                }
                None => Err(TypeError::MissingVariant {
                    expr: expr.clone(),
                    union_ty: expected_ty.clone(),
                    variant_name: variant_name.clone(),
                }),
            }
        }

        // Empty arrays
        CExpr::Array(_, ref elems) => match *expected_ty.inner {
            Type::HostArray(ref elem_ty) => elems
                .iter()
                .map(|elem| check_ty(ctx, elem, elem_ty))
                .collect(),
            _ => unimplemented!(), // FIXME
        },

        // Inferrable expressions
        CExpr::Inf(ref iexpr) => {
            let inferred_ty = infer_ty(ctx, iexpr)?;
            if &inferred_ty == expected_ty {
                Ok(())
            } else {
                Err(TypeError::InferenceMismatch {
                    expr: iexpr.clone(),
                    expected: expected_ty.clone(),
                    found: inferred_ty,
                })
            }
        }
    }
}

/// Infer the type of an expression in the context
pub fn infer_ty(ctx: &Context, expr: &RcIExpr) -> Result<RcType, TypeError> {
    use syntax::ast::{HostTypeConst, IExpr, Type, Unop};

    match *expr.inner {
        // Annotated types
        IExpr::Ann(_, ref expr, ref ty) => {
            check_ty(ctx, expr, ty)?;
            Ok(ty.clone())
        }

        // Constants are easy!
        IExpr::Const(_, c) => Ok(Type::HostConst(c.ty_const_of()).into()),

        // Variables
        IExpr::Var(_, Var::Free(ref name)) => Err(TypeError::UnboundVariable {
            expr: expr.clone(),
            name: name.clone(),
        }),
        IExpr::Var(_, Var::Bound(Named(_, i))) => match ctx.lookup_ty(i) {
            Ok((_, ty)) => Ok(ty.clone()),
            Err(scope) => Err(TypeError::ExprBindingExpected {
                expr: expr.clone(),
                found: scope.clone(),
            }),
        },

        // Abstraction
        IExpr::Lam(_, ref params, ref body_expr) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();
            ctx.extend(Scope::ExprLam(params.clone()));
            let param_tys = params.iter().map(|param| param.1.clone()).collect();

            Ok(Type::HostArrow(param_tys, infer_ty(&ctx, body_expr)?).into())
        }

        // Applications
        IExpr::App(_, ref fn_expr, ref arg_exprs) => {
            let fn_ty = infer_ty(ctx, fn_expr)?;

            if let Type::HostArrow(ref param_tys, ref ret_ty) = *fn_ty.inner {
                if arg_exprs.len() == param_tys.len() {
                    for (arg_expr, param_ty) in arg_exprs.iter().zip(param_tys) {
                        check_ty(ctx, arg_expr, param_ty)?;
                    }

                    return Ok(ret_ty.clone());
                } else {
                    unimplemented!(); // FIXME
                }
            }

            Err(TypeError::Mismatch {
                expr: fn_expr.clone(),
                expected: ExpectedType::Arrow,
                found: fn_ty,
            })
        }

        // Unary operators
        IExpr::Unop(_, op, ref operand_expr) => {
            use syntax::ast::Type::HostConst;

            let operand_ty = infer_ty(ctx, operand_expr)?;

            match (op, &*operand_ty.inner) {
                (Unop::Neg, &HostConst(HostTypeConst::Signed(_)))
                | (Unop::Neg, &HostConst(HostTypeConst::Float(_))) => Ok(operand_ty),
                (Unop::Neg, _) => Err(TypeError::Mismatch {
                    expr: expr.clone(),
                    expected: ExpectedType::Signed,
                    found: operand_ty,
                }),
                (Unop::Not, &HostConst(HostTypeConst::Bool)) => Ok(operand_ty),
                (Unop::Not, _) => Err(TypeError::Mismatch {
                    expr: expr.clone(),
                    expected: ExpectedType::Actual(HostConst(HostTypeConst::Bool).into()),
                    found: operand_ty,
                }),
            }
        }

        // Binary operators
        IExpr::Binop(_, op, ref lhs_expr, ref rhs_expr) => {
            use syntax::ast::Type::{HostArray, HostConst};

            fn binop_err(
                context: Binop,
                expr: &RcIExpr,
                lhs_ty: RcType,
                rhs_ty: RcType,
            ) -> TypeError {
                TypeError::BinaryOperands {
                    context,
                    expr: expr.clone(),
                    lhs_ty,
                    rhs_ty,
                }
            }

            let lhs_ty = infer_ty(ctx, lhs_expr)?;
            let rhs_ty = infer_ty(ctx, rhs_expr)?;

            match (&*lhs_ty.inner, &*rhs_ty.inner) {
                (&HostConst(HostTypeConst::Bool), &HostConst(HostTypeConst::Bool)) => match op {
                    Binop::Or | Binop::And | Binop::Eq | Binop::Ne => Ok(lhs_ty.clone()),
                    _ => Err(binop_err(op, expr, lhs_ty.clone(), rhs_ty.clone())),
                },
                (&HostConst(HostTypeConst::Float(l)), &HostConst(HostTypeConst::Float(r)))
                    if l == r =>
                {
                    match op {
                        Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                            Ok(HostConst(HostTypeConst::Bool).into())
                        }
                        Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => Ok(lhs_ty.clone()),
                        _ => Err(binop_err(op, expr, lhs_ty.clone(), rhs_ty.clone())),
                    }
                }
                (&HostConst(HostTypeConst::Signed(l)), &HostConst(HostTypeConst::Signed(r)))
                    if l == r =>
                {
                    match op {
                        Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                            Ok(HostConst(HostTypeConst::Bool).into())
                        }
                        Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => Ok(lhs_ty.clone()),
                        _ => Err(binop_err(op, expr, lhs_ty.clone(), rhs_ty.clone())),
                    }
                }
                (
                    &HostConst(HostTypeConst::Unsigned(l)),
                    &HostConst(HostTypeConst::Unsigned(r)),
                ) if l == r =>
                {
                    match op {
                        Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                            Ok(HostConst(HostTypeConst::Bool).into())
                        }
                        Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => Ok(lhs_ty.clone()),
                        _ => Err(binop_err(op, expr, lhs_ty.clone(), rhs_ty.clone())),
                    }
                }
                (&HostArray(ref l), &HostArray(ref r)) if l == r => match op {
                    Binop::Eq | Binop::Ne => Ok(HostConst(HostTypeConst::Bool).into()),
                    _ => Err(binop_err(op, expr, lhs_ty.clone(), rhs_ty.clone())),
                },
                (_, _) => Err(binop_err(op, expr, lhs_ty.clone(), rhs_ty.clone())),
            }
        }

        // Struct expressions
        IExpr::Struct(ref fields) => {
            let field_tys = fields
                .iter()
                .map(|field| {
                    Ok(Field {
                        doc: Rc::clone(&field.doc),
                        name: field.name.clone(),
                        value: infer_ty(ctx, &field.value)?,
                    })
                })
                .collect::<Result<_, _>>()?;

            Ok(Type::HostStruct(field_tys).into())
        }

        // Field projection
        IExpr::Proj(_, ref struct_expr, ref field_name) => {
            let struct_ty = infer_ty(ctx, struct_expr)?;

            match struct_ty.lookup_host_field(field_name).cloned() {
                Some(field_ty) => Ok(field_ty),
                None => Err(TypeError::MissingField {
                    expr: struct_expr.clone(),
                    struct_ty: struct_ty.clone(),
                    field_name: field_name.clone(),
                }),
            }
        }

        // Array subscript
        IExpr::Subscript(_, ref array_expr, ref index_expr) => {
            let index_ty = infer_ty(ctx, index_expr)?;
            match *index_ty.inner {
                Type::HostConst(HostTypeConst::Unsigned(_)) => {}
                _ => {
                    return Err(TypeError::Mismatch {
                        expr: index_expr.clone(),
                        expected: ExpectedType::Unsigned,
                        found: index_ty,
                    })
                }
            }

            let array_ty = infer_ty(ctx, array_expr)?;
            match *array_ty.inner {
                Type::HostArray(ref elem_ty) => Ok(elem_ty.clone()),
                _ => Err(TypeError::Mismatch {
                    expr: array_expr.clone(),
                    expected: ExpectedType::Array,
                    found: array_ty.clone(),
                }),
            }
        }

        // Cast Expressions
        IExpr::Cast(_, ref src_expr, ref dst_ty) => {
            let src_ty = infer_ty(ctx, src_expr)?;

            match *dst_ty.inner {
                Type::HostConst(HostTypeConst::Float(_))
                | Type::HostConst(HostTypeConst::Signed(_))
                | Type::HostConst(HostTypeConst::Unsigned(_)) => match *src_ty.inner {
                    Type::HostConst(HostTypeConst::Float(_))
                    | Type::HostConst(HostTypeConst::Signed(_))
                    | Type::HostConst(HostTypeConst::Unsigned(_)) => Ok(dst_ty.clone()),
                    _ => Err(TypeError::Mismatch {
                        expr: src_expr.clone(),
                        expected: ExpectedType::Numeric,
                        found: src_ty.clone(),
                    }),
                },
                _ => Err(TypeError::InvalidCastType {
                    expr: expr.clone(),
                    found: dst_ty.clone(),
                }),
            }
        }
    }
}

// Kinding

fn simplify_ty(ctx: &Context, ty: &RcType) -> RcType {
    use syntax::ast::Type;

    fn compute_ty(ctx: &Context, ty: &RcType) -> Option<RcType> {
        match *ty.inner {
            Type::Var(_, Var::Bound(Named(_, i))) => match ctx.lookup_ty_def(i) {
                Ok((_, def_ty)) => Some(def_ty.clone()),
                Err(_) => None,
            },
            Type::App(_, ref fn_ty, ref arg_tys) => match *fn_ty.inner {
                Type::Lam(_, _, ref body_ty) => {
                    // FIXME: Avoid clone
                    let mut body = body_ty.clone();
                    body.instantiate(arg_tys);
                    Some(body)
                }
                _ => None,
            },
            _ => None,
        }
    }

    let ty = match *ty.inner {
        Type::App(_, ref fn_ty, _) => simplify_ty(ctx, fn_ty),
        // FIXME: Avoid clone
        _ => ty.clone(),
    };

    match compute_ty(ctx, &ty) {
        Some(ty) => simplify_ty(ctx, &ty),
        None => ty,
    }
}

/// An error that was encountered during kind checking
#[derive(Debug, Clone, PartialEq)]
pub enum KindError {
    /// A variable of the requested name was not bound in this scope
    UnboundVariable { ty: RcType, name: Name },
    /// Variable bound in the context was not at the type level
    TypeBindingExpected { ty: RcType, found: Scope },
    /// One kind was expected, but another was found
    Mismatch {
        ty: RcType,
        expected: RcKind,
        found: RcKind,
    },
    /// A type error
    Type(TypeError),
}

impl From<TypeError> for KindError {
    fn from(src: TypeError) -> KindError {
        KindError::Type(src)
    }
}

/// Check that a binary type has the given kind in the context
fn check_kind(ctx: &Context, ty: &RcType, expected_kind: &RcKind) -> Result<(), KindError> {
    let found = infer_kind(ctx, ty)?;

    if &found == expected_kind {
        Ok(())
    } else {
        Err(KindError::Mismatch {
            ty: ty.clone(),
            expected: expected_kind.clone(),
            found,
        })
    }
}

/// Infer the kind of a binary type in the context
pub fn infer_kind(ctx: &Context, ty: &RcType) -> Result<RcKind, KindError> {
    use syntax::ast::{HostTypeConst, Type, TypeConst};

    match *ty.inner {
        // Variables
        Type::Var(_, Var::Free(ref name)) => Err(KindError::UnboundVariable {
            ty: ty.clone(),
            name: name.clone(),
        }),
        Type::Var(_, Var::Bound(Named(_, i))) => match ctx.lookup_kind(i) {
            Ok((_, kind)) => Ok(kind.clone()),
            Err(scope) => Err(KindError::TypeBindingExpected {
                ty: ty.clone(),
                found: scope.clone(),
            }),
        },

        // Type constants
        Type::Const(TypeConst::Empty)
        | Type::Const(TypeConst::Error)
        | Type::Const(TypeConst::U8)
        | Type::Const(TypeConst::I8)
        | Type::Const(TypeConst::U16(_))
        | Type::Const(TypeConst::U24(_))
        | Type::Const(TypeConst::U32(_))
        | Type::Const(TypeConst::U64(_))
        | Type::Const(TypeConst::I16(_))
        | Type::Const(TypeConst::I24(_))
        | Type::Const(TypeConst::I32(_))
        | Type::Const(TypeConst::I64(_))
        | Type::Const(TypeConst::F32(_))
        | Type::Const(TypeConst::F64(_)) => Ok(Kind::Binary.into()),

        // Type abstraction
        Type::Lam(_, ref param_tys, ref body_ty) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();

            ctx.extend(Scope::TypeLam(
                param_tys
                    .iter()
                    .map(|named| Named(named.0.clone(), Kind::Binary.into()))
                    .collect(),
            ));
            check_kind(&ctx, body_ty, &Kind::Binary.into())?;

            Ok(
                Kind::Arrow(
                    (0..param_tys.len()).map(|_| Kind::Binary.into()).collect(),
                    Kind::Binary.into(),
                ).into(),
            )
        }

        // Type application
        Type::App(_, ref fn_ty, ref arg_tys) => {
            let expected = Kind::Arrow(
                (0..arg_tys.len()).map(|_| Kind::Binary.into()).collect(),
                Kind::Binary.into(),
            ).into();

            check_kind(ctx, fn_ty, &expected)?;

            for arg_ty in arg_tys {
                check_kind(ctx, arg_ty, &Kind::Binary.into())?
            }

            Ok(Kind::Binary.into())
        }

        // Array types
        Type::Array(_, ref elem_ty, ref size_expr) => {
            check_kind(ctx, elem_ty, &Kind::Binary.into())?;

            let size_ty = infer_ty(ctx, size_expr)?;
            match *size_ty.inner {
                Type::HostConst(HostTypeConst::Unsigned(_)) => Ok(Kind::Binary.into()),
                _ => Err(
                    TypeError::Mismatch {
                        expr: size_expr.clone(),
                        expected: ExpectedType::Signed,
                        found: size_ty,
                    }.into(),
                ),
            }
        }

        // Conditional types
        Type::Assert(_, ref ty, ref pred_expr) => {
            check_kind(ctx, ty, &Kind::Binary.into())?;
            let pred_ty =
                Type::HostArrow(vec![ty.repr()], Type::HostConst(HostTypeConst::Bool).into())
                    .into();
            check_ty(ctx, pred_expr, &pred_ty)?;

            Ok(Kind::Binary.into())
        }

        // Interpreted types
        Type::Interp(_, ref ty, ref conv_expr, ref repr_ty) => {
            check_kind(ctx, ty, &Kind::Binary.into())?;
            let conv_ty = Type::HostArrow(vec![ty.repr()], repr_ty.clone()).into();
            check_ty(ctx, conv_expr, &conv_ty)?;

            Ok(Kind::Binary.into())
        }

        // Conditional types
        Type::Cond(_, ref options) => {
            let bool_ty = Type::HostConst(HostTypeConst::Bool).into();

            for option in options {
                check_ty(ctx, &option.value.0, &bool_ty)?;
                check_kind(ctx, &option.value.1, &Kind::Binary.into())?;
            }

            Ok(Kind::Binary.into())
        }

        // Struct type
        Type::Struct(_, ref fields) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();

            for field in fields {
                check_kind(&ctx, &field.value, &Kind::Binary.into())?;

                let field_ty = simplify_ty(&ctx, &field.value);
                ctx.extend(Scope::ExprLam(
                    vec![Named(Name::user(field.name.clone()), field_ty.repr())],
                ));
            }

            Ok(Kind::Binary.into())
        }

        _ => unimplemented!(),
    }
}

pub fn check_module(module: &Module) -> Result<(), KindError> {
    let mut ctx = Context::new();

    for definition in &module.definitions {
        let name = Name::user(definition.name.clone());
        let ty = definition.body_ty.clone();
        let kind = infer_kind(&ctx, &ty)?;

        ctx.extend(Scope::TypeDef(vec![Named(name, (ty, kind))]));
    }

    Ok(())
}
