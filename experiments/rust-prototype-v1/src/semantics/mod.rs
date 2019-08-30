//! Type and kind-checking for our DDL

use codespan::ByteSpan;
use std::rc::Rc;

use name::Name;
use syntax::core::{Binop, Context, Expr, Field, FloatType, Kind, Module, RcExpr, RcKind, RcType,
                   Scope, SignedType, Type, TypeConst, UnsignedType};
use var::{Named, Var};

#[cfg(test)]
mod tests;
mod errors;

pub use self::errors::{KindError, TypeError};

// Representations

/// Convert a type or expression to its corresponding host representation
pub trait Repr<T> {
    fn repr(&self) -> T;
}

impl Repr<RcKind> for RcKind {
    fn repr(&self) -> RcKind {
        match *self.inner {
            Kind::Binary => Kind::Host.into(),
            Kind::Host => panic!("ICE: tried to find the repr of Kind::Host"),
            Kind::Arrow(ref params, ref ret) => {
                Kind::Arrow(params.iter().map(RcKind::repr).collect(), ret.repr()).into()
            }
        }
    }
}

impl Repr<TypeConst> for TypeConst {
    /// Convert a binary type constant to its corresponding host representation
    fn repr(&self) -> TypeConst {
        match *self {
            TypeConst::Empty => TypeConst::Unit,
            TypeConst::Error => TypeConst::Bottom,
            TypeConst::U8 => TypeConst::Unsigned(UnsignedType::U8),
            TypeConst::I8 => TypeConst::Signed(SignedType::I8),
            TypeConst::U16(_) => TypeConst::Unsigned(UnsignedType::U16),
            TypeConst::U24(_) => TypeConst::Unsigned(UnsignedType::U24),
            TypeConst::U32(_) => TypeConst::Unsigned(UnsignedType::U32),
            TypeConst::U64(_) => TypeConst::Unsigned(UnsignedType::U64),
            TypeConst::I16(_) => TypeConst::Signed(SignedType::I16),
            TypeConst::I24(_) => TypeConst::Signed(SignedType::I24),
            TypeConst::I32(_) => TypeConst::Signed(SignedType::I32),
            TypeConst::I64(_) => TypeConst::Signed(SignedType::I64),
            TypeConst::F32(_) => TypeConst::Float(FloatType::F32),
            TypeConst::F64(_) => TypeConst::Float(FloatType::F64),

            _ => panic!("Called TypeConst::repr on {:?}", self),
        }
    }
}

impl Repr<RcType> for RcType {
    /// Returns the host representation of the binary type
    fn repr(&self) -> RcType {
        match *self.inner {
            Type::Var(_, ref v) => Type::HostVar(v.clone()).into(),
            Type::Const(ty_const) => Type::Const(ty_const.repr()).into(),
            Type::Lam(span, ref params, ref body_ty) => {
                let repr_params = params
                    .iter()
                    .map(|param| Named::new(param.name.clone(), param.inner.repr()))
                    .collect();

                Type::Lam(span, repr_params, body_ty.repr()).into()
            }
            Type::App(span, ref fn_ty, ref arg_tys) => {
                let arg_tys = arg_tys.iter().map(|arg| arg.repr()).collect();

                Type::App(span, fn_ty.repr(), arg_tys).into()
            }

            Type::Array(_, ref elem_ty, _) => Type::HostArray(elem_ty.repr()).into(),
            Type::Assert(_, ref ty, _) => ty.repr(),
            Type::Interp(_, _, _, ref repr_ty) => repr_ty.clone(),
            Type::Cond(_, ref options) => {
                let repr_variants = options
                    .iter()
                    .map(|variant| Field {
                        doc: Rc::clone(&variant.doc),
                        name: variant.name.clone(),
                        value: variant.value.1.repr(),
                    })
                    .collect();

                Type::HostUnion(repr_variants).into()
            }
            Type::Struct(_, ref fields) => {
                let repr_fields = fields
                    .iter()
                    .map(|field| Field {
                        doc: Rc::clone(&field.doc),
                        name: field.name.clone(),
                        value: field.value.repr(),
                    })
                    .collect();

                Type::HostStruct(repr_fields).into()
            }

            _ => unimplemented!(),
        }
    }
}

// Typing

/// Check that an expression has the given type in the context
pub fn check_ty(ctx: &Context, expr: &RcExpr, expected_ty: &RcType) -> Result<(), TypeError> {
    match *expr.inner {
        // Variant introduction
        Expr::Intro(_, ref variant_name, ref expr) => {
            // FIXME: Kindcheck union_ty
            match expected_ty.lookup_host_variant(variant_name).cloned() {
                Some(variant_ty) => {
                    check_ty(ctx, expr, &variant_ty)?;
                    Ok(())
                }
                None => Err(TypeError::MissingVariant {
                    span: ByteSpan::default(), // TODO: expr.span(),
                    union_ty: expected_ty.clone(),
                    variant_name: variant_name.clone(),
                }),
            }
        }

        // Empty arrays
        Expr::Array(_, ref elems) => match *expected_ty.inner {
            Type::HostArray(ref elem_ty) => elems
                .iter()
                .map(|elem| check_ty(ctx, elem, elem_ty))
                .collect(),
            _ => unimplemented!(), // FIXME
        },

        // Inferrable expressions
        _ => {
            let inferred_ty = infer_ty(ctx, expr)?;
            if &inferred_ty == expected_ty {
                Ok(())
            } else {
                Err(TypeError::Mismatch {
                    span: ByteSpan::default(), // TODO: expr.span(),
                    expected: expected_ty.clone(),
                    found: inferred_ty,
                })
            }
        }
    }
}

/// Infer the type of an expression in the context
pub fn infer_ty(ctx: &Context, expr: &RcExpr) -> Result<RcType, TypeError> {
    match *expr.inner {
        // Annotated types
        Expr::Ann(_, ref expr, ref ty) => {
            check_ty(ctx, expr, ty)?;
            Ok(ty.clone())
        }

        // Constants are easy!
        Expr::Const(_, c) => Ok(Type::Const(c.ty_const_of()).into()),

        // Variables
        Expr::Var(_, Var::Free(ref name)) => Err(TypeError::UndefinedName {
            var_span: ByteSpan::default(), // TODO: expr.span(),
            name: name.clone(),
        }),
        Expr::Var(_, Var::Bound(Named { inner: i, .. })) => match ctx.lookup_ty(i) {
            Ok((_, ty)) => Ok(ty.clone()),
            Err(scope) => Err(TypeError::ExpectedExpr {
                var_span: ByteSpan::default(), // TODO: expr.span(),
                found: scope.clone(),
            }),
        },

        // Abstraction
        Expr::Lam(_, ref params, ref body_expr) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();
            ctx.extend(Scope::ExprLam(params.clone()));
            let param_tys = params.iter().map(|param| param.inner.clone()).collect();

            Ok(Type::Arrow(param_tys, infer_ty(&ctx, body_expr)?).into())
        }

        // TODO: Better error
        Expr::Intro(_, _, _) => unimplemented!(),
        // TODO: Infer types
        Expr::Array(_, _) => unimplemented!(),

        // Applications
        Expr::App(_, ref fn_expr, ref arg_exprs) => {
            let fn_ty = infer_ty(ctx, fn_expr)?;

            if let Type::Arrow(ref param_tys, ref ret_ty) = *fn_ty.inner {
                if arg_exprs.len() == param_tys.len() {
                    for (arg_expr, param_ty) in arg_exprs.iter().zip(param_tys) {
                        check_ty(ctx, arg_expr, param_ty)?;
                    }

                    return Ok(ret_ty.clone());
                } else {
                    unimplemented!(); // FIXME
                }
            } else {
                Err(TypeError::ArgsAppliedToNonFunction {
                    fn_span: ByteSpan::default(),  // TODO: fn_expr.span(),
                    arg_span: ByteSpan::default(), // TODO: arg_expr.span(),
                    found: fn_ty.clone(),
                })
            }
        }

        // Unary operators
        Expr::Unop(_, op, ref operand_expr) => {
            use syntax::core::Unop;

            let operand_ty = infer_ty(ctx, operand_expr)?;

            match (op, &*operand_ty.inner) {
                (Unop::Neg, &Type::Const(TypeConst::Signed(_)))
                | (Unop::Neg, &Type::Const(TypeConst::Float(_))) => Ok(operand_ty),
                (Unop::Neg, _) => Err(TypeError::NegOnUnsigned {
                    operand_span: ByteSpan::default(), // TODO: operand_expr.span(),
                    operand_ty,
                }),
                (Unop::Not, &Type::Const(TypeConst::Bool)) => Ok(operand_ty),
                (Unop::Not, _) => Err(TypeError::Mismatch {
                    span: ByteSpan::default(), // TODO: expr.span(),
                    found: operand_ty,
                    expected: Type::Const(TypeConst::Bool).into(),
                }),
            }
        }

        // Binary operators
        Expr::Binop(_, op, ref lhs_expr, ref rhs_expr) => {
            use syntax::core::Type::{Const, HostArray};

            fn binop_err(
                binop: Binop,
                _expr: &RcExpr,
                lhs_ty: RcType,
                rhs_ty: RcType,
            ) -> TypeError {
                TypeError::BinaryOperands {
                    expr_span: ByteSpan::default(), // TODO: expr.span(),
                    binop,
                    lhs_ty,
                    rhs_ty,
                }
            }

            let lhs_ty = infer_ty(ctx, lhs_expr)?;
            let rhs_ty = infer_ty(ctx, rhs_expr)?;

            match (&*lhs_ty.inner, &*rhs_ty.inner) {
                (&Const(TypeConst::Bool), &Const(TypeConst::Bool)) => match op {
                    Binop::Or | Binop::And | Binop::Eq | Binop::Ne => Ok(lhs_ty.clone()),
                    _ => Err(binop_err(op, expr, lhs_ty.clone(), rhs_ty.clone())),
                },
                (&Const(TypeConst::Float(l)), &Const(TypeConst::Float(r))) if l == r => match op {
                    Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                        Ok(Const(TypeConst::Bool).into())
                    }
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => Ok(lhs_ty.clone()),
                    _ => Err(binop_err(op, expr, lhs_ty.clone(), rhs_ty.clone())),
                },
                (&Const(TypeConst::Signed(l)), &Const(TypeConst::Signed(r))) if l == r => {
                    match op {
                        Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                            Ok(Const(TypeConst::Bool).into())
                        }
                        Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => Ok(lhs_ty.clone()),
                        _ => Err(binop_err(op, expr, lhs_ty.clone(), rhs_ty.clone())),
                    }
                }
                (&Const(TypeConst::Unsigned(l)), &Const(TypeConst::Unsigned(r))) if l == r => {
                    match op {
                        Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                            Ok(Const(TypeConst::Bool).into())
                        }
                        Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => Ok(lhs_ty.clone()),
                        _ => Err(binop_err(op, expr, lhs_ty.clone(), rhs_ty.clone())),
                    }
                }
                (&HostArray(ref l), &HostArray(ref r)) if l == r => match op {
                    Binop::Eq | Binop::Ne => Ok(Const(TypeConst::Bool).into()),
                    _ => Err(binop_err(op, expr, lhs_ty.clone(), rhs_ty.clone())),
                },
                (_, _) => Err(binop_err(op, expr, lhs_ty.clone(), rhs_ty.clone())),
            }
        }

        // Struct expressions
        Expr::Struct(ref fields) => {
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
        Expr::Proj(_, ref struct_expr, ref field_name) => {
            let struct_ty = infer_ty(ctx, struct_expr)?;

            match struct_ty.lookup_host_field(field_name).cloned() {
                Some(field_ty) => Ok(field_ty),
                None => Err(TypeError::MissingField {
                    span: ByteSpan::default(), // TODO: struct_expr.span(),
                    struct_ty: struct_ty.clone(),
                    field_name: field_name.clone(),
                }),
            }
        }

        // Array subscript
        Expr::Subscript(_, ref array_expr, ref index_expr) => {
            let index_ty = infer_ty(ctx, index_expr)?;
            match *index_ty.inner {
                Type::Const(TypeConst::Unsigned(_)) => {}
                _ => {
                    return Err(TypeError::UnexpectedIndexType {
                        index_span: ByteSpan::default(), // TODO: index_expr.clone(),
                        found: index_ty,
                    });
                }
            }

            let array_ty = infer_ty(ctx, array_expr)?;
            match *array_ty.inner {
                Type::HostArray(ref elem_ty) => Ok(elem_ty.clone()),
                _ => Err(TypeError::SubscriptOnNonArray {
                    index_span: ByteSpan::default(),  // TODO: index_expr.span(),
                    target_span: ByteSpan::default(), // TODO: array_ty.span()
                    target_ty: array_ty.clone(),
                }),
            }
        }

        // Cast Expressions
        Expr::Cast(_, ref src_expr, ref dst_ty) => {
            let src_ty = infer_ty(ctx, src_expr)?;

            match *dst_ty.inner {
                Type::Const(TypeConst::Float(_))
                | Type::Const(TypeConst::Signed(_))
                | Type::Const(TypeConst::Unsigned(_)) => match *src_ty.inner {
                    Type::Const(TypeConst::Float(_))
                    | Type::Const(TypeConst::Signed(_))
                    | Type::Const(TypeConst::Unsigned(_)) => return Ok(dst_ty.clone()),
                    _ => {}
                },
                _ => {}
            }

            Err(TypeError::InvalidCast {
                src_span: ByteSpan::default(), // TODO: src_expr.span(),
                dst_span: ByteSpan::default(), // TODO: src_expr.span(),
                src_ty: src_ty.clone(),
                dst_ty: src_ty.clone(),
            })
        }
    }
}

// Kinding

fn simplify_ty(ctx: &Context, ty: &RcType) -> RcType {
    use syntax::core::Type;

    fn compute_ty(ctx: &Context, ty: &RcType) -> Option<RcType> {
        match *ty.inner {
            Type::Var(_, Var::Bound(Named { inner: i, .. })) => match ctx.lookup_ty_def(i) {
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

/// Check that a binary type has the given kind in the context
fn check_kind(ctx: &Context, ty: &RcType, expected_kind: &RcKind) -> Result<(), KindError> {
    let found = infer_kind(ctx, ty)?;

    if &found == expected_kind {
        Ok(())
    } else {
        Err(KindError::Mismatch {
            span: ByteSpan::default(), // TODO: ty.span(),
            expected: expected_kind.clone(),
            found,
        })
    }
}

/// Infer the kind of a binary type in the context
pub fn infer_kind(ctx: &Context, ty: &RcType) -> Result<RcKind, KindError> {
    use syntax::core::{Type, TypeConst};

    match *ty.inner {
        // Variables
        Type::Var(_, Var::Free(ref name)) => Err(KindError::UndefinedName {
            var_span: ByteSpan::default(), // TODO: ty.span(),
            name: name.clone(),
        }),
        Type::Var(_, Var::Bound(Named { inner: i, .. })) => match ctx.lookup_kind(i) {
            Ok((_, kind)) => Ok(kind.clone()),
            Err(scope) => Err(KindError::ExpectedType {
                span: ByteSpan::default(), // TODO: ty.span(),
                found: scope.clone(),
            }),
        },

        // Type constants
        Type::Const(c) => match c {
            TypeConst::Empty
            | TypeConst::Error
            | TypeConst::U8
            | TypeConst::I8
            | TypeConst::U16(_)
            | TypeConst::U24(_)
            | TypeConst::U32(_)
            | TypeConst::U64(_)
            | TypeConst::I16(_)
            | TypeConst::I24(_)
            | TypeConst::I32(_)
            | TypeConst::I64(_)
            | TypeConst::F32(_)
            | TypeConst::F64(_) => Ok(Kind::Binary.into()),
            TypeConst::Unit
            | TypeConst::Bottom
            | TypeConst::Bool
            | TypeConst::Float(_)
            | TypeConst::Signed(_)
            | TypeConst::Unsigned(_) => Ok(Kind::Host.into()),
        },

        // Arrow types
        Type::Arrow(ref param_tys, ref body_ty) => {
            for param_ty in param_tys {
                infer_kind(&ctx, param_ty)?;
            }

            infer_kind(&ctx, body_ty)
        }

        // Type abstraction
        Type::Lam(_, ref params, ref body_ty) => {
            // FIXME: avoid cloning the environment
            // FIXME: Do we want to invalidate hetrogeneous kind arguments and higher kinds?
            let mut ctx = ctx.clone();
            ctx.extend(Scope::TypeLam(params.clone()));
            let param_kinds = params.iter().map(|param| param.inner.clone()).collect();

            Ok(Kind::Arrow(param_kinds, infer_kind(&ctx, body_ty)?).into())
        }

        // Type application
        Type::App(_, ref fn_ty, ref arg_tys) => {
            let fn_kind = infer_kind(ctx, fn_ty)?;

            if let Kind::Arrow(ref param_kinds, ref ret_kind) = *fn_kind.inner {
                if arg_tys.len() == param_kinds.len() {
                    for (arg_ty, param_kind) in arg_tys.iter().zip(param_kinds) {
                        check_kind(ctx, arg_ty, param_kind)?;
                    }

                    return Ok(ret_kind.clone());
                } else {
                    unimplemented!(); // FIXME
                }
            }

            Err(KindError::NotATypeConstructor {
                fn_span: ByteSpan::default(), // TODO: fn_ty.span(),
                arg_spans: vec![],            // TODO: arg_tys.iter().map(|a| a.span()).collect(),
                found: fn_kind,
            })
        }

        // Array types
        Type::Array(_, ref elem_ty, ref size_expr) => {
            check_kind(ctx, elem_ty, &Kind::Binary.into())?;

            let size_ty = infer_ty(ctx, size_expr)?;
            match *size_ty.inner {
                Type::Const(TypeConst::Unsigned(_)) => Ok(Kind::Binary.into()),
                _ => Err(KindError::UnexpectedArraySizeType {
                    size_span: ByteSpan::default(), // TODO: size_expr.span(),
                    found: size_ty,
                }),
            }
        }

        // Conditional types
        Type::Assert(_, ref ty, ref pred_expr) => {
            check_kind(ctx, ty, &Kind::Binary.into())?;
            let pred_ty = Type::Arrow(vec![ty.repr()], Type::Const(TypeConst::Bool).into()).into();
            check_ty(ctx, pred_expr, &pred_ty)?;

            Ok(Kind::Binary.into())
        }

        // Interpreted types
        Type::Interp(_, ref ty, ref conv_expr, ref repr_ty) => {
            check_kind(ctx, ty, &Kind::Binary.into())?;
            let conv_ty = Type::Arrow(vec![ty.repr()], repr_ty.clone()).into();
            check_ty(ctx, conv_expr, &conv_ty)?;

            Ok(Kind::Binary.into())
        }

        // Conditional types
        Type::Cond(_, ref options) => {
            let bool_ty = Type::Const(TypeConst::Bool).into();

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
                ctx.extend(Scope::ExprLam(vec![
                    Named::new(Name::user(field.name.clone()), field_ty.repr()),
                ]));
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

        ctx.extend(Scope::TypeDef(vec![Named::new(name, (ty, kind))]));
    }

    Ok(())
}
