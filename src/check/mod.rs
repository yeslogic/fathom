//! Type and kind-checking for our DDL

use syntax::{binary, host};
use syntax::{Binding, Ctx, Named, Var};

#[cfg(test)]
mod tests;

// Typing

/// An error that was encountered during type checking
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError<N> {
    FieldNotFound(N, host::Type<N>),
    TypeInExpressionPosition,
    UnboundVariable(N),
    NegOperand(host::Type<N>),
    NotOperand(host::Type<N>),
    RelOperands(host::Binop, host::Type<N>, host::Type<N>),
    CmpOperands(host::Binop, host::Type<N>, host::Type<N>),
    ArithOperands(host::Binop, host::Type<N>, host::Type<N>),
}

/// Returns the type of a host expression, checking that it is properly formed
/// in the environment
pub fn ty_of<N>(ctx: &Ctx<N>, expr: &host::Expr<N>) -> Result<host::Type<N>, TypeError<N>>
where
    N: Clone + PartialEq,
{
    use syntax::host::{Binop, Const, Expr, Type, TypeConst, Unop};

    match *expr {
        // Constants are easy!
        Expr::Const(Const::Bit(_)) => Ok(Type::Const(TypeConst::Bit)),
        Expr::Const(Const::Bool(_)) => Ok(Type::Const(TypeConst::Bool)),
        Expr::Const(Const::Int(_)) => Ok(Type::Const(TypeConst::Int)),

        // Variables
        Expr::Var(Var::Free(ref x)) => Err(TypeError::UnboundVariable(x.clone())),
        Expr::Var(Var::Bound(Named(_, i))) => match ctx.lookup_ty(i) {
            Some(ty) => Ok(ty.clone()),
            None => Err(TypeError::TypeInExpressionPosition),
        },

        // Unary operators
        Expr::Unop(op, ref expr) => match op {
            Unop::Neg => match ty_of(ctx, &**expr)? {
                Type::Const(TypeConst::Int) => Ok(Type::Const(TypeConst::Int)),
                ty1 => Err(TypeError::NegOperand(ty1)),
            },
            Unop::Not => match ty_of(ctx, &**expr)? {
                Type::Const(TypeConst::Bool) => Ok(Type::Const(TypeConst::Bool)),
                ty1 => Err(TypeError::NotOperand(ty1)),
            },
        },

        // Binary operators
        Expr::Binop(op, ref lhs_expr, ref rhs_expr) => {
            let lhs_ty = ty_of(ctx, &**lhs_expr)?;
            let ty2 = ty_of(ctx, &**rhs_expr)?;

            match op {
                // Relational operators
                Binop::Or | Binop::And => match (lhs_ty, ty2) {
                    (Type::Const(TypeConst::Bool), Type::Const(TypeConst::Bool)) => {
                        Ok(Type::Const(TypeConst::Bool))
                    }
                    (lhs_ty, ty2) => Err(TypeError::RelOperands(op, lhs_ty, ty2)),
                },

                // Comparison operators
                Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                    match (lhs_ty, ty2) {
                        (Type::Const(TypeConst::Bit), Type::Const(TypeConst::Bit)) |
                        (Type::Const(TypeConst::Bool), Type::Const(TypeConst::Bool)) |
                        (Type::Const(TypeConst::Int), Type::Const(TypeConst::Int)) => {
                            Ok(Type::Const(TypeConst::Bool))
                        }
                        (lhs_ty, ty2) => Err(TypeError::CmpOperands(op, lhs_ty, ty2)),
                    }
                }

                // Arithmetic operators
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => match (lhs_ty, ty2) {
                    (Type::Const(TypeConst::Int), Type::Const(TypeConst::Int)) => {
                        Ok(Type::Const(TypeConst::Int))
                    }
                    (lhs_ty, ty2) => Err(TypeError::ArithOperands(op, lhs_ty, ty2)),
                },
            }
        }

        // Field projection
        Expr::Proj(ref expr, ref name) => {
            let expr_ty = ty_of(ctx, &**expr)?;

            match expr_ty.lookup_field(name) {
                None => Err(TypeError::FieldNotFound(name.clone(), expr_ty.clone())),
                Some(fty) => Ok(fty.clone()),
            }
        }
    }
}

// Kinding

pub fn simplify_ty<N>(ctx: &Ctx<N>, ty: &binary::Type<N>) -> binary::Type<N>
where
    N: Clone + PartialEq,
{
    use syntax::binary::Type;

    fn compute_ty<N>(ctx: &Ctx<N>, ty: &binary::Type<N>) -> Option<binary::Type<N>>
    where
        N: Clone + PartialEq,
    {
        match *ty {
            Type::Var(Var::Bound(Named(_, i))) => ctx.lookup_ty_def(i).cloned(),
            Type::App(ref fn_ty, ref arg_ty) => match **fn_ty {
                Type::Abs(_, ref body_ty) => {
                    // FIXME: Avoid clone
                    let mut body = (**body_ty).clone();
                    body.instantiate(arg_ty);
                    Some(body)
                }
                _ => None,
            },
            _ => None,
        }
    }

    let ty = match *ty {
        Type::App(ref fn_ty, _) => simplify_ty(ctx, &**fn_ty),
        // FIXME: Avoid clone
        _ => ty.clone(),
    };

    match compute_ty(ctx, &ty) {
        Some(ty) => simplify_ty(ctx, &ty),
        None => ty,
    }
}

/// An error that was encountered during kind checking
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KindError<N> {
    ValueInExpressionPosition,
    ExpectedTypeKind,
    ExpectedArrowKind,
    ExpectedIntegerArraySize,
    ExpectedBooleanCondPredicate,
    MismatchedArguments(binary::Kind, binary::Kind),
    UnboundVariable(N),
    Type(TypeError<N>),
}

impl<N> From<TypeError<N>> for KindError<N> {
    fn from(src: TypeError<N>) -> KindError<N> {
        KindError::Type(src)
    }
}

/// Returns the kind of a binary type, checking that it is properly formed in
/// the environment
pub fn kind_of<N>(ctx: &Ctx<N>, ty: &binary::Type<N>) -> Result<binary::Kind, KindError<N>>
where
    N: Clone + PartialEq,
{
    use syntax::binary::{Kind, Type, TypeConst};

    match *ty {
        // Variables
        Type::Var(Var::Free(ref x)) => Err(KindError::UnboundVariable(x.clone())),
        Type::Var(Var::Bound(Named(_, i))) => match ctx.lookup_kind(i) {
            Some(kind) => Ok(kind.clone()),
            None => Err(KindError::ValueInExpressionPosition),
        },

        // Bit type
        Type::Const(TypeConst::Bit) => Ok(Kind::Type),

        // Array types
        Type::Array(ref elem_ty, ref size_expr) => {
            if kind_of(ctx, &**elem_ty)? != Kind::Type {
                return Err(KindError::ExpectedTypeKind);
            }

            match ty_of(ctx, &**size_expr)? {
                host::Type::Const(host::TypeConst::Int) => Ok(Kind::Type),
                _ => Err(KindError::ExpectedIntegerArraySize),
            }
        }

        // Conditional types
        Type::Cond(Named(_, ref ty), ref pred_expr) => {
            if kind_of(ctx, &**ty)? != Kind::Type {
                return Err(KindError::ExpectedTypeKind);
            }

            match ty_of(ctx, &**pred_expr)? {
                host::Type::Const(host::TypeConst::Bool) => Ok(Kind::Type),
                _ => Err(KindError::ExpectedBooleanCondPredicate),
            }
        }

        // Type abstraction
        Type::Abs(Named(_, ref param_kind), ref body_ty) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();
            ctx.extend(Binding::Type(param_kind.clone()));
            Ok(Kind::arrow(param_kind.clone(), kind_of(&ctx, &**body_ty)?))
        }

        // Union types
        Type::Union(ref tys) => {
            for ty in tys {
                if kind_of(ctx, ty)? != Kind::Type {
                    return Err(KindError::ExpectedTypeKind);
                }
            }

            Ok(Kind::Type)
        }

        // Struct type
        Type::Struct(ref fields) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();

            for field in fields {
                if kind_of(&ctx, &field.value)? != Kind::Type {
                    return Err(KindError::ExpectedTypeKind);
                }

                let field_ty = simplify_ty(&ctx, &field.value);
                let repr_ty = field_ty.repr().unwrap(); // FIXME: unwrap
                ctx.extend(Binding::Expr(repr_ty));
            }

            Ok(Kind::Type)
        }

        // Type application
        Type::App(ref fn_ty, ref arg_ty) => {
            let fn_kind = kind_of(ctx, &**fn_ty)?;
            let arg_kind = kind_of(ctx, &**arg_ty)?;

            match fn_kind {
                Kind::Type => Err(KindError::ExpectedArrowKind),
                Kind::Arrow(param_kind, ret_kind) => if *param_kind == arg_kind {
                    Ok(*ret_kind)
                } else {
                    Err(KindError::MismatchedArguments(
                        (*param_kind).clone(),
                        arg_kind.clone(),
                    ))
                },
            }
        }
    }
}
