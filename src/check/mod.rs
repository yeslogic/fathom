//! Type and kind-checking for our DDL

use syntax::{binary, host};
use syntax::{Binding, Ctx, Named, Var};

#[cfg(test)]
mod tests;

// Typing

/// An error that was encountered during type checking
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError<N, E> {
    FieldNotFound(N, host::Type<N, E>),
    TypeInExpressionPosition,
    UnboundVariable(N),
    NegOperand(host::Type<N, E>),
    NotOperand(host::Type<N, E>),
    RelOperands(host::Binop, host::Type<N, E>, host::Type<N, E>),
    CmpOperands(host::Binop, host::Type<N, E>, host::Type<N, E>),
    ArithOperands(host::Binop, host::Type<N, E>, host::Type<N, E>),
}

/// Returns the type of a host expression, checking that it is properly formed
/// in the environment
pub fn ty_of<N, T, E>(ctx: &Ctx<N, T, E>, expr: &E) -> Result<host::Type<N, E>, TypeError<N, E>>
where
    N: Clone + PartialEq,
    T: binary::TypeNode<N, E>,
    E: host::ExprNode<N>,
{
    use syntax::host::{Binop, Const, ExprF, Type, TypeConst, TypeF, Unop};

    match *expr.as_ref() {
        // Constants are easy!
        ExprF::Const(Const::Bit(_)) => Ok(Type(TypeF::Const(TypeConst::Bit))),
        ExprF::Const(Const::Bool(_)) => Ok(Type(TypeF::Const(TypeConst::Bool))),
        ExprF::Const(Const::Int(_)) => Ok(Type(TypeF::Const(TypeConst::Int))),

        // Variables
        ExprF::Var(Var::Free(ref x)) => Err(TypeError::UnboundVariable(x.clone())),
        ExprF::Var(Var::Bound(Named(_, i))) => match ctx.lookup_ty(i) {
            Some(ty) => Ok(ty.clone()),
            None => Err(TypeError::TypeInExpressionPosition),
        },

        // Unary operators
        ExprF::Unop(op, ref expr) => match op {
            Unop::Neg => match ty_of(ctx, &**expr)? {
                Type(TypeF::Const(TypeConst::Int)) => Ok(Type(TypeF::Const(TypeConst::Int))),
                ty1 => Err(TypeError::NegOperand(ty1)),
            },
            Unop::Not => match ty_of(ctx, &**expr)? {
                Type(TypeF::Const(TypeConst::Bool)) => Ok(Type(TypeF::Const(TypeConst::Bool))),
                ty1 => Err(TypeError::NotOperand(ty1)),
            },
        },

        // Binary operators
        ExprF::Binop(op, ref lhs_expr, ref rhs_expr) => {
            let lhs_ty = ty_of(ctx, &**lhs_expr)?;
            let ty2 = ty_of(ctx, &**rhs_expr)?;

            match op {
                // Relational operators
                Binop::Or | Binop::And => match (lhs_ty, ty2) {
                    (Type(TypeF::Const(TypeConst::Bool)), Type(TypeF::Const(TypeConst::Bool))) => {
                        Ok(Type(TypeF::Const(TypeConst::Bool)))
                    }
                    (lhs_ty, ty2) => Err(TypeError::RelOperands(op, lhs_ty, ty2)),
                },

                // Comparison operators
                Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => match (
                    lhs_ty,
                    ty2,
                ) {
                    (Type(TypeF::Const(TypeConst::Bit)), Type(TypeF::Const(TypeConst::Bit))) |
                    (Type(TypeF::Const(TypeConst::Bool)), Type(TypeF::Const(TypeConst::Bool))) |
                    (Type(TypeF::Const(TypeConst::Int)), Type(TypeF::Const(TypeConst::Int))) => {
                        Ok(Type(TypeF::Const(TypeConst::Bool)))
                    }
                    (lhs_ty, ty2) => Err(TypeError::CmpOperands(op, lhs_ty, ty2)),
                },

                // Arithmetic operators
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => match (lhs_ty, ty2) {
                    (Type(TypeF::Const(TypeConst::Int)), Type(TypeF::Const(TypeConst::Int))) => {
                        Ok(Type(TypeF::Const(TypeConst::Int)))
                    }
                    (lhs_ty, ty2) => Err(TypeError::ArithOperands(op, lhs_ty, ty2)),
                },
            }
        }

        // Field projection
        ExprF::Proj(ref expr, ref name) => {
            let expr_ty = ty_of(ctx, &**expr)?;

            match expr_ty.as_ref().lookup_field(name) {
                None => Err(TypeError::FieldNotFound(name.clone(), expr_ty.clone())),
                Some(fty) => Ok(fty.clone()),
            }
        }
    }
}

// Kinding

pub fn simplify_ty<N, T, E>(ctx: &Ctx<N, T, E>, ty: &T) -> T
where
    N: Clone + PartialEq,
    T: binary::TypeNode<N, E>,
    E: host::ExprNode<N>,
{
    use syntax::binary::TypeF;

    fn compute_ty<N, T, E>(ctx: &Ctx<N, T, E>, ty: &T) -> Option<T>
    where
        N: Clone + PartialEq,
        T: binary::TypeNode<N, E>,
        E: host::ExprNode<N>,
    {
        match *ty.as_ref() {
            TypeF::Var(Var::Bound(Named(_, i))) => ctx.lookup_ty_def(i).cloned(),
            TypeF::App(ref fn_ty, ref arg_ty) => match *T::as_ref(fn_ty) {
                TypeF::Abs(_, ref body_ty) => {
                    // FIXME: Avoid clone
                    let mut body = (**body_ty).clone();
                    T::as_mut(&mut body).instantiate(T::as_ref(arg_ty));
                    Some(body)
                }
                _ => None,
            },
            _ => None,
        }
    }

    let ty = match *ty.as_ref() {
        TypeF::App(ref fn_ty, _) => simplify_ty(ctx, &**fn_ty),
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
pub enum KindError<N, E> {
    ValueInExpressionPosition,
    ExpectedTypeKind,
    ExpectedArrowKind,
    ExpectedIntegerArraySize,
    ExpectedBooleanCondPredicate,
    MismatchedArguments(binary::Kind, binary::Kind),
    UnboundVariable(N),
    Type(TypeError<N, E>),
}

impl<N, E> From<TypeError<N, E>> for KindError<N, E> {
    fn from(src: TypeError<N, E>) -> KindError<N, E> {
        KindError::Type(src)
    }
}

/// Returns the kind of a binary type, checking that it is properly formed in
/// the environment
pub fn kind_of<N, T, E>(ctx: &Ctx<N, T, E>, ty: &T) -> Result<binary::Kind, KindError<N, E>>
where
    N: Clone + PartialEq,
    T: binary::TypeNode<N, E>,
    E: host::ExprNode<N>,
{
    use syntax::binary::{Kind, TypeConst, TypeF};

    match *ty.as_ref() {
        // Variables
        TypeF::Var(Var::Free(ref x)) => Err(KindError::UnboundVariable(x.clone())),
        TypeF::Var(Var::Bound(Named(_, i))) => match ctx.lookup_kind(i) {
            Some(kind) => Ok(kind.clone()),
            None => Err(KindError::ValueInExpressionPosition),
        },

        // Bit type
        TypeF::Const(TypeConst::Bit) => Ok(Kind::Type),

        // Array types
        TypeF::Array(ref elem_ty, ref size_expr) => {
            if kind_of(ctx, &**elem_ty)? != Kind::Type {
                return Err(KindError::ExpectedTypeKind);
            }

            match ty_of(ctx, &**size_expr)? {
                host::Type(host::TypeF::Const(host::TypeConst::Int)) => Ok(Kind::Type),
                _ => Err(KindError::ExpectedIntegerArraySize),
            }
        }

        // Conditional types
        TypeF::Cond(Named(_, ref ty), ref pred_expr) => {
            if kind_of(ctx, &**ty)? != Kind::Type {
                return Err(KindError::ExpectedTypeKind);
            }

            match ty_of(ctx, &**pred_expr)? {
                host::Type(host::TypeF::Const(host::TypeConst::Bool)) => Ok(Kind::Type),
                _ => Err(KindError::ExpectedBooleanCondPredicate),
            }
        }

        // Type abstraction
        TypeF::Abs(Named(_, ref param_kind), ref body_ty) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();
            ctx.extend(Binding::Type(param_kind.clone()));
            Ok(Kind::arrow(param_kind.clone(), kind_of(&ctx, &**body_ty)?))
        }

        // Union types
        TypeF::Union(ref tys) => {
            for ty in tys {
                if kind_of(ctx, ty)? != Kind::Type {
                    return Err(KindError::ExpectedTypeKind);
                }
            }

            Ok(Kind::Type)
        }

        // Struct type
        TypeF::Struct(ref fields) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();

            for field in fields {
                if kind_of(&ctx, &field.value)? != Kind::Type {
                    return Err(KindError::ExpectedTypeKind);
                }

                let field_ty = simplify_ty(&ctx, &field.value);
                let repr_ty = T::as_ref(&field_ty).repr().unwrap(); // FIXME: unwrap
                ctx.extend(Binding::Expr(repr_ty));
            }

            Ok(Kind::Type)
        }

        // Type application
        TypeF::App(ref fn_ty, ref arg_ty) => {
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
