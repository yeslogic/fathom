//! Type and kind-checking for our DDL

use std::rc::Rc;

use name::{Name, Named};
use syntax::ast::{binary, host, Field, Program, Var};
use self::context::{Binding, Context};

mod context;
#[cfg(test)]
mod tests;

// Typing

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedType<N> {
    Array,
    Arrow,
    Actual(host::RcType<N>),
}

/// An error that was encountered during type checking
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError<N> {
    /// A variable of the requested name was not bound in this scope
    UnboundVariable { expr: host::RcExpr<N>, name: N },
    /// Variable bound in the context was not at the value level
    ExprBindingExpected {
        expr: host::RcExpr<N>,
        found: Named<N, Binding<N>>,
    },
    /// One type was expected, but another was found
    Mismatch {
        expr: host::RcExpr<N>,
        found: host::RcType<N>,
        expected: ExpectedType<N>,
    },
    /// Unexpected operand types in a equality comparison
    EqualityOperands {
        expr: host::RcExpr<N>,
        lhs_ty: host::RcType<N>,
        rhs_ty: host::RcType<N>,
    },
    /// A field was missing when projecting on a record
    MissingField {
        expr: host::RcExpr<N>,
        struct_ty: host::RcType<N>,
        field_name: N,
    },
    /// A variant was missing when introducing on a union
    MissingVariant {
        expr: host::RcExpr<N>,
        union_ty: host::RcType<N>,
        variant_name: N,
    },
}

/// Returns the type of a host expression, checking that it is properly formed
/// in the environment
pub fn ty_of<N: Name>(
    ctx: &Context<N>,
    expr: &host::RcExpr<N>,
) -> Result<host::RcType<N>, TypeError<N>> {
    use syntax::ast::host::{Binop, Expr, Type, TypeConst, Unop};

    match **expr {
        // Constants are easy!
        Expr::Const(_, c) => Ok(Rc::new(Type::Const(c.ty_const_of()))),

        // Variables
        Expr::Var(_, Var::Free(ref name)) => Err(TypeError::UnboundVariable {
            expr: expr.clone(),
            name: name.clone(),
        }),
        Expr::Var(_, Var::Bound(Named(_, i))) => match ctx.lookup_ty(i) {
            Ok(Named(_, ty)) => Ok(ty.clone()),
            Err(Named(name, binding)) => Err(TypeError::ExprBindingExpected {
                expr: expr.clone(),
                found: Named(name.clone(), binding.clone()),
            }),
        },

        // Primitive expressions
        Expr::Prim(_, ref repr_ty) => Ok(repr_ty.clone()),

        // Unary operators
        Expr::Unop(_, op, ref expr) => match op {
            Unop::Neg => {
                expect_ty(ctx, expr, Type::int())?;
                Ok(Rc::new(Type::int()))
            }
            Unop::Not => {
                expect_ty(ctx, expr, Type::bool())?;
                Ok(Rc::new(Type::bool()))
            }
        },

        // Binary operators
        Expr::Binop(_, op, ref lhs_expr, ref rhs_expr) => {
            match op {
                // Relational operators
                Binop::Or | Binop::And => {
                    expect_ty(ctx, lhs_expr, Type::bool())?;
                    expect_ty(ctx, rhs_expr, Type::bool())?;

                    Ok(Rc::new(Type::bool()))
                }

                // Equality operators
                Binop::Eq | Binop::Ne => {
                    let lhs_ty = ty_of(ctx, lhs_expr)?;
                    let rhs_ty = ty_of(ctx, rhs_expr)?;

                    match (&*lhs_ty, &*rhs_ty) {
                        (&Type::Const(TypeConst::U8), &Type::Const(TypeConst::U8)) |
                        (&Type::Const(TypeConst::Bool), &Type::Const(TypeConst::Bool)) |
                        (&Type::Const(TypeConst::Int), &Type::Const(TypeConst::Int)) => {
                            Ok(Rc::new(Type::bool()))
                        }
                        (lhs_ty, rhs_ty) => Err(TypeError::EqualityOperands {
                            expr: expr.clone(),
                            lhs_ty: Rc::new(lhs_ty.clone()),
                            rhs_ty: Rc::new(rhs_ty.clone()),
                        }),
                    }
                }

                // Comparison ops
                Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                    expect_ty(ctx, lhs_expr, Type::int())?;
                    expect_ty(ctx, rhs_expr, Type::int())?;

                    Ok(Rc::new(Type::bool()))
                }

                // Arithmetic operators
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
                    expect_ty(ctx, lhs_expr, Type::int())?;
                    expect_ty(ctx, rhs_expr, Type::int())?;

                    Ok(Rc::new(Type::int()))
                }
            }
        }

        // Struct expressions
        Expr::Struct(ref fields) => {
            let field_tys = fields
                .iter()
                .map(|field| {
                    Ok(Field::new(field.name.clone(), ty_of(ctx, &field.value)?))
                })
                .collect::<Result<_, _>>()?;

            Ok(Rc::new(Type::Struct(field_tys)))
        }

        // Field projection
        Expr::Proj(_, ref struct_expr, ref field_name) => {
            let struct_ty = ty_of(ctx, struct_expr)?;

            match struct_ty.lookup_field(field_name).cloned() {
                Some(field_ty) => Ok(field_ty),
                None => Err(TypeError::MissingField {
                    expr: struct_expr.clone(),
                    struct_ty: struct_ty.clone(),
                    field_name: field_name.clone(),
                }),
            }
        }

        // Variant introduction
        Expr::Intro(_, ref variant_name, ref expr, ref union_ty) => {
            // FIXME: Kindcheck union_ty
            match union_ty.lookup_variant(variant_name).cloned() {
                Some(variant_ty) => {
                    expect_ty(ctx, expr, variant_ty)?;
                    Ok(union_ty.clone())
                }
                None => Err(TypeError::MissingVariant {
                    expr: expr.clone(),
                    union_ty: union_ty.clone(),
                    variant_name: variant_name.clone(),
                }),
            }
        }

        // Array subscript
        Expr::Subscript(_, ref array_expr, ref index_expr) => {
            expect_ty(ctx, index_expr, Type::int())?;

            match *ty_of(ctx, array_expr)? {
                Type::Array(ref elem_ty) => Ok(elem_ty.clone()),
                ref found => Err(TypeError::Mismatch {
                    expr: array_expr.clone(),
                    expected: ExpectedType::Array,
                    found: Rc::new(found.clone()),
                }),
            }
        }

        // Abstraction
        Expr::Abs(_, Named(ref param_name, ref param_ty), ref body_expr) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();
            ctx.extend(param_name.clone(), Binding::Expr(param_ty.clone()));
            Ok(Rc::new(
                Type::arrow(param_ty.clone(), ty_of(&ctx, body_expr)?),
            ))
        }

        // Applications
        Expr::App(_, ref fn_expr, ref arg_expr) => {
            let fn_ty = ty_of(ctx, fn_expr)?;

            if let Type::Arrow(ref param_ty, ref ret_ty) = *fn_ty {
                expect_ty(ctx, arg_expr, param_ty.clone())?;
                return Ok(ret_ty.clone());
            }

            Err(TypeError::Mismatch {
                expr: fn_expr.clone(),
                expected: ExpectedType::Arrow,
                found: fn_ty,
            })
        }
    }
}

// Kinding

fn simplify_ty<N: Name>(ctx: &Context<N>, ty: &binary::RcType<N>) -> binary::RcType<N> {
    use syntax::ast::binary::Type;

    fn compute_ty<N: Name>(ctx: &Context<N>, ty: &binary::RcType<N>) -> Option<binary::RcType<N>> {
        match **ty {
            Type::Var(_, Var::Bound(Named(_, i))) => match ctx.lookup_ty_def(i) {
                Ok(Named(_, def_ty)) => Some(def_ty.clone()),
                Err(_) => None,
            },
            Type::App(_, ref fn_ty, ref arg_ty) => match **fn_ty {
                Type::Abs(_, _, ref body_ty) => {
                    // FIXME: Avoid clone
                    let mut body = body_ty.clone();
                    Rc::make_mut(&mut body).instantiate(arg_ty);
                    Some(body)
                }
                _ => None,
            },
            _ => None,
        }
    }

    let ty = match **ty {
        Type::App(_, ref fn_ty, _) => simplify_ty(ctx, fn_ty),
        // FIXME: Avoid clone
        _ => ty.clone(),
    };

    match compute_ty(ctx, &ty) {
        Some(ty) => simplify_ty(ctx, &ty),
        None => ty,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedKind {
    Arrow,
    Actual(binary::RcKind),
}

/// An error that was encountered during kind checking
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KindError<N> {
    /// A variable of the requested name was not bound in this scope
    UnboundVariable { ty: binary::RcType<N>, name: N },
    /// Variable bound in the context was not at the type level
    TypeBindingExpected {
        ty: binary::RcType<N>,
        found: Named<N, Binding<N>>,
    },
    /// One kind was expected, but another was found
    Mismatch {
        ty: binary::RcType<N>,
        expected: ExpectedKind,
        found: binary::RcKind,
    },
    /// A type error
    Type(TypeError<N>),
}

impl<N> From<TypeError<N>> for KindError<N> {
    fn from(src: TypeError<N>) -> KindError<N> {
        KindError::Type(src)
    }
}

/// Returns the kind of a binary type, checking that it is properly formed in
/// the environment
pub fn kind_of<N: Name>(
    ctx: &Context<N>,
    ty: &binary::RcType<N>,
) -> Result<binary::RcKind, KindError<N>> {
    use syntax::ast::binary::{Kind, Type, TypeConst};

    match **ty {
        // Variables
        Type::Var(_, Var::Free(ref name)) => Err(KindError::UnboundVariable {
            ty: ty.clone(),
            name: name.clone(),
        }),
        Type::Var(_, Var::Bound(Named(_, i))) => match ctx.lookup_kind(i) {
            Ok(Named(_, kind)) => Ok(kind.clone()),
            Err(Named(name, binding)) => Err(KindError::TypeBindingExpected {
                ty: ty.clone(),
                found: Named(name.clone(), binding.clone()),
            }),
        },

        // Byte type
        Type::Const(TypeConst::U8) => Ok(Rc::new(Kind::Type)),

        // Array types
        Type::Array(_, ref elem_ty, ref size_expr) => {
            expect_ty_kind(ctx, elem_ty)?;
            expect_ty(ctx, size_expr, host::Type::int())?;

            Ok(Rc::new(Kind::Type))
        }

        // Conditional types
        Type::Assert(_, ref ty, ref pred_expr) => {
            expect_ty_kind(ctx, ty)?;
            let pred_ty = host::Type::arrow(ty.repr(), host::Type::bool());
            expect_ty(ctx, pred_expr, pred_ty)?;

            Ok(Rc::new(Kind::Type))
        }

        // Interpreted types
        Type::Interp(_, ref ty, ref conv_expr, ref host_ty) => {
            expect_ty_kind(ctx, ty)?;
            let conv_ty = host::Type::arrow(ty.repr(), host_ty.clone());
            expect_ty(ctx, conv_expr, conv_ty)?;

            Ok(Rc::new(Kind::Type))
        }

        // Type abstraction
        Type::Abs(_, Named(ref name, ref param_kind), ref body_ty) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();
            ctx.extend(name.clone(), Binding::Type(param_kind.clone()));
            Ok(Rc::new(
                Kind::arrow(param_kind.clone(), kind_of(&ctx, body_ty)?),
            ))
        }

        // Union types
        Type::Union(_, ref fields) => {
            for field in fields {
                expect_ty_kind(ctx, &field.value)?;
            }

            Ok(Rc::new(Kind::Type))
        }

        // Struct type
        Type::Struct(_, ref fields) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();

            for field in fields {
                expect_ty_kind(&ctx, &field.value)?;

                let field_ty = simplify_ty(&ctx, &field.value);
                ctx.extend(field.name.clone(), Binding::Expr(field_ty.repr()));
            }

            Ok(Rc::new(Kind::Type))
        }

        // Type application
        Type::App(_, ref fn_ty, ref arg_ty) => match *kind_of(ctx, fn_ty)? {
            Kind::Type => Err(KindError::Mismatch {
                ty: fn_ty.clone(),
                found: Rc::new(Kind::Type),
                expected: ExpectedKind::Arrow,
            }),
            Kind::Arrow(ref param_kind, ref ret_kind) => {
                expect_kind(ctx, arg_ty, param_kind.clone())?;
                Ok(ret_kind.clone())
            }
        },
    }
}

pub fn check_program<N: Name>(program: &Program<N>) -> Result<(), KindError<N>> {
    let mut ctx = Context::new();

    for def in &program.defs {
        let def_kind = kind_of(&ctx, &def.ty)?;
        ctx.extend(def.name.clone(), Binding::TypeDef(def.ty.clone(), def_kind));
    }

    Ok(())
}

// Expectations

fn expect_ty<N: Name, T1>(
    ctx: &Context<N>,
    expr: &host::RcExpr<N>,
    expected: T1,
) -> Result<host::RcType<N>, TypeError<N>>
where
    T1: Into<host::RcType<N>>,
{
    let found = ty_of(ctx, expr)?;
    let expected = expected.into();

    if found == expected {
        Ok(found)
    } else {
        Err(TypeError::Mismatch {
            expr: expr.clone(),
            expected: ExpectedType::Actual(expected),
            found,
        })
    }
}

fn expect_kind<N: Name>(
    ctx: &Context<N>,
    ty: &binary::RcType<N>,
    expected: binary::RcKind,
) -> Result<binary::RcKind, KindError<N>> {
    let found = kind_of(ctx, ty)?;

    if found == expected {
        Ok(found)
    } else {
        Err(KindError::Mismatch {
            ty: ty.clone(),
            expected: ExpectedKind::Actual(expected),
            found,
        })
    }
}

fn expect_ty_kind<N: Name>(ctx: &Context<N>, ty: &binary::RcType<N>) -> Result<(), KindError<N>> {
    use syntax::ast::binary::Kind;

    expect_kind(ctx, ty, Rc::new(Kind::Type)).map(|_| ())
}
