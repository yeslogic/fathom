//! Type and kind-checking for our DDL

use std::rc::Rc;

use name::Named;
use syntax::ast::{binary, host, Field, Program};
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
    Actual(host::RcType),
}

/// An error that was encountered during type checking
#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    /// A variable of the requested name was not bound in this scope
    UnboundVariable { expr: host::RcExpr, name: String },
    /// Variable bound in the context was not at the value level
    ExprBindingExpected { expr: host::RcExpr, found: Scope },
    /// One type was expected, but another was found
    Mismatch {
        expr: host::RcExpr,
        found: host::RcType,
        expected: ExpectedType,
    },
    /// Unexpected operand types in a binary operator expression
    BinaryOperands {
        context: host::Binop,
        expr: host::RcExpr,
        lhs_ty: host::RcType,
        rhs_ty: host::RcType,
    },
    /// A field was missing when projecting on a record
    MissingField {
        expr: host::RcExpr,
        struct_ty: host::RcType,
        field_name: String,
    },
    /// A variant was missing when introducing on a union
    MissingVariant {
        expr: host::RcExpr,
        union_ty: host::RcType,
        variant_name: String,
    },
    /// An invalid type was supplied to the cast expression
    InvalidCastType {
        expr: host::RcExpr,
        found: host::RcType,
    },
}

/// Returns the type of a host expression, checking that it is properly formed
/// in the environment
pub fn ty_of(ctx: &Context, expr: &host::RcExpr) -> Result<host::RcType, TypeError> {
    use syntax::ast::host::{Binop, Expr, Type, TypeConst, Unop};

    match *expr.inner {
        // Constants are easy!
        Expr::Const(_, c) => Ok(Type::Const(c.ty_const_of()).into()),

        // Variables
        Expr::Var(_, Var::Free(ref name)) => Err(TypeError::UnboundVariable {
            expr: expr.clone(),
            name: name.clone(),
        }),
        Expr::Var(_, Var::Bound(Named(_, i))) => match ctx.lookup_ty(i) {
            Ok((_, ty)) => Ok(ty.clone()),
            Err(scope) => Err(TypeError::ExprBindingExpected {
                expr: expr.clone(),
                found: scope.clone(),
            }),
        },

        // Unary operators
        Expr::Unop(_, op, ref operand_expr) => {
            use syntax::ast::host::Type::Const;

            let operand_ty = ty_of(ctx, operand_expr)?;

            match (op, &*operand_ty.inner) {
                (Unop::Neg, &Const(TypeConst::Signed(_)))
                | (Unop::Neg, &Const(TypeConst::Float(_))) => Ok(operand_ty),
                (Unop::Neg, _) => Err(TypeError::Mismatch {
                    expr: expr.clone(),
                    expected: ExpectedType::Signed,
                    found: operand_ty,
                }),
                (Unop::Not, &Const(TypeConst::Bool)) => Ok(operand_ty),
                (Unop::Not, _) => Err(TypeError::Mismatch {
                    expr: expr.clone(),
                    expected: ExpectedType::Actual(Type::Const(TypeConst::Bool).into()),
                    found: operand_ty,
                }),
            }
        }

        // Binary operators
        Expr::Binop(_, op, ref lhs_expr, ref rhs_expr) => {
            use syntax::ast::host::Type::Const;

            fn binop_err(
                context: Binop,
                expr: &host::RcExpr,
                lhs_ty: host::RcType,
                rhs_ty: host::RcType,
            ) -> TypeError {
                TypeError::BinaryOperands {
                    context,
                    expr: expr.clone(),
                    lhs_ty,
                    rhs_ty,
                }
            }

            let lhs_ty = ty_of(ctx, lhs_expr)?;
            let rhs_ty = ty_of(ctx, rhs_expr)?;

            match (&*lhs_ty.inner, &*rhs_ty.inner) {
                (&Const(TypeConst::Bool), &Const(TypeConst::Bool)) => match op {
                    Binop::Or | Binop::And | Binop::Eq | Binop::Ne => Ok(lhs_ty),
                    _ => Err(binop_err(op, expr, lhs_ty, rhs_ty)),
                },
                (&Const(TypeConst::Float(l)), &Const(TypeConst::Float(r))) if l == r => match op {
                    Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                        Ok(Const(TypeConst::Bool).into())
                    }
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => Ok(lhs_ty),
                    _ => Err(binop_err(op, expr, lhs_ty, rhs_ty)),
                },
                (&Const(TypeConst::Signed(l)), &Const(TypeConst::Signed(r))) if l == r => {
                    match op {
                        Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                            Ok(Const(TypeConst::Bool).into())
                        }
                        Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => Ok(lhs_ty),
                        _ => Err(binop_err(op, expr, lhs_ty, rhs_ty)),
                    }
                }
                (&Const(TypeConst::Unsigned(l)), &Const(TypeConst::Unsigned(r))) if l == r => {
                    match op {
                        Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                            Ok(Const(TypeConst::Bool).into())
                        }
                        Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => Ok(lhs_ty),
                        _ => Err(binop_err(op, expr, lhs_ty, rhs_ty)),
                    }
                }
                (_, _) => Err(binop_err(op, expr, lhs_ty, rhs_ty)),
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
                        value: ty_of(ctx, &field.value)?,
                    })
                })
                .collect::<Result<_, _>>()?;

            Ok(Type::Struct(field_tys).into())
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
                    expect_ty(ctx, expr, &variant_ty)?;
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
            let index_ty = ty_of(ctx, index_expr)?;
            match *index_ty.inner {
                Type::Const(TypeConst::Unsigned(_)) => {}
                _ => {
                    return Err(TypeError::Mismatch {
                        expr: index_expr.clone(),
                        expected: ExpectedType::Unsigned,
                        found: index_ty,
                    })
                }
            }

            let array_ty = ty_of(ctx, array_expr)?;
            match *array_ty.inner {
                Type::Array(ref elem_ty) => Ok(elem_ty.clone()),
                _ => Err(TypeError::Mismatch {
                    expr: array_expr.clone(),
                    expected: ExpectedType::Array,
                    found: array_ty.clone(),
                }),
            }
        }

        // Cast Expressions
        Expr::Cast(_, ref src_expr, ref dst_ty) => {
            let src_ty = ty_of(ctx, src_expr)?;

            match *dst_ty.inner {
                Type::Const(TypeConst::Float(_))
                | Type::Const(TypeConst::Signed(_))
                | Type::Const(TypeConst::Unsigned(_)) => match *src_ty.inner {
                    Type::Const(TypeConst::Float(_))
                    | Type::Const(TypeConst::Signed(_))
                    | Type::Const(TypeConst::Unsigned(_)) => Ok(dst_ty.clone()),
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

        // Abstraction
        Expr::Lam(_, ref params, ref body_expr) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();
            ctx.extend(Scope::ExprLam(params.clone()));
            let param_tys = params.iter().map(|param| param.1.clone()).collect();

            Ok(Type::Arrow(param_tys, ty_of(&ctx, body_expr)?).into())
        }

        // Applications
        Expr::App(_, ref fn_expr, ref arg_exprs) => {
            let fn_ty = ty_of(ctx, fn_expr)?;

            if let Type::Arrow(ref param_tys, ref ret_ty) = *fn_ty.inner {
                if arg_exprs.len() == param_tys.len() {
                    for (arg_expr, param_ty) in arg_exprs.iter().zip(param_tys) {
                        expect_ty(ctx, arg_expr, param_ty)?;
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
    }
}

// Kinding

fn simplify_ty(ctx: &Context, ty: &binary::RcType) -> binary::RcType {
    use syntax::ast::binary::Type;

    fn compute_ty(ctx: &Context, ty: &binary::RcType) -> Option<binary::RcType> {
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
    UnboundVariable { ty: binary::RcType, name: String },
    /// Variable bound in the context was not at the type level
    TypeBindingExpected { ty: binary::RcType, found: Scope },
    /// One kind was expected, but another was found
    Mismatch {
        ty: binary::RcType,
        expected: binary::Kind,
        found: binary::Kind,
    },
    /// A type error
    Type(TypeError),
}

impl From<TypeError> for KindError {
    fn from(src: TypeError) -> KindError {
        KindError::Type(src)
    }
}

/// Returns the kind of a binary type, checking that it is properly formed in
/// the environment
pub fn kind_of(ctx: &Context, ty: &binary::RcType) -> Result<binary::Kind, KindError> {
    use syntax::ast::binary::{Kind, Type, TypeConst};

    match *ty.inner {
        // Variables
        Type::Var(_, Var::Free(ref name)) => Err(KindError::UnboundVariable {
            ty: ty.clone(),
            name: name.clone(),
        }),
        Type::Var(_, Var::Bound(Named(_, i))) => match ctx.lookup_kind(i) {
            Ok((_, kind)) => Ok(*kind),
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
        | Type::Const(TypeConst::F64(_)) => Ok(Kind::Type),

        // Array types
        Type::Array(_, ref elem_ty, ref size_expr) => {
            expect_ty_kind(ctx, elem_ty)?;

            let size_ty = ty_of(ctx, size_expr)?;
            match *size_ty.inner {
                host::Type::Const(host::TypeConst::Unsigned(_)) => Ok(Kind::Type),
                _ => Err(TypeError::Mismatch {
                    expr: size_expr.clone(),
                    expected: ExpectedType::Signed,
                    found: size_ty,
                }.into()),
            }
        }

        // Conditional types
        Type::Assert(_, ref ty, ref pred_expr) => {
            expect_ty_kind(ctx, ty)?;
            let pred_ty = host::Type::Arrow(
                vec![ty.repr()],
                host::Type::Const(host::TypeConst::Bool).into(),
            ).into();
            expect_ty(ctx, pred_expr, &pred_ty)?;

            Ok(Kind::Type)
        }

        // Interpreted types
        Type::Interp(_, ref ty, ref conv_expr, ref repr_ty) => {
            expect_ty_kind(ctx, ty)?;
            let conv_ty = host::Type::Arrow(vec![ty.repr()], repr_ty.clone()).into();
            expect_ty(ctx, conv_expr, &conv_ty)?;

            Ok(Kind::Type)
        }

        // Type abstraction
        Type::Lam(_, ref param_tys, ref body_ty) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();

            ctx.extend(Scope::TypeLam(
                param_tys
                    .iter()
                    .map(|named| Named(named.0.clone(), Kind::Type))
                    .collect(),
            ));
            expect_ty_kind(&ctx, body_ty)?;

            Ok(Kind::arrow(param_tys.len() as u32))
        }

        // Conditional types
        Type::Cond(_, ref options) => {
            let bool_ty = host::Type::Const(host::TypeConst::Bool).into();

            for option in options {
                expect_ty(ctx, &option.value.0, &bool_ty)?;
                expect_ty_kind(ctx, &option.value.1)?;
            }

            Ok(Kind::Type)
        }

        // Struct type
        Type::Struct(_, ref fields) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();

            for field in fields {
                expect_ty_kind(&ctx, &field.value)?;

                let field_ty = simplify_ty(&ctx, &field.value);
                ctx.extend(Scope::ExprLam(vec![
                    Named(field.name.clone(), field_ty.repr()),
                ]));
            }

            Ok(Kind::Type)
        }

        // Type application
        Type::App(_, ref fn_ty, ref arg_tys) => {
            expect_kind(ctx, fn_ty, Kind::arrow(arg_tys.len() as u32))?;

            for arg_ty in arg_tys {
                expect_ty_kind(ctx, arg_ty)?
            }

            Ok(Kind::Type)
        }
    }
}

pub fn check_program(program: &Program) -> Result<(), KindError> {
    let mut ctx = Context::new();

    for definition in &program.definitions {
        let definition_kind = kind_of(&ctx, &definition.body_ty)?;
        ctx.extend(Scope::TypeDef(vec![
            Named(
                definition.name.clone(),
                (definition.body_ty.clone(), definition_kind),
            ),
        ]));
    }

    Ok(())
}

// Expectations

fn expect_ty(
    ctx: &Context,
    expr: &host::RcExpr,
    expected: &host::RcType,
) -> Result<host::RcType, TypeError> {
    let found = ty_of(ctx, expr)?;

    if &found == expected {
        Ok(found)
    } else {
        Err(TypeError::Mismatch {
            expr: expr.clone(),
            expected: ExpectedType::Actual(expected.clone()),
            found,
        })
    }
}

fn expect_kind(
    ctx: &Context,
    ty: &binary::RcType,
    expected: binary::Kind,
) -> Result<binary::Kind, KindError> {
    let found = kind_of(ctx, ty)?;

    if found == expected {
        Ok(found)
    } else {
        Err(KindError::Mismatch {
            ty: ty.clone(),
            expected: expected,
            found,
        })
    }
}

fn expect_ty_kind(ctx: &Context, ty: &binary::RcType) -> Result<(), KindError> {
    use syntax::ast::binary::Kind;

    expect_kind(ctx, ty, Kind::Type).map(|_| ())
}
