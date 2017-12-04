//! Type and kind-checking for our DDL

use std::rc::Rc;

use name::{Name, Named};
use syntax::ast::{binary, host, Field, Program};
use self::context::{Context, Scope};
use var::Var;

mod context;
#[cfg(test)]
mod tests;

// Typing

#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedType<N> {
    Array,
    Arrow,
    Unsigned,
    Signed,
    Numeric,
    Actual(host::RcType<N>),
}

/// An error that was encountered during type checking
#[derive(Debug, Clone, PartialEq)]
pub enum TypeError<N> {
    /// A variable of the requested name was not bound in this scope
    UnboundVariable { expr: host::RcExpr<N>, name: N },
    /// Variable bound in the context was not at the value level
    ExprBindingExpected {
        expr: host::RcExpr<N>,
        found: Scope<N>,
    },
    /// One type was expected, but another was found
    Mismatch {
        expr: host::RcExpr<N>,
        found: host::RcType<N>,
        expected: ExpectedType<N>,
    },
    /// Unexpected operand types in a binary operator expression
    BinaryOperands {
        context: host::Binop,
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
    /// An invalid type was supplied to the cast expression
    InvalidCastType {
        expr: host::RcExpr<N>,
        found: host::RcType<N>,
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
            expr: Rc::clone(expr),
            name: name.clone(),
        }),
        Expr::Var(_, Var::Bound(Named(_, i))) => match ctx.lookup_ty(i) {
            Ok((_, ty)) => Ok(Rc::clone(ty)),
            Err(scope) => Err(TypeError::ExprBindingExpected {
                expr: Rc::clone(expr),
                found: scope.clone(),
            }),
        },

        // Primitive expressions
        Expr::Prim(_, ref repr_ty) => Ok(Rc::clone(repr_ty)),

        // Unary operators
        Expr::Unop(_, op, ref operand_expr) => {
            use syntax::ast::host::Type::Const;

            let operand_ty = ty_of(ctx, operand_expr)?;

            match (op, &*operand_ty) {
                (Unop::Neg, &Const(TypeConst::Signed(_))) |
                (Unop::Neg, &Const(TypeConst::Float(_))) => Ok(operand_ty),
                (Unop::Neg, _) => Err(TypeError::Mismatch {
                    expr: Rc::clone(expr),
                    expected: ExpectedType::Signed,
                    found: operand_ty,
                }),
                (Unop::Not, &Const(TypeConst::Bool)) => Ok(operand_ty),
                (Unop::Not, _) => Err(TypeError::Mismatch {
                    expr: Rc::clone(expr),
                    expected: ExpectedType::Actual(Rc::new(Type::Const(TypeConst::Bool))),
                    found: operand_ty,
                }),
            }
        }

        // Binary operators
        Expr::Binop(_, op, ref lhs_expr, ref rhs_expr) => {
            use syntax::ast::host::Type::Const;

            fn binop_err<N: Name>(
                context: Binop,
                expr: &host::RcExpr<N>,
                lhs_ty: host::RcType<N>,
                rhs_ty: host::RcType<N>,
            ) -> TypeError<N> {
                TypeError::BinaryOperands {
                    context,
                    expr: Rc::clone(expr),
                    lhs_ty,
                    rhs_ty,
                }
            }

            let lhs_ty = ty_of(ctx, lhs_expr)?;
            let rhs_ty = ty_of(ctx, rhs_expr)?;

            match (&*lhs_ty, &*rhs_ty) {
                (&Const(TypeConst::Bool), &Const(TypeConst::Bool)) => match op {
                    Binop::Or | Binop::And | Binop::Eq | Binop::Ne => Ok(lhs_ty),
                    _ => Err(binop_err(op, expr, lhs_ty, rhs_ty)),
                },
                (&Const(TypeConst::Float(l)), &Const(TypeConst::Float(r))) if l == r => match op {
                    Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                        Ok(Rc::new(Const(TypeConst::Bool)))
                    }
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => Ok(lhs_ty),
                    _ => Err(binop_err(op, expr, lhs_ty, rhs_ty)),
                },
                (&Const(TypeConst::Signed(l)), &Const(TypeConst::Signed(r))) if l == r => {
                    match op {
                        Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                            Ok(Rc::new(Const(TypeConst::Bool)))
                        }
                        Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => Ok(lhs_ty),
                        _ => Err(binop_err(op, expr, lhs_ty, rhs_ty)),
                    }
                }
                (&Const(TypeConst::Unsigned(l)), &Const(TypeConst::Unsigned(r))) if l == r => {
                    match op {
                        Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                            Ok(Rc::new(Const(TypeConst::Bool)))
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

            Ok(Rc::new(Type::Struct(field_tys)))
        }

        // Field projection
        Expr::Proj(_, ref struct_expr, ref field_name) => {
            let struct_ty = ty_of(ctx, struct_expr)?;

            match struct_ty.lookup_field(field_name).cloned() {
                Some(field_ty) => Ok(field_ty),
                None => Err(TypeError::MissingField {
                    expr: Rc::clone(struct_expr),
                    struct_ty: Rc::clone(&struct_ty),
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
                    Ok(Rc::clone(union_ty))
                }
                None => Err(TypeError::MissingVariant {
                    expr: Rc::clone(expr),
                    union_ty: Rc::clone(union_ty),
                    variant_name: variant_name.clone(),
                }),
            }
        }

        // Array subscript
        Expr::Subscript(_, ref array_expr, ref index_expr) => {
            let index_ty = ty_of(ctx, index_expr)?;
            match *index_ty {
                Type::Const(TypeConst::Unsigned(_)) => {}
                _ => {
                    return Err(TypeError::Mismatch {
                        expr: Rc::clone(index_expr),
                        expected: ExpectedType::Unsigned,
                        found: index_ty,
                    })
                }
            }

            let array_ty = ty_of(ctx, array_expr)?;
            match *array_ty {
                Type::Array(ref elem_ty) => Ok(Rc::clone(elem_ty)),
                _ => Err(TypeError::Mismatch {
                    expr: Rc::clone(array_expr),
                    expected: ExpectedType::Array,
                    found: Rc::clone(&array_ty),
                }),
            }
        }

        // Cast Expressions
        Expr::Cast(_, ref src_expr, ref dst_ty) => {
            let src_ty = ty_of(ctx, src_expr)?;

            match **dst_ty {
                Type::Const(TypeConst::Float(_)) |
                Type::Const(TypeConst::Signed(_)) |
                Type::Const(TypeConst::Unsigned(_)) => match *src_ty {
                    Type::Const(TypeConst::Float(_)) |
                    Type::Const(TypeConst::Signed(_)) |
                    Type::Const(TypeConst::Unsigned(_)) => Ok(Rc::clone(dst_ty)),
                    _ => Err(TypeError::Mismatch {
                        expr: Rc::clone(src_expr),
                        expected: ExpectedType::Numeric,
                        found: Rc::clone(&src_ty),
                    }),
                },
                _ => Err(TypeError::InvalidCastType {
                    expr: Rc::clone(expr),
                    found: Rc::clone(dst_ty),
                }),
            }
        }

        // Abstraction
        Expr::Abs(_, ref params, ref body_expr) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();
            ctx.extend(Scope::ExprAbs(params.clone()));
            let param_tys = params.iter().map(|param| Rc::clone(&param.1)).collect();

            Ok(Rc::new(Type::Arrow(param_tys, ty_of(&ctx, body_expr)?)))
        }

        // Applications
        Expr::App(_, ref fn_expr, ref arg_exprs) => {
            let fn_ty = ty_of(ctx, fn_expr)?;

            if let Type::Arrow(ref param_tys, ref ret_ty) = *fn_ty {
                if arg_exprs.len() == param_tys.len() {
                    for (arg_expr, param_ty) in arg_exprs.iter().zip(param_tys) {
                        expect_ty(ctx, arg_expr, param_ty)?;
                    }

                    return Ok(Rc::clone(ret_ty));
                } else {
                    unimplemented!(); // FIXME
                }
            }

            Err(TypeError::Mismatch {
                expr: Rc::clone(fn_expr),
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
                Ok((_, def_ty)) => Some(Rc::clone(def_ty)),
                Err(_) => None,
            },
            Type::App(_, ref fn_ty, ref arg_tys) => match **fn_ty {
                Type::Abs(_, _, ref body_ty) => {
                    // FIXME: Avoid clone
                    let mut body = Rc::clone(body_ty);
                    Rc::make_mut(&mut body).instantiate(arg_tys);
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
        _ => Rc::clone(ty),
    };

    match compute_ty(ctx, &ty) {
        Some(ty) => simplify_ty(ctx, &ty),
        None => ty,
    }
}

/// An error that was encountered during kind checking
#[derive(Debug, Clone, PartialEq)]
pub enum KindError<N> {
    /// A variable of the requested name was not bound in this scope
    UnboundVariable { ty: binary::RcType<N>, name: N },
    /// Variable bound in the context was not at the type level
    TypeBindingExpected {
        ty: binary::RcType<N>,
        found: Scope<N>,
    },
    /// One kind was expected, but another was found
    Mismatch {
        ty: binary::RcType<N>,
        expected: binary::Kind,
        found: binary::Kind,
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
) -> Result<binary::Kind, KindError<N>> {
    use syntax::ast::binary::{Kind, Type, TypeConst};

    match **ty {
        // Variables
        Type::Var(_, Var::Free(ref name)) => Err(KindError::UnboundVariable {
            ty: Rc::clone(ty),
            name: name.clone(),
        }),
        Type::Var(_, Var::Bound(Named(_, i))) => match ctx.lookup_kind(i) {
            Ok((_, kind)) => Ok(*kind),
            Err(scope) => Err(KindError::TypeBindingExpected {
                ty: Rc::clone(ty),
                found: scope.clone(),
            }),
        },

        // Type constants
        Type::Const(TypeConst::Empty) |
        Type::Const(TypeConst::Error) |
        Type::Const(TypeConst::U8) |
        Type::Const(TypeConst::I8) |
        Type::Const(TypeConst::U16(_)) |
        Type::Const(TypeConst::U24(_)) |
        Type::Const(TypeConst::U32(_)) |
        Type::Const(TypeConst::U64(_)) |
        Type::Const(TypeConst::I16(_)) |
        Type::Const(TypeConst::I24(_)) |
        Type::Const(TypeConst::I32(_)) |
        Type::Const(TypeConst::I64(_)) |
        Type::Const(TypeConst::F32(_)) |
        Type::Const(TypeConst::F64(_)) => Ok(Kind::Type),

        // Array types
        Type::Array(_, ref elem_ty, ref size_expr) => {
            expect_ty_kind(ctx, elem_ty)?;

            let size_ty = ty_of(ctx, size_expr)?;
            match *size_ty {
                host::Type::Const(host::TypeConst::Unsigned(_)) => Ok(Kind::Type),
                _ => Err(
                    TypeError::Mismatch {
                        expr: Rc::clone(size_expr),
                        expected: ExpectedType::Signed,
                        found: size_ty,
                    }.into(),
                ),
            }
        }

        // Conditional types
        Type::Assert(_, ref ty, ref pred_expr) => {
            expect_ty_kind(ctx, ty)?;
            let pred_ty = Rc::new(host::Type::Arrow(
                vec![ty.repr()],
                Rc::new(host::Type::Const(host::TypeConst::Bool)),
            ));
            expect_ty(ctx, pred_expr, &pred_ty)?;

            Ok(Kind::Type)
        }

        // Interpreted types
        Type::Interp(_, ref ty, ref conv_expr, ref repr_ty) => {
            expect_ty_kind(ctx, ty)?;
            let conv_ty = Rc::new(host::Type::Arrow(vec![ty.repr()], Rc::clone(repr_ty)));
            expect_ty(ctx, conv_expr, &conv_ty)?;

            Ok(Kind::Type)
        }

        // Type abstraction
        Type::Abs(_, ref param_tys, ref body_ty) => {
            // FIXME: avoid cloning the environment
            let mut ctx = ctx.clone();

            expect_ty_kind(&ctx, body_ty)?;
            let kind = Kind::arrow(param_tys.len() as u32);

            ctx.extend(Scope::TypeAbs(
                param_tys
                    .iter()
                    .map(|named| Named(named.0.clone(), Kind::Type))
                    .collect(),
            ));

            Ok(kind)
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
                ctx.extend(Scope::ExprAbs(
                    vec![Named(field.name.clone(), field_ty.repr())],
                ));
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

pub fn check_program<N: Name>(program: &Program<N>) -> Result<(), KindError<N>> {
    let mut ctx = Context::new();

    for definition in &program.definitions {
        let definition_kind = kind_of(&ctx, &definition.ty)?;
        ctx.extend(Scope::TypeDef(vec![
            Named(
                definition.name.clone(),
                (Rc::clone(&definition.ty), definition_kind),
            ),
        ]));
    }

    Ok(())
}

// Expectations

fn expect_ty<N: Name>(
    ctx: &Context<N>,
    expr: &host::RcExpr<N>,
    expected: &host::RcType<N>,
) -> Result<host::RcType<N>, TypeError<N>> {
    let found = ty_of(ctx, expr)?;

    if &found == expected {
        Ok(found)
    } else {
        Err(TypeError::Mismatch {
            expr: Rc::clone(expr),
            expected: ExpectedType::Actual(Rc::clone(expected)),
            found,
        })
    }
}

fn expect_kind<N: Name>(
    ctx: &Context<N>,
    ty: &binary::RcType<N>,
    expected: binary::Kind,
) -> Result<binary::Kind, KindError<N>> {
    let found = kind_of(ctx, ty)?;

    if found == expected {
        Ok(found)
    } else {
        Err(KindError::Mismatch {
            ty: Rc::clone(ty),
            expected: expected,
            found,
        })
    }
}

fn expect_ty_kind<N: Name>(ctx: &Context<N>, ty: &binary::RcType<N>) -> Result<(), KindError<N>> {
    use syntax::ast::binary::Kind;

    expect_kind(ctx, ty, Kind::Type).map(|_| ())
}
