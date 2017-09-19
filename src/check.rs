//! Typechecking for our DDL
//!
//! # Syntax
//!
//! ## Kinds
//!
//! ```plain
//! κ ::=
//!         Type        kind of types
//! ```
//!
//! ## Expressions
//!
//! ```plain
//! e ::=
//!         x           variables
//!         n           integer number
//! ```
//!
//! ## Boolean Expressions
//!
//! ```plain
//! b ::=
//!         true        true value
//!         false       false value
//!         ¬b          not
//!         b₁ ∨ b₂     disjunction
//!         b₁ ∧ b₂     conjunction
//!         e₁ = e₂     equality
//!         e₁ ≠ e₂     inequality
//!         e₁ < e₂     less than
//!         e₁ ≤ e₂     less than or equal
//!         e₁ > e₂     greater than
//!         e₁ ≥ e₂     greater than or equal
//! ```
//!
//! ## Terms
//!
//! ```plain
//! τ ::=
//!         c                   type constants
//!         α                   variables
//!         τ₁ + τ₂             sum
//!         Σ x:τ₁ .τ₂          dependent pair
//!         [τ; e]              array
//!         { x:τ | b }         constrained type
//! ```
//!
//! In the `ast`, we represent the above as the following:
//!
//! - `Type::Var`: variables
//!
//! - `Type::Union`: series of unions
//!
//! - `Type::Struct`: nested dependent pairs
//!
//!   For example, the struct:
//!
//!   ```plain
//!   struct { len : u16, reserved : u16, data : [u16; len] }
//!   ```
//!
//!   Would be desugared into:
//!
//!   ```plain
//!   Σ len:u16 . Σ reserved:u16 . [u16; len]
//!   ```
//!
//!   Note how later fields have access to the data in previous fields.
//!
//! - `Type::Array`: TODO
//!
//! - `Type::Where`: constrained type

use ast::{Binop, Definition, Expr, Kind, Type, TypeConst, Unop};
use env::Env;
use source::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    UnboundType(Span, String),
    ExpectedUnsignedIntInArraySizeExpr(Span, Type),
    FailedToEvaluateArraySize(Span, ExprError),
    FailedToEvaluatePredicate(Span, ExprError),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprError {
    UnboundVariable(Span, String),
}

impl<'parent> Env<'parent> {
    pub fn check_defs<I: IntoIterator<Item = Definition>>(
        &mut self,
        defs: I,
    ) -> Result<(), TypeError> {
        for def in defs {
            self.check_ty(&def.ty)?;
            self.add_ty(def.name, def.ty);
        }
        Ok(())
    }

    /// `Γ ⊢ τ : κ`
    ///
    /// ```plain
    /// ―――――――――――――――――――― (CONST)
    ///     Γ ⊢ c : Type
    ///
    ///
    ///         α ∈ Γ
    /// ―――――――――――――――――――― (VAR)
    ///     Γ ⊢ α : Type
    ///
    ///
    ///     Γ ⊢ τ₁ : Type        Γ ⊢ τ₂ : Type
    /// ―――――――――――――――――――――――――――――――――――――――――― (SUM)
    ///              Γ ⊢ τ₁ + τ₂ : Type
    ///
    ///
    ///     Γ ⊢ τ₁ : Type        Γ, x:τ₁ ⊢ τ₂ : Type
    /// ―――――――――――――――――――――――――――――――――――――――――――――――――― (DEPENDENT-PAIR)
    ///              Γ ⊢ Σ x:τ₁ .τ₂ : Type
    ///
    ///
    ///     Γ ⊢ τ : Type        Γ ⊢ e : Int
    /// ―――――――――――――――――――――――――――――――――――――――――― (ARRAY)
    ///              [τ; e] : Type
    ///
    ///
    ///     Γ ⊢ τ : Type      Γ, x:τ ⊢ b : Bool
    /// ―――――――――――――――――――――――――――――――――――――――――― (CON)
    ///               { x:τ | b }
    /// ```
    pub fn check_ty(&self, ty: &Type) -> Result<Kind, TypeError> {
        match *ty {
            // CONST
            Type::Const(_, _) => Ok(Kind::Type), // Easypeasy

            // VAR
            Type::Var(span, ref name) => {
                // TODO: kind of var?
                // α ∈ Γ
                match self.lookup_ty(name) {
                    Some(_) => Ok(Kind::Type),
                    None => Err(TypeError::UnboundType(span, name.clone())),
                }
            }

            // SUM
            Type::Union(_, ref tys) => {
                for ty in tys {
                    // Γ ⊢ τ₁ : Type
                    self.check_ty(&ty)?;
                }
                Ok(Kind::Type)
            }

            // DEPENDENT-PAIR
            Type::Struct(_, ref fields) => {
                // TODO: prevent name shadowing?
                let mut inner_env = self.extend();
                for field in fields {
                    // Γ ⊢ τ₁ : Type
                    inner_env.check_ty(&field.ty)?;
                    // Γ, x:τ₁ ⊢ τ₂ : Type
                    inner_env.add_binding(field.name.clone(), field.ty.clone());
                }
                Ok(Kind::Type)
            }

            // ARRAY
            Type::Array(span, ref ty, ref size) => {
                self.check_ty(ty)?;
                let expr_ty = self.check_expr(size).map_err(|err| {
                    TypeError::FailedToEvaluateArraySize(span, err)
                })?;

                match expr_ty {
                    Type::Const(_, _) => Ok(Kind::Type), // FIXME: should be int
                    ty => Err(TypeError::ExpectedUnsignedIntInArraySizeExpr(span, ty)),
                }
            }

            // CON
            Type::Where(span, ref ty, ref param, ref pred) => {
                self.check_ty(ty)?;

                let mut inner_env = self.extend();
                // TODO: prevent name shadowing?
                inner_env.add_binding(param.clone(), (**ty).clone());
                match self.check_expr(pred) {
                    Ok(Type::Const(_, TypeConst::Bool)) => Ok(Kind::Type),
                    Ok(_) => unimplemented!(), // FIXME: better errors
                    Err(err) => Err(TypeError::FailedToEvaluatePredicate(span, err)),
                }
            }
        }
    }

    /// `Γ ⊢ e : τ`
    pub fn check_expr(&self, expr: &Expr) -> Result<Type, ExprError> {
        use ast::TypeConst::{Bool, I, U, UnknownInt};
        use ast::Type::Const;

        match *expr {
            Expr::Const(_, _) => Ok(Type::unknown_int(Span::start())),
            Expr::Var(span, ref name) => {
                match self.lookup_binding(name) {
                    Some(ty) => Ok(ty.clone()),
                    None => Err(ExprError::UnboundVariable(span, name.clone())),
                }
            }
            Expr::Unop(_, op, ref value) => {
                match op {
                    Unop::Not => {
                        match self.check_expr(value)? {
                            ty @ Const(_, Bool) => Ok(ty),
                            _ => unimplemented!(), // FIXME: better errors
                        }
                    }
                    Unop::Neg => {
                        match self.check_expr(value)? {
                            ty @ Const(_, UnknownInt) |
                            ty @ Const(_, U(_, _)) |
                            ty @ Const(_, I(_, _)) => Ok(ty),
                            _ => unimplemented!(), // FIXME: better errors
                        }
                    }
                }
            }
            Expr::Binop(_, op, ref lhs, ref rhs) => {
                match op {
                    Binop::Or | Binop::And => {
                        let lhs_ty = self.check_expr(lhs)?;
                        let rhs_ty = self.check_expr(rhs)?;

                        match (lhs_ty, rhs_ty) {
                            (ty @ Const(_, Bool), Const(_, Bool)) => Ok(ty),
                            (_, _) => unimplemented!(), // FIXME: better errors
                        }
                    }
                    Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                        let lhs_ty = self.check_expr(lhs)?;
                        let rhs_ty = self.check_expr(rhs)?;

                        // FIXME: Ugh
                        match (lhs_ty, rhs_ty) {
                            // Coerce to LHS if the RHS is less specific
                            (Const(_, U(_, _)), Const(_, UnknownInt)) |
                            (Const(_, I(_, _)), Const(_, UnknownInt)) |
                            // Coerce to RHS if the LHS is less specific
                            (Const(_, UnknownInt), Const(_, U(_, _))) |
                            (Const(_, UnknownInt), Const(_, I(_, _))) => Ok(Type::bool(Span::start())),
                            // Same type if LHS == RHS
                            (Const(_, U(ls, le)), Const(_, U(rs, re))) => {
                                if ls == rs && le == re {
                                    Ok(Type::bool(Span::start()))
                                } else {
                                    unimplemented!()
                                }
                            }
                            // Same type if LHS == RHS
                            (Const(_, I(ls, le)), Const(_, I(rs, re))) => {
                                if ls == rs && le == re {
                                    Ok(Type::bool(Span::start()))
                                } else {
                                    unimplemented!()
                                }
                            }
                            // Error!
                            (_, _) => unimplemented!(), // FIXME: better errors
                        }
                    }
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
                        let lhs_ty = self.check_expr(lhs)?;
                        let rhs_ty = self.check_expr(rhs)?;

                        // FIXME: Ugh
                        match (lhs_ty, rhs_ty) {
                            // Coerce to LHS if the RHS is less specific
                            (lhs_ty @ Const(_, U(_, _)), Const(_, UnknownInt)) |
                            (lhs_ty @ Const(_, I(_, _)), Const(_, UnknownInt)) => Ok(lhs_ty),
                            // Coerce to RHS if the LHS is less specific
                            (Const(_, UnknownInt), rhs_ty @ Const(_, U(_, _))) |
                            (Const(_, UnknownInt), rhs_ty @ Const(_, I(_, _))) => Ok(rhs_ty),
                            // Same type if LHS == RHS
                            (Const(span, U(ls, le)), Const(_, U(rs, re))) => {
                                if ls == rs && le == re {
                                    Ok(Const(span, U(ls, le)))
                                } else {
                                    unimplemented!()
                                }
                            }
                            // Same type if LHS == RHS
                            (Const(span, I(ls, le)), Const(_, I(rs, re))) => {
                                if ls == rs && le == re {
                                    Ok(Const(span, I(ls, le)))
                                } else {
                                    unimplemented!()
                                }
                            }
                            // Error!
                            (_, _) => unimplemented!(), // FIXME: better errors
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    use ast::Endianness;
    use parser;
    use source::BytePos as B;
    use super::*;

    #[test]
    fn ty_const() {
        let env = Env::default();
        let ty = Type::i(Span::start(), 16, Endianness::Target);

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn var() {
        let env = Env::default();
        let ty = parser::parse_ty(&env, "u8").unwrap();

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn var_missing() {
        let env = Env::default();
        let ty = parser::parse_ty(&env, "Foo").unwrap();

        assert_eq!(
            env.check_ty(&ty),
            Err(TypeError::UnboundType(
                Span::new(B(0), B(3)),
                "Foo".to_owned(),
            ))
        );
    }

    #[test]
    fn union() {
        let env = Env::default();
        let ty = parser::parse_ty(&env, "union { u8, u16, i32 }").unwrap();

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn union_element_missing() {
        let env = Env::default();
        let ty = parser::parse_ty(&env, "union { u8, Foo, i32 }").unwrap();

        assert_eq!(
            env.check_ty(&ty),
            Err(TypeError::UnboundType(
                Span::new(B(12), B(15)),
                "Foo".to_owned(),
            ))
        );
    }

    #[test]
    fn pair() {
        let env = Env::default();
        let ty = parser::parse_ty(&env, "struct { x: u8, y: u8 }").unwrap();

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn dependent_pair() {
        let env = Env::default();
        let ty = parser::parse_ty(&env, "struct { len: u8, data: [u8; len] }").unwrap();

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn array() {
        let env = Env::default();
        let ty = parser::parse_ty(&env, "[u8; 16]").unwrap();

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn array_len() {
        let mut env = Env::default();
        let len_ty = Type::u(Span::start(), 32, Endianness::Target);
        env.add_binding("len", len_ty);
        let ty = parser::parse_ty(&env, "[u8; len]").unwrap();

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn array_bad_type() {
        let mut env = Env::default();
        let len_ty = parser::parse_ty(&env, "struct {}").unwrap();
        env.add_binding("len", len_ty.clone());
        let ty = parser::parse_ty(&env, "[u8; len]").unwrap();

        assert_eq!(
            env.check_ty(&ty),
            Err(TypeError::ExpectedUnsignedIntInArraySizeExpr(
                Span::new(B(0), B(9)),
                len_ty,
            ))
        );
    }
}
