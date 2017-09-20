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
//!         ℕ           natural number
//!         true        true value
//!         false       false value
//!         -e          negation
//!         ¬e          not
//!         e₁ ∨ e₂     disjunction
//!         e₁ ∧ e₂     conjunction
//!         e₁ = e₂     equality
//!         e₁ ≠ e₂     inequality
//!         e₁ < e₂     less than
//!         e₁ ≤ e₂     less than or equal
//!         e₁ > e₂     greater than
//!         e₁ ≥ e₂     greater than or equal
//!         e₁ + e₂     addition
//!         e₁ - e₂     subtraction
//!         e₁ * e₂     multiplication
//!         e₁ / e₂     division
//! ```
//!
//! ## Types
//!
//! ```plain
//! E ::=
//!         Le                  little endian
//!         Be                  big endian
//!
//! c ::=
//!         Bool                booleans
//!         UInt(ℕ, E)          unsigned integer with byte size and endianness
//!         Int(ℕ, E)           signed integer with byte size and endianness
//!         UnknownInt(n)       an unknown integer literal
//!
//! τ ::=
//!         c                   type constants
//!         α                   variables
//!         τ₁ + τ₂             sum
//!         Σ x:τ₁ .τ₂          dependent pair
//!         [τ; e]              array
//!         { x:τ | e }         constrained type
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

use ast::{Binop, Const, Definition, Expr, Kind, Type, TypeConst, Unop};
use env::Env;
use source::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KindError {
    Type(TypeError),
    UnboundType(Span, String),
    ArraySizeExpectedUInt(Span, Type),
    WherePredicateExpectedBool(Span, Type),
}

impl From<TypeError> for KindError {
    fn from(src: TypeError) -> KindError {
        KindError::Type(src)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    UnboundVariable(Span, String),
    Unexpected(Span, Type),
}

/// The subtyping relation: `τ₁ <: τ₂`
///
/// # Rules
///
/// ```plain
/// ―――――――――――――――――――― (S-REFL)
///        τ <: τ
///
///
/// ――――――――――――――――――――――――――――――――――――― (S-UINT)
///      UnknownInt(ℕ₂) <: UInt(ℕ₁, E)
///
///
/// ――――――――――――――――――――――――――――――――――――― (S-INT)
///      UnknownInt(ℕ₂) <: Int(ℕ₁, E)
/// ```
pub fn is_subtype(sty: &Type, ty: &Type) -> bool {
    use ast::Type::Const;

    match (sty, ty) {
        // S-REFL
        (sty, ty) if sty == ty => true,

        // S-UINT, S-INT
        (&Const(TypeConst::UnknownInt), &Const(TypeConst::U(_, _))) |
        (&Const(TypeConst::UnknownInt), &Const(TypeConst::I(_, _))) => true,

        (_, _) => false,
    }
}

impl<'parent> Env<'parent> {
    pub fn check_defs<I>(&mut self, defs: I) -> Result<(), KindError>
    where
        I: IntoIterator<Item = Definition>,
    {
        for def in defs {
            kind_of(self, &def.ty)?;
            self.add_ty(def.name, def.ty);
        }
        Ok(())
    }
}

/// The kinding relation: `Γ ⊢ τ : κ`
///
/// # Rules
///
/// ```plain
/// ―――――――――――――――――――― (K-CONST)
///     Γ ⊢ c : Type
///
///
///         α ∈ Γ
/// ―――――――――――――――――――― (K-VAR)
///     Γ ⊢ α : Type
///
///
///     Γ ⊢ τ₁ : Type        Γ ⊢ τ₂ : Type
/// ―――――――――――――――――――――――――――――――――――――――――― (K-SUM)
///              Γ ⊢ τ₁ + τ₂ : Type
///
///
///     Γ ⊢ τ₁ : Type        Γ, x:τ₁ ⊢ τ₂ : Type
/// ―――――――――――――――――――――――――――――――――――――――――――――――――― (K-DEPENDENT-PAIR)
///              Γ ⊢ Σ x:τ₁ .τ₂ : Type
///
///
///     Γ ⊢ τ : Type        Γ ⊢ e : UInt(ℕ, E)
/// ――――――――――――――――――――――――――――――――――――――――――――――― (K-ARRAY-UINT)
///               Γ ⊢ [τ; e] :
///
///
///     Γ ⊢ τ : Type        Γ ⊢ e : UnknownInt(ℕ)
/// ―――――――――――――――――――――――――――――――――――――――――――――――――― (K-ARRAY-UNKNOWN-INT)
///               Γ ⊢ [τ; e] : Type
///
///
///     Γ ⊢ τ : Type      Γ, x:τ ⊢ b : Bool
/// ―――――――――――――――――――――――――――――――――――――――――― (K-CON)
///           Γ ⊢ { x:τ | b } : Type
/// ```
pub fn kind_of(env: &Env, ty: &Type) -> Result<Kind, KindError> {
    match *ty {
        // K-CONST
        Type::Const(_) => Ok(Kind::Type), // Easypeasy

        // K-VAR
        Type::Var(span, ref name) => {
            // TODO: kind of var?
            // α ∈ Γ
            match env.lookup_ty(name) {
                Some(_) => Ok(Kind::Type),
                None => Err(KindError::UnboundType(span, name.clone())),
            }
        }

        // K-SUM
        Type::Union(_, ref tys) => {
            for ty in tys {
                // Γ ⊢ τ₁ : Type
                kind_of(env, &ty)?;
            }
            Ok(Kind::Type)
        }

        // K-DEPENDENT-PAIR
        Type::Struct(_, ref fields) => {
            // TODO: prevent name shadowing?
            let mut inner_env = env.extend();
            for field in fields {
                // Γ ⊢ τ₁ : Type
                kind_of(&inner_env, &field.ty)?;
                // Γ, x:τ₁ ⊢ τ₂ : Type
                inner_env.add_binding(field.name.clone(), field.ty.clone());
            }
            Ok(Kind::Type)
        }

        // K-ARRAY-???
        Type::Array(span, ref ty, ref size) => {
            kind_of(env, ty)?;
            let expr_ty = type_of(env, size)?;

            match expr_ty {
                // K-ARRAY-UNKNOWN-INT
                Type::Const(TypeConst::UnknownInt) |
                // K-ARRAY-UINT
                Type::Const(TypeConst::U(_, _)) => Ok(Kind::Type),
                ty => Err(KindError::ArraySizeExpectedUInt(span, ty)),
            }
        }

        // K-CON
        Type::Where(span, ref ty, ref param, ref pred) => {
            kind_of(env, ty)?;

            let mut inner_env = env.extend();
            // TODO: prevent name shadowing?
            inner_env.add_binding(param.clone(), (**ty).clone());
            match type_of(env, pred)? {
                Type::Const(TypeConst::Bool) => Ok(Kind::Type),
                pred_ty => Err(KindError::WherePredicateExpectedBool(span, pred_ty)),
            }
        }
    }
}

fn type_of_bool_binop(env: &Env, lhs: &Expr, rhs: &Expr) -> Result<Type, TypeError> {
    use ast::TypeConst::Bool;
    use ast::Type::Const;

    let lhs_ty = type_of(env, lhs)?;
    let rhs_ty = type_of(env, rhs)?;

    match (lhs_ty, rhs_ty) {
        (ty @ Const(Bool), Const(Bool)) => Ok(ty),
        (_, _) => unimplemented!(), // FIXME: better errors
    }
}

fn type_of_comparison_binop(env: &Env, lhs: &Expr, rhs: &Expr) -> Result<Type, TypeError> {
    use ast::Type::Const;

    let lhs_ty = type_of(env, lhs)?;
    let rhs_ty = type_of(env, rhs)?;

    // FIXME: Ugh
    match (lhs_ty, rhs_ty) {
        // Coerce to LHS if the RHS is less specific
        (Const(TypeConst::U(_, _)), Const(TypeConst::UnknownInt)) |
        (Const(TypeConst::I(_, _)), Const(TypeConst::UnknownInt)) |
        // Coerce to RHS if the LHS is less specific
        (Const(TypeConst::UnknownInt), Const(TypeConst::U(_, _))) |
        (Const(TypeConst::UnknownInt), Const(TypeConst::I(_, _))) => {
            Ok(Type::bool())
        }
        // Same type if LHS == RHS
        (Const(TypeConst::U(ls, le)), Const(TypeConst::U(rs, re))) => {
            if ls == rs && le == re {
                Ok(Type::bool())
            } else {
                unimplemented!()
            }
        }
        // Same type if LHS == RHS
        (Const(TypeConst::I(ls, le)), Const(TypeConst::I(rs, re))) => {
            if ls == rs && le == re {
                Ok(Type::bool())
            } else {
                unimplemented!()
            }
        }
        // Error!
        (_, _) => unimplemented!(), // FIXME: better errors
    }
}

fn type_of_int_binop(env: &Env, lhs: &Expr, rhs: &Expr) -> Result<Type, TypeError> {
    use ast::Type::Const;

    let lhs_ty = type_of(env, lhs)?;
    let rhs_ty = type_of(env, rhs)?;

    // FIXME: Ugh
    match (lhs_ty, rhs_ty) {
        (ty @ Const(TypeConst::UnknownInt), Const(TypeConst::UnknownInt)) |
        // Coerce to LHS if the RHS is less specific
        (ty @ Const(TypeConst::U(_, _)), Const(TypeConst::UnknownInt)) |
        (ty @ Const(TypeConst::I(_, _)), Const(TypeConst::UnknownInt)) |
        // Coerce to RHS if the LHS is less specific
        (Const(TypeConst::UnknownInt), ty @ Const(TypeConst::U(_, _))) |
        (Const(TypeConst::UnknownInt), ty @ Const(TypeConst::I(_, _))) => Ok(ty),
        // Same type if LHS == RHS
        (Const(TypeConst::U(ls, le)), Const(TypeConst::U(rs, re))) => {
            if ls == rs && le == re {
                Ok(Const(TypeConst::U(ls, le)))
            } else {
                unimplemented!()
            }
        }
        // Same type if LHS == RHS
        (Const(TypeConst::I(ls, le)), Const(TypeConst::I(rs, re))) => {
            if ls == rs && le == re {
                Ok(Const(TypeConst::I(ls, le)))
            } else {
                unimplemented!()
            }
        }
        // Error!
        (_, _) => unimplemented!(), // FIXME: better errors
    }
}

/// The typing relation: `Γ ⊢ e : τ`
///
/// # Rules
///
/// ```plain
/// ―――――――――――――――――――――――――――― (T-TRUE)
///       Γ ⊢ true : Bool
///
///
/// ―――――――――――――――――――――――――――― (T-FALSE)
///       Γ ⊢ false : Bool
///
///
/// ―――――――――――――――――――――――――――― (T-UNKNOWN-INT)
///     Γ ⊢ ℕ : UnknownInt(ℕ)
///
///
///           x : τ ∈ Γ
/// ―――――――――――――――――――――――――――― (T-VAR)
///           Γ ⊢ x : τ
///
///
///         Γ ⊢ e : Bool
/// ―――――――――――――――――――――――――――― (T-NOT)
///         Γ ⊢ ¬e : Bool
///
///
///     Γ ⊢ e : τ       UnknownInt(ℕ) <: τ
/// ――――――――――――――――――――――――――――――――――――――――― (T-NEG)
///              Γ ⊢ -e : τ
///
/// ```
pub fn type_of(env: &Env, expr: &Expr) -> Result<Type, TypeError> {
    match *expr {
        // T-TRUE, T-FALSE
        Expr::Const(_, Const::Bool(_)) => Ok(Type::bool()),

        // FIXME: T-UNKNOWN-INT
        Expr::Const(_, Const::UInt(_)) => Ok(Type::unknown_int()),

        // T-VAR
        Expr::Var(span, ref name) => {
            match env.lookup_binding(name) {
                Some(ty) => Ok(ty.clone()),
                None => Err(TypeError::UnboundVariable(span, name.clone())),
            }
        }

        Expr::Unop(span, op, ref value) => {
            match (op, type_of(env, value)?) {
                // T-NOT
                (Unop::Not, ty @ Type::Const(TypeConst::Bool)) => Ok(ty),
                (Unop::Not, ty) => Err(TypeError::Unexpected(span, ty)),
                // T-NEG
                (Unop::Neg, ref ty) if is_subtype(&Type::unknown_int(), ty) => Ok(ty.clone()),
                (Unop::Neg, ty) => Err(TypeError::Unexpected(span, ty)),
            }
        }

        // FIXME: T-???
        Expr::Binop(_, op, ref lhs, ref rhs) => {
            match op {
                Binop::Or | Binop::And => type_of_bool_binop(env, lhs, rhs),
                Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                    type_of_comparison_binop(env, lhs, rhs)
                }
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
                    type_of_int_binop(env, lhs, rhs)
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

    // Add expressions

    mod type_of {
        use super::*;

        mod add_expr {
            use super::*;

            #[test]
            fn uint_with_uint() {
                let mut env = Env::default();
                let len_ty = Type::u(32, Endianness::Target);
                env.add_binding("len", len_ty.clone());

                let expr = parser::parse_expr(&env, "len + len").unwrap();
                assert_eq!(type_of(&env, &expr), Ok(len_ty));
            }

            #[test]
            fn unknown_with_uint() {
                let mut env = Env::default();
                let len_ty = Type::u(32, Endianness::Target);
                env.add_binding("len", len_ty.clone());

                let expr = parser::parse_expr(&env, "1 + len").unwrap();
                assert_eq!(type_of(&env, &expr), Ok(len_ty.clone()));
            }

            #[test]
            fn uint_with_unknown() {
                let mut env = Env::default();
                let len_ty = Type::u(32, Endianness::Target);
                env.add_binding("len", len_ty.clone());

                let expr = parser::parse_expr(&env, "len + 1").unwrap();
                assert_eq!(type_of(&env, &expr), Ok(len_ty.clone()));
            }

            #[test]
            fn unknown_with_unknown() {
                let env = Env::default();
                let expr = parser::parse_expr(&env, "1 + 1").unwrap();

                assert_eq!(type_of(&env, &expr), Ok(Type::unknown_int()));
            }
        }

        mod mul_expr {
            use super::*;

            #[test]
            fn uint_with_uint() {
                let mut env = Env::default();
                let len_ty = Type::u(32, Endianness::Target);
                env.add_binding("len", len_ty.clone());

                let expr = parser::parse_expr(&env, "len * len").unwrap();
                assert_eq!(type_of(&env, &expr), Ok(len_ty));
            }

            #[test]
            fn unknown_with_uint() {
                let mut env = Env::default();
                let len_ty = Type::u(32, Endianness::Target);
                env.add_binding("len", len_ty.clone());

                let expr = parser::parse_expr(&env, "1 * len").unwrap();
                assert_eq!(type_of(&env, &expr), Ok(len_ty.clone()));
            }

            #[test]
            fn uint_with_unknown() {
                let mut env = Env::default();
                let len_ty = Type::u(32, Endianness::Target);
                env.add_binding("len", len_ty.clone());

                let expr = parser::parse_expr(&env, "len * 1").unwrap();
                assert_eq!(type_of(&env, &expr), Ok(len_ty.clone()));
            }

            #[test]
            fn unknown_with_unknown() {
                let env = Env::default();
                let expr = parser::parse_expr(&env, "1 * 1").unwrap();

                assert_eq!(type_of(&env, &expr), Ok(Type::unknown_int()));
            }
        }
    }

    mod kind_of {
        use super::*;

        #[test]
        fn ty_const() {
            let env = Env::default();
            let ty = Type::i(16, Endianness::Target);

            assert_eq!(kind_of(&env, &ty), Ok(Kind::Type));
        }

        #[test]
        fn var() {
            let env = Env::default();
            let ty = parser::parse_ty(&env, "u8").unwrap();

            assert_eq!(kind_of(&env, &ty), Ok(Kind::Type));
        }

        #[test]
        fn var_missing() {
            let env = Env::default();
            let ty = parser::parse_ty(&env, "Foo").unwrap();

            assert_eq!(
                kind_of(&env, &ty),
                Err(KindError::UnboundType(
                    Span::new(B(0), B(3)),
                    "Foo".to_owned(),
                ))
            );
        }

        #[test]
        fn union() {
            let env = Env::default();
            let ty = parser::parse_ty(&env, "union { u8, u16, i32 }").unwrap();

            assert_eq!(kind_of(&env, &ty), Ok(Kind::Type));
        }

        #[test]
        fn union_element_missing() {
            let env = Env::default();
            let ty = parser::parse_ty(&env, "union { u8, Foo, i32 }").unwrap();

            assert_eq!(
                kind_of(&env, &ty),
                Err(KindError::UnboundType(
                    Span::new(B(12), B(15)),
                    "Foo".to_owned(),
                ))
            );
        }

        #[test]
        fn pair() {
            let env = Env::default();
            let ty = parser::parse_ty(&env, "struct { x: u8, y: u8 }").unwrap();

            assert_eq!(kind_of(&env, &ty), Ok(Kind::Type));
        }

        #[test]
        fn dependent_pair() {
            let env = Env::default();
            let ty = parser::parse_ty(&env, "struct { len: u8, data: [u8; len] }").unwrap();

            assert_eq!(kind_of(&env, &ty), Ok(Kind::Type));
        }

        #[test]
        fn array() {
            let env = Env::default();
            let ty = parser::parse_ty(&env, "[u8; 16]").unwrap();

            assert_eq!(kind_of(&env, &ty), Ok(Kind::Type));
        }

        #[test]
        fn array_len() {
            let mut env = Env::default();
            let len_ty = Type::u(32, Endianness::Target);
            env.add_binding("len", len_ty);
            let ty = parser::parse_ty(&env, "[u8; len]").unwrap();

            assert_eq!(kind_of(&env, &ty), Ok(Kind::Type));
        }

        #[test]
        fn array_singned_int_size() {
            let mut env = Env::default();
            let len_ty = parser::parse_ty(&env, "i8").unwrap();
            env.add_binding("len", len_ty.clone());
            let ty = parser::parse_ty(&env, "[u8; len]").unwrap();

            assert_eq!(
                kind_of(&env, &ty),
                Err(KindError::ArraySizeExpectedUInt(
                    Span::new(B(0), B(9)),
                    len_ty,
                ))
            );
        }

        #[test]
        fn array_struct_size() {
            let mut env = Env::default();
            let len_ty = parser::parse_ty(&env, "struct {}").unwrap();
            env.add_binding("len", len_ty.clone());
            let ty = parser::parse_ty(&env, "[u8; len]").unwrap();

            assert_eq!(
                kind_of(&env, &ty),
                Err(KindError::ArraySizeExpectedUInt(
                    Span::new(B(0), B(9)),
                    len_ty,
                ))
            );
        }
    }
}
