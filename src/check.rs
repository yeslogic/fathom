//! Typechecking
//!
//! # Syntax
//!
//! ## Expressions
//!
//! ```plain
//! e ::=
//!         x                   variables
//!
//!         n                   natural number
//!         true                true value
//!         false               false value
//!
//!         -e                  negation
//!         ¬e                  not
//!         op(Rel, e₁, e₂)     relational binary operation
//!         op(Cmp, e₁, e₂)     comparison binary operation
//!         op(Arith, e₁, e₂)   arithmetic binary operation
//!
//! Rel ::=
//!         ∨                   disjunction operator
//!         ∧                   conjunction operator
//!
//! Cmp ::=
//!         =                   equality operator
//!         ≠                   inequality operator
//!         <                   less than operator
//!         ≤                   less than or equal operator
//!         >                   greater than operator
//!         ≥                   greater than or equal operator
//!
//! Arith ::=
//!         +                   addition operator
//!         -                   subtraction operator
//!         *                   multiplication operator
//!         /                   division operator
//! ```
//!
//! ## Types
//!
//! ```plain
//! E ::=
//!         Le                  little endian
//!         Be                  big endian
//!
//! τ ::=
//!         α                   type variables
//!
//!         Bool                booleans
//!
//!         UInt(n, E)          unsigned integer with byte size and endianness
//!         SInt(n, E)          two's complement signed integer with byte size and endianness
//!         SingletonInt(n)     matches a single integer
//!         RangedInt(n₁, n₂)   matches a ranged integer
//!
//!         τ₁ + τ₂             sum
//!         Σ x:τ₁ .τ₂          dependent pair
//!         [τ; e]              array
//!         { x:τ | e }         constrained type
//! ```
//!
//! ## Kinds
//!
//! ```plain
//! κ ::=
//!         Type        kind of types
//!         Binary      kind of binary types
//! ```
//!
//! # Types in the AST
//!
//! In the `ast`, we represent the above as the following:
//!
//! - `Type::Var`: variables
//! - `Type::Union`: series of unions
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
//! - `Type::Where`: constrained type

use ast::{Binop, Const, Definition, Expr, Kind, Type, Unop};
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
    UnexpectedUnaryOperand(Span, Unop, Type),
    UnexpectedBinaryLhs(Span, Type),
    UnexpectedBinaryRhs(Span, Type),
    ExpectedNumericOperands(Span, Type, Type),
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
///        n₃ ≤ n₁             n₂ ≤ n₄
/// ―――――――――――――――――――――――――――――――――――――――――― (S-RANGED-INT)
///   RangedInt(n₁, n₂) <: RangedInt(n₃, n₄)
///
///
///          0 ≤ n₁          n₂ ≤ 2^n₃ - 1
/// ――――――――――――――――――――――――――――――――――――――――――――――― (S-RANGED-INT-UINT)
///         RangedInt(n₁, n₂) <: UInt(n₃, E)
///
///
///    -2^(n₂ - 1) ≤ n₁       n₂ ≤ 2^(n₃ - 1) - 1
/// ――――――――――――――――――――――――――――――――――――――――――――――― (S-RANGED-INT-SINT)
///        RangedInt(n₁, n₂) <: SInt(n₃, E)
/// ```
pub fn is_subtype(sty: &Type, ty: &Type) -> bool {
    match (sty, ty) {
        // S-REFL
        (sty, ty) if sty == ty => true,

        // S-RANGED-INT
        (&Type::RangedInt(slo, shi), &Type::RangedInt(lo, hi)) if lo <= slo && shi <= hi => true,

        // S-RANGED-INT-UINT
        // FIXME: check size
        (&Type::RangedInt(_, _), &Type::UInt(_, _)) => true,

        // S-RANGED-INT-SINT
        // FIXME: check size
        (&Type::RangedInt(_, _), &Type::SInt(_, _)) => true,

        (_, _) => false,
    }
}

pub fn is_numeric(ty: &Type) -> bool {
    match *ty {
        Type::RangedInt(_, _) | Type::UInt(_, _) | Type::SInt(_, _) => true,
        // Ignore floats for now...
        _ => false,
    }
}

impl<'parent> Env<'parent> {
    pub fn check_defs<I>(&mut self, defs: I) -> Result<(), KindError>
    where
        I: IntoIterator<Item = Definition>,
    {
        for def in defs {
            match kind_of(self, &def.ty)? {
                Kind::Type => unimplemented!(), // FIXME: Better errors
                Kind::Binary => self.add_ty(def.name, def.ty),
            }
        }
        Ok(())
    }
}

/// The kinding relation: `Γ ⊢ τ : κ`
///
/// # Rules
///
/// ```plain
/// ――――――――――――――――――――――― (K-BOOL)
///    Γ ⊢ Bool : Type
///
///
///              n₁ ≤ n₂
/// ―――――――――――――――――――――――――――――――――――  (K-RANGED-INT)
///    Γ ⊢ RangedInt(n₁, n₂) : Binary
///
///
///               n > 0
/// ―――――――――――――――――――――――――――――――――――  (K-UINT)
///      Γ ⊢ UInt(n, E) : Binary
///
///
///               n > 0
/// ―――――――――――――――――――――――――――――――――――  (K-SINT)
///      Γ ⊢ SInt(n, E) : Binary
///
///
///        α ∈ Γ
/// ―――――――――――――――――――― (K-VAR)
///    Γ ⊢ α : Binary
///
///
///     Γ ⊢ τ₁ : Binary     Γ ⊢ τ₂ : Binary
/// ―――――――――――――――――――――――――――――――――――――――――― (K-SUM)
///              Γ ⊢ τ₁ + τ₂ : Binary
///
///
///     Γ ⊢ τ₁ : Binary        Γ, x:τ₁ ⊢ τ₂ : Binary
/// ―――――――――――――――――――――――――――――――――――――――――――――――――――――― (K-DEPENDENT-PAIR)
///              Γ ⊢ Σ x:τ₁ .τ₂ : Binary
///
///
///     Γ ⊢ τ : Binary      Γ ⊢ e : UInt(n, E)
/// ―――――――――――――――――――――――――――――――――――――――――――――― (K-ARRAY-UINT)
///               Γ ⊢ [τ; e] : Binary
///
///
///    Γ ⊢ τ : Binary    Γ ⊢ e : RangedInt(n₁, n₂)    n₁ ≥ 0
/// ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――― (K-ARRAY-RANGED-INT)
///               Γ ⊢ [τ; e] : Binary
///
///
///     Γ ⊢ τ : Binary    Γ, x:τ ⊢ e : Bool
/// ―――――――――――――――――――――――――――――――――――――――――― (K-CON)
///           Γ ⊢ { x:τ | e } : Binary
/// ```
pub fn kind_of(env: &Env, ty: &Type) -> Result<Kind, KindError> {
    match *ty {
        // K-BOOL
        Type::Bool => Ok(Kind::Type),

        // K-RANGED-INT
        Type::RangedInt(lo, hi) if lo <= hi => Ok(Kind::Binary),
        Type::RangedInt(_, _) => unimplemented!(), // FIXME: Better errors

        // K-UINT
        Type::UInt(size, _) if size > 0 => Ok(Kind::Binary),
        Type::UInt(_, _) => unimplemented!(), // FIXME: Better errors

        // K-SINT
        Type::SInt(size, _) if size > 0 => Ok(Kind::Binary),
        Type::SInt(_, _) => unimplemented!(), // FIXME: Better errors

        // K-???
        Type::Float(_, _) => Ok(Kind::Binary),

        // K-VAR
        Type::Var(span, ref name) => {
            // TODO: kind of var? - expecting only binary types in the context
            match env.lookup_ty(name) {
                Some(_) => Ok(Kind::Binary),
                None => Err(KindError::UnboundType(span, name.clone())),
            }
        }

        // K-SUM
        Type::Union(_, ref tys) => {
            for ty in tys {
                match kind_of(env, &ty)? {
                    Kind::Type => unimplemented!(), // FIXME: Better errors
                    Kind::Binary => {}
                }
            }
            Ok(Kind::Binary)
        }

        // K-DEPENDENT-PAIR
        Type::Struct(_, ref fields) => {
            // TODO: prevent name shadowing?
            let mut inner_env = env.extend();
            for field in fields {
                match kind_of(&inner_env, &field.ty)? {
                    Kind::Type => unimplemented!(), // FIXME: Better errors
                    Kind::Binary => inner_env.add_binding(field.name.clone(), field.ty.clone()),
                }
            }
            Ok(Kind::Binary)
        }

        // K-ARRAY-...
        Type::Array(span, ref ty, ref size) => {
            match kind_of(env, ty)? {
                Kind::Type => unimplemented!(), // FIXME: Better errors
                Kind::Binary => {
                    let expr_ty = type_of(env, size)?;

                    match expr_ty {
                        // K-ARRAY-RANGED-INT
                        Type::RangedInt(lo, _) if lo >= 0 => Ok(Kind::Binary),
                        Type::RangedInt(_, _) => unimplemented!(), // FIXME: Better errors

                        // K-ARRAY-UINT
                        Type::UInt(_, _) => Ok(Kind::Binary),
                        ty => Err(KindError::ArraySizeExpectedUInt(span, ty)),
                    }
                }
            }
        }

        // K-CON
        Type::Where(span, ref ty, ref param, ref pred) => {
            match kind_of(env, ty)? {
                Kind::Type => unimplemented!(), // FIXME: Better errors
                Kind::Binary => {
                    let mut inner_env = env.extend();
                    // TODO: prevent name shadowing?
                    inner_env.add_binding(param.clone(), (**ty).clone());
                    match type_of(env, pred)? {
                        Type::Bool => Ok(Kind::Binary),
                        pred_ty => Err(KindError::WherePredicateExpectedBool(span, pred_ty)),
                    }
                }
            }
        }
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
/// ―――――――――――――――――――――――――――― (T-RANGED-INT)
///   Γ ⊢ n : RangedInt(n, n)
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
///     Γ ⊢ e : τ       isNumeric(τ)
/// ――――――――――――――――――――――――――――――――――― (T-NEG)
///              Γ ⊢ -e : τ
///
///
///      Γ ⊢ e₁ : Bool       Γ ⊢ e₂ : Bool
/// ――――――――――――――――――――――――――――――――――――――――― (T-REL)
///         Γ ⊢ op(Rel, e₁, e₂) : Bool
///
///
///   Γ ⊢ e₁ : τ₁     Γ ⊢ e₂ : τ₂      τ₁ <: τ₂      isNumeric(τ₂)
/// ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――― (T-CMP-LHS)
///                      Γ ⊢ op(Cmp, e₁, e₂) : Bool
///
///
///   Γ ⊢ e₁ : τ₁     Γ ⊢ e₂ : τ₂      τ₂ <: τ₁      isNumeric(τ₁)
/// ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――― (T-CMP-RHS)
///                      Γ ⊢ op(Cmp, e₁, e₂) : Bool
///
///
///   Γ ⊢ e₁ : τ₁    Γ ⊢ e₂ : τ₂      τ₁ <: τ₂      isNumeric(τ₂)
/// ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――― (T-ARITH-LHS)
///                    Γ ⊢ op(Arith, e₁, e₂) : τ₂
///
///
///   Γ ⊢ e₁ : τ₁    Γ ⊢ e₂ : τ₂      τ₂ <: τ₁      isNumeric(τ₁)
/// ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――― (T-ARITH-RHS)
///                    Γ ⊢ op(Arith, e₁, e₂) : τ₁
/// ```
pub fn type_of(env: &Env, expr: &Expr) -> Result<Type, TypeError> {
    match *expr {
        // T-TRUE, T-FALSE
        Expr::Const(_, Const::Bool(_)) => Ok(Type::Bool),

        // T-RANGED-INT
        Expr::Const(_, Const::Int(value)) => Ok(Type::RangedInt(value, value)),

        // T-VAR
        Expr::Var(span, ref name) => match env.lookup_binding(name) {
            Some(ty) => Ok(ty.clone()),
            None => Err(TypeError::UnboundVariable(span, name.clone())),
        },

        Expr::Unop(span, op, ref value) => {
            let value_ty = type_of(env, value)?;

            match op {
                // T-NOT
                Unop::Not => if value_ty == Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError::UnexpectedUnaryOperand(span, op, value_ty))
                },
                // T-NEG
                Unop::Neg => if is_numeric(&value_ty) {
                    Ok(value_ty)
                } else {
                    Err(TypeError::UnexpectedUnaryOperand(span, op, value_ty))
                },
            }
        }

        Expr::Binop(span, op, ref lhs, ref rhs) => {
            let lhs_ty = type_of(env, lhs)?;
            let rhs_ty = type_of(env, rhs)?;

            match op {
                // T-REL
                Binop::Or | Binop::And => if lhs_ty != Type::Bool {
                    Err(TypeError::UnexpectedBinaryLhs(span, lhs_ty))
                } else if rhs_ty != Type::Bool {
                    Err(TypeError::UnexpectedBinaryRhs(span, rhs_ty))
                } else {
                    Ok(Type::Bool)
                },
                // T-CMP-...
                Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Gt | Binop::Ge => {
                    // T-CMP-LHS
                    if is_subtype(&lhs_ty, &rhs_ty) && is_numeric(&rhs_ty) {
                        Ok(Type::Bool)
                    // T-CMP-RHS
                    } else if is_subtype(&rhs_ty, &lhs_ty) && is_numeric(&lhs_ty) {
                        Ok(Type::Bool)
                    } else {
                        Err(TypeError::ExpectedNumericOperands(span, lhs_ty, rhs_ty))
                    }
                }
                // T-ARITH-...
                // FIXME: These rules are incompatible with the way we formulated S-SINGLETON-INT
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
                    // T-ARITH-LHS
                    if is_subtype(&lhs_ty, &rhs_ty) && is_numeric(&rhs_ty) {
                        Ok(rhs_ty)
                    // T-ARITH-RHS
                    } else if is_subtype(&rhs_ty, &lhs_ty) && is_numeric(&lhs_ty) {
                        Ok(lhs_ty)
                    } else {
                        Err(TypeError::ExpectedNumericOperands(span, lhs_ty, rhs_ty))
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

    mod type_of {
        use super::*;

        mod add_expr {
            use super::*;

            #[test]
            fn uint_with_uint() {
                let mut env = Env::default();
                let len_ty = Type::UInt(32, Endianness::Target);
                env.add_binding("len", len_ty.clone());

                let expr = parser::parse_expr(&env, "len + len").unwrap();
                assert_eq!(type_of(&env, &expr), Ok(len_ty));
            }

            #[test]
            fn unknown_with_uint() {
                let mut env = Env::default();
                let len_ty = Type::UInt(32, Endianness::Target);
                env.add_binding("len", len_ty.clone());

                let expr = parser::parse_expr(&env, "1 + len").unwrap();
                assert_eq!(type_of(&env, &expr), Ok(len_ty.clone()));
            }

            #[test]
            fn uint_with_unknown() {
                let mut env = Env::default();
                let len_ty = Type::UInt(32, Endianness::Target);
                env.add_binding("len", len_ty.clone());

                let expr = parser::parse_expr(&env, "len + 1").unwrap();
                assert_eq!(type_of(&env, &expr), Ok(len_ty.clone()));
            }

            #[test]
            fn unknown_with_unknown() {
                let env = Env::default();
                let expr = parser::parse_expr(&env, "1 + 1").unwrap();

                assert_eq!(type_of(&env, &expr), Ok(Type::RangedInt(1, 1)));
            }
        }

        mod mul_expr {
            use super::*;

            #[test]
            fn uint_with_uint() {
                let mut env = Env::default();
                let len_ty = Type::UInt(32, Endianness::Target);
                env.add_binding("len", len_ty.clone());

                let expr = parser::parse_expr(&env, "len * len").unwrap();
                assert_eq!(type_of(&env, &expr), Ok(len_ty));
            }

            #[test]
            fn unknown_with_uint() {
                let mut env = Env::default();
                let len_ty = Type::UInt(32, Endianness::Target);
                env.add_binding("len", len_ty.clone());

                let expr = parser::parse_expr(&env, "1 * len").unwrap();
                assert_eq!(type_of(&env, &expr), Ok(len_ty.clone()));
            }

            #[test]
            fn uint_with_unknown() {
                let mut env = Env::default();
                let len_ty = Type::UInt(32, Endianness::Target);
                env.add_binding("len", len_ty.clone());

                let expr = parser::parse_expr(&env, "len * 1").unwrap();
                assert_eq!(type_of(&env, &expr), Ok(len_ty.clone()));
            }

            #[test]
            fn unknown_with_unknown() {
                let env = Env::default();
                let expr = parser::parse_expr(&env, "1 * 1").unwrap();

                assert_eq!(type_of(&env, &expr), Ok(Type::RangedInt(1, 1)));
            }
        }
    }

    mod kind_of {
        use super::*;

        mod const_ty {
            use super::*;

            #[test]
            fn ty_const() {
                let env = Env::default();
                let ty = Type::SInt(16, Endianness::Target);

                assert_eq!(kind_of(&env, &ty), Ok(Kind::Binary));
            }
        }

        mod var_ty {
            use super::*;

            #[test]
            fn defined() {
                let env = Env::default();
                let ty = parser::parse_ty(&env, "u8").unwrap();

                assert_eq!(kind_of(&env, &ty), Ok(Kind::Binary));
            }

            #[test]
            fn undefined() {
                let env = Env::default();
                let ty = parser::parse_ty(&env, "Foo").unwrap();

                assert_eq!(
                    kind_of(&env, &ty),
                    Err(KindError::UnboundType(
                        Span::new(B(0), B(3)),
                        "Foo".to_owned(),
                    ),)
                );
            }
        }

        mod union_ty {
            use super::*;

            #[test]
            fn simple() {
                let env = Env::default();
                let ty = parser::parse_ty(&env, "union { u8, u16, i32 }").unwrap();

                assert_eq!(kind_of(&env, &ty), Ok(Kind::Binary));
            }

            #[test]
            fn undefined_element() {
                let env = Env::default();
                let ty = parser::parse_ty(&env, "union { u8, Foo, i32 }").unwrap();

                assert_eq!(
                    kind_of(&env, &ty),
                    Err(KindError::UnboundType(
                        Span::new(B(12), B(15)),
                        "Foo".to_owned(),
                    ),)
                );
            }
        }

        mod struct_ty {
            use super::*;

            #[test]
            fn simple() {
                let env = Env::default();
                let ty = parser::parse_ty(&env, "struct { x: u8, y: u8 }").unwrap();

                assert_eq!(kind_of(&env, &ty), Ok(Kind::Binary));
            }

            #[test]
            fn dependent() {
                let env = Env::default();
                let ty = parser::parse_ty(&env, "struct { len: u8, data: [u8; len] }").unwrap();

                assert_eq!(kind_of(&env, &ty), Ok(Kind::Binary));
            }
        }

        mod array_ty {
            use super::*;

            #[test]
            fn constant_size() {
                let env = Env::default();
                let ty = parser::parse_ty(&env, "[u8; 16]").unwrap();

                assert_eq!(kind_of(&env, &ty), Ok(Kind::Binary));
            }

            #[test]
            fn constant_variable_size() {
                let mut env = Env::default();
                let len_ty = Type::UInt(32, Endianness::Target);
                env.add_binding("len", len_ty);
                let ty = parser::parse_ty(&env, "[u8; len]").unwrap();

                assert_eq!(kind_of(&env, &ty), Ok(Kind::Binary));
            }

            #[test]
            fn signed_int_size() {
                let mut env = Env::default();
                let len_ty = parser::parse_ty(&env, "i8").unwrap();
                env.add_binding("len", len_ty.clone());
                let ty = parser::parse_ty(&env, "[u8; len]").unwrap();

                assert_eq!(
                    kind_of(&env, &ty),
                    Err(KindError::ArraySizeExpectedUInt(
                        Span::new(B(0), B(9)),
                        len_ty,
                    ),)
                );
            }

            #[test]
            fn struct_size() {
                let mut env = Env::default();
                let len_ty = parser::parse_ty(&env, "struct {}").unwrap();
                env.add_binding("len", len_ty.clone());
                let ty = parser::parse_ty(&env, "[u8; len]").unwrap();

                assert_eq!(
                    kind_of(&env, &ty),
                    Err(KindError::ArraySizeExpectedUInt(
                        Span::new(B(0), B(9)),
                        len_ty,
                    ),)
                );
            }
        }
    }
}
