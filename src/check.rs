use std::collections::HashMap;

use ast::{Expr, Kind, Type, TypeConst};
use source::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    UnboundType(Span, String),
    ExpectedUnsignedIntInArraySizeExpr(Span, Type),
    FailedToEvaluateArraySize(Span, ExprError),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprError {
    UnboundVariable(Span, String),
}

/// An environment of bindings and types
///
/// ```plain
/// Γ ::=
///         ·           empty environment
///         Γ, x:σ      environment extension
/// ```
#[derive(Debug)]
pub struct Env<'a> {
    parent: Option<&'a Env<'a>>,
    tys: HashMap<String, Type>,
    bindings: HashMap<String, Type>,
}

impl<'a> Env<'a> {
    pub fn new() -> Env<'static> {
        Env {
            parent: None,
            tys: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn extend(&self) -> Env {
        Env {
            parent: Some(self),
            tys: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn add_ty<S>(&mut self, name: S, ty: Type)
    where
        S: Into<String>,
    {
        self.tys.insert(name.into(), ty);
    }

    pub fn add_binding<S>(&mut self, name: S, ty: Type)
    where
        S: Into<String>,
    {
        self.bindings.insert(name.into(), ty);
    }

    pub fn lookup_ty(&self, name: &str) -> Option<&Type> {
        self.tys.get(name).or_else(|| {
            self.parent.and_then(|env| env.lookup_ty(name))
        })
    }

    pub fn lookup_binding(&self, name: &str) -> Option<&Type> {
        self.bindings.get(name).or_else(|| {
            self.parent.and_then(|env| env.lookup_binding(name))
        })
    }

    /// Check that the given definitions specify a valid data format
    ///
    /// This implements the `Γ ⊢ τ : κ` judgement rule, as described below
    ///
    /// # Syntax
    ///
    /// ## Kinds
    ///
    /// ```plain
    /// κ ::=
    ///         Type        kind of types
    /// ```
    ///
    /// ## Terms
    ///
    /// ```plain
    /// τ ::=
    ///         c                   type constants
    ///         α                   variables
    ///         τ₁ + τ₂             sum
    ///         Σ x:τ₁ .τ₂          dependent pair
    ///         [τ; e]              array
    /// ```
    ///
    /// In the `ast`, we represent the above as the following:
    ///
    /// - `Type::Ident`: variables
    ///
    /// - `Type::Union`: series of unions
    ///
    /// - `Type::Struct`: nested dependent pairs
    ///
    ///   For example, the record:
    ///
    ///   ```plain
    ///   { len : u16, reserved : u16, data : [u16; len] }
    ///   ```
    ///
    ///   Would be desugared into:
    ///
    ///   ```plain
    ///   Σ len:u16 . Σ reserved:u16 . [u16; len]
    ///   ```
    ///
    ///   Note how later fields have access to the data in previous fields.
    ///
    /// - `Type::Array`: TODO
    ///
    /// ## Expressions
    ///
    /// ```plain
    /// e ::=
    ///         x           variables
    ///         n           integer number
    /// ```
    ///
    /// # Judgments
    ///
    /// ## `Γ ⊢ τ : κ`
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
    /// ```
    pub fn check_ty(&self, ty: &Type) -> Result<Kind, TypeError> {
        match *ty {
            // CONST
            Type::Const(_, _) => Ok(Kind::Type), // Easypeasy

            // VAR
            Type::Ident(span, ref ident) => {
                // TODO: kind of ident?
                // α ∈ Γ
                match self.lookup_ty(ident) {
                    Some(_) => Ok(Kind::Type),
                    None => Err(TypeError::UnboundType(span, ident.clone())),
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
        }
    }

    /// # `Γ ⊢ e : τ`
    pub fn check_expr(&self, expr: &Expr) -> Result<Type, ExprError> {
        match *expr {
            Expr::Const(_, _) => Ok(Type::Const(Span::start(), TypeConst::U32)), // FIXME
            Expr::Var(span, ref name) => {
                match self.lookup_binding(name) {
                    Some(ty) => Ok(ty.clone()),
                    None => Err(ExprError::UnboundVariable(span, name.clone())),
                }
            }
        }
    }
}

impl Default for Env<'static> {
    fn default() -> Env<'static> {
        let mut env = Env::new();
        env.add_ty("u8", Type::Const(Span::start(), TypeConst::U8));
        env.add_ty("u16", Type::Const(Span::start(), TypeConst::U16));
        env.add_ty("u32", Type::Const(Span::start(), TypeConst::U32));
        env.add_ty("u64", Type::Const(Span::start(), TypeConst::U64));
        env.add_ty("i8", Type::Const(Span::start(), TypeConst::I8));
        env.add_ty("i16", Type::Const(Span::start(), TypeConst::I16));
        env.add_ty("i32", Type::Const(Span::start(), TypeConst::I32));
        env.add_ty("i64", Type::Const(Span::start(), TypeConst::I64));
        env
    }
}

#[cfg(test)]
pub mod tests {
    use parser;
    use source::BytePos as B;
    use super::*;

    #[test]
    fn ty_const() {
        let env = Env::default();
        let ty = Type::Const(Span::start(), TypeConst::I16);

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn ident() {
        let env = Env::default();
        let ty = parser::parse_ty("u8").unwrap();

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn ident_missing() {
        let env = Env::default();
        let ty = parser::parse_ty("Foo").unwrap();

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
        let ty = parser::parse_ty("u8 | u16 | i32").unwrap();

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn union_element_missing() {
        let env = Env::default();
        let ty = parser::parse_ty("u8 | Foo | i32").unwrap();

        assert_eq!(
            env.check_ty(&ty),
            Err(TypeError::UnboundType(
                Span::new(B(5), B(8)),
                "Foo".to_owned(),
            ))
        );
    }

    #[test]
    fn pair() {
        let env = Env::default();
        let ty = parser::parse_ty("{ x: u8, y: u8 }").unwrap();

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn dependent_pair() {
        let env = Env::default();
        let ty = parser::parse_ty("{ len: u8, data: [u8; len] }").unwrap();

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn array() {
        let env = Env::default();
        let ty = parser::parse_ty("[u8; 16]").unwrap();

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn array_len() {
        let mut env = Env::default();
        env.add_binding("len", Type::Const(Span::start(), TypeConst::U32));
        let ty = parser::parse_ty("[u8; len]").unwrap();

        assert_eq!(env.check_ty(&ty), Ok(Kind::Type));
    }

    #[test]
    fn array_bad_type() {
        let mut env = Env::default();
        let len_ty = Type::struct_(Span::start(), vec![]);
        env.add_binding("len", len_ty.clone());
        let ty = parser::parse_ty("[u8; len]").unwrap();

        assert_eq!(
            env.check_ty(&ty),
            Err(TypeError::ExpectedUnsignedIntInArraySizeExpr(
                Span::new(B(0), B(9)),
                len_ty,
            ))
        );
    }
}
