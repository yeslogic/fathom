//! The syntax of our data description language

use std::fmt;
use std::rc::Rc;

use name::Named;
use source::Span;
use syntax::ast::{self, Field, Substitutions};
use syntax::parser::ast::host::Expr as ParseExpr;
use var::{ScopeIndex, Var};

/// Kinds of host type
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Kind {
    /// Kind of types
    Type,
    /// Kind of type functions
    ///
    /// For now we only allow type arguments of kind `Type`. We represent this
    /// as an arity count
    Arrow { arity: u32 },
}

impl Kind {
    /// Kind of type functions
    pub fn arrow(arity: u32) -> Kind {
        Kind::Arrow { arity }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FloatType {
    /// IEE-754 32-bit float
    F32,
    /// IEE-754 64-bit float
    F64,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SignedType {
    /// Signed 8-bit integer
    I8,
    /// Signed 16-bit integer
    I16,
    /// Signed 24-bit integer
    I24,
    /// Signed 32-bit integer
    I32,
    /// Signed 64-bit integer
    I64,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnsignedType {
    /// Unsigned 8-bit integer
    U8,
    /// Unsigned 16-bit integer
    U16,
    /// Unsigned 24-bit integer
    U24,
    /// Unsigned 32-bit integer
    U32,
    /// Unsigned 64-bit integer
    U64,
}

/// A type constant in the host language
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeConst {
    /// Unit
    Unit,
    /// Bottom
    Bottom,
    /// Boolean
    Bool,
    /// Float
    Float(FloatType),
    /// Signed Integers
    Signed(SignedType),
    /// Unsigned Integers
    Unsigned(UnsignedType),
}

/// A host type
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// A type variable: eg. `T`
    Var(Var),
    /// A type constant
    Const(TypeConst),
    /// Arrow type: eg. `(T, ..) -> U`
    Arrow(Vec<RcType>, RcType),
    /// An array, eg. `[T]`
    Array(RcType),
    /// A union of types: eg. `union { variant : T, ... }`
    Union(Vec<Field<RcType>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Vec<Field<RcType>>),
    /// Type abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    Lam(Vec<Named<()>>, RcType),
    /// Type application: eg. `T(U, V)`
    App(RcType, Vec<RcType>),
}

#[derive(Clone, PartialEq)]
pub struct RcType {
    pub inner: Rc<Type>,
}

impl From<Type> for RcType {
    fn from(src: Type) -> RcType {
        RcType {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl RcType {
    /// Type abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    pub fn abs<T1>(param_names: &[&str], body_ty: T1) -> RcType
    where
        T1: Into<RcType>,
    {
        let params = param_names
            .iter()
            .map(|&name| Named(String::from(name), ()))
            .collect();

        let mut body_ty = body_ty.into();
        body_ty.abstract_names(param_names);

        Type::Lam(params, body_ty).into()
    }

    /// Attempt to lookup the type of a field
    ///
    /// Returns `None` if the type is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &str) -> Option<&RcType> {
        match *self.inner {
            Type::Struct(ref fields) => ast::lookup_field(fields, name),
            _ => None,
        }
    }

    /// Attempt to lookup the type of a variant
    ///
    /// Returns `None` if the type is not a union or the field is not
    /// present in the union.
    pub fn lookup_variant(&self, name: &str) -> Option<&RcType> {
        match *self.inner {
            Type::Union(ref variants) => ast::lookup_field(variants, name),
            _ => None,
        }
    }

    /// Replace occurrences of the free variables that exist as keys on
    /// `substs` with their corresponding types.
    pub fn substitute(&mut self, substs: &Substitutions) {
        *self = match *Rc::make_mut(&mut self.inner) {
            Type::Var(Var::Free(ref name)) => match substs.get(name) {
                None => return,
                Some(ty) => ty.repr().clone(),
            },
            Type::Var(Var::Bound(_)) | Type::Const(_) => return,
            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    param_ty.substitute(substs);
                }
                ret_ty.substitute(substs);

                return;
            }
            Type::Array(ref mut elem_ty) => {
                elem_ty.substitute(substs);
                return;
            }
            Type::Union(ref mut variants) => {
                for variant in variants {
                    variant.value.substitute(substs);
                }
                return;
            }
            Type::Struct(ref mut fields) => {
                for field in fields {
                    field.value.substitute(substs);
                }
                return;
            }
            Type::Lam(_, ref mut body_ty) => {
                body_ty.substitute(substs);
                return;
            }
            Type::App(ref mut fn_ty, ref mut arg_tys) => {
                fn_ty.substitute(substs);

                for arg_ty in arg_tys {
                    arg_ty.substitute(substs);
                }

                return;
            }
        };
    }

    pub fn abstract_names_at(&mut self, names: &[&str], scope: ScopeIndex) {
        match *Rc::make_mut(&mut self.inner) {
            Type::Var(ref mut var) => var.abstract_names_at(names, scope),
            Type::Const(_) => {}
            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    param_ty.abstract_names_at(names, scope);
                }
                ret_ty.abstract_names_at(names, scope);
            }
            Type::Array(ref mut elem_ty) => {
                elem_ty.abstract_names_at(names, scope);
            }
            Type::Union(ref mut variants) => for variant in variants {
                variant.value.abstract_names_at(names, scope);
            },
            Type::Struct(ref mut fields) => for field in fields {
                field.value.abstract_names_at(names, scope);
            },
            Type::Lam(_, ref mut body_ty) => {
                body_ty.abstract_names_at(names, scope.succ());
            }
            Type::App(ref mut fn_ty, ref mut arg_tys) => {
                fn_ty.abstract_names_at(names, scope);

                for arg_ty in arg_tys {
                    arg_ty.abstract_names_at(names, scope);
                }
            }
        }
    }

    /// Add one layer of abstraction around the type by replacing all the
    /// free variables in `names` with an appropriate De Bruijn index.
    ///
    /// This results in a one 'dangling' index, and so care must be taken
    /// to wrap it in another type that marks the introduction of a new
    /// scope.
    pub fn abstract_names(&mut self, names: &[&str]) {
        self.abstract_names_at(names, ScopeIndex(0))
    }

    fn instantiate_at(&mut self, scope: ScopeIndex, tys: &[RcType]) {
        // FIXME: ensure that expressions are not bound at the same scope
        *self = match *Rc::make_mut(&mut self.inner) {
            Type::Var(Var::Bound(Named(_, var))) if var.scope == scope => {
                tys[var.binding.0 as usize].clone()
            }
            Type::Var(Var::Bound(_)) | Type::Var(Var::Free(_)) | Type::Const(_) => return,
            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    param_ty.instantiate_at(scope, tys);
                }

                ret_ty.instantiate_at(scope, tys);
                return;
            }
            Type::Array(ref mut elem_ty) => {
                elem_ty.instantiate_at(scope, tys);
                return;
            }
            Type::Union(ref mut variants) => {
                for variant in variants {
                    variant.value.instantiate_at(scope, tys);
                }
                return;
            }
            Type::Struct(ref mut fields) => {
                for field in fields {
                    field.value.instantiate_at(scope, tys);
                }
                return;
            }
            Type::Lam(_, ref mut ty) => {
                ty.instantiate_at(scope.succ(), tys);
                return;
            }
            Type::App(ref mut ty, ref mut arg_tys) => {
                ty.instantiate_at(scope, tys);

                for arg_ty in arg_tys {
                    arg_ty.instantiate_at(scope, tys);
                }

                return;
            }
        };
    }

    /// Remove one layer of abstraction in the type by replacing the
    /// appropriate bound variables with copies of `ty`.
    pub fn instantiate(&mut self, tys: &[RcType]) {
        self.instantiate_at(ScopeIndex(0), tys);
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntSuffix {
    Signed(SignedType),
    Unsigned(UnsignedType),
}

#[derive(Copy, Clone, PartialEq)]
pub enum Const {
    /// A boolean constant: eg. `true`, `false`
    Bool(bool),
    /// An integer constant: eg. `0u8`, `1i64`, `2i16`, ...
    Int(u64, IntSuffix),
    /// A floating point constant: eg. `0f32`, `1.32f64`, ...
    Float(f64, FloatType),
}

impl Const {
    pub fn ty_const_of(self) -> TypeConst {
        match self {
            Const::Bool(_) => TypeConst::Bool,
            Const::Int(_, IntSuffix::Unsigned(suffix)) => TypeConst::Unsigned(suffix),
            Const::Int(_, IntSuffix::Signed(suffix)) => TypeConst::Signed(suffix),
            Const::Float(_, suffix) => TypeConst::Float(suffix),
        }
    }
}

impl fmt::Debug for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Const::Bool(value) => write!(f, "Bool({:?})", value),
            Const::Int(value, suffix) => write!(f, "Int({:?}, {:?})", value, suffix),
            Const::Float(value, suffix) => write!(f, "Float({:?}, {:?})", value, suffix),
        }
    }
}

/// An unary operator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Unop {
    /// Not: eg. `!x`
    Not,
    /// Negation: eg. `-x`
    Neg,
}

/// A binary operator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Binop {
    /// Disjunction: eg. `x | y`
    Or,
    /// Conjunction: eg. `x & y`
    And,
    /// Equality: eg. `x == y`
    Eq,
    /// Inequality: eg. `x != y`
    Ne,
    /// Less than or equal: eg. `x <= y`
    Le,
    /// Less than: eg. `x < y`
    Lt,
    /// Greater than: eg. `x > y`
    Gt,
    /// Greater than or equal: eg. `x >= y`
    Ge,
    /// Addition: eg. `x + y`
    Add,
    /// Subtraction: eg. `x - y`
    Sub,
    /// Multiplication: eg. `x * y`
    Mul,
    /// Division: eg. `x / y`
    Div,
}

/// A host expression
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A constant value
    Const(Span, Const),
    /// A variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    Var(Span, Var),
    /// An unary operator expression
    Unop(Span, Unop, RcExpr),
    /// A binary operator expression
    Binop(Span, Binop, RcExpr, RcExpr),
    /// A struct initialization expression
    Struct(Vec<Field<RcExpr>>),
    /// Field projection, eg: `x.field`
    Proj(Span, RcExpr, String),
    /// Variant introduction, eg: `.variant1 x : union { variant1 : T }`
    ///
    /// We require a type annotation because we don't have inference
    /// implemented in the type checker yet.
    //
    // TODO: add type inference to remove the need for this annotation
    Intro(Span, String, RcExpr, RcType),
    /// Array index, eg: `x[i]`
    Subscript(Span, RcExpr, RcExpr),
    /// Cast expression, eg: `x as u32`
    Cast(Span, RcExpr, RcType),
    /// Abstraction, eg: `\(x : T, ..) -> x`
    Lam(Span, Vec<Named<RcType>>, RcExpr),
    /// Application, eg: `f(x, ..)`
    App(Span, RcExpr, Vec<RcExpr>),
}

#[derive(Clone, PartialEq)]
pub struct RcExpr {
    pub inner: Rc<Expr>,
}

impl From<Expr> for RcExpr {
    fn from(src: Expr) -> RcExpr {
        RcExpr {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl RcExpr {
    /// Abstraction, eg: `\(x : T, ..) -> x`
    pub fn abs<E1>(span: Span, params: Vec<Named<RcType>>, body_expr: E1) -> RcExpr
    where
        E1: Into<RcExpr>,
    {
        let mut body_expr = body_expr.into();

        {
            let param_names = params.iter().map(|param| &*param.0).collect::<Vec<_>>();
            body_expr.abstract_names(&param_names[..]);
        }

        Expr::Lam(span, params, body_expr).into()
    }

    /// Attempt to lookup the value of a field
    ///
    /// Returns `None` if the expression is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &str) -> Option<&RcExpr> {
        match *self.inner {
            Expr::Struct(ref fields) => ast::lookup_field(fields, name),
            _ => None,
        }
    }

    /// Replace occurrences of the free variables that exist as keys on
    /// `substs` with their corresponding types.
    pub fn substitute(&mut self, substs: &Substitutions) {
        match *Rc::make_mut(&mut self.inner) {
            Expr::Var(_, Var::Free(ref name)) => match substs.get(name) {
                None => {}
                Some(ty) => panic!("Expected to substitute an expression, but found {:?}", ty),
            },
            Expr::Var(_, Var::Bound(_)) | Expr::Const(_, _) => {}
            Expr::Unop(_, _, ref mut expr) | Expr::Proj(_, ref mut expr, _) => {
                expr.substitute(substs);
            }
            Expr::Intro(_, _, ref mut expr, ref mut ty) => {
                expr.substitute(substs);
                ty.substitute(substs);
            }
            Expr::Binop(_, _, ref mut lhs_expr, ref mut rhs_expr) => {
                lhs_expr.substitute(substs);
                rhs_expr.substitute(substs);
            }
            Expr::Struct(ref mut fields) => for field in fields {
                field.value.substitute(substs);
            },
            Expr::Subscript(_, ref mut array_expr, ref mut index_expr) => {
                array_expr.substitute(substs);
                index_expr.substitute(substs);
            }
            Expr::Cast(_, ref mut src_expr, ref mut dst_ty) => {
                src_expr.substitute(substs);
                dst_ty.substitute(substs);
            }
            Expr::Lam(_, ref mut args, ref mut body_expr) => {
                for &mut Named(_, ref mut arg_ty) in args {
                    arg_ty.substitute(substs);
                }

                body_expr.substitute(substs);
            }
            Expr::App(_, ref mut fn_expr, ref mut arg_exprs) => {
                fn_expr.substitute(substs);

                for arg_expr in arg_exprs {
                    arg_expr.substitute(substs);
                }
            }
        }
    }

    pub fn abstract_names_at(&mut self, names: &[&str], scope: ScopeIndex) {
        match *Rc::make_mut(&mut self.inner) {
            Expr::Var(_, ref mut var) => var.abstract_names_at(names, scope),
            Expr::Const(_, _) => {}
            Expr::Unop(_, _, ref mut expr) | Expr::Proj(_, ref mut expr, _) => {
                expr.abstract_names_at(names, scope);
            }
            Expr::Intro(_, _, ref mut expr, ref mut ty) => {
                expr.abstract_names_at(names, scope);
                ty.abstract_names_at(names, scope);
            }
            Expr::Binop(_, _, ref mut lhs_expr, ref mut rhs_expr) => {
                lhs_expr.abstract_names_at(names, scope);
                rhs_expr.abstract_names_at(names, scope);
            }
            Expr::Struct(ref mut fields) => for field in fields {
                field.value.abstract_names_at(names, scope);
            },
            Expr::Subscript(_, ref mut array_expr, ref mut index_expr) => {
                array_expr.abstract_names_at(names, scope);
                index_expr.abstract_names_at(names, scope);
            }
            Expr::Cast(_, ref mut src_expr, _) => {
                src_expr.abstract_names_at(names, scope);
                // TODO: abstract dst_ty???
            }
            Expr::Lam(_, ref mut args, ref mut body_expr) => {
                for &mut Named(_, ref mut arg_ty) in args {
                    arg_ty.abstract_names_at(names, scope);
                }

                body_expr.abstract_names_at(names, scope.succ());
            }
            Expr::App(_, ref mut fn_expr, ref mut arg_exprs) => {
                fn_expr.abstract_names_at(names, scope);

                for arg_expr in arg_exprs {
                    arg_expr.abstract_names_at(names, scope);
                }
            }
        }
    }

    pub fn abstract_names(&mut self, names: &[&str]) {
        self.abstract_names_at(names, ScopeIndex(0));
    }
}

impl<'src> From<&'src ParseExpr<'src>> for RcExpr {
    fn from(src: &'src ParseExpr<'src>) -> RcExpr {
        match *src {
            ParseExpr::Const(span, c) => Expr::Const(span, c).into(),
            ParseExpr::Var(span, name) => Expr::Var(span, Var::free(name)).into(),
            ParseExpr::Unop(span, op, ref expr) => {
                let expr = RcExpr::from(&**expr);

                Expr::Unop(span, op, expr).into()
            }
            ParseExpr::Binop(span, op, ref lhs_expr, ref rhs_expr) => {
                let lhs_expr = RcExpr::from(&**lhs_expr);
                let rhs_expr = RcExpr::from(&**rhs_expr);

                Expr::Binop(span, op, lhs_expr, rhs_expr).into()
            }
            ParseExpr::Proj(span, ref struct_expr, field_name) => {
                let struct_expr = RcExpr::from(&**struct_expr);
                let field_name = String::from(field_name);

                Expr::Proj(span, struct_expr, field_name).into()
            }
            ParseExpr::Subscript(span, ref array_expr, ref index_expr) => {
                let array_expr = RcExpr::from(&**array_expr);
                let index_expr = RcExpr::from(&**index_expr);

                Expr::Subscript(span, array_expr, index_expr).into()
            }
            ParseExpr::Cast(span, ref expr, ty) => {
                let expr = RcExpr::from(&**expr);
                let ty = Type::Const(ty).into();

                Expr::Cast(span, expr, ty).into()
            }
        }
    }
}
