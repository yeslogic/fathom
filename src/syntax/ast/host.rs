//! The syntax of our data description language

use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

use name::{Name, Named};
use source::Span;
use syntax::ast::{self, Field};
use var::{BoundVar, ScopeIndex, Var};

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
pub enum IntSuffix {
    Signed(SignedType),
    Unsigned(UnsignedType),
}

impl FromStr for IntSuffix {
    type Err = ParseIntSuffixError;

    fn from_str(src: &str) -> Result<IntSuffix, ParseIntSuffixError> {
        match src {
            "u8" => Ok(IntSuffix::Unsigned(UnsignedType::U8)),
            "u16" => Ok(IntSuffix::Unsigned(UnsignedType::U16)),
            "u24" => Ok(IntSuffix::Unsigned(UnsignedType::U24)),
            "u32" => Ok(IntSuffix::Unsigned(UnsignedType::U32)),
            "u64" => Ok(IntSuffix::Unsigned(UnsignedType::U64)),
            "i8" => Ok(IntSuffix::Signed(SignedType::I8)),
            "i16" => Ok(IntSuffix::Signed(SignedType::I16)),
            "i32" => Ok(IntSuffix::Signed(SignedType::I32)),
            "i64" => Ok(IntSuffix::Signed(SignedType::I64)),
            "" => Err(ParseIntSuffixError::Missing),
            _ => Err(ParseIntSuffixError::Invalid),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParseIntSuffixError {
    Invalid,
    Missing,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Const {
    /// A boolean constant: eg. `true`, `false`
    Bool(bool),
    /// An integer constant: eg. `0u8`, `1i64`, `2i16`, ...
    Int(u64, IntSuffix),
}

impl Const {
    pub fn ty_const_of(self) -> TypeConst {
        match self {
            Const::Bool(_) => TypeConst::Bool,
            Const::Int(_, IntSuffix::Unsigned(suffix)) => TypeConst::Unsigned(suffix),
            Const::Int(_, IntSuffix::Signed(suffix)) => TypeConst::Signed(suffix),
        }
    }
}

impl fmt::Debug for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Const::Bool(value) => write!(f, "Bool({:?})", value),
            Const::Int(value, suffix) => write!(f, "Int({:?}, {:?})", value, suffix),
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<N> {
    /// A constant value
    Const(Span, Const),
    /// Primitive expressions
    Prim(&'static str, RcType<N>),
    /// A variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    Var(Span, Var<N>),
    /// An unary operator expression
    Unop(Span, Unop, RcExpr<N>),
    /// A binary operator expression
    Binop(Span, Binop, RcExpr<N>, RcExpr<N>),
    /// A struct initialization expression
    Struct(Vec<Field<N, RcExpr<N>>>),
    /// Field projection, eg: `x.field`
    Proj(Span, RcExpr<N>, N),
    /// Variant introduction, eg: `.variant1 x : union { variant1 : T }`
    ///
    /// We require a type annotation because we don't have inference
    /// implemented in the type checker yet.
    //
    // TODO: add type inference to remove the need for this annotation
    Intro(Span, N, RcExpr<N>, RcType<N>),
    /// Array index, eg: `x[i]`
    Subscript(Span, RcExpr<N>, RcExpr<N>),
    /// Abstraction, eg: `\(x : T, ..) -> x`
    Abs(Span, Vec<Named<N, RcType<N>>>, RcExpr<N>),
    /// Application, eg: `f(x, ..)`
    App(Span, RcExpr<N>, Vec<RcExpr<N>>),
}

pub type RcExpr<N> = Rc<Expr<N>>;

impl<N: Name> Expr<N> {
    /// A boolean constant: eg. `true`, `false`
    pub fn bool(span: Span, value: bool) -> Expr<N> {
        Expr::Const(span, Const::Bool(value))
    }

    /// Primitive expressions
    pub fn prim<T1: Into<RcType<N>>>(name: &'static str, repr_ty: T1) -> Expr<N> {
        Expr::Prim(name, repr_ty.into())
    }

    /// A free variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    pub fn fvar<N1: Into<N>>(span: Span, x: N1) -> Expr<N> {
        Expr::Var(span.into(), Var::free(x))
    }

    /// A bound variable
    pub fn bvar<N1: Into<N>>(span: Span, x: N1, var: BoundVar) -> Expr<N> {
        Expr::Var(span.into(), Var::bound(x, var))
    }

    /// An unary operator expression
    pub fn unop<E1: Into<RcExpr<N>>>(span: Span, op: Unop, x: E1) -> Expr<N> {
        Expr::Unop(span, op, x.into())
    }

    /// A binary operator expression
    pub fn binop<E1, E2>(span: Span, op: Binop, x: E1, y: E2) -> Expr<N>
    where
        E1: Into<RcExpr<N>>,
        E2: Into<RcExpr<N>>,
    {
        Expr::Binop(span, op, x.into(), y.into())
    }

    /// A binary operator expression
    pub fn struct_(fields: Vec<Field<N, RcExpr<N>>>) -> Expr<N> {
        Expr::Struct(fields)
    }

    /// Field projection, eg: `x.field`
    pub fn proj<E1, N1>(span: Span, expr: E1, field_name: N1) -> Expr<N>
    where
        E1: Into<RcExpr<N>>,
        N1: Into<N>,
    {
        Expr::Proj(span, expr.into(), field_name.into())
    }

    /// Variant introduction, eg: `.variant1 x : union { variant1 : T }`
    ///
    /// We require a type annotation because we don't have inference
    /// implemented in the type checker yet.
    pub fn intro<N1, E1, T1>(span: Span, variant_name: N1, expr: E1, union_ty: T1) -> Expr<N>
    where
        N1: Into<N>,
        E1: Into<RcExpr<N>>,
        T1: Into<RcType<N>>,
    {
        Expr::Intro(span, variant_name.into(), expr.into(), union_ty.into())
    }

    /// Array subscript, eg: `x[i]`
    pub fn subscript<E1, E2>(span: Span, expr: E1, index_expr: E2) -> Expr<N>
    where
        E1: Into<RcExpr<N>>,
        E2: Into<RcExpr<N>>,
    {
        Expr::Subscript(span, expr.into(), index_expr.into())
    }

    /// Abstraction, eg: `\(x : T, ..) -> x`
    pub fn abs<E1>(span: Span, params: Vec<Named<N, RcType<N>>>, body_expr: E1) -> Expr<N>
    where
        E1: Into<RcExpr<N>>,
    {
        let param_names = params
            .iter()
            .map(|param| param.0.clone())
            .collect::<Vec<_>>();

        let mut body_expr = body_expr.into();
        Rc::make_mut(&mut body_expr).abstract_names(&param_names);

        Expr::Abs(span, params, body_expr)
    }

    /// Application, eg: `f(x, ..)`
    pub fn app<E1>(span: Span, fn_expr: E1, arg_exprs: Vec<RcExpr<N>>) -> Expr<N>
    where
        E1: Into<RcExpr<N>>,
    {
        Expr::App(span, fn_expr.into(), arg_exprs)
    }

    /// Attempt to lookup the value of a field
    ///
    /// Returns `None` if the expression is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &N) -> Option<&RcExpr<N>> {
        match *self {
            Expr::Struct(ref fields) => ast::lookup_field(fields, name),
            _ => None,
        }
    }

    pub fn abstract_names_at(&mut self, names: &[N], scope: ScopeIndex) {
        match *self {
            Expr::Var(_, ref mut var) => var.abstract_names_at(names, scope),
            Expr::Const(_, _) => {}
            Expr::Prim(_, ref mut repr_ty) => Rc::make_mut(repr_ty).abstract_names_at(names, scope),
            Expr::Unop(_, _, ref mut expr) | Expr::Proj(_, ref mut expr, _) => {
                Rc::make_mut(expr).abstract_names_at(names, scope);
            }
            Expr::Intro(_, _, ref mut expr, ref mut ty) => {
                Rc::make_mut(expr).abstract_names_at(names, scope);
                Rc::make_mut(ty).abstract_names_at(names, scope);
            }
            Expr::Binop(_, _, ref mut lhs_expr, ref mut rhs_expr) => {
                Rc::make_mut(lhs_expr).abstract_names_at(names, scope);
                Rc::make_mut(rhs_expr).abstract_names_at(names, scope);
            }
            Expr::Struct(ref mut fields) => for field in fields {
                Rc::make_mut(&mut field.value).abstract_names_at(names, scope);
            },
            Expr::Subscript(_, ref mut array_expr, ref mut index_expr) => {
                Rc::make_mut(array_expr).abstract_names_at(names, scope);
                Rc::make_mut(index_expr).abstract_names_at(names, scope);
            }
            Expr::Abs(_, ref mut args, ref mut body_expr) => {
                for &mut Named(_, ref mut arg_ty) in args {
                    Rc::make_mut(arg_ty).abstract_names_at(names, scope);
                }

                Rc::make_mut(body_expr).abstract_names_at(names, scope.succ());
            }
            Expr::App(_, ref mut fn_expr, ref mut arg_exprs) => {
                Rc::make_mut(fn_expr).abstract_names_at(names, scope);

                for arg_expr in arg_exprs {
                    Rc::make_mut(arg_expr).abstract_names_at(names, scope);
                }
            }
        }
    }

    pub fn abstract_names(&mut self, names: &[N]) {
        self.abstract_names_at(names, ScopeIndex(0));
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FloatType {
    /// 32-bit float
    F32,
    /// 64-bit float
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<N> {
    /// A type variable: eg. `T`
    Var(Var<N>),
    /// A type constant
    Const(TypeConst),
    /// Arrow type: eg. `(T, ..) -> U`
    Arrow(Vec<RcType<N>>, RcType<N>),
    /// An array, eg. `[T]`
    Array(RcType<N>),
    /// A union of types: eg. `union { variant : T, ... }`
    Union(Vec<Field<N, RcType<N>>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Vec<Field<N, RcType<N>>>),
    /// Type abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    Abs(Vec<Named<N, ()>>, RcType<N>),
    /// Type application: eg. `T(U, V)`
    App(RcType<N>, Vec<RcType<N>>),
}

pub type RcType<N> = Rc<Type<N>>;

impl<N: Name> Type<N> {
    /// A free type variable: eg. `T`
    pub fn fvar<N1: Into<N>>(name: N1) -> Type<N> {
        Type::Var(Var::free(name))
    }

    /// A bound type variable
    pub fn bvar<N1: Into<N>>(name: N1, var: BoundVar) -> Type<N> {
        Type::Var(Var::bound(name, var))
    }

    /// Boolean type constant
    pub fn bool() -> Type<N> {
        Type::Const(TypeConst::Bool)
    }

    /// Arrow type: eg. `(T, ..) -> U`
    pub fn arrow<T1>(param_tys: Vec<RcType<N>>, ret_ty: T1) -> Type<N>
    where
        T1: Into<RcType<N>>,
    {
        Type::Arrow(param_tys, ret_ty.into())
    }

    /// An array of the specified type, with a size: eg. `[T; n]`
    pub fn array<T1: Into<RcType<N>>>(elem_ty: T1) -> Type<N> {
        Type::Array(elem_ty.into())
    }

    /// A union of types: eg. `union { T, ... }`
    pub fn union(variants: Vec<Field<N, RcType<N>>>) -> Type<N> {
        Type::Union(variants)
    }

    /// A struct type, with fields: eg. `struct { field : T, ... }`
    pub fn struct_(fields: Vec<Field<N, RcType<N>>>) -> Type<N> {
        Type::Struct(fields)
    }

    /// Type abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    pub fn abs<T1>(param_names: &[N], body_ty: T1) -> Type<N>
    where
        T1: Into<RcType<N>>,
    {
        let params = param_names
            .iter()
            .map(|name| Named(name.clone(), ()))
            .collect();

        let mut body_ty = body_ty.into();
        Rc::make_mut(&mut body_ty).abstract_names(&param_names);

        Type::Abs(params, body_ty)
    }

    /// Type application: eg. `T(U, V)`
    pub fn app<T1>(fn_ty: T1, arg_tys: Vec<RcType<N>>) -> Type<N>
    where
        T1: Into<RcType<N>>,
    {
        Type::App(fn_ty.into(), arg_tys)
    }

    /// Attempt to lookup the type of a field
    ///
    /// Returns `None` if the type is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &N) -> Option<&RcType<N>> {
        match *self {
            Type::Struct(ref fields) => ast::lookup_field(fields, name),
            _ => None,
        }
    }

    /// Attempt to lookup the type of a variant
    ///
    /// Returns `None` if the type is not a union or the field is not
    /// present in the union.
    pub fn lookup_variant(&self, name: &N) -> Option<&RcType<N>> {
        match *self {
            Type::Union(ref variants) => ast::lookup_field(variants, name),
            _ => None,
        }
    }

    pub fn abstract_names_at(&mut self, names: &[N], scope: ScopeIndex) {
        match *self {
            Type::Var(ref mut var) => var.abstract_names_at(names, scope),
            Type::Const(_) => {}
            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    Rc::make_mut(param_ty).abstract_names_at(names, scope);
                }
                Rc::make_mut(ret_ty).abstract_names_at(names, scope);
            }
            Type::Array(ref mut elem_ty) => {
                Rc::make_mut(elem_ty).abstract_names_at(names, scope);
            }
            Type::Union(ref mut variants) => for variant in variants {
                Rc::make_mut(&mut variant.value).abstract_names_at(names, scope);
            },
            Type::Struct(ref mut fields) => for field in fields {
                Rc::make_mut(&mut field.value).abstract_names_at(names, scope);
            },
            Type::Abs(_, ref mut body_ty) => {
                Rc::make_mut(body_ty).abstract_names_at(names, scope.succ());
            }
            Type::App(ref mut fn_ty, ref mut arg_tys) => {
                Rc::make_mut(fn_ty).abstract_names_at(names, scope);

                for arg_ty in arg_tys {
                    Rc::make_mut(arg_ty).abstract_names_at(names, scope);
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
    pub fn abstract_names(&mut self, names: &[N]) {
        self.abstract_names_at(names, ScopeIndex(0))
    }

    fn instantiate_at(&mut self, scope: ScopeIndex, tys: &[RcType<N>]) {
        // FIXME: ensure that expressions are not bound at the same scope
        match *self {
            Type::Var(Var::Bound(Named(_, var))) => if var.scope == scope {
                *self = (*tys[var.binding.0 as usize]).clone();
            },
            Type::Var(Var::Free(_)) | Type::Const(_) => {}
            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    Rc::make_mut(param_ty).instantiate_at(scope, tys);
                }

                Rc::make_mut(ret_ty).instantiate_at(scope, tys);
            }
            Type::Array(ref mut elem_ty) => {
                Rc::make_mut(elem_ty).instantiate_at(scope, tys);
            }
            Type::Union(ref mut variants) => for variant in variants {
                Rc::make_mut(&mut variant.value).instantiate_at(scope, tys);
            },
            Type::Struct(ref mut fields) => for field in fields {
                Rc::make_mut(&mut field.value).instantiate_at(scope, tys);
            },
            Type::Abs(_, ref mut ty) => {
                Rc::make_mut(ty).instantiate_at(scope.succ(), tys);
            }
            Type::App(ref mut ty, ref mut arg_tys) => {
                Rc::make_mut(ty).instantiate_at(scope, tys);

                for arg_ty in arg_tys {
                    Rc::make_mut(arg_ty).instantiate_at(scope, tys);
                }
            }
        };
    }

    /// Remove one layer of abstraction in the type by replacing the
    /// appropriate bound variables with copies of `ty`.
    pub fn instantiate(&mut self, tys: &[RcType<N>]) {
        self.instantiate_at(ScopeIndex(0), tys);
    }
}
