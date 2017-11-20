//! The syntax of our data description language

use std::fmt;
use std::rc::Rc;

use name::{Name, Named};
use source::Span;
use syntax::ast::{self, Field, Var};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    /// Kind of types
    Type,
    /// Kind of type functions
    Arrow(RcKind, RcKind),
}

pub type RcKind = Rc<Kind>;

impl Kind {
    /// Kind of type functions
    pub fn arrow<K1: Into<RcKind>, K2: Into<RcKind>>(lhs: K1, rhs: K2) -> Kind {
        Kind::Arrow(lhs.into(), rhs.into())
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Const {
    /// A single bit
    U8(u8),
    /// A boolean constant: eg. `true`, `false`
    Bool(bool),
    /// An integer constant: eg. `0`, `1`, `2`, ...
    Int(i64),
}

impl Const {
    pub fn ty_const_of(self) -> TypeConst {
        match self {
            Const::U8(_) => TypeConst::U8,
            Const::Bool(_) => TypeConst::Bool,
            Const::Int(_) => TypeConst::Int,
        }
    }
}

impl fmt::Debug for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Const::U8(value) => write!(f, "U8({:?})", value),
            Const::Bool(value) => write!(f, "Bool({:?})", value),
            Const::Int(value) => write!(f, "Int({:?})", value),
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
    Var(Span, Var<N, u32>),
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
    /// Abstraction, eg: `\(x : T) -> x`
    Abs(Span, Named<N, RcType<N>>, RcExpr<N>),
    /// Application, eg: `f x`
    App(Span, RcExpr<N>, RcExpr<N>),
}

pub type RcExpr<N> = Rc<Expr<N>>;

impl<N: Name> Expr<N> {
    /// A byte constant: eg. `0`, `1`, `2`, ..., `255`
    pub fn u8(span: Span, value: u8) -> Expr<N> {
        Expr::Const(span, Const::U8(value))
    }

    /// A boolean constant: eg. `true`, `false`
    pub fn bool(span: Span, value: bool) -> Expr<N> {
        Expr::Const(span, Const::Bool(value))
    }

    /// An integer constant: eg. `0`, `1`, `2`, ...
    pub fn int(span: Span, value: i64) -> Expr<N> {
        Expr::Const(span, Const::Int(value))
    }

    /// Primitive expressions
    pub fn prim<T1: Into<RcType<N>>>(name: &'static str, repr_ty: T1) -> Expr<N> {
        Expr::Prim(name, repr_ty.into())
    }

    /// A free variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    pub fn fvar<N1: Into<N>>(span: Span, x: N1) -> Expr<N> {
        Expr::Var(span, Var::Free(x.into()))
    }

    /// A bound variable
    pub fn bvar<N1: Into<N>>(span: Span, x: N1, i: u32) -> Expr<N> {
        Expr::Var(span, Var::Bound(Named(x.into(), i)))
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

    /// Abstraction, eg: `\(x : T) -> x`
    pub fn abs<N1, T1, E1>(span: Span, (param_name, param_ty): (N1, T1), body_expr: E1) -> Expr<N>
    where
        N1: Into<N>,
        T1: Into<RcType<N>>,
        E1: Into<RcExpr<N>>,
    {
        let param_name = param_name.into();
        let mut body_expr = body_expr.into();
        Rc::make_mut(&mut body_expr).abstract_name(&param_name);
        Expr::Abs(span, Named(param_name, param_ty.into()), body_expr)
    }

    /// Application: eg. `f x`
    pub fn app<E1, E2>(span: Span, fn_expr: E1, arg_expr: E2) -> Expr<N>
    where
        E1: Into<RcExpr<N>>,
        E2: Into<RcExpr<N>>,
    {
        Expr::App(span, fn_expr.into(), arg_expr.into())
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

    pub fn abstract_name_at(&mut self, name: &N, level: u32) {
        match *self {
            Expr::Var(_, ref mut var) => var.abstract_name_at(name, level),
            Expr::Const(_, _) => {}
            Expr::Prim(_, ref mut repr_ty) => Rc::make_mut(repr_ty).abstract_name_at(name, level),
            Expr::Unop(_, _, ref mut expr) | Expr::Proj(_, ref mut expr, _) => {
                Rc::make_mut(expr).abstract_name_at(name, level);
            }
            Expr::Intro(_, _, ref mut expr, ref mut ty) => {
                Rc::make_mut(expr).abstract_name_at(name, level);
                Rc::make_mut(ty).abstract_name_at(name, level);
            }
            Expr::Binop(_, _, ref mut lhs_expr, ref mut rhs_expr) => {
                Rc::make_mut(lhs_expr).abstract_name_at(name, level);
                Rc::make_mut(rhs_expr).abstract_name_at(name, level);
            }
            Expr::Struct(ref mut fields) => for field in fields {
                Rc::make_mut(&mut field.value).abstract_name_at(name, level);
            },
            Expr::Subscript(_, ref mut array_expr, ref mut index_expr) => {
                Rc::make_mut(array_expr).abstract_name_at(name, level);
                Rc::make_mut(index_expr).abstract_name_at(name, level);
            }
            Expr::Abs(_, Named(_, ref mut arg_ty), ref mut body_expr) => {
                Rc::make_mut(arg_ty).abstract_name_at(name, level);
                Rc::make_mut(body_expr).abstract_name_at(name, level + 1);
            }
            Expr::App(_, ref mut fn_expr, ref mut arg_expr) => {
                Rc::make_mut(fn_expr).abstract_name_at(name, level);
                Rc::make_mut(arg_expr).abstract_name_at(name, level);
            }
        }
    }

    pub fn abstract_name(&mut self, name: &N) {
        self.abstract_name_at(name, 0);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConst {
    /// Byte
    U8,
    /// Boolean
    Bool,
    /// Integer
    Int,
}

/// A host type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<N> {
    /// A type variable: eg. `T`
    Var(Var<N, u32>),
    /// A type constant
    Const(TypeConst),
    /// Arrow type: eg. `T -> U`
    Arrow(RcType<N>, RcType<N>),
    /// An array, eg. `[T]`
    Array(RcType<N>),
    /// A union of types: eg. `union { variant : T, ... }`
    Union(Vec<Field<N, RcType<N>>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Vec<Field<N, RcType<N>>>),
    /// Type abstraction: eg. `\(a : Type) -> T`
    Abs(Named<N, RcKind>, RcType<N>),
    /// Type application: eg. `T U V`
    App(RcType<N>, RcType<N>),
}

pub type RcType<N> = Rc<Type<N>>;

impl<N: Name> Type<N> {
    /// A free type variable: eg. `T`
    pub fn fvar<N1: Into<N>>(x: N1) -> Type<N> {
        Type::Var(Var::Free(x.into()))
    }

    /// A bound type variable
    pub fn bvar<N1: Into<N>>(x: N, i: u32) -> Type<N> {
        Type::Var(Var::Bound(Named(x.into(), i)))
    }

    /// Byte type constant
    pub fn u8() -> Type<N> {
        Type::Const(TypeConst::U8)
    }

    /// Boolean type constant
    pub fn bool() -> Type<N> {
        Type::Const(TypeConst::Bool)
    }

    /// Integer type constant
    pub fn int() -> Type<N> {
        Type::Const(TypeConst::Int)
    }

    /// Arrow type: eg. `T -> U`
    pub fn arrow<T1, E1>(lhs_ty: T1, rhs_ty: E1) -> Type<N>
    where
        T1: Into<RcType<N>>,
        E1: Into<RcType<N>>,
    {
        Type::Arrow(lhs_ty.into(), rhs_ty.into())
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

    /// Type abstraction: eg. `\(a : Type) -> T`
    pub fn abs<N1, K1, T1>((param_name, param_kind): (N1, K1), body_ty: T1) -> Type<N>
    where
        N1: Into<N>,
        K1: Into<RcKind>,
        T1: Into<RcType<N>>,
    {
        let param_name = param_name.into();
        let mut body_ty = body_ty.into();
        Rc::make_mut(&mut body_ty).abstract_name(&param_name);
        Type::Abs(Named(param_name, param_kind.into()), body_ty)
    }

    /// Type application: eg. `T U V`
    pub fn app<T1, T2>(ty1: T1, ty2: T2) -> Type<N>
    where
        T1: Into<RcType<N>>,
        T2: Into<RcType<N>>,
    {
        Type::App(ty1.into(), ty2.into())
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

    pub fn abstract_name_at(&mut self, name: &N, level: u32) {
        match *self {
            Type::Var(ref mut var) => var.abstract_name_at(name, level),
            Type::Const(_) => {}
            Type::Arrow(ref mut lhs_ty, ref mut rhs_ty) => {
                Rc::make_mut(lhs_ty).abstract_name_at(name, level);
                Rc::make_mut(rhs_ty).abstract_name_at(name, level);
            }
            Type::Array(ref mut elem_ty) => {
                Rc::make_mut(elem_ty).abstract_name_at(name, level);
            }
            Type::Union(ref mut variants) => for variant in variants {
                Rc::make_mut(&mut variant.value).abstract_name_at(name, level);
            },
            Type::Struct(ref mut fields) => for field in fields {
                Rc::make_mut(&mut field.value).abstract_name_at(name, level);
            },
            Type::Abs(_, ref mut body_ty) => {
                Rc::make_mut(body_ty).abstract_name_at(name, level + 1);
            }
            Type::App(ref mut fn_ty, ref mut arg_ty) => {
                Rc::make_mut(fn_ty).abstract_name_at(name, level);
                Rc::make_mut(arg_ty).abstract_name_at(name, level);
            }
        }
    }

    /// Add one layer of abstraction around the type by replacing all the
    /// free variables called `name` with an appropriate De Bruijn index.
    ///
    /// This results in a one 'dangling' index, and so care must be taken
    /// to wrap it in another type that marks the introduction of a new
    /// scope.
    pub fn abstract_name(&mut self, name: &N) {
        self.abstract_name_at(name, 0)
    }

    fn instantiate_at(&mut self, level: u32, src: &Type<N>) {
        // FIXME: ensure that expressions are not bound at the same level
        match *self {
            Type::Var(Var::Bound(Named(_, i))) => if i == level {
                *self = src.clone();
            },
            Type::Var(Var::Free(_)) | Type::Const(_) => {}
            Type::Arrow(ref mut lhs_ty, ref mut rhs_ty) => {
                Rc::make_mut(lhs_ty).instantiate_at(level, src);
                Rc::make_mut(rhs_ty).instantiate_at(level, src);
            }
            Type::Array(ref mut elem_ty) => {
                Rc::make_mut(elem_ty).instantiate_at(level, src);
            }
            Type::Union(ref mut variants) => for variant in variants {
                Rc::make_mut(&mut variant.value).instantiate_at(level, src);
            },
            Type::Struct(ref mut fields) => for field in fields {
                Rc::make_mut(&mut field.value).instantiate_at(level, src);
            },
            Type::Abs(_, ref mut ty) => {
                Rc::make_mut(ty).instantiate_at(level + 1, src);
            }
            Type::App(ref mut ty, ref mut arg_ty) => {
                Rc::make_mut(ty).instantiate_at(level, src);
                Rc::make_mut(arg_ty).instantiate_at(level, src);
            }
        };
    }

    /// Remove one layer of abstraction in the type by replacing the
    /// appropriate bound variables with copies of `ty`.
    pub fn instantiate(&mut self, ty: &Type<N>) {
        self.instantiate_at(0, ty);
    }
}
