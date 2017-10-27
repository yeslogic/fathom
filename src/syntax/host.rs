//! The syntax of our data description language

use std::fmt;

use source::Span;
use syntax::{self, Field, Name, Named, Var};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Const {
    /// A single bit
    Bit(bool),
    /// A boolean constant: eg. `true`, `false`
    Bool(bool),
    /// An integer constant: eg. `0`, `1`, `2`, ...
    Int(i64),
}

impl Const {
    pub fn ty_const_of(self) -> TypeConst {
        match self {
            Const::Bit(_) => TypeConst::Bit,
            Const::Bool(_) => TypeConst::Bool,
            Const::Int(_) => TypeConst::Int,
        }
    }
}

impl fmt::Debug for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Const::Bit(value) => write!(f, "Bit({:?})", value),
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
    Prim(&'static str, Box<Type<N>>),
    /// A variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    Var(Span, Var<N, u32>),
    /// An unary operator expression
    Unop(Span, Unop, Box<Expr<N>>),
    /// A binary operator expression
    Binop(Span, Binop, Box<Expr<N>>, Box<Expr<N>>),
    /// Field projection, eg: `x.field`
    Proj(Span, Box<Expr<N>>, N),
    /// Array index, eg: `x[i]`
    Subscript(Span, Box<Expr<N>>, Box<Expr<N>>),
    /// Abstraction, eg: `\(x : T) -> x`
    Abs(Span, Named<N, Box<Type<N>>>, Box<Expr<N>>),
}

impl<N: Name> Expr<N> {
    /// A bit constant: eg. `0b`, `01`
    pub fn bit(span: Span, value: bool) -> Expr<N> {
        Expr::Const(span, Const::Bit(value))
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
    pub fn prim<T1: Into<Box<Type<N>>>>(name: &'static str, repr_ty: T1) -> Expr<N> {
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
    pub fn unop<E1: Into<Box<Expr<N>>>>(span: Span, op: Unop, x: E1) -> Expr<N> {
        Expr::Unop(span, op, x.into())
    }

    /// A binary operator expression
    pub fn binop<E1, E2>(span: Span, op: Binop, x: E1, y: E2) -> Expr<N>
    where
        E1: Into<Box<Expr<N>>>,
        E2: Into<Box<Expr<N>>>,
    {
        Expr::Binop(span, op, x.into(), y.into())
    }

    /// Field projection, eg: `x.field`
    pub fn proj<E1, N1>(span: Span, expr: E1, field_name: N1) -> Expr<N>
    where
        E1: Into<Box<Expr<N>>>,
        N1: Into<N>,
    {
        Expr::Proj(span, expr.into(), field_name.into())
    }

    /// Array subscript, eg: `x[i]`
    pub fn subscript<E1, E2>(span: Span, expr: E1, index_expr: E2) -> Expr<N>
    where
        E1: Into<Box<Expr<N>>>,
        E2: Into<Box<Expr<N>>>,
    {
        Expr::Subscript(span, expr.into(), index_expr.into())
    }

    /// Abstraction, eg: `\(x : T) -> x`
    pub fn abs<N1, T1, E1>(span: Span, (param_name, param_ty): (N1, T1), body_expr: E1) -> Expr<N>
    where
        N1: Into<N>,
        T1: Into<Box<Type<N>>>,
        E1: Into<Box<Expr<N>>>,
    {
        let param_name = param_name.into();
        let mut body_expr = body_expr.into();
        body_expr.abstract_name(&param_name);
        Expr::Abs(span, Named(param_name, param_ty.into()), body_expr)
    }

    pub fn abstract_name_at(&mut self, name: &N, level: u32) {
        match *self {
            Expr::Var(_, ref mut var) => var.abstract_name_at(name, level),
            Expr::Const(_, _) => {}
            Expr::Prim(_, ref mut repr_ty) => repr_ty.abstract_name_at(name, level),
            Expr::Unop(_, _, ref mut expr) | Expr::Proj(_, ref mut expr, _) => {
                expr.abstract_name_at(name, level);
            }
            Expr::Binop(_, _, ref mut lhs_expr, ref mut rhs_expr) => {
                lhs_expr.abstract_name_at(name, level);
                rhs_expr.abstract_name_at(name, level);
            }
            Expr::Subscript(_, ref mut array_expr, ref mut index_expr) => {
                array_expr.abstract_name_at(name, level);
                index_expr.abstract_name_at(name, level);
            }
            Expr::Abs(_, _, ref mut body_expr) => {
                body_expr.abstract_name_at(name, level + 1);
            }
        }
    }

    pub fn abstract_name(&mut self, name: &N) {
        self.abstract_name_at(name, 0);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConst {
    /// Bit
    Bit,
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
    Arrow(Box<Type<N>>, Box<Type<N>>),
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Box<Type<N>>, Box<Expr<N>>),
    /// A union of types: eg. `union { T, ... }`
    Union(Vec<Type<N>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Vec<Field<N, Type<N>>>),
}

impl<N: Name> Type<N> {
    /// A free type variable: eg. `T`
    pub fn fvar<N1: Into<N>>(x: N1) -> Type<N> {
        Type::Var(Var::Free(x.into()))
    }

    /// A bound type variable
    pub fn bvar<N1: Into<N>>(x: N, i: u32) -> Type<N> {
        Type::Var(Var::Bound(Named(x.into(), i)))
    }

    /// Bit type constant
    pub fn bit() -> Type<N> {
        Type::Const(TypeConst::Bit)
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
        T1: Into<Box<Type<N>>>,
        E1: Into<Box<Type<N>>>,
    {
        Type::Arrow(lhs_ty.into(), rhs_ty.into())
    }

    /// An array of the specified type, with a size: eg. `[T; n]`
    pub fn array<T1, E1>(elem_ty: T1, size_expr: E1) -> Type<N>
    where
        T1: Into<Box<Type<N>>>,
        E1: Into<Box<Expr<N>>>,
    {
        Type::Array(elem_ty.into(), size_expr.into())
    }

    /// A union of types: eg. `union { T, ... }`
    pub fn union(tys: Vec<Type<N>>) -> Type<N> {
        Type::Union(tys)
    }

    /// A struct type, with fields: eg. `struct { field : T, ... }`
    pub fn struct_(mut fields: Vec<Field<N, Type<N>>>) -> Type<N> {
        // We maintain a list of the seen field names. This will allow us to
        // recover the index of these variables as we abstract later fields...
        let mut seen_names = Vec::with_capacity(fields.len());

        for field in &mut fields {
            for (level, name) in seen_names.iter().rev().enumerate() {
                field.value.abstract_name_at(name, level as u32);
            }

            // Record that the field has been 'seen'
            seen_names.push(field.name.clone());
        }

        Type::Struct(fields)
    }

    pub fn lookup_field(&self, name: &N) -> Option<&Type<N>> {
        match *self {
            Type::Struct(ref fields) => syntax::lookup_field(fields, name),
            _ => None,
        }
    }

    pub fn abstract_name_at(&mut self, name: &N, level: u32) {
        match *self {
            Type::Var(ref mut var) => var.abstract_name_at(name, level),
            Type::Const(_) => {}
            Type::Arrow(ref mut lhs_ty, ref mut rhs_ty) => {
                lhs_ty.abstract_name_at(name, level);
                rhs_ty.abstract_name_at(name, level);
            }
            Type::Array(ref mut elem_ty, ref mut size_expr) => {
                elem_ty.abstract_name_at(name, level);
                size_expr.abstract_name_at(name, level);
            }
            Type::Union(ref mut tys) => for ty in tys {
                ty.abstract_name_at(name, level);
            },
            Type::Struct(ref mut fields) => for (i, field) in fields.iter_mut().enumerate() {
                field.value.abstract_name_at(name, level + i as u32);
            },
        }
    }

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
                lhs_ty.instantiate_at(level, src);
                rhs_ty.instantiate_at(level, src);
            }
            Type::Array(ref mut elem_ty, _) => {
                elem_ty.instantiate_at(level, src);
            }
            Type::Union(ref mut tys) => for ty in tys {
                ty.instantiate_at(level, src);
            },
            Type::Struct(ref mut fields) => for (i, field) in fields.iter_mut().enumerate() {
                field.value.instantiate_at(level + i as u32, src);
            },
        };
    }

    pub fn instantiate(&mut self, ty: &Type<N>) {
        self.instantiate_at(0, ty);
    }
}
