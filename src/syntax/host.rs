//! The syntax of our data description language

use std::fmt;

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
    Const(Const),
    /// A variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    Var(Var<N, Named<N, u32>>),
    /// An unary operator expression
    Unop(Unop, Box<Expr<N>>),
    /// A binary operator expression
    Binop(Binop, Box<Expr<N>>, Box<Expr<N>>),
    /// Field projection, eg: `x.field`
    Proj(Box<Expr<N>>, N),
}

impl<N: Name> Expr<N> {
    /// A bit constant: eg. `0b`, `01`
    pub fn bit(value: bool) -> Expr<N> {
        Expr::Const(Const::Bit(value))
    }

    /// A boolean constant: eg. `true`, `false`
    pub fn bool(value: bool) -> Expr<N> {
        Expr::Const(Const::Bool(value))
    }

    /// An integer constant: eg. `0`, `1`, `2`, ...
    pub fn int(value: i64) -> Expr<N> {
        Expr::Const(Const::Int(value))
    }

    /// A free variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    pub fn fvar<N1: Into<N>>(x: N1) -> Expr<N> {
        Expr::Var(Var::Free(x.into()))
    }

    /// A bound variable
    pub fn bvar<N1: Into<N>>(x: N1, i: u32) -> Expr<N> {
        Expr::Var(Var::Bound(Named(x.into(), i)))
    }

    /// An unary operator expression
    pub fn unop<E1: Into<Box<Expr<N>>>>(op: Unop, x: E1) -> Expr<N> {
        Expr::Unop(op, x.into())
    }

    /// A binary operator expression
    pub fn binop<E1: Into<Box<Expr<N>>>, E2: Into<Box<Expr<N>>>>(
        op: Binop,
        x: E1,
        y: E2,
    ) -> Expr<N> {
        Expr::Binop(op, x.into(), y.into())
    }

    /// Field projection, eg: `x.field`
    pub fn proj<E1: Into<Box<Expr<N>>>, M: Into<N>>(e: E1, field: M) -> Expr<N> {
        Expr::Proj(e.into(), field.into())
    }

    pub fn abstract_level_with<F>(&mut self, level: u32, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        match *self {
            Expr::Var(ref mut v) => v.abstract_with(f),
            Expr::Const(_) => {}
            Expr::Unop(_, ref mut e1) | Expr::Proj(ref mut e1, _) => {
                e1.abstract_level_with(level, f);
            }
            Expr::Binop(_, ref mut e1, ref mut e2) => {
                e1.abstract_level_with(level, f);
                e2.abstract_level_with(level, f);
            }
        }
    }

    pub fn abstract_with<F>(&mut self, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        self.abstract_level_with(0, &f);
    }

    pub fn abstract_name(&mut self, x: &N) {
        self.abstract_with(&|y| if x == y {
            Some(Named(x.clone(), 0))
        } else {
            None
        });
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
    Var(Var<N, Named<N, u32>>),
    /// A type constant
    Const(TypeConst),
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

    /// An array of the specified type, with a size: eg. `[T; n]`
    pub fn array<T1: Into<Box<Type<N>>>, E1: Into<Box<Expr<N>>>>(
        elem_ty: T1,
        size_expr: E1,
    ) -> Type<N> {
        Type::Array(elem_ty.into(), size_expr.into())
    }

    /// A union of types: eg. `union { T, ... }`
    pub fn union(tys: Vec<Type<N>>) -> Type<N> {
        Type::Union(tys)
    }

    /// A struct type, with fields: eg. `struct { field : T, ... }`
    pub fn struct_<Fs>(fields: Fs) -> Type<N>
    where
        Fs: IntoIterator<Item = Field<N, Type<N>>>,
    {
        // We maintain a list of the seen field names. This will allow us to
        // recover the index of these variables as we abstract later fields...
        let mut seen_names = Vec::new();
        let f = |field: Field<N, Type<N>>| {
            let field = field.map_value(|mut ty| {
                ty.abstract_with(&|x| {
                    seen_names
                        .iter()
                        .position(|y| x == y)
                        .map(|i| Named(x.clone(), i as u32))
                });
                ty
            });

            // Record that the field has been 'seen'
            seen_names.push(field.name.clone());

            field
        };

        Type::Struct(fields.into_iter().map(f).collect())
    }

    pub fn lookup_field(&self, name: &N) -> Option<&Type<N>> {
        match *self {
            Type::Struct(ref fields) => syntax::lookup_field(fields, name),
            _ => None,
        }
    }

    pub fn abstract_level_with<F>(&mut self, level: u32, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        match *self {
            Type::Var(ref mut var) => var.abstract_with(f),
            Type::Const(_) => {}
            Type::Array(ref mut elem_ty, ref mut size_expr) => {
                elem_ty.abstract_level_with(level, f);
                size_expr.abstract_level_with(level, f);
            }
            Type::Union(ref mut tys) => for ty in tys {
                ty.abstract_level_with(level, f);
            },
            Type::Struct(ref mut fields) => for (i, field) in fields.iter_mut().enumerate() {
                field.value.abstract_level_with(level + i as u32, f);
            },
        }
    }

    pub fn abstract_with<F>(&mut self, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        self.abstract_level_with(0, &f);
    }

    pub fn abstract_name(&mut self, x: &N) {
        self.abstract_with(&|y| if x == y {
            Some(Named(x.clone(), 0))
        } else {
            None
        })
    }

    fn instantiate_level(&mut self, level: u32, src: &Type<N>) {
        // Bleh: Running into non-lexical liftetime problems here!
        // Just so you know that I'm not going completely insane....
        // FIXME: ensure that expressions are not bound at the same level
        *self = match *self {
            Type::Var(Var::Bound(ref i)) => if *i == level {
                src.clone()
            } else {
                return;
            },
            Type::Var(Var::Free(_)) | Type::Const(_) => return,
            Type::Array(ref mut elem_ty, _) => {
                elem_ty.instantiate_level(level, src);
                return;
            }
            Type::Union(ref mut tys) => {
                for ty in tys {
                    ty.instantiate_level(level, src);
                }
                return;
            }
            Type::Struct(ref mut fields) => {
                for (i, field) in fields.iter_mut().enumerate() {
                    field.value.instantiate_level(level + i as u32, src);
                }
                return;
            }
        };
    }

    pub fn instantiate(&mut self, ty: &Type<N>) {
        self.instantiate_level(0, ty);
    }
}
