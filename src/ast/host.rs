//! The syntax of our data description language

use std::fmt;

use source::Spanned;
use ast::{Field, Named, Var};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Const {
    /// A boolean constant: eg. `true`, `false`
    Bool(bool),
    /// An integer constant: eg. `0`, `1`, `2`, ...
    Int(i64),
}

impl fmt::Debug for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
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

/// An expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<N> {
    /// A constant value
    Const(Const),
    /// A variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    Var(Var<N, Named<N, u32>>),
    /// An unary operator expression
    Unop(Unop, Box<SpannedExpr<N>>),
    /// A binary operator expression
    Binop(Binop, Box<SpannedExpr<N>>, Box<SpannedExpr<N>>),
    /// Field projection, eg: `x.field`
    Proj(Box<SpannedExpr<N>>, N),
}

pub type SpannedExpr<N> = Spanned<Expr<N>>;

impl<N> Expr<N> {
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
    pub fn fvar<M: Into<N>>(x: M) -> Expr<N> {
        Expr::Var(Var::Free(x.into()))
    }

    /// A bound variable
    pub fn bvar(x: N, i: u32) -> Expr<N> {
        Expr::Var(Var::Bound(Named(x, i)))
    }

    /// An unary operator expression
    pub fn unop<T>(op: Unop, x: T) -> Expr<N>
    where
        T: Into<Box<SpannedExpr<N>>>,
    {
        Expr::Unop(op, x.into())
    }

    /// A binary operator expression
    pub fn binop<T, U>(op: Binop, x: T, y: U) -> Expr<N>
    where
        T: Into<Box<SpannedExpr<N>>>,
        U: Into<Box<SpannedExpr<N>>>,
    {
        Expr::Binop(op, x.into(), y.into())
    }

    /// Field projection, eg: `x.field`
    pub fn proj<T, M: Into<N>>(e: T, field: M) -> Expr<N>
    where
        T: Into<Box<SpannedExpr<N>>>,
    {
        Expr::Proj(e.into(), field.into())
    }

    /// `true` if the expression contains no free variables
    pub fn is_closed(&self) -> bool {
        match *self {
            Expr::Var(ref v) => v.is_closed(),
            Expr::Const(_) => true,
            Expr::Unop(_, ref e) => e.value.is_closed(),
            Expr::Binop(_, ref e1, ref e2) => e1.value.is_closed() && e2.value.is_closed(),
            Expr::Proj(ref e, _) => e.value.is_closed(),
        }
    }

    pub fn abstract_level_with<F>(&mut self, level: u32, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        match *self {
            Expr::Var(ref mut v) => v.abstract_with(f),
            Expr::Const(_) => {}
            Expr::Unop(_, ref mut e) | Expr::Proj(ref mut e, _) => {
                e.value.abstract_level_with(level, f);
            }
            Expr::Binop(_, ref mut e1, ref mut e2) => {
                e1.value.abstract_level_with(level, f);
                e2.value.abstract_level_with(level, f);
            }
        }
    }

    pub fn abstract_with<F>(&mut self, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        self.abstract_level_with(0, &f);
    }

    pub fn abstract_name(&mut self, x: &N)
    where
        N: PartialEq + Clone,
    {
        self.abstract_with(&|y| if x == y {
            Some(Named(x.clone(), 0))
        } else {
            None
        });
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeExpr<N> {
    pub body: Expr<N>,
}

pub type SpannedScopeExpr<N> = Spanned<ScopeExpr<N>>;


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<N> {
    /// A type variable: eg. `T`
    Var(Var<N, Named<N, u32>>),
    /// Boolean
    Bool,
    /// Integer
    Int,
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Box<SpannedType<N>>, SpannedExpr<N>),
    /// A union of types: eg. `union { T, ... }`
    Union(Vec<SpannedType<N>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Vec<Field<N, Type<N>>>),
}

pub type SpannedType<N> = Spanned<Type<N>>;

impl<N> Type<N> {
    /// A free type variable: eg. `T`
    pub fn fvar<M: Into<N>>(x: M) -> Type<N> {
        Type::Var(Var::Free(x.into()))
    }

    /// A bound type variable
    pub fn bvar(x: N, i: u32) -> Type<N> {
        Type::Var(Var::Bound(Named(x, i)))
    }

    /// An array of the specified type, with a size: eg. `[T; n]`
    pub fn array<T>(ty: T, size: SpannedExpr<N>) -> Type<N>
    where
        T: Into<Box<SpannedType<N>>>,
    {
        Type::Array(ty.into(), size)
    }

    /// A union of types: eg. `union { T, ... }`
    pub fn union(tys: Vec<SpannedType<N>>) -> Type<N> {
        Type::Union(tys)
    }

    /// A struct type, with fields: eg. `struct { field : T, ... }`
    pub fn struct_<Fs>(fields: Fs) -> Type<N>
    where
        N: PartialEq + Clone,
        Fs: IntoIterator<Item = Field<N, Type<N>>>,
    {
        // We maintain a list of the seen field names. This will allow us to
        // recover the index of these variables as we abstract later fields...
        let mut seen_names = Vec::new();
        Type::Struct(
            fields
                .into_iter()
                .map(|field| {
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
                    seen_names.push(field.name.value.clone());

                    field
                })
                .collect(),
        )
    }

    /// `true` if the type contains no free variables in itself or its sub-expressions
    pub fn is_closed(&self) -> bool {
        match *self {
            Type::Var(ref v) => v.is_closed(),
            Type::Bool | Type::Int => true,
            Type::Array(ref t, ref e) => t.value.is_closed() && e.value.is_closed(),
            Type::Union(ref ts) => ts.iter().all(|t| t.value.is_closed()),
            Type::Struct(ref fs) => fs.iter().all(|f| f.value.value.is_closed()),
        }
    }

    pub fn abstract_level_with<F>(&mut self, level: u32, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        match *self {
            Type::Var(ref mut v) => v.abstract_with(f),
            Type::Bool | Type::Int => {}
            Type::Array(ref mut ty, ref mut e) => {
                ty.value.abstract_level_with(level, f);
                e.value.abstract_level_with(level, f);
            }
            Type::Union(ref mut tys) => for ty in tys {
                ty.value.abstract_level_with(level, f);
            },
            Type::Struct(ref mut fs) => for (i, field) in fs.iter_mut().enumerate() {
                field.value.value.abstract_level_with(level + i as u32, f);
            },
        }
    }

    pub fn abstract_with<F>(&mut self, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        self.abstract_level_with(0, &f);
    }

    pub fn abstract_name(&mut self, x: &N)
    where
        N: PartialEq + Clone,
    {
        self.abstract_with(&|y| if x == y {
            Some(Named(x.clone(), 0))
        } else {
            None
        })
    }

    fn instantiate_level(&mut self, level: u32, src: &Type<N>)
    where
        N: Clone,
    {
        // Bleh: Running into non-lexical liftetime problems here!
        // Just so you know that I'm not going completely insane....
        // FIXME: ensure that expressions are not bound at the same level
        *self = match *self {
            Type::Var(Var::Bound(ref i)) => if *i == level {
                src.clone()
            } else {
                return;
            },
            Type::Var(Var::Free(_)) | Type::Bool | Type::Int => return,
            Type::Array(ref mut ty, _) => {
                ty.value.instantiate_level(level, src);
                return;
            }
            Type::Union(ref mut tys) => {
                for ty in tys {
                    ty.value.instantiate_level(level, src);
                }
                return;
            }
            Type::Struct(ref mut fs) => {
                for (i, f) in fs.iter_mut().enumerate() {
                    f.value.value.instantiate_level(level + i as u32, src);
                }
                return;
            }
        };
    }

    pub fn instantiate(&mut self, ty: &Type<N>)
    where
        N: Clone,
    {
        self.instantiate_level(0, ty);
    }
}
