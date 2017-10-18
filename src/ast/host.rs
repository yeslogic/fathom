//! The syntax of our data description language

use std::fmt;

use source::Span;
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

/// A host expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprF<N, E> {
    /// A constant value
    Const(Const),
    /// A variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    Var(Var<N, Named<N, u32>>),
    /// An unary operator expression
    Unop(Unop, Box<E>),
    /// A binary operator expression
    Binop(Binop, Box<E>, Box<E>),
    /// Field projection, eg: `x.field`
    Proj(Box<E>, N),
}

/// The recursive innards of an `Expr`
///
/// This does the job of tying the recursive knot for `Expr`, turning
/// `ExprF` into a tree of expressions.
///
/// We could have just inlined this inside `Expr`, but this type alias is
/// handy to have around for defining our conversion impls.
pub type ExprRec<N> = ExprF<N, Expr<N>>;

/// A tree of host expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr<N>(pub ExprRec<N>);

impl<N> Into<ExprRec<N>> for Expr<N> {
    fn into(self) -> ExprRec<N> {
        self.0
    }
}

impl<N> AsRef<ExprRec<N>> for Expr<N> {
    fn as_ref(&self) -> &ExprRec<N> {
        &self.0
    }
}

impl<N> AsMut<ExprRec<N>> for Expr<N> {
    fn as_mut(&mut self) -> &mut ExprRec<N> {
        &mut self.0
    }
}

/// The recursive innards of a `SpannedExpr`
///
/// This does the job of tying the recursive knot for `SpannedExpr`, turning
/// `ExprF` into a tree of spanned expressions.
///
/// We could have just inlined this inside `SpannedExpr`, but this type alias is
/// handy to have around for defining our conversion impls.
pub type SpannedExprRec<N> = ExprF<N, SpannedExpr<N>>;

/// A tree of spanned host expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpannedExpr<N> {
    pub span: Span,
    pub inner: SpannedExprRec<N>,
}

impl<N> Into<SpannedExprRec<N>> for SpannedExpr<N> {
    fn into(self) -> SpannedExprRec<N> {
        self.inner
    }
}

impl<N> AsRef<SpannedExprRec<N>> for SpannedExpr<N> {
    fn as_ref(&self) -> &SpannedExprRec<N> {
        &self.inner
    }
}

impl<N> AsMut<SpannedExprRec<N>> for SpannedExpr<N> {
    fn as_mut(&mut self) -> &mut SpannedExprRec<N> {
        &mut self.inner
    }
}

impl<N, E> ExprF<N, E> {
    /// A boolean constant: eg. `true`, `false`
    pub fn bool(value: bool) -> ExprF<N, E> {
        ExprF::Const(Const::Bool(value))
    }

    /// An integer constant: eg. `0`, `1`, `2`, ...
    pub fn int(value: i64) -> ExprF<N, E> {
        ExprF::Const(Const::Int(value))
    }

    /// A free variable, referring to an integer that exists in the current
    /// context: eg. `len`, `num_tables`
    pub fn fvar<N1: Into<N>>(x: N1) -> ExprF<N, E> {
        ExprF::Var(Var::Free(x.into()))
    }

    /// A bound variable
    pub fn bvar<N1: Into<N>>(x: N1, i: u32) -> ExprF<N, E> {
        ExprF::Var(Var::Bound(Named(x.into(), i)))
    }

    /// An unary operator expression
    pub fn unop<E1: Into<Box<E>>>(op: Unop, x: E1) -> ExprF<N, E> {
        ExprF::Unop(op, x.into())
    }

    /// A binary operator expression
    pub fn binop<E1: Into<Box<E>>, E2: Into<Box<E>>>(op: Binop, x: E1, y: E2) -> ExprF<N, E> {
        ExprF::Binop(op, x.into(), y.into())
    }

    /// Field projection, eg: `x.field`
    pub fn proj<E1: Into<Box<E>>, M: Into<N>>(e: E1, field: M) -> ExprF<N, E> {
        ExprF::Proj(e.into(), field.into())
    }
}

impl<N, E> ExprF<N, E>
where
    E: AsRef<ExprF<N, E>> + AsMut<ExprF<N, E>>,
{
    /// `true` if the expression contains no free variables
    pub fn is_closed(&self) -> bool {
        match *self {
            ExprF::Var(ref v) => v.is_closed(),
            ExprF::Const(_) => true,
            ExprF::Unop(_, ref e1) => E::as_ref(e1).is_closed(),
            ExprF::Binop(_, ref e1, ref e2) => {
                E::as_ref(e1).is_closed() && E::as_ref(e2).is_closed()
            }
            ExprF::Proj(ref e1, _) => E::as_ref(e1).is_closed(),
        }
    }

    pub fn abstract_level_with<F>(&mut self, level: u32, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        match *self {
            ExprF::Var(ref mut v) => v.abstract_with(f),
            ExprF::Const(_) => {}
            ExprF::Unop(_, ref mut e1) | ExprF::Proj(ref mut e1, _) => {
                E::as_mut(e1).abstract_level_with(level, f);
            }
            ExprF::Binop(_, ref mut e1, ref mut e2) => {
                E::as_mut(e1).abstract_level_with(level, f);
                E::as_mut(e2).abstract_level_with(level, f);
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

/// A host type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeF<N, T, E> {
    /// A type variable: eg. `T`
    Var(Var<N, Named<N, u32>>),
    /// Boolean
    Bool,
    /// Integer
    Int,
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Box<T>, Box<E>),
    /// A union of types: eg. `union { T, ... }`
    Union(Vec<T>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Vec<Field<N, T>>),
}

/// The recursive innards of a `Type`
///
/// This does the job of tying the recursive knot for `Type`, turning
/// `TypeF` into a tree of expressions.
///
/// We could have just inlined this inside `Type`, but this type alias is
/// handy to have around for defining our conversion impls.
pub type TypeRec<N> = TypeF<N, Type<N>, Expr<N>>;

/// A tree of types
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type<N>(pub TypeRec<N>);

impl<N> Into<TypeRec<N>> for Type<N> {
    fn into(self) -> TypeRec<N> {
        self.0
    }
}

impl<N> AsRef<TypeRec<N>> for Type<N> {
    fn as_ref(&self) -> &TypeRec<N> {
        &self.0
    }
}

impl<N> AsMut<TypeRec<N>> for Type<N> {
    fn as_mut(&mut self) -> &mut TypeRec<N> {
        &mut self.0
    }
}

/// The recursive innards of a `SpannedType`
///
/// This does the job of tying the recursive knot for `SpannedType`, turning
/// `TypeF` into a tree of spanned expressions.
///
/// We could have just inlined this inside `SpannedType`, but this type alias is
/// handy to have around for defining our conversion impls.
pub type SpannedTypeRec<N> = TypeF<N, SpannedType<N>, SpannedExpr<N>>;

/// A tree of types
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpannedType<N> {
    pub span: Span,
    pub inner: SpannedTypeRec<N>,
}

impl<N> Into<SpannedTypeRec<N>> for SpannedType<N> {
    fn into(self) -> SpannedTypeRec<N> {
        self.inner
    }
}

impl<N> AsRef<SpannedTypeRec<N>> for SpannedType<N> {
    fn as_ref(&self) -> &SpannedTypeRec<N> {
        &self.inner
    }
}

impl<N> AsMut<SpannedTypeRec<N>> for SpannedType<N> {
    fn as_mut(&mut self) -> &mut SpannedTypeRec<N> {
        &mut self.inner
    }
}

impl<N, T, E> TypeF<N, T, E> {
    /// A free type variable: eg. `T`
    pub fn fvar<N1: Into<N>>(x: N1) -> TypeF<N, T, E> {
        TypeF::Var(Var::Free(x.into()))
    }

    /// A bound type variable
    pub fn bvar<N1: Into<N>>(x: N, i: u32) -> TypeF<N, T, E> {
        TypeF::Var(Var::Bound(Named(x.into(), i)))
    }

    /// An array of the specified type, with a size: eg. `[T; n]`
    pub fn array<T1: Into<Box<T>>, E1: Into<Box<E>>>(ty: T1, size: E1) -> TypeF<N, T, E> {
        TypeF::Array(ty.into(), size.into())
    }

    /// A union of types: eg. `union { T, ... }`
    pub fn union(tys: Vec<T>) -> TypeF<N, T, E> {
        TypeF::Union(tys)
    }
}

impl<N, T, E> TypeF<N, T, E>
where
    T: AsRef<TypeF<N, T, E>> + AsMut<TypeF<N, T, E>>,
    E: AsRef<ExprF<N, E>> + AsMut<ExprF<N, E>>,
{
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    pub fn struct_<Fs>(fields: Fs) -> TypeF<N, T, E>
    where
        N: PartialEq + Clone,
        Fs: IntoIterator<Item = Field<N, T>>,
    {
        // We maintain a list of the seen field names. This will allow us to
        // recover the index of these variables as we abstract later fields...
        let mut seen_names = Vec::new();
        let f = |field: Field<N, T>| {
            let field = field.map_value(|mut ty| {
                T::as_mut(&mut ty).abstract_with(&|x| {
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

        TypeF::Struct(fields.into_iter().map(f).collect())
    }

    /// `true` if the type contains no free variables in itself or its sub-expressions
    pub fn is_closed(&self) -> bool {
        match *self {
            TypeF::Var(ref v) => v.is_closed(),
            TypeF::Bool | TypeF::Int => true,
            TypeF::Array(ref t, ref e) => T::as_ref(t).is_closed() && E::as_ref(e).is_closed(),
            TypeF::Union(ref ts) => ts.iter().all(|t| T::as_ref(t).is_closed()),
            TypeF::Struct(ref fs) => fs.iter().all(|f| T::as_ref(&f.value).is_closed()),
        }
    }

    pub fn abstract_level_with<F>(&mut self, level: u32, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        match *self {
            TypeF::Var(ref mut v) => v.abstract_with(f),
            TypeF::Bool | TypeF::Int => {}
            TypeF::Array(ref mut ty, ref mut e) => {
                T::as_mut(ty).abstract_level_with(level, f);
                E::as_mut(e).abstract_level_with(level, f);
            }
            TypeF::Union(ref mut tys) => for ty in tys {
                T::as_mut(ty).abstract_level_with(level, f);
            },
            TypeF::Struct(ref mut fs) => for (i, field) in fs.iter_mut().enumerate() {
                T::as_mut(&mut field.value).abstract_level_with(level + i as u32, f);
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

    fn instantiate_level(&mut self, level: u32, src: &TypeF<N, T, E>)
    where
        N: Clone,
        T: Clone,
        E: Clone,
    {
        // Bleh: Running into non-lexical liftetime problems here!
        // Just so you know that I'm not going completely insane....
        // FIXME: ensure that expressions are not bound at the same level
        *self = match *self {
            TypeF::Var(Var::Bound(ref i)) => if *i == level {
                src.clone()
            } else {
                return;
            },
            TypeF::Var(Var::Free(_)) | TypeF::Bool | TypeF::Int => return,
            TypeF::Array(ref mut ty, _) => {
                T::as_mut(ty).instantiate_level(level, src);
                return;
            }
            TypeF::Union(ref mut tys) => {
                for ty in tys {
                    T::as_mut(ty).instantiate_level(level, src);
                }
                return;
            }
            TypeF::Struct(ref mut fs) => {
                for (i, f) in fs.iter_mut().enumerate() {
                    T::as_mut(&mut f.value).instantiate_level(level + i as u32, src);
                }
                return;
            }
        };
    }

    pub fn instantiate(&mut self, ty: &TypeF<N, T, E>)
    where
        N: Clone,
        T: Clone,
        E: Clone,
    {
        self.instantiate_level(0, ty);
    }
}
