//! The syntax of our data description language

use source::Span;
use ast::{host, Field, Named, Var};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Type,
    Arrow(Box<Kind>, Box<Kind>),
}

impl Kind {
    pub fn arrow<T, U>(lhs: T, rhs: U) -> Kind
    where
        T: Into<Box<Kind>>,
        U: Into<Box<Kind>>,
    {
        Kind::Arrow(lhs.into(), rhs.into())
    }
}

/// A binary type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeF<N, T, E> {
    /// A type variable: eg. `T`
    Var(Var<N, Named<N, u32>>),
    /// Bit
    Bit,
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Box<T>, Box<E>),
    /// A union of types: eg. `union { T, ... }`
    Union(Vec<T>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Vec<Field<N, T>>),
    /// A type constrained by a predicate: eg. `T where x => x == 3`
    Cond(Named<N, Box<T>>, Box<E>),
    /// Type abstraction: eg. `\(a : Type) -> T`
    Abs(Vec<Named<N, Kind>>, Box<T>),
    /// Type application: eg. `T U V`
    App(Box<T>, Vec<T>),
}

/// A binary type AST
pub trait TypeNode<N, E: host::ExprNode<N>>: Sized + AsRef<TypeF<N, Self, E>> + AsMut<TypeF<N, Self, E>> {}

/// The recursive innards of a `Type`
///
/// This does the job of tying the recursive knot for `Type`, turning
/// `TypeF` into a tree of expressions.
///
/// We could have just inlined this inside `Type`, but this type alias is
/// handy to have around for defining our conversion impls.
pub type TypeRec<N> = TypeF<N, Type<N>, host::Expr<N>>;

/// A tree of binary types
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type<N>(pub TypeRec<N>);

impl<N> TypeNode<N, host::Expr<N>> for Type<N> {}

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
pub type SpannedTypeRec<N> = TypeF<N, SpannedType<N>, host::SpannedExpr<N>>;

/// A tree of spanned binary types
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpannedType<N> {
    pub span: Span,
    pub inner: SpannedTypeRec<N>,
}

impl<N> TypeNode<N, host::SpannedExpr<N>> for SpannedType<N> {}

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
    pub fn bvar<N1: Into<N>>(x: N1, i: u32) -> TypeF<N, T, E> {
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
    T: TypeNode<N, E>,
    E: host::ExprNode<N>,
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

    /// A type constrained by a predicate: eg. `T where x => x == 3`
    pub fn cond<N1, T1, E1>(ty: T1, param: N1, pred: E1) -> TypeF<N, T, E>
    where
        N: PartialEq + Clone,
        N1: Into<N>,
        T1: Into<Box<T>>,
        E1: Into<Box<E>>,
    {
        let param = param.into();
        let mut pred = pred.into();
        E::as_mut(&mut pred).abstract_name(&param);

        TypeF::Cond(Named(param, ty.into()), pred)
    }

    /// Type abstraction: eg. `\(a : Type) -> T`
    pub fn abs<T1>(params: Vec<Named<N, Kind>>, ty: T1) -> TypeF<N, T, E>
    where
        N: PartialEq + Clone,
        T1: Into<Box<T>>,
    {
        let mut ty = ty.into();
        T::as_mut(&mut ty).abstract_with(&|x| {
            params
                .iter()
                .position(|&Named(ref y, _)| x == y)
                .map(|i| Named(x.clone(), i as u32))
        });

        TypeF::Abs(params, ty)
    }

    /// Type application: eg. `T U V`
    pub fn app<T1: Into<Box<T>>>(ty: T1, args: Vec<T>) -> TypeF<N, T, E> {
        TypeF::App(ty.into(), args)
    }

    /// `true` if the type contains no free variables in itself or its sub-expressions
    pub fn is_closed(&self) -> bool {
        match *self {
            TypeF::Var(ref v) => v.is_closed(),
            TypeF::Bit => true,
            TypeF::Array(ref t, ref e) => T::as_ref(t).is_closed() && E::as_ref(e).is_closed(),
            TypeF::Union(ref ts) => ts.iter().all(|t| T::as_ref(t).is_closed()),
            TypeF::Struct(ref fs) => fs.iter().all(|f| T::as_ref(&f.value).is_closed()),
            TypeF::Cond(Named(_, ref t), ref e) => {
                T::as_ref(t).is_closed() && E::as_ref(e).is_closed()
            }
            TypeF::Abs(_, ref t) => T::as_ref(t).is_closed(),
            TypeF::App(ref t, ref ts) => {
                T::as_ref(t).is_closed() && ts.iter().all(|t| T::as_ref(t).is_closed())
            }
        }
    }

    fn abstract_level_with<F>(&mut self, level: u32, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        match *self {
            TypeF::Var(ref mut v) => v.abstract_with(f),
            TypeF::Bit => {}
            TypeF::Array(ref mut ty, ref mut e) => {
                T::as_mut(ty).abstract_level_with(level, f);
                E::as_mut(e).abstract_level_with(level, f);
            }
            TypeF::Cond(Named(_, ref mut ty), ref mut e) => {
                T::as_mut(ty).abstract_level_with(level, f);
                E::as_mut(e).abstract_level_with(level, f);
            }
            TypeF::Abs(ref params, ref mut ty) => {
                T::as_mut(ty).abstract_level_with(level + params.len() as u32, f);
            }
            TypeF::Union(ref mut tys) => for ty in tys {
                T::as_mut(ty).abstract_level_with(level, f);
            },
            TypeF::Struct(ref mut fs) => for (i, field) in fs.iter_mut().enumerate() {
                T::as_mut(&mut field.value).abstract_level_with(level + i as u32, f);
            },
            TypeF::App(ref mut ty, ref mut tys) => {
                T::as_mut(ty).abstract_level_with(level, f);
                for ty in tys {
                    T::as_mut(ty).abstract_level_with(level, f);
                }
            }
        };
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
            TypeF::Var(Var::Free(_)) | TypeF::Bit => return,
            TypeF::Array(ref mut ty, _) => {
                T::as_mut(ty).instantiate_level(level, src);
                return;
            }
            TypeF::Cond(Named(_, ref mut ty), _) => {
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
            TypeF::Abs(ref params, ref mut ty) => {
                T::as_mut(ty).instantiate_level(level + params.len() as u32, src);
                return;
            }
            TypeF::App(ref mut ty, ref mut tys) => {
                T::as_mut(ty).instantiate_level(level, src);
                for ty in tys {
                    T::as_mut(ty).instantiate_level(level, src);
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
