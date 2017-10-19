//! The syntax of our data description language

use source::Span;
use syntax::{self, host, Field, Named, Var};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    /// Kind of types
    Type,
    /// Kind of type functions
    Arrow(Box<Kind>, Box<Kind>),
}

impl Kind {
    /// Kind of type functions
    pub fn arrow<K1: Into<Box<Kind>>, K2: Into<Box<Kind>>>(lhs: K1, rhs: K2) -> Kind {
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
    Abs(Named<N, Kind>, Box<T>),
    /// Type application: eg. `T U V`
    App(Box<T>, Box<T>),
}

/// A binary type AST
pub trait TypeNode<N, E: host::ExprNode<N>>
    : Clone + AsRef<TypeF<N, Self, E>> + AsMut<TypeF<N, Self, E>> {
}

/// The recursive innards of a `Type`
///
/// This does the job of tying the recursive knot for `Type`, turning
/// `TypeF` into a tree of expressions.
///
/// We could have just inlined this inside `Type`, but this type alias is
/// handy to have around for defining our conversion impls.
pub type TypeRec<N, E> = TypeF<N, Type<N, E>, E>;

/// A tree of binary types
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type<N, E>(pub TypeRec<N, E>);

impl<N: Clone, E: host::ExprNode<N>> TypeNode<N, E> for Type<N, E> {}

impl<N, E> Into<TypeRec<N, E>> for Type<N, E> {
    fn into(self) -> TypeRec<N, E> {
        self.0
    }
}

impl<N, E> AsRef<TypeRec<N, E>> for Type<N, E> {
    fn as_ref(&self) -> &TypeRec<N, E> {
        &self.0
    }
}

impl<N, E> AsMut<TypeRec<N, E>> for Type<N, E> {
    fn as_mut(&mut self) -> &mut TypeRec<N, E> {
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
pub type SpannedTypeRec<N, E> = TypeF<N, SpannedType<N, E>, E>;

/// A tree of spanned binary types
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpannedType<N, E> {
    pub span: Span,
    pub inner: SpannedTypeRec<N, E>,
}

impl<N: Clone, E: host::ExprNode<N>> TypeNode<N, E> for SpannedType<N, E> {}

impl<N, E> From<TypeRec<N, E>> for Type<N, E> {
    fn from(src: TypeRec<N, E>) -> Type<N, E> {
        Type(src)
    }
}

impl<N, E> AsRef<SpannedTypeRec<N, E>> for SpannedType<N, E> {
    fn as_ref(&self) -> &SpannedTypeRec<N, E> {
        &self.inner
    }
}

impl<N, E> AsMut<SpannedTypeRec<N, E>> for SpannedType<N, E> {
    fn as_mut(&mut self) -> &mut SpannedTypeRec<N, E> {
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
    pub fn array<T1: Into<Box<T>>, E1: Into<Box<E>>>(elem_ty: T1, size_expr: E1) -> TypeF<N, T, E> {
        TypeF::Array(elem_ty.into(), size_expr.into())
    }

    /// A union of types: eg. `union { T, ... }`
    pub fn union(tys: Vec<T>) -> TypeF<N, T, E> {
        TypeF::Union(tys)
    }

    /// Type application: eg. `T U V`
    pub fn app<T1: Into<Box<T>>, T2: Into<Box<T>>>(ty1: T1, ty2: T2) -> TypeF<N, T, E> {
        TypeF::App(ty1.into(), ty2.into())
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
    pub fn abs<T1>(param: Named<N, Kind>, body_ty: T1) -> TypeF<N, T, E>
    where
        N: PartialEq + Clone,
        T1: Into<Box<T>>,
    {
        let mut body_ty = body_ty.into();
        T::as_mut(&mut body_ty).abstract_name(&param.0);
        TypeF::Abs(param, body_ty)
    }

    fn abstract_level_with<F>(&mut self, level: u32, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        match *self {
            TypeF::Var(ref mut v) => v.abstract_with(f),
            TypeF::Bit => {}
            TypeF::Array(ref mut elem_ty, ref mut size_expr) => {
                T::as_mut(elem_ty).abstract_level_with(level, f);
                E::as_mut(size_expr).abstract_level_with(level, f);
            }
            TypeF::Cond(Named(_, ref mut ty), ref mut pred_expr) => {
                T::as_mut(ty).abstract_level_with(level, f);
                E::as_mut(pred_expr).abstract_level_with(level, f);
            }
            TypeF::Abs(_, ref mut body_ty) => {
                T::as_mut(body_ty).abstract_level_with(level + 1, f);
            }
            TypeF::Union(ref mut tys) => for ty in tys {
                T::as_mut(ty).abstract_level_with(level, f);
            },
            TypeF::Struct(ref mut fields) => for (i, field) in fields.iter_mut().enumerate() {
                T::as_mut(&mut field.value).abstract_level_with(level + i as u32, f);
            },
            TypeF::App(ref mut fn_ty, ref mut arg_ty) => {
                T::as_mut(fn_ty).abstract_level_with(level, f);
                T::as_mut(arg_ty).abstract_level_with(level, f);
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
            TypeF::Array(ref mut elem_ty, _) => {
                T::as_mut(elem_ty).instantiate_level(level, src);
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
            TypeF::Struct(ref mut fields) => {
                for (i, field) in fields.iter_mut().enumerate() {
                    T::as_mut(&mut field.value).instantiate_level(level + i as u32, src);
                }
                return;
            }
            TypeF::Abs(_, ref mut ty) => {
                T::as_mut(ty).instantiate_level(level + 1, src);
                return;
            }
            TypeF::App(ref mut ty, ref mut arg_ty) => {
                T::as_mut(ty).instantiate_level(level, src);
                T::as_mut(arg_ty).instantiate_level(level, src);
                return;
            }
        };
    }

    pub fn instantiate(&mut self, ty: &TypeF<N, T, E>)
    where
        N: Clone,
    {
        self.instantiate_level(0, ty);
    }

    pub fn repr(&self) -> Result<host::Type<N, E>, ()>
    where
        N: Clone,
    {
        match *self {
            TypeF::Var(ref v) => Ok(host::TypeF::Var(v.clone()).into()),
            TypeF::Bit => Ok(host::TypeF::Bit.into()),
            TypeF::Array(ref elem_ty, ref size_expr) => {
                let elem_repr_ty = Box::new(T::as_ref(elem_ty).repr()?);
                let size_expr = size_expr.clone();

                Ok(host::TypeF::Array(elem_repr_ty, size_expr).into())
            }
            TypeF::Cond(Named(_, ref ty), _) => T::as_ref(ty).repr(),
            TypeF::Union(ref tys) => {
                let repr_tys = tys.iter()
                    .map(T::as_ref)
                    .map(TypeF::repr)
                    .collect::<Result<_, _>>()?;

                Ok(host::TypeF::Union(repr_tys).into())
            }
            TypeF::Struct(ref fields) => {
                let repr_fields = fields
                    .iter()
                    .map(|f| {
                        T::as_ref(&f.value)
                            .repr()
                            .map(|ty| Field::new(f.name.clone(), ty))
                    })
                    .collect::<Result<_, _>>()?;

                Ok(host::TypeF::Struct(repr_fields).into())
            }
            TypeF::Abs(_, _) | TypeF::App(_, _) => Err(()),
        }
    }
}

impl<N, T, E> TypeF<N, T, E>
where
    N: PartialEq,
{
    pub fn lookup_field(&self, name: &N) -> Option<&T> {
        match *self {
            TypeF::Struct(ref fields) => syntax::lookup_field(fields, name),
            _ => None,
        }
    }
}
