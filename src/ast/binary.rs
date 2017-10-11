//! The syntax of our data description language

use source::Spanned;
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

/// A type definition
///
/// ```plain
/// Point = {
///     x : u16,
///     y : u16,
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition<N> {
    pub name: N,
    pub ty: SpannedType<N>,
}

pub type SpannedDefinition<N> = Spanned<Definition<N>>;

impl<N> Definition<N> {
    pub fn new<M: Into<N>>(name: M, ty: SpannedType<N>) -> Definition<N> {
        Definition {
            name: name.into(),
            ty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<N> {
    /// A type variable: eg. `T`
    Var(Var<N, Named<N, u32>>),
    /// Bit
    Bit,
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Box<SpannedType<N>>, host::SpannedExpr<N>),
    /// A union of types: eg. `union { T, ... }`
    Union(Vec<SpannedType<N>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Vec<Field<N, Type<N>>>),
    /// A type constrained by a predicate: eg. `T where x => x == 3`
    Cond(Named<Spanned<N>, Box<SpannedType<N>>>, host::SpannedExpr<N>),
    /// Type abstraction: eg. `\(a : Type) -> T`
    Abs(Vec<Spanned<Named<N, Kind>>>, Box<SpannedType<N>>),
    /// Type application: eg. `T U V`
    App(Box<SpannedType<N>>, Vec<SpannedType<N>>),
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
    pub fn array<T>(ty: T, size: host::SpannedExpr<N>) -> Type<N>
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

    /// A type constrained by a predicate: eg. `T where x => x == 3`
    pub fn cond<T, M: Into<N>>(ty: T, param: Spanned<M>, mut pred: host::SpannedExpr<N>) -> Type<N>
    where
        N: PartialEq + Clone,
        T: Into<Box<SpannedType<N>>>,
    {
        let param = param.map(M::into);
        pred.value.abstract_name(&param.value);

        Type::Cond(Named(param, ty.into()), pred)
    }

    /// Type abstraction: eg. `\(a : Type) -> T`
    pub fn abs<T>(params: Vec<Spanned<Named<N, Kind>>>, ty: T) -> Type<N>
    where
        N: PartialEq + Clone,
        T: Into<Box<SpannedType<N>>>,
    {
        let mut ty = ty.into();
        ty.value.abstract_with(&|x| {
            params
                .iter()
                .position(|param| x == &param.value.0)
                .map(|i| Named(x.clone(), i as u32))
        });

        Type::Abs(params, ty)
    }

    /// Type application: eg. `T U V`
    pub fn app<T>(ty: T, args: Vec<SpannedType<N>>) -> Type<N>
    where
        T: Into<Box<SpannedType<N>>>,
    {
        Type::App(ty.into(), args)
    }

    /// `true` if the type contains no free variables in itself or its sub-expressions
    pub fn is_closed(&self) -> bool {
        match *self {
            Type::Var(ref v) => v.is_closed(),
            Type::Bit => true,
            Type::Array(ref t, ref e) => t.value.is_closed() && e.value.is_closed(),
            Type::Union(ref ts) => ts.iter().all(|t| t.value.is_closed()),
            Type::Struct(ref fs) => fs.iter().all(|f| f.value.value.is_closed()),
            Type::Cond(ref t, ref e) => t.1.value.is_closed() && e.value.is_closed(),
            Type::Abs(_, ref t) => t.value.is_closed(),
            Type::App(ref t, ref ts) => {
                t.value.is_closed() && ts.iter().all(|t| t.value.is_closed())
            }
        }
    }

    fn abstract_level_with<F>(&mut self, level: u32, f: &F)
    where
        F: Fn(&N) -> Option<Named<N, u32>>,
    {
        match *self {
            Type::Var(ref mut v) => v.abstract_with(f),
            Type::Bit => {}
            Type::Array(ref mut ty, ref mut e) => {
                ty.value.abstract_level_with(level, f);
                e.value.abstract_level_with(level, f);
            }
            Type::Cond(ref mut ty, ref mut e) => {
                ty.1.value.abstract_level_with(level, f);
                e.value.abstract_level_with(level, f);
            }
            Type::Abs(ref params, ref mut ty) => {
                ty.value.abstract_level_with(level + params.len() as u32, f);
            }
            Type::Union(ref mut tys) => for ty in tys {
                ty.value.abstract_level_with(level, f);
            },
            Type::Struct(ref mut fs) => for (i, field) in fs.iter_mut().enumerate() {
                field.value.value.abstract_level_with(level + i as u32, f);
            },
            Type::App(ref mut ty, ref mut tys) => {
                ty.value.abstract_level_with(level, f);
                for ty in tys {
                    ty.value.abstract_level_with(level, f);
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
            Type::Var(Var::Free(_)) | Type::Bit => return,
            Type::Array(ref mut ty, _) => {
                ty.value.instantiate_level(level, src);
                return;
            }
            Type::Cond(ref mut ty, _) => {
                ty.1.value.instantiate_level(level, src);
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
            Type::Abs(ref params, ref mut ty) => {
                ty.value.instantiate_level(level + params.len() as u32, src);
                return;
            }
            Type::App(ref mut ty, ref mut tys) => {
                ty.value.instantiate_level(level, src);
                for ty in tys {
                    ty.value.instantiate_level(level, src);
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
