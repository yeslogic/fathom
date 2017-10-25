//! The syntax of our data description language

use syntax::{self, host, Field, Name, Named, Var};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConst {
    Bit,
}

/// A binary type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<N> {
    /// A type variable: eg. `T`
    Var(Var<N, Named<N, u32>>),
    /// Type constant
    Const(TypeConst),
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Box<Type<N>>, Box<host::Expr<N>>),
    /// A union of types: eg. `union { T, ... }`
    Union(Vec<Type<N>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Vec<Field<N, Type<N>>>),
    /// A type constrained by a predicate: eg. `T where x => x == 3`
    Cond(Named<N, Box<Type<N>>>, Box<host::Expr<N>>),
    /// An interpreted type
    Interp(
        Named<N, Box<Type<N>>>,
        Box<host::Expr<N>>,
        Box<host::Type<N>>,
    ),
    /// Type abstraction: eg. `\(a : Type) -> T`
    Abs(Named<N, Kind>, Box<Type<N>>),
    /// Type application: eg. `T U V`
    App(Box<Type<N>>, Box<Type<N>>),
}

impl<N: Name> Type<N> {
    /// A free type variable: eg. `T`
    pub fn fvar<N1: Into<N>>(x: N1) -> Type<N> {
        Type::Var(Var::Free(x.into()))
    }

    /// A bound type variable
    pub fn bvar<N1: Into<N>>(x: N1, i: u32) -> Type<N> {
        Type::Var(Var::Bound(Named(x.into(), i)))
    }

    /// An array of the specified type, with a size: eg. `[T; n]`
    pub fn array<T1: Into<Box<Type<N>>>, E1: Into<Box<host::Expr<N>>>>(
        elem_ty: T1,
        size_expr: E1,
    ) -> Type<N> {
        Type::Array(elem_ty.into(), size_expr.into())
    }

    /// A union of types: eg. `union { T, ... }`
    pub fn union(tys: Vec<Type<N>>) -> Type<N> {
        Type::Union(tys)
    }

    /// Type application: eg. `T U V`
    pub fn app<T1: Into<Box<Type<N>>>, T2: Into<Box<Type<N>>>>(ty1: T1, ty2: T2) -> Type<N> {
        Type::App(ty1.into(), ty2.into())
    }

    /// A struct type, with fields: eg. `struct { field : T, ... }`
    pub fn struct_(mut fields: Vec<Field<N, Type<N>>>) -> Type<N> {
        // We maintain a list of the seen field names. This will allow us to
        // recover the index of these variables as we abstract later fields...
        let mut seen_names = Vec::with_capacity(fields.len());

        for field in &mut fields {
            field.value.abstract_with(&|x| {
                seen_names
                    .iter()
                    .position(|y| x == y)
                    .map(|i| Named(x.clone(), i as u32))
            });

            // Record that the field has been 'seen'
            seen_names.push(field.name.clone());
        }

        Type::Struct(fields)
    }

    /// A type constrained by a predicate: eg. `T where x => x == 3`
    pub fn cond<N1, T1, E1>(ty: T1, param: N1, pred: E1) -> Type<N>
    where
        N1: Into<N>,
        T1: Into<Box<Type<N>>>,
        E1: Into<Box<host::Expr<N>>>,
    {
        let param = param.into();
        let mut pred = pred.into();
        pred.abstract_name(&param);

        Type::Cond(Named(param, ty.into()), pred)
    }

    /// An interpreted type
    pub fn interp<N1, T1, E1, T2>(ty: T1, param: N1, conv_expr: E1, repr_ty: T2) -> Type<N>
    where
        N1: Into<N>,
        T1: Into<Box<Type<N>>>,
        E1: Into<Box<host::Expr<N>>>,
        T2: Into<Box<host::Type<N>>>,
    {
        let param = param.into();
        let mut conv_expr = conv_expr.into();
        conv_expr.abstract_name(&param);

        Type::Interp(Named(param, ty.into()), conv_expr, repr_ty.into())
    }

    /// Type abstraction: eg. `\(a : Type) -> T`
    pub fn abs<T1>(param: Named<N, Kind>, body_ty: T1) -> Type<N>
    where
        T1: Into<Box<Type<N>>>,
    {
        let mut body_ty = body_ty.into();
        body_ty.abstract_name(&param.0);
        Type::Abs(param, body_ty)
    }

    pub fn lookup_field(&self, name: &N) -> Option<&Type<N>> {
        match *self {
            Type::Struct(ref fields) => syntax::lookup_field(fields, name),
            _ => None,
        }
    }

    fn abstract_level_with<F>(&mut self, level: u32, f: &F)
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
            Type::Cond(Named(_, ref mut ty), ref mut pred_expr) => {
                ty.abstract_level_with(level, f);
                pred_expr.abstract_level_with(level + 1, f);
            }
            Type::Interp(Named(_, ref mut ty), ref mut cond_expr, ref mut repr_ty) => {
                ty.abstract_level_with(level, f);
                cond_expr.abstract_level_with(level + 1, f);
                repr_ty.abstract_level_with(level, f);
            }
            Type::Abs(_, ref mut body_ty) => {
                body_ty.abstract_level_with(level + 1, f);
            }
            Type::App(ref mut fn_ty, ref mut arg_ty) => {
                fn_ty.abstract_level_with(level, f);
                arg_ty.abstract_level_with(level, f);
            }
        };
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
            Type::Cond(Named(_, ref mut ty), _) => {
                ty.instantiate_level(level + 1, src);
                return;
            }
            Type::Interp(Named(_, ref mut ty), _, _) => {
                ty.instantiate_level(level + 1, src);
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
            Type::Abs(_, ref mut ty) => {
                ty.instantiate_level(level + 1, src);
                return;
            }
            Type::App(ref mut ty, ref mut arg_ty) => {
                ty.instantiate_level(level, src);
                arg_ty.instantiate_level(level, src);
                return;
            }
        };
    }

    pub fn instantiate(&mut self, ty: &Type<N>) {
        self.instantiate_level(0, ty);
    }

    pub fn repr(&self) -> Result<host::Type<N>, ()> {
        match *self {
            Type::Var(ref v) => Ok(host::Type::Var(v.clone()).into()),
            Type::Const(TypeConst::Bit) => Ok(host::Type::Const(host::TypeConst::Bit).into()),
            Type::Array(ref elem_ty, ref size_expr) => {
                let elem_repr_ty = Box::new(elem_ty.repr()?);
                let size_expr = size_expr.clone();

                Ok(host::Type::Array(elem_repr_ty, size_expr).into())
            }
            Type::Cond(Named(_, ref ty), _) => ty.repr(),
            Type::Interp(Named(_, _), _, ref repr_ty) => Ok((**repr_ty).clone()),
            Type::Union(ref tys) => {
                let repr_tys = tys.iter().map(Type::repr).collect::<Result<_, _>>()?;

                Ok(host::Type::Union(repr_tys).into())
            }
            Type::Struct(ref fields) => {
                let repr_fields = fields
                    .iter()
                    .map(|f| f.value.repr().map(|ty| Field::new(f.name.clone(), ty)))
                    .collect::<Result<_, _>>()?;

                Ok(host::Type::Struct(repr_fields).into())
            }
            Type::Abs(_, _) | Type::App(_, _) => Err(()),
        }
    }
}
