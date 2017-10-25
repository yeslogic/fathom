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
    Var(Var<N, u32>),
    /// Type constant
    Const(TypeConst),
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Box<Type<N>>, Box<host::Expr<N>>),
    /// A union of types: eg. `union { T, ... }`
    Union(Vec<Type<N>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Vec<Field<N, Type<N>>>),
    /// A type constrained by a predicate: eg. `T where x => x == 3`
    Cond(Box<Type<N>>, Box<host::Expr<N>>),
    /// An interpreted type
    Interp(Box<Type<N>>, Box<host::Expr<N>>, Box<host::Type<N>>),
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

    /// Bit type constant
    pub fn bit() -> Type<N> {
        Type::Const(TypeConst::Bit)
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
            for name in &seen_names {
                field.value.abstract_name(name);
            }

            // Record that the field has been 'seen'
            seen_names.push(field.name.clone());
        }

        Type::Struct(fields)
    }

    /// A type constrained by a predicate: eg. `T where x => x == 3`
    pub fn cond<T1, E1>(ty: T1, pred: E1) -> Type<N>
    where
        T1: Into<Box<Type<N>>>,
        E1: Into<Box<host::Expr<N>>>,
    {
        Type::Cond(ty.into(), pred.into())
    }

    /// An interpreted type
    pub fn interp<T1, E1, T2>(ty: T1, conv: E1, repr_ty: T2) -> Type<N>
    where
        T1: Into<Box<Type<N>>>,
        E1: Into<Box<host::Expr<N>>>,
        T2: Into<Box<host::Type<N>>>,
    {
        Type::Interp(ty.into(), conv.into(), repr_ty.into())
    }

    /// Type abstraction: eg. `\(a : Type) -> T`
    pub fn abs<N1, T1>((param_name, param_kind): (N1, Kind), body_ty: T1) -> Type<N>
    where
        N1: Into<N>,
        T1: Into<Box<Type<N>>>,
    {
        let param_name = param_name.into();
        let mut body_ty = body_ty.into();
        body_ty.abstract_name(&param_name);
        Type::Abs(Named(param_name, param_kind), body_ty)
    }

    pub fn lookup_field(&self, name: &N) -> Option<&Type<N>> {
        match *self {
            Type::Struct(ref fields) => syntax::lookup_field(fields, name),
            _ => None,
        }
    }

    fn abstract_name_at(&mut self, name: &N, level: u32) {
        match *self {
            Type::Var(ref mut var) => var.abstract_name_at(name, level),
            Type::Const(_) => {}
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
            Type::Cond(ref mut ty, ref mut pred) => {
                ty.abstract_name_at(name, level);
                pred.abstract_name_at(name, level + 1);
            }
            Type::Interp(ref mut ty, ref mut conv, ref mut repr_ty) => {
                ty.abstract_name_at(name, level);
                conv.abstract_name_at(name, level + 1);
                repr_ty.abstract_name_at(name, level);
            }
            Type::Abs(_, ref mut body_ty) => {
                body_ty.abstract_name_at(name, level + 1);
            }
            Type::App(ref mut fn_ty, ref mut arg_ty) => {
                fn_ty.abstract_name_at(name, level);
                arg_ty.abstract_name_at(name, level);
            }
        }
    }

    pub fn abstract_name(&mut self, name: &N) {
        self.abstract_name_at(name, 0);
    }

    fn instantiate_at(&mut self, level: u32, src: &Type<N>) {
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
                elem_ty.instantiate_at(level, src);
                return;
            }
            Type::Cond(ref mut ty, _) => {
                ty.instantiate_at(level + 1, src);
                return;
            }
            Type::Interp(ref mut ty, _, _) => {
                ty.instantiate_at(level + 1, src);
                return;
            }
            Type::Union(ref mut tys) => {
                for ty in tys {
                    ty.instantiate_at(level, src);
                }
                return;
            }
            Type::Struct(ref mut fields) => {
                for (i, field) in fields.iter_mut().enumerate() {
                    field.value.instantiate_at(level + i as u32, src);
                }
                return;
            }
            Type::Abs(_, ref mut ty) => {
                ty.instantiate_at(level + 1, src);
                return;
            }
            Type::App(ref mut ty, ref mut arg_ty) => {
                ty.instantiate_at(level, src);
                arg_ty.instantiate_at(level, src);
                return;
            }
        };
    }

    pub fn instantiate(&mut self, ty: &Type<N>) {
        self.instantiate_at(0, ty);
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
            Type::Cond(ref ty, _) => ty.repr(),
            Type::Interp(_, _, ref repr_ty) => Ok((**repr_ty).clone()),
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

#[cfg(test)]
mod tests {
    use super::*;

    mod ty {
        use super::*;

        mod abs {
            use super::*;
            use self::Kind as K;

            type T = Type<&'static str>;

            #[test]
            fn id() {
                // λx.λy. x
                // λ  λ   1
                let ty = T::abs(("x", K::Type), T::fvar("x"));

                assert_snapshot!(ty_abs_id, ty);
            }

            // Examples from https://en.wikipedia.org/wiki/De_Bruijn_index

            #[test]
            fn k_combinator() {
                // λx.λy. x
                // λ  λ   1
                let ty = T::abs(("x", K::Type), T::abs(("y", K::Type), T::fvar("x")));

                assert_snapshot!(ty_abs_k_combinator, ty);
            }

            #[test]
            fn s_combinator() {
                // λx.λy.λz. x z (y z)
                // λ  λ  λ   2 0 (1 0)
                let ty = T::abs(
                    ("x", K::Type),
                    T::abs(
                        ("y", K::Type),
                        T::abs(
                            ("z", K::Type),
                            T::app(
                                T::app(T::fvar("x"), T::fvar("z")),
                                T::app(T::fvar("y"), T::fvar("z")),
                            ),
                        ),
                    ),
                );

                assert_snapshot!(ty_abs_s_combinator, ty);
            }

            #[test]
            fn complex() {
                // λz.(λy. y (λx. x)) (λx. z x)
                // λ  (λ   0 (λ   0)) (λ   1 0)
                let ty = T::abs(
                    ("z", K::Type),
                    T::app(
                        T::abs(
                            ("y", K::Type),
                            T::app(T::fvar("y"), T::abs(("x", K::Type), T::fvar("x"))),
                        ),
                        T::abs(("x", K::Type), T::app(T::fvar("z"), T::fvar("x"))),
                    ),
                );

                assert_snapshot!(ty_abs_complex, ty);
            }
        }
    }
}
