//! The syntax of our data description language

use source::Span;
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

/// An error that occurred when trying to convert a binary type to
/// its host representation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReprError<N> {
    NoCorrespondingHostType(Type<N>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConst {
    Bit,
}

/// A binary type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<N> {
    /// A type variable: eg. `T`
    Var(Span, Var<N, u32>),
    /// Type constant
    Const(TypeConst),
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Span, Box<Type<N>>, Box<host::Expr<N>>),
    /// A union of types: eg. `union { T, ... }`
    Union(Span, Vec<Type<N>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Span, Vec<Field<N, Type<N>>>),
    /// A type constrained by a predicate: eg. `T where x => x == 3`
    Cond(Span, Box<Type<N>>, Box<host::Expr<N>>),
    /// An interpreted type
    Interp(Span, Box<Type<N>>, Box<host::Expr<N>>, Box<host::Type<N>>),
    /// Type abstraction: eg. `\(a : Type) -> T`
    Abs(Span, Named<N, Kind>, Box<Type<N>>),
    /// Type application: eg. `T U V`
    App(Span, Box<Type<N>>, Box<Type<N>>),
}

impl<N: Name> Type<N> {
    /// A free type variable: eg. `T`
    pub fn fvar<N1: Into<N>>(span: Span, x: N1) -> Type<N> {
        Type::Var(span, Var::Free(x.into()))
    }

    /// A bound type variable
    pub fn bvar<N1: Into<N>>(span: Span, x: N1, i: u32) -> Type<N> {
        Type::Var(span, Var::Bound(Named(x.into(), i)))
    }

    /// Bit type constant
    pub fn bit() -> Type<N> {
        Type::Const(TypeConst::Bit)
    }

    /// An array of the specified type, with a size: eg. `[T; n]`
    pub fn array<T1, E1>(span: Span, elem_ty: T1, size_expr: E1) -> Type<N>
    where
        T1: Into<Box<Type<N>>>,
        E1: Into<Box<host::Expr<N>>>,
    {
        Type::Array(span, elem_ty.into(), size_expr.into())
    }

    /// A union of types: eg. `union { T, ... }`
    pub fn union(span: Span, tys: Vec<Type<N>>) -> Type<N> {
        Type::Union(span, tys)
    }

    /// Type application: eg. `T U V`
    pub fn app<T1, T2>(span: Span, ty1: T1, ty2: T2) -> Type<N>
    where
        T1: Into<Box<Type<N>>>,
        T2: Into<Box<Type<N>>>,
    {
        Type::App(span, ty1.into(), ty2.into())
    }

    /// A struct type, with fields: eg. `struct { field : T, ... }`
    pub fn struct_(span: Span, mut fields: Vec<Field<N, Type<N>>>) -> Type<N> {
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

        Type::Struct(span, fields)
    }

    /// A type constrained by a predicate: eg. `T where x => x == 3`
    pub fn cond<T1, E1>(span: Span, ty: T1, pred: E1) -> Type<N>
    where
        T1: Into<Box<Type<N>>>,
        E1: Into<Box<host::Expr<N>>>,
    {
        Type::Cond(span, ty.into(), pred.into())
    }

    /// An interpreted type
    pub fn interp<T1, E1, T2>(span: Span, ty: T1, conv: E1, repr_ty: T2) -> Type<N>
    where
        T1: Into<Box<Type<N>>>,
        E1: Into<Box<host::Expr<N>>>,
        T2: Into<Box<host::Type<N>>>,
    {
        Type::Interp(span, ty.into(), conv.into(), repr_ty.into())
    }

    /// Type abstraction: eg. `\(a : Type) -> T`
    pub fn abs<N1, T1>(span: Span, (param_name, param_kind): (N1, Kind), body_ty: T1) -> Type<N>
    where
        N1: Into<N>,
        T1: Into<Box<Type<N>>>,
    {
        let param_name = param_name.into();
        let mut body_ty = body_ty.into();
        body_ty.abstract_name(&param_name);
        Type::Abs(span, Named(param_name, param_kind), body_ty)
    }

    /// Attempt to lookup the type of a field
    ///
    /// Returns `None` if the expression is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &N) -> Option<&Type<N>> {
        match *self {
            Type::Struct(_, ref fields) => syntax::lookup_field(fields, name),
            _ => None,
        }
    }

    /// Replace occurrences of the free variable `name` with the given type
    pub fn substitute(&mut self, name: &N, src_ty: &Type<N>) {
        match *self {
            Type::Var(_, Var::Free(ref n)) if n == name => {}
            Type::Var(_, _) | Type::Const(_) => return,
            Type::Array(_, ref mut elem_ty, ref mut _size_expr) => {
                elem_ty.substitute(name, src_ty);
                // size_expr.substitute(name, src_ty);
                return;
            }
            Type::Union(_, ref mut tys) => {
                for ty in tys {
                    ty.substitute(name, src_ty);
                }
                return;
            }
            Type::Struct(_, ref mut fields) => {
                for field in fields.iter_mut() {
                    field.value.substitute(name, src_ty);
                }
                return;
            }
            Type::Cond(_, ref mut ty, ref mut _pred) => {
                ty.substitute(name, src_ty);
                // pred.substitute(name, src_ty);
                return;
            }
            Type::Interp(_, ref mut ty, ref mut _conv, ref mut _repr_ty) => {
                ty.substitute(name, src_ty);
                // conv.substitute(name, src_ty);
                // repr_ty.substitute(name, src_ty);
                return;
            }
            Type::Abs(_, _, ref mut body_ty) => {
                body_ty.substitute(name, src_ty);
                return;
            }
            Type::App(_, ref mut fn_ty, ref mut arg_ty) => {
                fn_ty.substitute(name, src_ty);
                arg_ty.substitute(name, src_ty);
                return;
            }
        }
        *self = src_ty.clone();
    }

    pub fn abstract_name_at(&mut self, name: &N, level: u32) {
        match *self {
            Type::Var(_, ref mut var) => var.abstract_name_at(name, level),
            Type::Const(_) => {}
            Type::Array(_, ref mut elem_ty, ref mut size_expr) => {
                elem_ty.abstract_name_at(name, level);
                size_expr.abstract_name_at(name, level);
            }
            Type::Union(_, ref mut tys) => for ty in tys {
                ty.abstract_name_at(name, level);
            },
            Type::Struct(_, ref mut fields) => for (i, field) in fields.iter_mut().enumerate() {
                field.value.abstract_name_at(name, level + i as u32);
            },
            Type::Cond(_, ref mut ty, ref mut pred) => {
                ty.abstract_name_at(name, level);
                pred.abstract_name_at(name, level + 1);
            }
            Type::Interp(_, ref mut ty, ref mut conv, ref mut repr_ty) => {
                ty.abstract_name_at(name, level);
                conv.abstract_name_at(name, level + 1);
                repr_ty.abstract_name_at(name, level);
            }
            Type::Abs(_, _, ref mut body_ty) => {
                body_ty.abstract_name_at(name, level + 1);
            }
            Type::App(_, ref mut fn_ty, ref mut arg_ty) => {
                fn_ty.abstract_name_at(name, level);
                arg_ty.abstract_name_at(name, level);
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
        self.abstract_name_at(name, 0);
    }

    fn instantiate_at(&mut self, level: u32, src: &Type<N>) {
        // FIXME: ensure that expressions are not bound at the same level
        match *self {
            Type::Var(_, Var::Bound(Named(_, i))) => if i == level {
                *self = src.clone();
            },
            Type::Var(_, Var::Free(_)) | Type::Const(_) => {}
            Type::Array(_, ref mut elem_ty, _) => {
                elem_ty.instantiate_at(level, src);
            }
            Type::Cond(_, ref mut ty, _) => {
                ty.instantiate_at(level + 1, src);
            }
            Type::Interp(_, ref mut ty, _, _) => {
                ty.instantiate_at(level + 1, src);
            }
            Type::Union(_, ref mut tys) => for ty in tys {
                ty.instantiate_at(level, src);
            },
            Type::Struct(_, ref mut fields) => for (i, field) in fields.iter_mut().enumerate() {
                field.value.instantiate_at(level + i as u32, src);
            },
            Type::Abs(_, _, ref mut ty) => {
                ty.instantiate_at(level + 1, src);
            }
            Type::App(_, ref mut ty, ref mut arg_ty) => {
                ty.instantiate_at(level, src);
                arg_ty.instantiate_at(level, src);
            }
        }
    }

    /// Remove one layer of abstraction in the type by replacing the
    /// appropriate bound variables with copies of `ty`.
    pub fn instantiate(&mut self, ty: &Type<N>) {
        self.instantiate_at(0, ty);
    }

    /// Returns the host representation of the binary type
    pub fn repr(&self) -> Result<host::Type<N>, ReprError<N>> {
        match *self {
            Type::Var(_, ref v) => Ok(host::Type::Var(v.clone()).into()),
            Type::Const(TypeConst::Bit) => Ok(host::Type::Const(host::TypeConst::Bit).into()),
            Type::Array(_, ref elem_ty, ref size_expr) => {
                let elem_repr_ty = Box::new(elem_ty.repr()?);
                let size_expr = size_expr.clone();

                Ok(host::Type::Array(elem_repr_ty, size_expr).into())
            }
            Type::Cond(_, ref ty, _) => ty.repr(),
            Type::Interp(_, _, _, ref repr_ty) => Ok((**repr_ty).clone()),
            Type::Union(_, ref tys) => {
                let repr_tys = tys.iter().map(Type::repr).collect::<Result<_, _>>()?;

                Ok(host::Type::Union(repr_tys).into())
            }
            Type::Struct(_, ref fields) => {
                let repr_fields = fields
                    .iter()
                    .map(|f| f.value.repr().map(|ty| Field::new(f.name.clone(), ty)))
                    .collect::<Result<_, _>>()?;

                Ok(host::Type::Struct(repr_fields).into())
            }
            Type::Abs(_, _, _) | Type::App(_, _, _) => {
                Err(ReprError::NoCorrespondingHostType(self.clone()))
            }
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
                // λx. x
                // λ   0
                let ty = T::abs(Span::start(), ("x", K::Type), T::fvar(Span::start(), "x"));

                assert_snapshot!(ty_abs_id, ty);
            }

            // Examples from https://en.wikipedia.org/wiki/De_Bruijn_index

            #[test]
            fn k_combinator() {
                // λx.λy. x
                // λ  λ   1
                let ty = T::abs(
                    Span::start(),
                    ("x", K::Type),
                    T::abs(Span::start(), ("y", K::Type), T::fvar(Span::start(), "x")),
                );

                assert_snapshot!(ty_abs_k_combinator, ty);
            }

            #[test]
            fn s_combinator() {
                // λx.λy.λz. x z (y z)
                // λ  λ  λ   2 0 (1 0)
                let ty = T::abs(
                    Span::start(),
                    ("x", K::Type),
                    T::abs(
                        Span::start(),
                        ("y", K::Type),
                        T::abs(
                            Span::start(),
                            ("z", K::Type),
                            T::app(
                                Span::start(),
                                T::app(
                                    Span::start(),
                                    T::fvar(Span::start(), "x"),
                                    T::fvar(Span::start(), "z"),
                                ),
                                T::app(
                                    Span::start(),
                                    T::fvar(Span::start(), "y"),
                                    T::fvar(Span::start(), "z"),
                                ),
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
                    Span::start(),
                    ("z", K::Type),
                    T::app(
                        Span::start(),
                        T::abs(
                            Span::start(),
                            ("y", K::Type),
                            T::app(
                                Span::start(),
                                T::fvar(Span::start(), "y"),
                                T::abs(Span::start(), ("x", K::Type), T::fvar(Span::start(), "x")),
                            ),
                        ),
                        T::abs(
                            Span::start(),
                            ("x", K::Type),
                            T::app(
                                Span::start(),
                                T::fvar(Span::start(), "z"),
                                T::fvar(Span::start(), "x"),
                            ),
                        ),
                    ),
                );

                assert_snapshot!(ty_abs_complex, ty);
            }
        }
    }
}
