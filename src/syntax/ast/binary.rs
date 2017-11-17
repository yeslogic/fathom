//! The syntax of our data description language

use std::rc::Rc;

use name::{Name, Named};
use source::Span;
use syntax::ast::{self, host, Field, Substitutions};
use var::{BoundVar, ScopeIndex, Var};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Kind {
    /// Kind of types
    Type,
    /// Kind of type functions
    ///
    /// For now we only allow type arguments of kind `Type`. We represent this
    /// as an arity count
    Arrow { arity: u32 },
}

impl Kind {
    /// Kind of type functions
    pub fn arrow(arity: u32) -> Kind {
        Kind::Arrow { arity }
    }

    pub fn repr(self) -> host::Kind {
        match self {
            Kind::Type => host::Kind::Type,
            Kind::Arrow { arity } => host::Kind::arrow(arity),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConst {
    U8,
}

/// A binary type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<N> {
    /// A type variable: eg. `T`
    Var(Span, Var<N>),
    /// Type constant
    Const(TypeConst),
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Span, RcType<N>, host::RcExpr<N>),
    /// A union of types: eg. `union { field : T, ... }`
    Union(Span, Vec<Field<N, RcType<N>>>),
    /// A struct type, with fields: eg. `struct { variant : T, ... }`
    Struct(Span, Vec<Field<N, RcType<N>>>),
    /// A type that is constrained by a predicate: eg. `T where x => x == 3`
    Assert(Span, RcType<N>, host::RcExpr<N>),
    /// An interpreted type
    Interp(Span, RcType<N>, host::RcExpr<N>, host::RcType<N>),
    /// Type abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    Abs(Span, Vec<Named<N, ()>>, RcType<N>),
    /// Type application: eg. `T(U, V)`
    App(Span, RcType<N>, Vec<RcType<N>>),
}

pub type RcType<N> = Rc<Type<N>>;

impl<N: Name> Type<N> {
    /// A free type variable: eg. `T`
    pub fn fvar<N1: Into<N>>(span: Span, x: N1) -> Type<N> {
        Type::Var(span, Var::Free(x.into()))
    }

    /// A bound type variable
    pub fn bvar<N1: Into<N>>(span: Span, x: N1, i: BoundVar) -> Type<N> {
        Type::Var(span, Var::Bound(Named(x.into(), i)))
    }

    /// Byte type constant
    pub fn u8() -> Type<N> {
        Type::Const(TypeConst::U8)
    }

    /// An array of the specified type, with a size: eg. `[T; n]`
    pub fn array<T1, E1>(span: Span, elem_ty: T1, size_expr: E1) -> Type<N>
    where
        T1: Into<RcType<N>>,
        E1: Into<host::RcExpr<N>>,
    {
        Type::Array(span, elem_ty.into(), size_expr.into())
    }

    /// A union of types: eg. `union { variant : T, ... }`
    pub fn union(span: Span, variants: Vec<Field<N, RcType<N>>>) -> Type<N> {
        Type::Union(span, variants)
    }

    /// A struct type, with fields: eg. `struct { field : T, ... }`
    pub fn struct_(span: Span, mut fields: Vec<Field<N, RcType<N>>>) -> Type<N> {
        // We maintain a list of the seen field names. This will allow us to
        // recover the index of these variables as we abstract later fields...
        let mut seen_names = Vec::<N>::with_capacity(fields.len());

        for field in &mut fields {
            for (scope, name) in seen_names.iter().rev().enumerate() {
                Rc::make_mut(&mut field.value)
                    .abstract_names_at(&[name.clone()], ScopeIndex(scope as u32));
            }

            // Record that the field has been 'seen'
            seen_names.push(field.name.clone());
        }

        Type::Struct(span, fields)
    }

    /// A type that is constrained by a predicate: eg. `T where x => x == 3`
    pub fn assert<T1, E1>(span: Span, ty: T1, pred: E1) -> Type<N>
    where
        T1: Into<RcType<N>>,
        E1: Into<host::RcExpr<N>>,
    {
        Type::Assert(span, ty.into(), pred.into())
    }

    /// An interpreted type
    pub fn interp<T1, E1, T2>(span: Span, ty: T1, conv: E1, repr_ty: T2) -> Type<N>
    where
        T1: Into<RcType<N>>,
        E1: Into<host::RcExpr<N>>,
        T2: Into<host::RcType<N>>,
    {
        Type::Interp(span, ty.into(), conv.into(), repr_ty.into())
    }

    /// Type abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    pub fn abs<T1>(span: Span, param_names: &[N], body_ty: T1) -> Type<N>
    where
        T1: Into<RcType<N>>,
    {
        let params = param_names
            .iter()
            .map(|name| Named(name.clone(), ()))
            .collect();

        let mut body_ty = body_ty.into();
        Rc::make_mut(&mut body_ty).abstract_names(&param_names);

        Type::Abs(span, params, body_ty)
    }

    /// Type application: eg. `T(U, V)`
    pub fn app<T1>(span: Span, fn_ty: T1, arg_tys: Vec<RcType<N>>) -> Type<N>
    where
        T1: Into<RcType<N>>,
    {
        Type::App(span, fn_ty.into(), arg_tys)
    }

    /// Attempt to lookup the type of a field
    ///
    /// Returns `None` if the type is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &N) -> Option<&RcType<N>> {
        match *self {
            Type::Struct(_, ref fields) => ast::lookup_field(fields, name),
            _ => None,
        }
    }

    /// Attempt to lookup the type of a variant
    ///
    /// Returns `None` if the type is not a union or the field is not
    /// present in the union.
    pub fn lookup_variant(&self, name: &N) -> Option<&RcType<N>> {
        match *self {
            Type::Union(_, ref variants) => ast::lookup_field(variants, name),
            _ => None,
        }
    }

    /// Replace occurrences of the free variables that exist as keys on
    /// `substs` with their corresponding types.
    pub fn substitute(&mut self, substs: &Substitutions<N>) {
        let subst_ty = match *self {
            Type::Var(_, Var::Free(ref name)) => match substs.get(name) {
                None => return,
                Some(ty) => ty.clone(),
            },
            Type::Var(_, Var::Bound(_)) | Type::Const(_) => return,
            Type::Array(_, ref mut elem_ty, ref mut _size_expr) => {
                Rc::make_mut(elem_ty).substitute(substs);
                // Rc::make_mut(size_expr).substitute(substs);
                return;
            }
            Type::Union(_, ref mut variants) => {
                for variant in variants {
                    Rc::make_mut(&mut variant.value).substitute(substs);
                }
                return;
            }
            Type::Struct(_, ref mut fields) => {
                for field in fields.iter_mut() {
                    Rc::make_mut(&mut field.value).substitute(substs);
                }
                return;
            }
            Type::Assert(_, ref mut ty, ref mut _pred) => {
                Rc::make_mut(ty).substitute(substs);
                // Rc::make_mut(pred).substitute(substs);
                return;
            }
            Type::Interp(_, ref mut ty, ref mut _conv, ref mut _repr_ty) => {
                Rc::make_mut(ty).substitute(substs);
                // Rc::make_mut(conv).substitute(substs);
                // Rc::make_mut(repr_ty).substitute(substs);
                return;
            }
            Type::Abs(_, _, ref mut body_ty) => {
                Rc::make_mut(body_ty).substitute(substs);
                return;
            }
            Type::App(_, ref mut fn_ty, ref mut arg_tys) => {
                Rc::make_mut(fn_ty).substitute(substs);

                for arg_ty in arg_tys {
                    Rc::make_mut(arg_ty).substitute(substs);
                }

                return;
            }
        };

        *self = subst_ty.clone();
    }

    pub fn abstract_names_at(&mut self, names: &[N], scope: ScopeIndex) {
        match *self {
            Type::Var(_, ref mut var) => var.abstract_names_at(names, scope),
            Type::Const(_) => {}
            Type::Array(_, ref mut elem_ty, ref mut size_expr) => {
                Rc::make_mut(elem_ty).abstract_names_at(names, scope);
                Rc::make_mut(size_expr).abstract_names_at(names, scope);
            }
            Type::Union(_, ref mut variants) => for variant in variants {
                Rc::make_mut(&mut variant.value).abstract_names_at(names, scope);
            },
            Type::Struct(_, ref mut fields) => for (i, field) in fields.iter_mut().enumerate() {
                Rc::make_mut(&mut field.value).abstract_names_at(names, scope.shift(i as u32));
            },
            Type::Assert(_, ref mut ty, ref mut pred) => {
                Rc::make_mut(ty).abstract_names_at(names, scope);
                Rc::make_mut(pred).abstract_names_at(names, scope.succ());
            }
            Type::Interp(_, ref mut ty, ref mut conv, ref mut repr_ty) => {
                Rc::make_mut(ty).abstract_names_at(names, scope);
                Rc::make_mut(conv).abstract_names_at(names, scope.succ());
                Rc::make_mut(repr_ty).abstract_names_at(names, scope);
            }
            Type::Abs(_, _, ref mut body_ty) => {
                Rc::make_mut(body_ty).abstract_names_at(names, scope.succ());
            }
            Type::App(_, ref mut fn_ty, ref mut arg_tys) => {
                Rc::make_mut(fn_ty).abstract_names_at(names, scope);

                for arg_ty in arg_tys {
                    Rc::make_mut(arg_ty).abstract_names_at(names, scope);
                }
            }
        }
    }

    /// Add one layer of abstraction around the type by replacing all the
    /// free variables in `names` with an appropriate De Bruijn index.
    ///
    /// This results in a one 'dangling' index, and so care must be taken
    /// to wrap it in another type that marks the introduction of a new
    /// scope.
    pub fn abstract_names(&mut self, names: &[N]) {
        self.abstract_names_at(names, ScopeIndex(0));
    }

    fn instantiate_at(&mut self, scope: ScopeIndex, tys: &[RcType<N>]) {
        // FIXME: ensure that expressions are not bound at the same scope
        match *self {
            Type::Var(_, Var::Bound(Named(_, var))) => if var.scope == scope {
                *self = (*tys[var.binding.0 as usize]).clone();
            },
            Type::Var(_, Var::Free(_)) | Type::Const(_) => {}
            Type::Array(_, ref mut elem_ty, _) => {
                Rc::make_mut(elem_ty).instantiate_at(scope, tys);
            }
            Type::Assert(_, ref mut ty, _) => {
                Rc::make_mut(ty).instantiate_at(scope.succ(), tys);
            }
            Type::Interp(_, ref mut ty, _, _) => {
                Rc::make_mut(ty).instantiate_at(scope.succ(), tys);
            }
            Type::Union(_, ref mut variants) => for variant in variants {
                Rc::make_mut(&mut variant.value).instantiate_at(scope, tys);
            },
            Type::Struct(_, ref mut fields) => for (i, field) in fields.iter_mut().enumerate() {
                Rc::make_mut(&mut field.value).instantiate_at(scope.shift(i as u32), tys);
            },
            Type::Abs(_, _, ref mut ty) => {
                Rc::make_mut(ty).instantiate_at(scope.succ(), tys);
            }
            Type::App(_, ref mut ty, ref mut arg_tys) => {
                Rc::make_mut(ty).instantiate_at(scope, tys);

                for arg_ty in arg_tys {
                    Rc::make_mut(arg_ty).instantiate_at(scope, tys);
                }
            }
        }
    }

    /// Remove one layer of abstraction in the type by replacing the
    /// appropriate bound variables with copies of `ty`.
    pub fn instantiate(&mut self, tys: &[RcType<N>]) {
        self.instantiate_at(ScopeIndex(0), tys);
    }

    /// Returns the host representation of the binary type
    pub fn repr(&self) -> host::RcType<N> {
        match *self {
            Type::Var(_, ref v) => Rc::new(host::Type::Var(v.clone())),
            Type::Const(TypeConst::U8) => Rc::new(host::Type::Const(host::TypeConst::U8)),
            Type::Array(_, ref elem_ty, _) => Rc::new(host::Type::Array(elem_ty.repr())),
            Type::Assert(_, ref ty, _) => ty.repr(),
            Type::Interp(_, _, _, ref repr_ty) => repr_ty.clone(),
            Type::Union(_, ref variants) => {
                let repr_variants = variants
                    .iter()
                    .map(|variant| {
                        Field::new(variant.name.clone(), variant.value.repr())
                    })
                    .collect();

                Rc::new(host::Type::Union(repr_variants))
            }
            Type::Struct(_, ref fields) => {
                let repr_fields = fields
                    .iter()
                    .map(|field| Field::new(field.name.clone(), field.value.repr()))
                    .collect();

                Rc::new(host::Type::Struct(repr_fields))
            }
            Type::Abs(_, ref params, ref body_ty) => {
                Rc::new(host::Type::Abs(params.clone(), body_ty.repr()))
            }
            Type::App(_, ref fn_ty, ref arg_tys) => {
                let arg_tys = arg_tys.iter().map(|arg| arg.repr()).collect();

                Rc::new(host::Type::App(fn_ty.repr(), arg_tys))
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

            type T = Type<&'static str>;

            #[test]
            fn id() {
                // λx. x
                // λ   0
                let ty = T::abs(Span::start(), &["x"], T::fvar(Span::start(), "x"));

                assert_debug_snapshot!(ty_abs_id, ty);
            }

            // Examples from https://en.wikipedia.org/wiki/De_Bruijn_index

            #[test]
            fn k_combinator() {
                // λx.λy. x
                // λ  λ   1
                let ty = T::abs(
                    Span::start(),
                    &["x"],
                    T::abs(Span::start(), &["y"], T::fvar(Span::start(), "x")),
                );

                assert_debug_snapshot!(ty_abs_k_combinator, ty);
            }

            #[test]
            fn s_combinator() {
                // λx.λy.λz. x z (y z)
                // λ  λ  λ   2 0 (1 0)
                let ty = T::abs(
                    Span::start(),
                    &["x"],
                    T::abs(
                        Span::start(),
                        &["y"],
                        T::abs(
                            Span::start(),
                            &["z"],
                            T::app(
                                Span::start(),
                                T::app(
                                    Span::start(),
                                    T::fvar(Span::start(), "x"),
                                    vec![Rc::new(T::fvar(Span::start(), "z"))],
                                ),
                                vec![
                                    Rc::new(T::app(
                                        Span::start(),
                                        T::fvar(Span::start(), "y"),
                                        vec![Rc::new(T::fvar(Span::start(), "z"))],
                                    )),
                                ],
                            ),
                        ),
                    ),
                );

                assert_debug_snapshot!(ty_abs_s_combinator, ty);
            }

            #[test]
            fn complex() {
                // λz.(λy. y (λx. x)) (λx. z x)
                // λ  (λ   0 (λ   0)) (λ   1 0)
                let ty = T::abs(
                    Span::start(),
                    &["z"],
                    T::app(
                        Span::start(),
                        T::abs(
                            Span::start(),
                            &["y"],
                            T::app(
                                Span::start(),
                                T::fvar(Span::start(), "y"),
                                vec![
                                    Rc::new(
                                        T::abs(Span::start(), &["x"], T::fvar(Span::start(), "x")),
                                    ),
                                ],
                            ),
                        ),
                        vec![
                            Rc::new(T::abs(
                                Span::start(),
                                &["x"],
                                T::app(
                                    Span::start(),
                                    T::fvar(Span::start(), "z"),
                                    vec![Rc::new(T::fvar(Span::start(), "x"))],
                                ),
                            )),
                        ],
                    ),
                );

                assert_debug_snapshot!(ty_abs_complex, ty);
            }
        }
    }
}
