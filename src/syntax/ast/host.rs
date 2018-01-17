//! The syntax of our data description language

use std::fmt;
use std::rc::Rc;

use name::{Ident, Name, Named};
use syntax::ast::{self, Field, FloatType, SignedType, Substitutions, UnsignedType};
use var::{ScopeIndex, Var};

/// A type constant in the host language
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeConst {
    /// Unit
    Unit,
    /// Bottom
    Bottom,
    /// Boolean
    Bool,
    /// Float
    Float(FloatType),
    /// Signed Integers
    Signed(SignedType),
    /// Unsigned Integers
    Unsigned(UnsignedType),
}

/// A host type
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// A type variable: eg. `T`
    Var(Var),
    /// A type constant
    Const(TypeConst),
    /// Type level lambda abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    Lam(Vec<Named<Name, ()>>, RcType),
    /// Type application: eg. `T(U, V)`
    App(RcType, Vec<RcType>),

    /// Arrow type: eg. `(T, ..) -> U`
    Arrow(Vec<RcType>, RcType),
    /// An array, eg. `[T]`
    Array(RcType),
    /// A union of types: eg. `union { variant : T, ... }`
    Union(Vec<Field<RcType>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Vec<Field<RcType>>),
}

#[derive(Clone, PartialEq)]
pub struct RcType {
    pub inner: Rc<Type>,
}

impl From<Type> for RcType {
    fn from(src: Type) -> RcType {
        RcType {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl RcType {
    /// Type level lambda abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    pub fn lam<T1>(params: Vec<Named<Name, ()>>, body_ty: T1) -> RcType
    where
        T1: Into<RcType>,
    {
        let mut body_ty = body_ty.into();

        {
            let param_names = params
                .iter()
                .map(|param| param.0.clone())
                .collect::<Vec<_>>();

            body_ty.abstract_names(&param_names[..]);
        }

        Type::Lam(params, body_ty).into()
    }

    /// Attempt to lookup the type of a field
    ///
    /// Returns `None` if the type is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &Ident) -> Option<&RcType> {
        match *self.inner {
            Type::Struct(ref fields) => ast::lookup_field(fields, name),
            _ => None,
        }
    }

    /// Attempt to lookup the type of a variant
    ///
    /// Returns `None` if the type is not a union or the field is not
    /// present in the union.
    pub fn lookup_variant(&self, name: &Ident) -> Option<&RcType> {
        match *self.inner {
            Type::Union(ref variants) => ast::lookup_field(variants, name),
            _ => None,
        }
    }

    /// Replace occurrences of the free variables that exist as keys on
    /// `substs` with their corresponding types.
    pub fn substitute(&mut self, substs: &Substitutions) {
        *self = match *Rc::make_mut(&mut self.inner) {
            Type::Var(Var::Free(ref name)) => match substs.get(name) {
                None => return,
                Some(ty) => ty.repr().clone(),
            },
            Type::Var(Var::Bound(_)) | Type::Const(_) => return,
            Type::Lam(_, ref mut body_ty) => {
                body_ty.substitute(substs);
                return;
            }
            Type::App(ref mut fn_ty, ref mut arg_tys) => {
                fn_ty.substitute(substs);

                for arg_ty in arg_tys {
                    arg_ty.substitute(substs);
                }

                return;
            }

            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    param_ty.substitute(substs);
                }
                ret_ty.substitute(substs);

                return;
            }
            Type::Array(ref mut elem_ty) => {
                elem_ty.substitute(substs);
                return;
            }
            Type::Union(ref mut variants) => {
                for variant in variants {
                    variant.value.substitute(substs);
                }
                return;
            }
            Type::Struct(ref mut fields) => {
                for field in fields {
                    field.value.substitute(substs);
                }
                return;
            }
        };
    }

    pub fn abstract_names_at(&mut self, names: &[Name], scope: ScopeIndex) {
        match *Rc::make_mut(&mut self.inner) {
            Type::Var(ref mut var) => var.abstract_names_at(names, scope),
            Type::Const(_) => {}
            Type::Lam(_, ref mut body_ty) => {
                body_ty.abstract_names_at(names, scope.succ());
            }
            Type::App(ref mut fn_ty, ref mut arg_tys) => {
                fn_ty.abstract_names_at(names, scope);

                for arg_ty in arg_tys {
                    arg_ty.abstract_names_at(names, scope);
                }
            }

            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    param_ty.abstract_names_at(names, scope);
                }
                ret_ty.abstract_names_at(names, scope);
            }
            Type::Array(ref mut elem_ty) => {
                elem_ty.abstract_names_at(names, scope);
            }
            Type::Union(ref mut variants) => for variant in variants {
                variant.value.abstract_names_at(names, scope);
            },
            Type::Struct(ref mut fields) => for field in fields {
                field.value.abstract_names_at(names, scope);
            },
        }
    }

    /// Add one layer of abstraction around the type by replacing all the
    /// free variables in `names` with an appropriate De Bruijn index.
    ///
    /// This results in a one 'dangling' index, and so care must be taken
    /// to wrap it in another type that marks the introduction of a new
    /// scope.
    pub fn abstract_names(&mut self, names: &[Name]) {
        self.abstract_names_at(names, ScopeIndex(0))
    }

    fn instantiate_at(&mut self, scope: ScopeIndex, tys: &[RcType]) {
        // FIXME: ensure that expressions are not bound at the same scope
        *self = match *Rc::make_mut(&mut self.inner) {
            Type::Var(Var::Bound(Named(_, var))) if var.scope == scope => {
                tys[var.binding.0 as usize].clone()
            }
            Type::Var(Var::Bound(_)) | Type::Var(Var::Free(_)) | Type::Const(_) => return,
            Type::Lam(_, ref mut ty) => {
                ty.instantiate_at(scope.succ(), tys);
                return;
            }
            Type::App(ref mut ty, ref mut arg_tys) => {
                ty.instantiate_at(scope, tys);

                for arg_ty in arg_tys {
                    arg_ty.instantiate_at(scope, tys);
                }

                return;
            }

            Type::Arrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    param_ty.instantiate_at(scope, tys);
                }

                ret_ty.instantiate_at(scope, tys);
                return;
            }
            Type::Array(ref mut elem_ty) => {
                elem_ty.instantiate_at(scope, tys);
                return;
            }
            Type::Union(ref mut variants) => {
                for variant in variants {
                    variant.value.instantiate_at(scope, tys);
                }
                return;
            }
            Type::Struct(ref mut fields) => {
                for field in fields {
                    field.value.instantiate_at(scope, tys);
                }
                return;
            }
        };
    }

    /// Remove one layer of abstraction in the type by replacing the
    /// appropriate bound variables with copies of `ty`.
    pub fn instantiate(&mut self, tys: &[RcType]) {
        self.instantiate_at(ScopeIndex(0), tys);
    }
}
