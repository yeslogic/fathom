//! The syntax of our data description language

use std::fmt;
use std::rc::Rc;

use name::{Ident, Name, Named};
use syntax::ast::{self, Field, FloatType, SignedType, Substitutions, UnsignedType};
use var::{ScopeIndex, Var};

/// A type constant in the host language
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum HostTypeConst {
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
    HostVar(Var),
    /// A type constant
    HostConst(HostTypeConst),
    /// Type level lambda abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    HostLam(Vec<Named<Name, ()>>, RcType),
    /// Type application: eg. `T(U, V)`
    HostApp(RcType, Vec<RcType>),

    /// Arrow type: eg. `(T, ..) -> U`
    HostArrow(Vec<RcType>, RcType),
    /// An array, eg. `[T]`
    HostArray(RcType),
    /// A union of types: eg. `union { variant : T, ... }`
    HostUnion(Vec<Field<RcType>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    HostStruct(Vec<Field<RcType>>),
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

        Type::HostLam(params, body_ty).into()
    }

    /// Attempt to lookup the type of a field
    ///
    /// Returns `None` if the type is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &Ident) -> Option<&RcType> {
        match *self.inner {
            Type::HostStruct(ref fields) => ast::lookup_field(fields, name),
            _ => None,
        }
    }

    /// Attempt to lookup the type of a variant
    ///
    /// Returns `None` if the type is not a union or the field is not
    /// present in the union.
    pub fn lookup_variant(&self, name: &Ident) -> Option<&RcType> {
        match *self.inner {
            Type::HostUnion(ref variants) => ast::lookup_field(variants, name),
            _ => None,
        }
    }

    /// Replace occurrences of the free variables that exist as keys on
    /// `substs` with their corresponding types.
    pub fn substitute(&mut self, substs: &Substitutions) {
        *self = match *Rc::make_mut(&mut self.inner) {
            Type::HostVar(Var::Free(ref name)) => match substs.get(name) {
                None => return,
                Some(ty) => ty.repr().clone(),
            },
            Type::HostVar(Var::Bound(_)) | Type::HostConst(_) => return,
            Type::HostLam(_, ref mut body_ty) => {
                body_ty.substitute(substs);
                return;
            }
            Type::HostApp(ref mut fn_ty, ref mut arg_tys) => {
                fn_ty.substitute(substs);

                for arg_ty in arg_tys {
                    arg_ty.substitute(substs);
                }

                return;
            }

            Type::HostArrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    param_ty.substitute(substs);
                }
                ret_ty.substitute(substs);

                return;
            }
            Type::HostArray(ref mut elem_ty) => {
                elem_ty.substitute(substs);
                return;
            }
            Type::HostUnion(ref mut variants) => {
                for variant in variants {
                    variant.value.substitute(substs);
                }
                return;
            }
            Type::HostStruct(ref mut fields) => {
                for field in fields {
                    field.value.substitute(substs);
                }
                return;
            }
        };
    }

    pub fn abstract_names_at(&mut self, names: &[Name], scope: ScopeIndex) {
        match *Rc::make_mut(&mut self.inner) {
            Type::HostVar(ref mut var) => var.abstract_names_at(names, scope),
            Type::HostConst(_) => {}
            Type::HostLam(_, ref mut body_ty) => {
                body_ty.abstract_names_at(names, scope.succ());
            }
            Type::HostApp(ref mut fn_ty, ref mut arg_tys) => {
                fn_ty.abstract_names_at(names, scope);

                for arg_ty in arg_tys {
                    arg_ty.abstract_names_at(names, scope);
                }
            }

            Type::HostArrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    param_ty.abstract_names_at(names, scope);
                }
                ret_ty.abstract_names_at(names, scope);
            }
            Type::HostArray(ref mut elem_ty) => {
                elem_ty.abstract_names_at(names, scope);
            }
            Type::HostUnion(ref mut variants) => for variant in variants {
                variant.value.abstract_names_at(names, scope);
            },
            Type::HostStruct(ref mut fields) => for field in fields {
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
            Type::HostVar(Var::Bound(Named(_, var))) if var.scope == scope => {
                tys[var.binding.0 as usize].clone()
            }
            Type::HostVar(Var::Bound(_)) | Type::HostVar(Var::Free(_)) | Type::HostConst(_) => {
                return
            }
            Type::HostLam(_, ref mut ty) => {
                ty.instantiate_at(scope.succ(), tys);
                return;
            }
            Type::HostApp(ref mut ty, ref mut arg_tys) => {
                ty.instantiate_at(scope, tys);

                for arg_ty in arg_tys {
                    arg_ty.instantiate_at(scope, tys);
                }

                return;
            }

            Type::HostArrow(ref mut param_tys, ref mut ret_ty) => {
                for param_ty in param_tys {
                    param_ty.instantiate_at(scope, tys);
                }

                ret_ty.instantiate_at(scope, tys);
                return;
            }
            Type::HostArray(ref mut elem_ty) => {
                elem_ty.instantiate_at(scope, tys);
                return;
            }
            Type::HostUnion(ref mut variants) => {
                for variant in variants {
                    variant.value.instantiate_at(scope, tys);
                }
                return;
            }
            Type::HostStruct(ref mut fields) => {
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
