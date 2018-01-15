//! The syntax of our data description language

use std::fmt;
use std::rc::Rc;

use name::Named;
use source::Span;
use syntax::ast::{self, host, Field, Substitutions};
use parser::ast::binary::Type as ParseType;
use var::{ScopeIndex, Var};

/// Kinds of binary types
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

    /// The host representation of the binary kind
    pub fn repr(self) -> host::Kind {
        match self {
            Kind::Type => host::Kind::Type,
            Kind::Arrow { arity } => host::Kind::arrow(arity),
        }
    }
}

/// The endianness (byte order) of a type
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Endianness {
    /// Big endian
    Big,
    /// Little endian
    Little,
}

/// A type constant in the binary language
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeConst {
    /// Empty binary type
    Empty,
    /// Error binary type
    Error,
    /// Unsigned 8-bit integer
    U8,
    /// Signed 8-bit integer
    I8,
    /// Unsigned 16-bit integer
    U16(Endianness),
    /// Unsigned 24-bit integer
    U24(Endianness),
    /// Unsigned 32-bit integer
    U32(Endianness),
    /// Unsigned 64-bit integer
    U64(Endianness),
    /// Signed 16-bit integer
    I16(Endianness),
    /// Signed 24-bit integer
    I24(Endianness),
    /// Signed 32-bit integer
    I32(Endianness),
    /// Signed 64-bit integer
    I64(Endianness),
    /// IEEE-754 32-bit float
    F32(Endianness),
    /// IEEE-754 64-bit float
    F64(Endianness),
}

impl TypeConst {
    /// Convert a bianary type constant to its corresponding host representation
    pub fn repr(self) -> host::TypeConst {
        use syntax::ast::host::{FloatType, SignedType, UnsignedType};

        match self {
            TypeConst::Empty => host::TypeConst::Unit,
            TypeConst::Error => host::TypeConst::Bottom,
            TypeConst::U8 => host::TypeConst::Unsigned(UnsignedType::U8),
            TypeConst::I8 => host::TypeConst::Signed(SignedType::I8),
            TypeConst::U16(_) => host::TypeConst::Unsigned(UnsignedType::U16),
            TypeConst::U24(_) => host::TypeConst::Unsigned(UnsignedType::U24),
            TypeConst::U32(_) => host::TypeConst::Unsigned(UnsignedType::U32),
            TypeConst::U64(_) => host::TypeConst::Unsigned(UnsignedType::U64),
            TypeConst::I16(_) => host::TypeConst::Signed(SignedType::I16),
            TypeConst::I24(_) => host::TypeConst::Signed(SignedType::I24),
            TypeConst::I32(_) => host::TypeConst::Signed(SignedType::I32),
            TypeConst::I64(_) => host::TypeConst::Signed(SignedType::I64),
            TypeConst::F32(_) => host::TypeConst::Float(FloatType::F32),
            TypeConst::F64(_) => host::TypeConst::Float(FloatType::F64),
        }
    }
}

/// A binary type
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// A type variable: eg. `T`
    Var(Span, Var),
    /// Type constant
    Const(TypeConst),
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Span, RcType, host::RcIExpr),
    /// Conditional types: eg. `cond { field : pred => T, ... }`
    Cond(Span, Vec<Field<(host::RcCExpr, RcType)>>),
    /// A struct type, with fields: eg. `struct { variant : T, ... }`
    Struct(Span, Vec<Field<RcType>>),
    /// A type that is constrained by a predicate: eg. `T where x => x == 3`
    Assert(Span, RcType, host::RcCExpr),
    /// An interpreted type
    Interp(Span, RcType, host::RcCExpr, host::RcType),
    /// Type level lambda abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    Lam(Span, Vec<Named<()>>, RcType),
    /// Type application: eg. `T(U, V)`
    App(Span, RcType, Vec<RcType>),
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
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    pub fn struct_(span: Span, mut fields: Vec<Field<RcType>>) -> RcType {
        // We maintain a list of the seen field names. This will allow us to
        // recover the index of these variables as we abstract later fields...
        let mut seen_names = Vec::<String>::with_capacity(fields.len());

        for field in &mut fields {
            for (scope, name) in seen_names.iter().rev().enumerate() {
                field
                    .value
                    .abstract_names_at(&[name], ScopeIndex(scope as u32));
            }

            // Record that the field has been 'seen'
            seen_names.push(field.name.clone());
        }

        Type::Struct(span, fields).into()
    }

    /// Type level lambda abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    pub fn lam<T1>(span: Span, param_names: &[&str], body_ty: T1) -> RcType
    where
        T1: Into<RcType>,
    {
        let params = param_names
            .iter()
            .map(|&name| Named(String::from(name), ()))
            .collect();

        let mut body_ty = body_ty.into();
        body_ty.abstract_names(param_names);

        Type::Lam(span, params, body_ty).into()
    }

    /// Attempt to lookup the type of a field
    ///
    /// Returns `None` if the type is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &str) -> Option<&RcType> {
        match *self.inner {
            Type::Struct(_, ref fields) => ast::lookup_field(fields, name),
            _ => None,
        }
    }

    /// Attempt to lookup the type of a variant
    ///
    /// Returns `None` if the type is not a union or the field is not
    /// present in the union.
    pub fn lookup_variant(&self, name: &str) -> Option<&(host::RcCExpr, RcType)> {
        match *self.inner {
            Type::Cond(_, ref options) => ast::lookup_field(options, name),
            _ => None,
        }
    }

    /// Replace occurrences of the free variables that exist as keys on
    /// `substs` with their corresponding types.
    pub fn substitute(&mut self, substs: &Substitutions) {
        let subst_ty = match *Rc::make_mut(&mut self.inner) {
            Type::Var(_, Var::Free(ref name)) => match substs.get(name) {
                None => return,
                Some(ty) => ty.clone(),
            },
            Type::Var(_, Var::Bound(_)) | Type::Const(_) => return,
            Type::Array(_, ref mut elem_ty, ref mut size_expr) => {
                elem_ty.substitute(substs);
                size_expr.substitute(substs);
                return;
            }
            Type::Cond(_, ref mut options) => {
                for option in options {
                    option.value.0.substitute(substs);
                    option.value.1.substitute(substs);
                }
                return;
            }
            Type::Struct(_, ref mut fields) => {
                for field in fields.iter_mut() {
                    field.value.substitute(substs);
                }
                return;
            }
            Type::Assert(_, ref mut ty, ref mut pred) => {
                ty.substitute(substs);
                pred.substitute(substs);
                return;
            }
            Type::Interp(_, ref mut ty, ref mut conv, ref mut repr_ty) => {
                ty.substitute(substs);
                conv.substitute(substs);
                repr_ty.substitute(substs);
                return;
            }
            Type::Lam(_, _, ref mut body_ty) => {
                body_ty.substitute(substs);
                return;
            }
            Type::App(_, ref mut fn_ty, ref mut arg_tys) => {
                fn_ty.substitute(substs);

                for arg_ty in arg_tys {
                    arg_ty.substitute(substs);
                }

                return;
            }
        };

        *self = subst_ty.clone();
    }

    pub fn abstract_names_at(&mut self, names: &[&str], scope: ScopeIndex) {
        match *Rc::make_mut(&mut self.inner) {
            Type::Var(_, ref mut var) => var.abstract_names_at(names, scope),
            Type::Const(_) => {}
            Type::Array(_, ref mut elem_ty, ref mut size_expr) => {
                elem_ty.abstract_names_at(names, scope);
                size_expr.abstract_names_at(names, scope);
            }
            Type::Cond(_, ref mut options) => for option in options {
                option.value.0.abstract_names_at(names, scope);
                option.value.1.abstract_names_at(names, scope);
            },
            Type::Struct(_, ref mut fields) => for (i, field) in fields.iter_mut().enumerate() {
                field.value.abstract_names_at(names, scope.shift(i as u32));
            },
            Type::Assert(_, ref mut ty, ref mut pred) => {
                ty.abstract_names_at(names, scope);
                pred.abstract_names_at(names, scope.succ());
            }
            Type::Interp(_, ref mut ty, ref mut conv, ref mut repr_ty) => {
                ty.abstract_names_at(names, scope);
                conv.abstract_names_at(names, scope.succ());
                repr_ty.abstract_names_at(names, scope);
            }
            Type::Lam(_, _, ref mut body_ty) => {
                body_ty.abstract_names_at(names, scope.succ());
            }
            Type::App(_, ref mut fn_ty, ref mut arg_tys) => {
                fn_ty.abstract_names_at(names, scope);

                for arg_ty in arg_tys {
                    arg_ty.abstract_names_at(names, scope);
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
    pub fn abstract_names(&mut self, names: &[&str]) {
        self.abstract_names_at(names, ScopeIndex(0));
    }

    fn instantiate_at(&mut self, scope: ScopeIndex, tys: &[RcType]) {
        // FIXME: ensure that expressions are not bound at the same scope
        *self = match *Rc::make_mut(&mut self.inner) {
            Type::Var(_, Var::Bound(Named(_, var))) if var.scope == scope => {
                tys[var.binding.0 as usize].clone()
            }
            Type::Var(_, Var::Bound(_)) | Type::Var(_, Var::Free(_)) | Type::Const(_) => return,
            Type::Array(_, ref mut elem_ty, _) => {
                elem_ty.instantiate_at(scope, tys);
                return;
            }
            Type::Assert(_, ref mut ty, _) => {
                ty.instantiate_at(scope.succ(), tys);
                return;
            }
            Type::Interp(_, ref mut ty, _, _) => {
                ty.instantiate_at(scope.succ(), tys);
                return;
            }
            Type::Cond(_, ref mut options) => {
                for option in options {
                    // option.value.0.instantiate_at(scope, tys);
                    option.value.1.instantiate_at(scope, tys);
                }
                return;
            }
            Type::Struct(_, ref mut fields) => {
                for (i, field) in fields.iter_mut().enumerate() {
                    field.value.instantiate_at(scope.shift(i as u32), tys);
                }
                return;
            }
            Type::Lam(_, _, ref mut ty) => {
                ty.instantiate_at(scope.succ(), tys);
                return;
            }
            Type::App(_, ref mut ty, ref mut arg_tys) => {
                ty.instantiate_at(scope, tys);

                for arg_ty in arg_tys {
                    arg_ty.instantiate_at(scope, tys);
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

    /// Returns the host representation of the binary type
    pub fn repr(&self) -> host::RcType {
        match *self.inner {
            Type::Var(_, ref v) => host::Type::Var(v.clone()).into(),
            Type::Const(ty_const) => host::Type::Const(ty_const.repr()).into(),
            Type::Array(_, ref elem_ty, _) => host::Type::Array(elem_ty.repr()).into(),
            Type::Assert(_, ref ty, _) => ty.repr(),
            Type::Interp(_, _, _, ref repr_ty) => repr_ty.clone(),
            Type::Cond(_, ref options) => {
                let repr_variants = options
                    .iter()
                    .map(|variant| Field {
                        doc: Rc::clone(&variant.doc),
                        name: variant.name.clone(),
                        value: variant.value.1.repr(),
                    })
                    .collect();

                host::Type::Union(repr_variants).into()
            }
            Type::Struct(_, ref fields) => {
                let repr_fields = fields
                    .iter()
                    .map(|field| Field {
                        doc: Rc::clone(&field.doc),
                        name: field.name.clone(),
                        value: field.value.repr(),
                    })
                    .collect();

                host::Type::Struct(repr_fields).into()
            }
            Type::Lam(_, ref params, ref body_ty) => {
                host::Type::Lam(params.clone(), body_ty.repr()).into()
            }
            Type::App(_, ref fn_ty, ref arg_tys) => {
                let arg_tys = arg_tys.iter().map(|arg| arg.repr()).collect();

                host::Type::App(fn_ty.repr(), arg_tys).into()
            }
        }
    }
}

impl<'src> From<&'src ParseType<'src>> for RcType {
    fn from(src: &'src ParseType<'src>) -> RcType {
        match *src {
            ParseType::Var(span, name) => Type::Var(span, Var::free(name)).into(),
            ParseType::Array(span, ref elem_ty, ref size_expr) => {
                let elem_ty = RcType::from(&**elem_ty);
                let size_expr = host::RcIExpr::from(&**size_expr);

                Type::Array(span, elem_ty, size_expr).into()
            }
            ParseType::Cond(span, ref options) => {
                let options = options
                    .iter()
                    .map(|variant| Field {
                        doc: variant.doc.join("\n").into(),
                        name: String::from(variant.name),
                        value: (
                            host::CExpr::Inf(host::RcIExpr::from(&variant.value.0)).into(),
                            RcType::from(&variant.value.1),
                        ),
                    })
                    .collect();

                Type::Cond(span, options).into()
            }
            ParseType::Struct(span, ref fields) => {
                let fields = fields
                    .iter()
                    .map(|field| Field {
                        doc: field.doc.join("\n").into(),
                        name: String::from(field.name),
                        value: RcType::from(&field.value),
                    })
                    .collect();

                RcType::struct_(span, fields)
            }
            ParseType::Where(span, ref ty, lo2, param_name, ref pred_expr) => {
                let ty = RcType::from(&**ty);
                let span2 = Span::new(lo2, span.hi());
                let pred_params = vec![Named(String::from(param_name), ty.repr())];
                let pred_expr = host::RcIExpr::lam(span2, pred_params, &**pred_expr);

                Type::Assert(span, ty, host::CExpr::Inf(pred_expr).into()).into()
            }
            ParseType::Compute(span, repr_ty, ref expr) => {
                let empty = Type::Const(TypeConst::Empty).into();
                let repr_ty = host::Type::Const(repr_ty).into();
                let conv_params = vec![Named("_".to_owned(), RcType::repr(&empty))];
                let conv = host::RcIExpr::lam(span, conv_params, &**expr);

                Type::Interp(span, empty.into(), host::CExpr::Inf(conv).into(), repr_ty).into()
            }
            ParseType::App(span, ref fn_ty, ref arg_tys) => {
                let fn_ty = RcType::from(&**fn_ty);
                let arg_tys = arg_tys.iter().map(|arg| RcType::from(&*arg)).collect();

                Type::App(span, fn_ty, arg_tys).into()
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
            use super::Type as T;
            use super::RcType as RcT;

            #[test]
            fn id() {
                // λx. x
                // λ   0
                let ty = RcT::lam(Span::start(), &["x"], T::Var(Span::start(), Var::free("x")));

                assert_debug_snapshot!(ty_abs_id, ty);
            }

            // Examples from https://en.wikipedia.org/wiki/De_Bruijn_index

            #[test]
            fn k_combinator() {
                // λx.λy. x
                // λ  λ   1
                let ty = RcT::lam(
                    Span::start(),
                    &["x"],
                    RcT::lam(Span::start(), &["y"], T::Var(Span::start(), Var::free("x"))),
                );

                assert_debug_snapshot!(ty_abs_k_combinator, ty);
            }

            #[test]
            fn s_combinator() {
                // λx.λy.λz. x z (y z)
                // λ  λ  λ   2 0 (1 0)
                let ty = RcT::lam(
                    Span::start(),
                    &["x"],
                    RcT::lam(
                        Span::start(),
                        &["y"],
                        RcT::lam(
                            Span::start(),
                            &["z"],
                            T::App(
                                Span::start(),
                                T::App(
                                    Span::start(),
                                    T::Var(Span::start(), Var::free("x")).into(),
                                    vec![T::Var(Span::start(), Var::free("z")).into()],
                                ).into(),
                                vec![
                                    T::App(
                                        Span::start(),
                                        T::Var(Span::start(), Var::free("y")).into(),
                                        vec![T::Var(Span::start(), Var::free("z")).into()],
                                    ).into(),
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
                let ty = RcT::lam(
                    Span::start(),
                    &["z"],
                    T::App(
                        Span::start(),
                        RcT::lam(
                            Span::start(),
                            &["y"],
                            T::App(
                                Span::start(),
                                T::Var(Span::start(), Var::free("y")).into(),
                                vec![
                                    RcT::lam(
                                        Span::start(),
                                        &["x"],
                                        T::Var(Span::start(), Var::free("x")),
                                    ),
                                ],
                            ),
                        ),
                        vec![
                            RcT::lam(
                                Span::start(),
                                &["x"],
                                T::App(
                                    Span::start(),
                                    T::Var(Span::start(), Var::free("z")).into(),
                                    vec![T::Var(Span::start(), Var::free("x")).into()],
                                ),
                            ),
                        ],
                    ),
                );

                assert_debug_snapshot!(ty_abs_complex, ty);
            }
        }
    }
}
