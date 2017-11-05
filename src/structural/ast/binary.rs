//! The syntax of our data description language

use pretty::{BoxDoc, Doc};
use std::fmt;
use std::rc::Rc;

use name::{Name, Named};
use source::Span;
use structural::ast::{self, host, Field, Substitutions, Var, DEFAULT_PRETTY_WIDTH};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    /// Kind of types
    Type,
    /// Kind of type functions
    Arrow(RcKind, RcKind),
}

pub type RcKind = Rc<Kind>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum KindPrec {
    Inner,
    Outer,
}

impl Kind {
    /// Kind of type functions
    pub fn arrow<K1: Into<RcKind>, K2: Into<RcKind>>(lhs: K1, rhs: K2) -> Kind {
        Kind::Arrow(lhs.into(), rhs.into())
    }

    /// Convert the kind into a pretty-printable document
    pub fn to_doc_prec(&self, prec: KindPrec) -> Doc<BoxDoc> {
        match *self {
            Kind::Type => Doc::text("Type"),
            Kind::Arrow(ref lhs_ty, ref rhs_ty) => {
                let inner = Doc::nil()
                    .append(lhs_ty.to_doc_prec(KindPrec::Inner))
                    .append(Doc::space())
                    .append(Doc::text("->"))
                    .group()
                    .append(Doc::space())
                    .append(rhs_ty.to_doc_prec(KindPrec::Outer));

                match prec {
                    KindPrec::Outer => inner,
                    KindPrec::Inner => Doc::nil()
                        .append(Doc::text("("))
                        .append(inner)
                        .append(Doc::text(")")),
                }
            }
        }
    }

    pub fn to_doc(&self) -> Doc<BoxDoc> {
        self.to_doc_prec(KindPrec::Outer).group()
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc()
            .render_fmt(f.width().unwrap_or(DEFAULT_PRETTY_WIDTH), f)
    }
}

/// An error that occurred when trying to convert a binary type to
/// its host representation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReprError<N> {
    NoCorrespondingHostType(Type<N>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TypeConst {
    Bit,
}

impl TypeConst {
    pub fn to_str(self) -> &'static str {
        match self {
            TypeConst::Bit => "Bit",
        }
    }
}

/// A binary type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<N> {
    /// A type variable: eg. `T`
    Var(Span, Var<N, u32>),
    /// Type constant
    Const(TypeConst),
    /// An array of the specified type, with a size: eg. `[T; n]`
    Array(Span, RcType<N>, host::RcExpr<N>),
    /// A union of types: eg. `union { T, ... }`
    Union(Span, Vec<RcType<N>>),
    /// A struct type, with fields: eg. `struct { field : T, ... }`
    Struct(Span, Vec<Field<N, RcType<N>>>),
    /// A type constrained by a predicate: eg. `T where x => x == 3`
    Cond(Span, RcType<N>, host::RcExpr<N>),
    /// An interpreted type
    Interp(Span, RcType<N>, host::RcExpr<N>, host::RcType<N>),
    /// Type abstraction: eg. `\(a : Type) -> T`
    Abs(Span, Named<N, RcKind>, RcType<N>),
    /// Type application: eg. `T U V`
    App(Span, RcType<N>, RcType<N>),
}

pub type RcType<N> = Rc<Type<N>>;

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
        T1: Into<RcType<N>>,
        E1: Into<host::RcExpr<N>>,
    {
        Type::Array(span, elem_ty.into(), size_expr.into())
    }

    /// A union of types: eg. `union { T, ... }`
    pub fn union(span: Span, tys: Vec<RcType<N>>) -> Type<N> {
        Type::Union(span, tys)
    }

    /// Type application: eg. `T U V`
    pub fn app<T1, T2>(span: Span, ty1: T1, ty2: T2) -> Type<N>
    where
        T1: Into<RcType<N>>,
        T2: Into<RcType<N>>,
    {
        Type::App(span, ty1.into(), ty2.into())
    }

    /// A struct type, with fields: eg. `struct { field : T, ... }`
    pub fn struct_(span: Span, mut fields: Vec<Field<N, RcType<N>>>) -> Type<N> {
        // We maintain a list of the seen field names. This will allow us to
        // recover the index of these variables as we abstract later fields...
        let mut seen_names = Vec::with_capacity(fields.len());

        for field in &mut fields {
            for (level, name) in seen_names.iter().rev().enumerate() {
                Rc::make_mut(&mut field.value).abstract_name_at(name, level as u32);
            }

            // Record that the field has been 'seen'
            seen_names.push(field.name.clone());
        }

        Type::Struct(span, fields)
    }

    /// A type constrained by a predicate: eg. `T where x => x == 3`
    pub fn cond<T1, E1>(span: Span, ty: T1, pred: E1) -> Type<N>
    where
        T1: Into<RcType<N>>,
        E1: Into<host::RcExpr<N>>,
    {
        Type::Cond(span, ty.into(), pred.into())
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

    /// Type abstraction: eg. `\(a : Type) -> T`
    pub fn abs<N1, K1, T1>(span: Span, (param_name, param_kind): (N1, K1), body_ty: T1) -> Type<N>
    where
        N1: Into<N>,
        K1: Into<RcKind>,
        T1: Into<RcType<N>>,
    {
        let param_name = param_name.into();
        let mut body_ty = body_ty.into();
        Rc::make_mut(&mut body_ty).abstract_name(&param_name);
        Type::Abs(span, Named(param_name, param_kind.into()), body_ty)
    }

    /// Attempt to lookup the type of a field
    ///
    /// Returns `None` if the expression is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &N) -> Option<&RcType<N>> {
        match *self {
            Type::Struct(_, ref fields) => ast::lookup_field(fields, name),
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
            Type::Union(_, ref mut tys) => {
                for ty in tys {
                    Rc::make_mut(ty).substitute(substs);
                }
                return;
            }
            Type::Struct(_, ref mut fields) => {
                for field in fields.iter_mut() {
                    Rc::make_mut(&mut field.value).substitute(substs);
                }
                return;
            }
            Type::Cond(_, ref mut ty, ref mut _pred) => {
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
            Type::App(_, ref mut fn_ty, ref mut arg_ty) => {
                Rc::make_mut(fn_ty).substitute(substs);
                Rc::make_mut(arg_ty).substitute(substs);
                return;
            }
        };

        *self = subst_ty.clone();
    }

    pub fn abstract_name_at(&mut self, name: &N, level: u32) {
        match *self {
            Type::Var(_, ref mut var) => var.abstract_name_at(name, level),
            Type::Const(_) => {}
            Type::Array(_, ref mut elem_ty, ref mut size_expr) => {
                Rc::make_mut(elem_ty).abstract_name_at(name, level);
                Rc::make_mut(size_expr).abstract_name_at(name, level);
            }
            Type::Union(_, ref mut tys) => for ty in tys {
                Rc::make_mut(ty).abstract_name_at(name, level);
            },
            Type::Struct(_, ref mut fields) => for (i, field) in fields.iter_mut().enumerate() {
                Rc::make_mut(&mut field.value).abstract_name_at(name, level + i as u32);
            },
            Type::Cond(_, ref mut ty, ref mut pred) => {
                Rc::make_mut(ty).abstract_name_at(name, level);
                Rc::make_mut(pred).abstract_name_at(name, level + 1);
            }
            Type::Interp(_, ref mut ty, ref mut conv, ref mut repr_ty) => {
                Rc::make_mut(ty).abstract_name_at(name, level);
                Rc::make_mut(conv).abstract_name_at(name, level + 1);
                Rc::make_mut(repr_ty).abstract_name_at(name, level);
            }
            Type::Abs(_, _, ref mut body_ty) => {
                Rc::make_mut(body_ty).abstract_name_at(name, level + 1);
            }
            Type::App(_, ref mut fn_ty, ref mut arg_ty) => {
                Rc::make_mut(fn_ty).abstract_name_at(name, level);
                Rc::make_mut(arg_ty).abstract_name_at(name, level);
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
                Rc::make_mut(elem_ty).instantiate_at(level, src);
            }
            Type::Cond(_, ref mut ty, _) => {
                Rc::make_mut(ty).instantiate_at(level + 1, src);
            }
            Type::Interp(_, ref mut ty, _, _) => {
                Rc::make_mut(ty).instantiate_at(level + 1, src);
            }
            Type::Union(_, ref mut tys) => for ty in tys {
                Rc::make_mut(ty).instantiate_at(level, src);
            },
            Type::Struct(_, ref mut fields) => for (i, field) in fields.iter_mut().enumerate() {
                Rc::make_mut(&mut field.value).instantiate_at(level + i as u32, src);
            },
            Type::Abs(_, _, ref mut ty) => {
                Rc::make_mut(ty).instantiate_at(level + 1, src);
            }
            Type::App(_, ref mut ty, ref mut arg_ty) => {
                Rc::make_mut(ty).instantiate_at(level, src);
                Rc::make_mut(arg_ty).instantiate_at(level, src);
            }
        }
    }

    /// Remove one layer of abstraction in the type by replacing the
    /// appropriate bound variables with copies of `ty`.
    pub fn instantiate(&mut self, ty: &Type<N>) {
        self.instantiate_at(0, ty);
    }

    /// Returns the host representation of the binary type
    pub fn repr(&self) -> Result<host::RcType<N>, ReprError<N>> {
        match *self {
            Type::Var(_, ref v) => Ok(Rc::new(host::Type::Var(v.clone()))),
            Type::Const(TypeConst::Bit) => Ok(Rc::new(host::Type::Const(host::TypeConst::Bit))),
            Type::Array(_, ref elem_ty, ref size_expr) => {
                let elem_repr_ty = elem_ty.repr()?;
                let size_expr = size_expr.clone();

                Ok(Rc::new(host::Type::array(elem_repr_ty, size_expr)))
            }
            Type::Cond(_, ref ty, _) => ty.repr(),
            Type::Interp(_, _, _, ref repr_ty) => Ok(repr_ty.clone()),
            Type::Union(_, ref tys) => {
                let repr_tys = tys.iter().map(|ty| ty.repr()).collect::<Result<_, _>>()?;

                Ok(Rc::new(host::Type::Union(repr_tys)))
            }
            Type::Struct(_, ref fields) => {
                let repr_fields = fields
                    .iter()
                    .map(|f| f.value.repr().map(|ty| Field::new(f.name.clone(), ty)))
                    .collect::<Result<_, _>>()?;

                Ok(Rc::new(host::Type::Struct(repr_fields)))
            }
            Type::Abs(_, _, _) | Type::App(_, _, _) => {
                Err(ReprError::NoCorrespondingHostType(self.clone()))
            }
        }
    }

    /// Convert the type into a pretty-printable document
    pub fn to_doc(&self) -> Doc<BoxDoc> {
        match *self {
            Type::Var(_, ref var) => Doc::text(var.name().to_string()),
            Type::Const(c) => Doc::text(c.to_str()),
            Type::Array(_, ref elem_ty, ref size_expr) => Doc::nil()
                .append(Doc::text("["))
                .append(elem_ty.to_doc())
                .append(Doc::text(";"))
                .append(Doc::space())
                .append(size_expr.to_doc())
                .append(Doc::text("]")),
            Type::Union(_, ref tys) => Doc::nil()
                .append(Doc::text("union {"))
                .append(Doc::space())
                .append(Doc::concat(tys.iter().map(|ty| {
                    ty.to_doc().append(Doc::text(",")).append(Doc::newline())
                })))
                .append(Doc::space())
                .append(Doc::text("}")),
            Type::Struct(_, ref fields) => Doc::nil()
                .append(Doc::text("struct {"))
                .append(Doc::space())
                .append(
                    Doc::concat(fields.iter().map(|field| {
                        Doc::text(field.name.to_string())
                            .append(Doc::space())
                            .append(Doc::text(":"))
                            .append(Doc::space())
                            .append(field.value.to_doc())
                            .append(Doc::text(","))
                            .append(Doc::newline())
                    })).nest(4),
                )
                .append(Doc::space())
                .append(Doc::text("}")),
            Type::Cond(_, ref ty, ref pred_expr) => Doc::nil()
                .append(ty.to_doc())
                .append(Doc::space())
                .append(Doc::text("where"))
                .append(Doc::space())
                // FIXME: clean up  lambda syntax...
                .append(pred_expr.to_doc()),
            Type::Interp(_, ref ty, ref conv_expr, ref repr_ty) => Doc::nil()
                .append(Doc::text("interp("))
                .append(ty.to_doc())
                .append(Doc::text(","))
                .append(Doc::space())
                .append(conv_expr.to_doc())
                .append(Doc::space())
                .append(Doc::text(":"))
                .append(Doc::space())
                .append(repr_ty.to_doc())
                .append(Doc::text(")")),
            // FIXME: Param list sugar: \(x y : Foo) (z : Bar) -> ...
            Type::Abs(_, Named(ref param_name, ref param_kind), ref body_ty) => Doc::nil()
                .append(Doc::text("\\"))
                .append(Doc::text(param_name.to_string()))
                .append(Doc::space())
                .append(Doc::text(":"))
                .append(Doc::space())
                .append(param_kind.to_doc())
                .append(Doc::space())
                .append(Doc::text("->"))
                .append(Doc::space())
                .append(body_ty.to_doc()),
            Type::App(_, ref fn_ty, ref arg_ty) => Doc::nil()
                .append(fn_ty.to_doc())
                .append(Doc::space())
                .append(arg_ty.to_doc()),
        }
    }
}

impl<N: Name> fmt::Display for Type<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc()
            .render_fmt(f.width().unwrap_or(DEFAULT_PRETTY_WIDTH), f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod kind {
        use super::*;

        mod pretty {
            use super::*;

            #[test]
            fn fmt_arrow() {
                let kind = Kind::arrow(
                    Kind::arrow(Kind::Type, Kind::Type),
                    Kind::arrow(Kind::Type, Kind::Type),
                );

                #[cfg_attr(rustfmt, rustfmt_skip)]
                let expected = [
                    "(Type -> Type) -> Type -> Type",
                ].join("\n");

                assert_eq!(format!("{}", kind), expected);
            }

            #[test]
            fn fmt_arrow_narrow() {
                let kind = Kind::arrow(
                    Kind::arrow(Kind::Type, Kind::Type),
                    Kind::arrow(Kind::Type, Kind::Type),
                );

                #[cfg_attr(rustfmt, rustfmt_skip)]
                let expected = [
                    "(Type -> Type) ->",
                    "Type ->",
                    "Type",
                ].join("\n");

                assert_eq!(format!("{:20}", kind), expected);
            }
        }
    }

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
