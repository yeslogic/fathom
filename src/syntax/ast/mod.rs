//! The syntax of our data description language

use std::fmt;
use std::rc::Rc;

use name::{Ident, Name, Named};
use parser::ast::Definition as ParseDefinition;
use parser::ast::Module as ParseModule;
use parser::ast::Type as ParseType;
use source::Span;
use var::{ScopeIndex, Var};

pub mod host;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub definitions: Vec<Definition>,
}

impl Module {
    pub fn new(mut definitions: Vec<Definition>) -> Module {
        // We maintain a list of the seen definition names. This will allow us to
        // recover the index of these variables as we abstract later definitions...
        let mut seen_names = Vec::<Ident>::new();

        for definition in &mut definitions {
            for (level, name) in seen_names.iter().rev().enumerate() {
                definition
                    .body_ty
                    .abstract_names_at(&[Name::user(name.clone())], ScopeIndex(level as u32));
            }

            // Record that the definition has been 'seen'
            seen_names.push(definition.name.clone());
        }

        Module { definitions }
    }

    pub fn substitute(&mut self, substs: &Substitutions) {
        for definition in &mut self.definitions {
            definition.body_ty.substitute(substs);
        }
    }

    pub fn from_parse(src: &ParseModule) -> Result<Module, ()> {
        Ok(Module::new(src.definitions
            .iter()
            .map(Definition::from_parse)
            .collect::<Result<_, _>>()?))
    }
}

pub fn base_defs() -> Substitutions {
    use syntax::ast::TypeConst as Tc;
    use syntax::ast::Endianness::{Big, Little};

    Substitutions {
        substs: vec![
            // TODO: (Name::user("true"), Expr::bool(true)),
            // TODO: (Name::user("false"), Expr::bool(false)),
            (Name::user("empty"), Type::Const(Tc::Empty).into()),
            (Name::user("error"), Type::Const(Tc::Error).into()),
            (Name::user("u8"), Type::Const(Tc::U8).into()),
            (Name::user("i8"), Type::Const(Tc::I8).into()),
            // Little endian primitives
            (Name::user("u16le"), Type::Const(Tc::U16(Little)).into()),
            (Name::user("u24le"), Type::Const(Tc::U24(Little)).into()),
            (Name::user("u32le"), Type::Const(Tc::U32(Little)).into()),
            (Name::user("u64le"), Type::Const(Tc::U64(Little)).into()),
            (Name::user("i16le"), Type::Const(Tc::I16(Little)).into()),
            (Name::user("i24le"), Type::Const(Tc::I24(Little)).into()),
            (Name::user("i32le"), Type::Const(Tc::I32(Little)).into()),
            (Name::user("i64le"), Type::Const(Tc::I64(Little)).into()),
            (Name::user("f32le"), Type::Const(Tc::F32(Little)).into()),
            (Name::user("f64le"), Type::Const(Tc::F64(Little)).into()),
            // Big endian primitives
            (Name::user("u16be"), Type::Const(Tc::U16(Big)).into()),
            (Name::user("u24be"), Type::Const(Tc::U24(Big)).into()),
            (Name::user("u32be"), Type::Const(Tc::U32(Big)).into()),
            (Name::user("u64be"), Type::Const(Tc::U64(Big)).into()),
            (Name::user("i16be"), Type::Const(Tc::I16(Big)).into()),
            (Name::user("i24be"), Type::Const(Tc::I24(Big)).into()),
            (Name::user("i32be"), Type::Const(Tc::I32(Big)).into()),
            (Name::user("i64be"), Type::Const(Tc::I64(Big)).into()),
            (Name::user("f32be"), Type::Const(Tc::F32(Big)).into()),
            (Name::user("f64be"), Type::Const(Tc::F64(Big)).into()),
        ],
    }
}

pub struct Substitutions {
    substs: Vec<(Name, RcType)>,
}

impl Substitutions {
    fn get(&self, name: &Name) -> Option<&RcType> {
        self.substs
            .iter()
            .find(|subst| &subst.0 == name)
            .map(|subst| &subst.1)
    }
}

/// A type definition
///
/// ```plain
/// Point = struct {
///     x : u16,
///     y : u16,
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Definition {
    /// Doc comment
    ///
    /// Note: This is ignored for comparison purposes
    pub doc: Rc<str>,
    /// The name of the defined type
    pub name: Ident,
    /// The binary type
    pub body_ty: RcType,
}

impl Definition {
    pub fn from_parse(src: &ParseDefinition) -> Result<Definition, ()> {
        let body_ty = RcType::from_parse(&src.body_ty)?;

        Ok(Definition {
            doc: src.doc.join("\n").into(),
            name: Ident::from(src.name),
            body_ty: match &src.param_names[..] {
                names if names.is_empty() => body_ty,
                names => {
                    let names = names
                        .iter()
                        .map(|&name| Named(Name::user(name), ()))
                        .collect();

                    RcType::lam(src.span, names, body_ty)
                }
            },
        })
    }
}

impl PartialEq for Definition {
    fn eq(&self, other: &Definition) -> bool {
        // Ignoring doc commment
        self.name == other.name && self.body_ty == other.body_ty
    }
}

/// A field in a struct type
#[derive(Debug, Clone)]
pub struct Field<T> {
    /// Doc comment
    ///
    /// Note: This is ignored for comparison purposes
    pub doc: Rc<str>,
    /// The name of the field
    pub name: Ident,
    /// The value that this field is associated with
    pub value: T,
}

impl<T> Field<T> {
    /// Apply the function `f` to the field value and return the wrapped result
    pub fn map_value<U, F: FnMut(T) -> U>(self, mut f: F) -> Field<U> {
        Field {
            doc: self.doc,
            name: self.name,
            value: f(self.value),
        }
    }
}

impl<T: PartialEq> PartialEq for Field<T> {
    fn eq(&self, other: &Field<T>) -> bool {
        // Ignoring doc commment
        self.name == other.name && self.value == other.value
    }
}

fn lookup_field<'a, T>(fields: &'a [Field<T>], name: &Ident) -> Option<&'a T> {
    fields
        .iter()
        .find(|field| &field.name == name)
        .map(|field| &field.value)
}

// Kinds

/// Kinds of type
#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    /// Kind of binary types
    Binary,
    /// Kind of host types
    Host,
    /// Kind of type functions
    Arrow(Vec<RcKind>, RcKind),
}

impl RcKind {
    /// Kind of type functions
    pub fn repr(&self) -> RcKind {
        match *self.inner {
            Kind::Binary => Kind::Host.into(),
            Kind::Host => panic!("ICE: tried to find the repr of Kind::Host"),
            Kind::Arrow(ref params, ref ret) => {
                Kind::Arrow(params.iter().map(RcKind::repr).collect(), ret.repr()).into()
            }
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct RcKind {
    pub inner: Rc<Kind>,
}

impl From<Kind> for RcKind {
    fn from(src: Kind) -> RcKind {
        RcKind {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}


// Types

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
    /// Type level lambda abstraction: eg. `\(a, ..) -> T`
    ///
    /// For now we only allow type arguments of kind `Type`
    Lam(Span, Vec<Named<Name, ()>>, RcType),
    /// Type application: eg. `T(U, V)`
    App(Span, RcType, Vec<RcType>),

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
    pub fn lam<T1>(span: Span, params: Vec<Named<Name, ()>>, body_ty: T1) -> RcType
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

        Type::Lam(span, params, body_ty).into()
    }

    /// A struct type, with fields: eg. `struct { field : T, ... }`
    pub fn struct_(span: Span, mut fields: Vec<Field<RcType>>) -> RcType {
        // We maintain a list of the seen field names. This will allow us to
        // recover the index of these variables as we abstract later fields...
        let mut seen_names = Vec::<Name>::with_capacity(fields.len());

        for field in &mut fields {
            for (scope, name) in seen_names.iter().rev().enumerate() {
                field
                    .value
                    .abstract_names_at(&[name.clone()], ScopeIndex(scope as u32));
            }

            // Record that the field has been 'seen'
            seen_names.push(Name::user(field.name.clone()));
        }

        Type::Struct(span, fields).into()
    }

    /// Attempt to lookup the type of a field
    ///
    /// Returns `None` if the type is not a struct or the field is not
    /// present in the struct.
    pub fn lookup_field(&self, name: &Ident) -> Option<&RcType> {
        match *self.inner {
            Type::Struct(_, ref fields) => lookup_field(fields, name),
            _ => None,
        }
    }

    /// Attempt to lookup the type of a variant
    ///
    /// Returns `None` if the type is not a union or the field is not
    /// present in the union.
    pub fn lookup_variant(&self, name: &Ident) -> Option<&(host::RcCExpr, RcType)> {
        match *self.inner {
            Type::Cond(_, ref options) => lookup_field(options, name),
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
        };

        *self = subst_ty.clone();
    }

    pub fn abstract_names_at(&mut self, names: &[Name], scope: ScopeIndex) {
        match *Rc::make_mut(&mut self.inner) {
            Type::Var(_, ref mut var) => var.abstract_names_at(names, scope),
            Type::Const(_) => {}
            Type::Lam(_, _, ref mut body_ty) => {
                body_ty.abstract_names_at(names, scope.succ());
            }
            Type::App(_, ref mut fn_ty, ref mut arg_tys) => {
                fn_ty.abstract_names_at(names, scope);

                for arg_ty in arg_tys {
                    arg_ty.abstract_names_at(names, scope);
                }
            }

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
        }
    }

    /// Add one layer of abstraction around the type by replacing all the
    /// free variables in `names` with an appropriate De Bruijn index.
    ///
    /// This results in a one 'dangling' index, and so care must be taken
    /// to wrap it in another type that marks the introduction of a new
    /// scope.
    pub fn abstract_names(&mut self, names: &[Name]) {
        self.abstract_names_at(names, ScopeIndex(0));
    }

    fn instantiate_at(&mut self, scope: ScopeIndex, tys: &[RcType]) {
        // FIXME: ensure that expressions are not bound at the same scope
        *self = match *Rc::make_mut(&mut self.inner) {
            Type::Var(_, Var::Bound(Named(_, var))) if var.scope == scope => {
                tys[var.binding.0 as usize].clone()
            }
            Type::Var(_, Var::Bound(_)) | Type::Var(_, Var::Free(_)) | Type::Const(_) => return,
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
            Type::Lam(_, ref params, ref body_ty) => {
                host::Type::Lam(params.clone(), body_ty.repr()).into()
            }
            Type::App(_, ref fn_ty, ref arg_tys) => {
                let arg_tys = arg_tys.iter().map(|arg| arg.repr()).collect();

                host::Type::App(fn_ty.repr(), arg_tys).into()
            }

            Type::Array(_, ref elem_ty, _) => host::Type::Array(elem_ty.repr()).into(),
            Type::Assert(_, ref ty, _) => ty.repr(),
            Type::Interp(_, _, _, ref repr_ty) => repr_ty.clone(),
            Type::Cond(_, ref options) => {
                let repr_variants = options
                    .iter()
                    .map(|variant| {
                        Field {
                            doc: Rc::clone(&variant.doc),
                            name: variant.name.clone(),
                            value: variant.value.1.repr(),
                        }
                    })
                    .collect();

                host::Type::Union(repr_variants).into()
            }
            Type::Struct(_, ref fields) => {
                let repr_fields = fields
                    .iter()
                    .map(|field| {
                        Field {
                            doc: Rc::clone(&field.doc),
                            name: field.name.clone(),
                            value: field.value.repr(),
                        }
                    })
                    .collect();

                host::Type::Struct(repr_fields).into()
            }
        }
    }

    pub fn from_parse(src: &ParseType) -> Result<RcType, ()> {
        match *src {
            ParseType::Var(span, name) => Ok(Type::Var(span, Var::free(Name::user(name))).into()),
            ParseType::App(span, ref fn_ty, ref arg_tys) => {
                let fn_ty = RcType::from_parse(&**fn_ty)?;
                let arg_tys = arg_tys
                    .iter()
                    .map(|arg| RcType::from_parse(&*arg))
                    .collect::<Result<_, _>>()?;

                Ok(Type::App(span, fn_ty, arg_tys).into())
            }

            ParseType::Array(span, ref elem_ty, ref size_expr) => {
                let elem_ty = RcType::from_parse(&**elem_ty)?;
                let size_expr = host::RcIExpr::from_parse(&**size_expr)?;

                Ok(Type::Array(span, elem_ty, size_expr).into())
            }
            ParseType::Cond(span, ref options) => {
                let options = options
                    .iter()
                    .map(|variant| {
                        let ty = host::RcIExpr::from_parse(&variant.value.0)?;

                        Ok(Field {
                            doc: variant.doc.join("\n").into(),
                            name: Ident::from(variant.name),
                            value: (
                                host::CExpr::Inf(ty).into(),
                                RcType::from_parse(&variant.value.1)?,
                            ),
                        })
                    })
                    .collect::<Result<_, _>>()?;

                Ok(Type::Cond(span, options).into())
            }
            ParseType::Struct(span, ref fields) => {
                let fields = fields
                    .iter()
                    .map(|field| {
                        Ok(Field {
                            doc: field.doc.join("\n").into(),
                            name: Ident::from(field.name),
                            value: RcType::from_parse(&field.value)?,
                        })
                    })
                    .collect::<Result<_, _>>()?;

                Ok(RcType::struct_(span, fields))
            }
            ParseType::Where(span, ref ty, lo2, param_name, ref pred_expr) => {
                let ty = RcType::from_parse(&**ty)?;
                let pred_fn = host::RcIExpr::lam(
                    Span::new(lo2, span.hi()),
                    vec![Named(Name::user(param_name), ty.repr())],
                    host::RcIExpr::from_parse(&**pred_expr)?,
                );

                Ok(Type::Assert(span, ty, host::CExpr::Inf(pred_fn).into()).into())
            }
            ParseType::Compute(span, repr_ty, ref expr) => {
                let empty = Type::Const(TypeConst::Empty).into();
                let repr_ty = host::Type::Const(repr_ty).into();
                let conv_fn = host::CExpr::Inf(host::RcIExpr::lam(
                    span,
                    vec![Named(Name::Abstract, RcType::repr(&empty))],
                    host::RcIExpr::from_parse(&**expr)?,
                )).into();

                Ok(Type::Interp(span, empty.into(), conv_fn, repr_ty).into())
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
                let ty = RcT::lam(
                    Span::start(),
                    vec![Named(Name::user("x"), ())],
                    T::Var(Span::start(), Var::free(Name::user("x"))),
                );

                assert_debug_snapshot!(ty_abs_id, ty);
            }

            // Examples from https://en.wikipedia.org/wiki/De_Bruijn_index

            #[test]
            fn k_combinator() {
                // λx.λy. x
                // λ  λ   1
                let ty = RcT::lam(
                    Span::start(),
                    vec![Named(Name::user("x"), ())],
                    RcT::lam(
                        Span::start(),
                        vec![Named(Name::user("y"), ())],
                        T::Var(Span::start(), Var::free(Name::user("x"))),
                    ),
                );

                assert_debug_snapshot!(ty_abs_k_combinator, ty);
            }

            #[test]
            fn s_combinator() {
                // λx.λy.λz. x z (y z)
                // λ  λ  λ   2 0 (1 0)
                let ty = RcT::lam(
                    Span::start(),
                    vec![Named(Name::user("x"), ())],
                    RcT::lam(
                        Span::start(),
                        vec![Named(Name::user("y"), ())],
                        RcT::lam(
                            Span::start(),
                            vec![Named(Name::user("z"), ())],
                            T::App(
                                Span::start(),
                                T::App(
                                    Span::start(),
                                    T::Var(Span::start(), Var::free(Name::user("x"))).into(),
                                    vec![T::Var(Span::start(), Var::free(Name::user("z"))).into()],
                                ).into(),
                                vec![
                                    T::App(
                                        Span::start(),
                                        T::Var(Span::start(), Var::free(Name::user("y"))).into(),
                                        vec![
                                            T::Var(Span::start(), Var::free(Name::user("z")))
                                                .into(),
                                        ],
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
                    vec![Named(Name::user("z"), ())],
                    T::App(
                        Span::start(),
                        RcT::lam(
                            Span::start(),
                            vec![Named(Name::user("y"), ())],
                            T::App(
                                Span::start(),
                                T::Var(Span::start(), Var::free(Name::user("y"))).into(),
                                vec![
                                    RcT::lam(
                                        Span::start(),
                                        vec![Named(Name::user("x"), ())],
                                        T::Var(Span::start(), Var::free(Name::user("x"))),
                                    ),
                                ],
                            ),
                        ),
                        vec![
                            RcT::lam(
                                Span::start(),
                                vec![Named(Name::user("x"), ())],
                                T::App(
                                    Span::start(),
                                    T::Var(Span::start(), Var::free(Name::user("z"))).into(),
                                    vec![T::Var(Span::start(), Var::free(Name::user("x"))).into()],
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
