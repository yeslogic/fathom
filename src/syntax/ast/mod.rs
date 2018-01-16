//! The syntax of our data description language

use std::fmt;
use std::rc::Rc;

use name::{Ident, Name, Named};
use parser::ast::Definition as ParseDefinition;
use parser::ast::Module as ParseModule;
use var::ScopeIndex;

pub mod binary;
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
    use syntax::ast::binary::{Type, TypeConst as Tc};
    use syntax::ast::binary::Endianness::{Big, Little};

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
    substs: Vec<(Name, binary::RcType)>,
}

impl Substitutions {
    fn get(&self, name: &Name) -> Option<&binary::RcType> {
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
    pub body_ty: binary::RcType,
}

impl Definition {
    pub fn from_parse(src: &ParseDefinition) -> Result<Definition, ()> {
        let body_ty = binary::RcType::from_parse(&src.body_ty)?;

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

                    binary::RcType::lam(src.span, names, body_ty)
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
