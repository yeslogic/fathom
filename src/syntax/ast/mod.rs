//! The syntax of our data description language

use std::collections::BTreeMap;
use std::rc::Rc;

use parser::ast::Definition as ParseDefinition;
use parser::ast::Module as ParseModule;
use var::ScopeIndex;

pub mod binary;
pub mod host;

/// A field in a struct type
#[derive(Debug, Clone)]
pub struct Field<T> {
    /// Doc comment
    ///
    /// Note: This is ignored for comparison purposes
    pub doc: Rc<str>,
    /// The name of the field
    pub name: String,
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

fn lookup_field<'a, T>(fields: &'a [Field<T>], name: &str) -> Option<&'a T> {
    fields
        .iter()
        .find(|field| &field.name == name)
        .map(|field| &field.value)
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
    pub name: String,
    /// The binary type
    pub body_ty: binary::RcType,
}

impl Definition {
    pub fn from_parse(src: &ParseDefinition) -> Result<Definition, ()> {
        let body_ty = binary::RcType::from_parse(&src.body_ty)?;

        Ok(Definition {
            doc: src.doc.join("\n").into(),
            name: String::from(src.name),
            body_ty: match &src.param_names[..] {
                names if names.is_empty() => body_ty,
                names => binary::RcType::lam(src.span, names, body_ty),
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

pub type Substitutions = BTreeMap<String, binary::RcType>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub definitions: Vec<Definition>,
}

impl Module {
    pub fn new(mut definitions: Vec<Definition>) -> Module {
        // We maintain a list of the seen definition names. This will allow us to
        // recover the index of these variables as we abstract later definitions...
        let mut seen_names = Vec::<String>::new();

        for definition in &mut definitions {
            for (level, name) in seen_names.iter().rev().enumerate() {
                definition
                    .body_ty
                    .abstract_names_at(&[name], ScopeIndex(level as u32));
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
    use syntax::ast::binary::{Endianness, Type, TypeConst};

    btreemap! {
        // TODO: "true" => Expr::bool(true)
        // TODO: "false" => Expr::bool(false)
        "empty".into() => Type::Const(TypeConst::Empty).into(),
        "error".into() => Type::Const(TypeConst::Error).into(),
        "u8".into() => Type::Const(TypeConst::U8).into(),
        "i8".into() => Type::Const(TypeConst::I8).into(),
        // Little endian primitives
        "u16le".into() => Type::Const(TypeConst::U16(Endianness::Little)).into(),
        "u24le".into() => Type::Const(TypeConst::U24(Endianness::Little)).into(),
        "u32le".into() => Type::Const(TypeConst::U32(Endianness::Little)).into(),
        "u64le".into() => Type::Const(TypeConst::U64(Endianness::Little)).into(),
        "i16le".into() => Type::Const(TypeConst::I16(Endianness::Little)).into(),
        "i24le".into() => Type::Const(TypeConst::I24(Endianness::Little)).into(),
        "i32le".into() => Type::Const(TypeConst::I32(Endianness::Little)).into(),
        "i64le".into() => Type::Const(TypeConst::I64(Endianness::Little)).into(),
        "f32le".into() => Type::Const(TypeConst::F32(Endianness::Little)).into(),
        "f64le".into() => Type::Const(TypeConst::F64(Endianness::Little)).into(),
        // Big endian primitives
        "u16be".into() => Type::Const(TypeConst::U16(Endianness::Big)).into(),
        "u24be".into() => Type::Const(TypeConst::U24(Endianness::Big)).into(),
        "u32be".into() => Type::Const(TypeConst::U32(Endianness::Big)).into(),
        "u64be".into() => Type::Const(TypeConst::U64(Endianness::Big)).into(),
        "i16be".into() => Type::Const(TypeConst::I16(Endianness::Big)).into(),
        "i24be".into() => Type::Const(TypeConst::I24(Endianness::Big)).into(),
        "i32be".into() => Type::Const(TypeConst::I32(Endianness::Big)).into(),
        "i64be".into() => Type::Const(TypeConst::I64(Endianness::Big)).into(),
        "f32be".into() => Type::Const(TypeConst::F32(Endianness::Big)).into(),
        "f64be".into() => Type::Const(TypeConst::F64(Endianness::Big)).into(),
    }
}
