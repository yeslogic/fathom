//! The syntax of our data description language

use std::collections::BTreeMap;
use std::rc::Rc;

use name::Name;
use var::ScopeIndex;

pub mod binary;
pub mod host;

/// A field in a struct type
#[derive(Debug, Clone)]
pub struct Field<N, T> {
    /// Doc comment
    ///
    /// Note: This is ignored for comparison purposes
    pub doc: Rc<str>,
    /// The name of the field
    pub name: N,
    /// The value that this field is associated with
    pub value: T,
}

impl<N, T> Field<N, T> {
    /// Apply the function `f` to the field name and return the wrapped result
    pub fn map_name<M, F: FnMut(N) -> M>(self, mut f: F) -> Field<M, T> {
        Field {
            doc: self.doc,
            name: f(self.name),
            value: self.value,
        }
    }

    /// Apply the function `f` to the field value and return the wrapped result
    pub fn map_value<U, F: FnMut(T) -> U>(self, mut f: F) -> Field<N, U> {
        Field {
            doc: self.doc,
            name: self.name,
            value: f(self.value),
        }
    }
}

impl<N: PartialEq, T: PartialEq> PartialEq for Field<N, T> {
    fn eq(&self, other: &Field<N, T>) -> bool {
        // Ignoring doc commment
        self.name == other.name && self.value == other.value
    }
}

fn lookup_field<'a, N, T>(fields: &'a [Field<N, T>], name: &N) -> Option<&'a T>
where
    N: PartialEq,
{
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
pub struct Definition<N> {
    /// Doc comment
    ///
    /// Note: This is ignored for comparison purposes
    pub doc: Rc<str>,
    /// The name of the defined type
    pub name: N,
    /// The binary type
    pub ty: binary::RcType<N>,
}

impl<N: PartialEq> PartialEq for Definition<N> {
    fn eq(&self, other: &Definition<N>) -> bool {
        // Ignoring doc commment
        self.name == other.name && self.ty == other.ty
    }
}

pub type Substitutions<N> = BTreeMap<N, binary::Type<N>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Program<N> {
    pub definitions: Vec<Definition<N>>,
}

impl<N: Name> Program<N> {
    pub fn new(mut definitions: Vec<Definition<N>>) -> Program<N> {
        // We maintain a list of the seen definition names. This will allow us to
        // recover the index of these variables as we abstract later definitions...
        let mut seen_names = Vec::<N>::new();

        for definition in &mut definitions {
            for (level, name) in seen_names.iter().rev().enumerate() {
                Rc::make_mut(&mut definition.ty)
                    .abstract_names_at(&[name.clone()], ScopeIndex(level as u32));
            }

            // Record that the definition has been 'seen'
            seen_names.push(definition.name.clone());
        }

        Program { definitions }
    }

    pub fn substitute(&mut self, substs: &Substitutions<N>) {
        for definition in &mut self.definitions {
            Rc::make_mut(&mut definition.ty).substitute(substs);
        }
    }
}

pub fn base_defs<N: Name + for<'a> From<&'a str>>() -> Substitutions<N> {
    use syntax::ast::binary::{Endianness, Type, TypeConst};

    btreemap! {
        // TODO: "true" = Expr::bool(true)
        // TODO: "false" = Expr::bool(false)
        "empty".into() => Type::Const(TypeConst::Empty),
        "u8".into() => Type::Const(TypeConst::U8),
        "i8".into() => Type::Const(TypeConst::I8),
        // Little endian primitives
        "u16le".into() => Type::Const(TypeConst::U16(Endianness::Little)),
        "u24le".into() => Type::Const(TypeConst::U24(Endianness::Little)),
        "u32le".into() => Type::Const(TypeConst::U32(Endianness::Little)),
        "u64le".into() => Type::Const(TypeConst::U64(Endianness::Little)),
        "i16le".into() => Type::Const(TypeConst::I16(Endianness::Little)),
        "i24le".into() => Type::Const(TypeConst::I24(Endianness::Little)),
        "i32le".into() => Type::Const(TypeConst::I32(Endianness::Little)),
        "i64le".into() => Type::Const(TypeConst::I64(Endianness::Little)),
        "f32le".into() => Type::Const(TypeConst::F32(Endianness::Little)),
        "f64le".into() => Type::Const(TypeConst::F64(Endianness::Little)),
        // Big endian primitives
        "u16be".into() => Type::Const(TypeConst::U16(Endianness::Big)),
        "u24be".into() => Type::Const(TypeConst::U24(Endianness::Big)),
        "u32be".into() => Type::Const(TypeConst::U32(Endianness::Big)),
        "u64be".into() => Type::Const(TypeConst::U64(Endianness::Big)),
        "i16be".into() => Type::Const(TypeConst::I16(Endianness::Big)),
        "i24be".into() => Type::Const(TypeConst::I24(Endianness::Big)),
        "i32be".into() => Type::Const(TypeConst::I32(Endianness::Big)),
        "i64be".into() => Type::Const(TypeConst::I64(Endianness::Big)),
        "f32be".into() => Type::Const(TypeConst::F32(Endianness::Big)),
        "f64be".into() => Type::Const(TypeConst::F64(Endianness::Big)),
    }
}
