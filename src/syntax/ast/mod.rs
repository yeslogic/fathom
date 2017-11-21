//! The syntax of our data description language

use std::collections::BTreeMap;
use std::rc::Rc;

use name::Name;
use var::ScopeIndex;

pub mod binary;
pub mod host;

/// A field in a struct type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field<N, T> {
    pub name: N,
    pub value: T,
}

impl<N, T> Field<N, T> {
    pub fn new<M: Into<N>, U: Into<T>>(name: M, value: U) -> Field<N, T> {
        Field {
            name: name.into(),
            value: value.into(),
        }
    }

    /// Apply the function `f` to the field name and return the wrapped result
    pub fn map_name<M, F: FnMut(N) -> M>(self, mut f: F) -> Field<M, T> {
        Field {
            name: f(self.name),
            value: self.value,
        }
    }

    /// Apply the function `f` to the field value and return the wrapped result
    pub fn map_value<U, F: FnMut(T) -> U>(self, mut f: F) -> Field<N, U> {
        Field {
            name: self.name,
            value: f(self.value),
        }
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
/// Point = {
///     x : u16,
///     y : u16,
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition<N> {
    pub name: N,
    pub ty: binary::RcType<N>,
}

impl<N> Definition<N> {
    pub fn new<N1: Into<N>, T1: Into<binary::RcType<N>>>(name: N1, ty: T1) -> Definition<N> {
        Definition {
            name: name.into(),
            ty: ty.into(),
        }
    }
}

pub type Substitutions<N> = BTreeMap<N, binary::Type<N>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<N> {
    pub defs: Vec<Definition<N>>,
}

impl<N: Name> Program<N> {
    pub fn new(mut defs: Vec<Definition<N>>) -> Program<N> {
        // We maintain a list of the seen definition names. This will allow us to
        // recover the index of these variables as we abstract later definitions...
        let mut seen_names = Vec::<N>::new();

        for def in &mut defs {
            for (level, name) in seen_names.iter().rev().enumerate() {
                Rc::make_mut(&mut def.ty)
                    .abstract_names_at(&[name.clone()], ScopeIndex(level as u32));
            }

            // Record that the definition has been 'seen'
            seen_names.push(def.name.clone());
        }

        Program { defs }
    }

    pub fn substitute(&mut self, substs: &Substitutions<N>) {
        for def in &mut self.defs {
            Rc::make_mut(&mut def.ty).substitute(substs);
        }
    }
}

pub fn base_defs<N: Name + for<'a> From<&'a str>>() -> Substitutions<N> {
    use syntax::ast::binary::{Type, TypeConst};

    fn prim_array_ty<N: Name>(size: i64, conv_name: &'static str) -> binary::Type<N> {
        use source::Span;
        use syntax::ast::host::Expr;

        let array_ty = Type::array(Span::start(), Type::u8(), Expr::int(Span::start(), size));
        let conv_ty = host::Type::arrow(vec![array_ty.repr()], host::Type::int());

        Type::interp(
            Span::start(),
            array_ty,
            Expr::prim(conv_name, conv_ty),
            host::Type::int(),
        )
    }

    btreemap! {
        // TODO: "true" = Expr::bool(true)
        // TODO: "false" = Expr::bool(false)
        "u8".into() => Type::Const(TypeConst::U8),
        // Little endian primitives
        "u16le".into() => prim_array_ty(2, "from_u16le"),
        "u32le".into() => prim_array_ty(4, "from_u32le"),
        "u64le".into() => prim_array_ty(8, "from_u64le"),
        "i8le".into() => prim_array_ty(8, "from_i8le"),
        "i16le".into() => prim_array_ty(2, "from_i16le"),
        "i32le".into() => prim_array_ty(4, "from_i32le"),
        "i64le".into() => prim_array_ty(8, "from_i64le"),
        "f32le".into() => prim_array_ty(4, "from_f32le"),
        "f64le".into() => prim_array_ty(8, "from_f64le"),
        // Big endian primitives
        "u16be".into() => prim_array_ty(2, "from_u16be"),
        "u32be".into() => prim_array_ty(4, "from_u32be"),
        "u64be".into() => prim_array_ty(8, "from_u64be"),
        "i8be".into() => prim_array_ty(8, "from_i8be"),
        "i16be".into() => prim_array_ty(2, "from_i16be"),
        "i32be".into() => prim_array_ty(4, "from_i32be"),
        "i64be".into() => prim_array_ty(8, "from_i64be"),
        "f32be".into() => prim_array_ty(4, "from_f32be"),
        "f64be".into() => prim_array_ty(8, "from_f64be"),
    }
}
