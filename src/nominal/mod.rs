//! A simple, nominal intermediate representation that can be easily
//! translated to Rust or other languages with similar type systems
//! that require structs and tagged unions to be declared up front.
//!
//! For example, the following data description:
//!
//! ```plain
//! Baz = [
//!     struct { x : u8 },
//! ];
//!
//! Bar = Baz;
//!
//! Foo = struct {
//!     data0 : union { u8, Bar, struct {} },
//!     data1 : struct { inner: Bar },
//!     data2 : [struct{}],
//! };
//! ```
//!
//! will be translated to:
//!
//! ```plain
//! alias Baz = [Baz::Elem];
//!
//! struct Baz::Elem { x : u8 };
//!
//! alias Bar = Baz;
//!
//! struct Foo {
//!     data0 : Foo::data0,
//!     data1 : Foo::data1,
//!     data2 : [Foo::data2::Entry],
//! };
//!
//! union Foo::data0 {
//!     u8,
//!     Bar,
//!     Foo::data0::Variant2,
//! };
//!
//! struct Foo::data1 {
//!     inner : Bar,
//! };
//!
//! struct Foo::data2::Entry {};
//! ```
//!
//! A further step will need to be added to convert the separated paths into
//! flat identifiers that do not clash with each other.

use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

use name::Name;
use syntax::{self, binary, host, Field};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<N> {
    pub defs: BTreeMap<Path<N>, Definition<N>>,
}

impl<N: Name> Program<N> {
    pub fn new() -> Program<N> {
        Program {
            defs: BTreeMap::new(),
        }
    }

    pub fn define<P: Into<Path<N>>>(&mut self, path: P, def: Definition<N>) {
        let path = path.into();
        assert!(
            !self.defs.contains_key(&path),
            "Found duplicate top level definition for {}",
            path
        );

        self.defs.insert(path, def);
    }

    pub fn define_alias<P: Into<Path<N>>>(&mut self, path: P, ty: Type<N>) {
        self.define(path, Definition::Alias(ty));
    }

    pub fn define_struct<P: Into<Path<N>>>(&mut self, path: P, fields: Vec<Field<N, Type<N>>>) {
        self.define(path, Definition::Struct(fields));
    }

    pub fn define_union<P: Into<Path<N>>>(&mut self, path: P, variants: Vec<Type<N>>) {
        self.define(path, Definition::Union(variants));
    }
}

/// A fully qualified path to a type definition
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Path<N> {
    /// The base definition name from the source AST
    pub base: N,
    /// The path through a structural type in the source AST
    pub children: Vec<N>,
}

impl<N: Name> Path<N> {
    pub fn new(base: N) -> Path<N> {
        Path {
            base,
            children: vec![],
        }
    }

    pub fn append_child<N1: Into<N>>(&self, name: N1) -> Path<N> {
        let mut path = self.clone();
        path.children.push(name.into());
        path
    }
}

impl<N: fmt::Display> fmt::Display for Path<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.base)?;
        for child in &self.children {
            write!(f, "::{}", child)?;
        }
        Ok(())
    }
}

impl<'a, N: From<&'a str>> From<&'a str> for Path<N> {
    fn from(src: &'a str) -> Path<N> {
        let mut parts = src.split("::").map(N::from);

        let base = parts.next().unwrap();
        let children = parts.collect();

        Path { base, children }
    }
}

/// Top level type definitions
///
/// The names of these are declared when they are stored in the `Program` struct
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Definition<N> {
    /// Type alias
    Alias(Type<N>),
    /// Struct definition
    Struct(Vec<Field<N, Type<N>>>),
    /// Union type definition
    Union(Vec<Type<N>>),
}

/// Structural types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<N> {
    /// A fully qualified path to a type definition
    Path(Path<N>),
    /// Array types. These are usually available in languages as primitives,
    /// so there is no need to generate new types for these
    Array(RcType<N>, host::RcExpr<N>),
    /// Types dependent on some kind of condition
    Cond(RcType<N>, host::RcExpr<N>),
    /// Interpreted types
    Interp(RcType<N>, host::RcExpr<N>, host::RcType<N>),
}

pub type RcType<N> = Rc<Type<N>>;

impl<N: Name> Type<N> {
    pub fn path<P: Into<Path<N>>>(path: P) -> Type<N> {
        Type::Path(path.into())
    }

    pub fn array<T1, E1>(elem_ty: T1, size_expr: E1) -> Type<N>
    where
        T1: Into<RcType<N>>,
        E1: Into<host::RcExpr<N>>,
    {
        Type::Array(elem_ty.into(), size_expr.into())
    }
}

pub fn flatten_program(src_program: &syntax::Program<String>) -> Program<String> {
    let mut program = Program::new();

    for def in &src_program.defs {
        // Begin tracking the path of this definition from the root name of the
        // source definition. This will be appended to in order to provide a
        // fully qualified path through the type definitions, eg:
        // `Foo::field::Entry::Variant2::...`
        let path = Path::new(def.name.clone());

        match *def.ty {
            binary::Type::Struct(_, ref fields) => {
                let fields = flatten_fields(&mut program, &path, fields);
                program.define_struct(path, fields);
            }
            binary::Type::Union(_, ref variants) => {
                let variants = flatten_variants(&mut program, &path, variants);
                program.define_union(path, variants);
            }
            _ => {
                let ty = flatten_ty(&mut program, &path, &def.ty);
                program.define_alias(path, ty);
            }
        }
    }

    program
}

fn flatten_fields(
    program: &mut Program<String>,
    path: &Path<String>,
    fields: &[Field<String, binary::RcType<String>>],
) -> Vec<Field<String, Type<String>>> {
    fields
        .iter()
        .map(|field| {
            let field_path = path.append_child(field.name.clone());
            let ty = flatten_ty(program, &field_path, &field.value);

            Field::new(field.name.clone(), ty)
        })
        .collect()
}

fn flatten_variants(
    program: &mut Program<String>,
    path: &Path<String>,
    variants: &[binary::RcType<String>],
) -> Vec<Type<String>> {
    variants
        .iter()
        .enumerate()
        .map(|(i, variant_ty)| {
            let variant_path = path.append_child(format!("Variant{}", i));
            flatten_ty(program, &variant_path, &variant_ty)
        })
        .collect()
}

fn flatten_ty(
    program: &mut Program<String>,
    path: &Path<String>,
    ty: &binary::RcType<String>,
) -> Type<String> {
    use name::Named;
    use syntax::Var;

    match **ty {
        binary::Type::Var(_, Var::Bound(Named(ref name, _))) => Type::path(name.as_str()),
        binary::Type::Var(_, Var::Free(_)) => unimplemented!(),
        binary::Type::Const(_) => unimplemented!(),
        binary::Type::Array(_, ref elem_ty, ref size_expr) => {
            let elem_path = path.append_child("Elem");
            let elem_ty = flatten_ty(program, &elem_path, elem_ty);

            Type::array(elem_ty, size_expr.clone())
        }
        binary::Type::Union(_, ref variants) => {
            let variants = flatten_variants(program, path, variants);
            program.define_union(path.clone(), variants);

            Type::path(path.clone())
        }
        binary::Type::Struct(_, ref fields) => {
            let fields = flatten_fields(program, path, fields);
            program.define_struct(path.clone(), fields);

            Type::path(path.clone())
        }
        binary::Type::Cond(_, ref ty, ref pred_expr) => {
            let inner_path = path.append_child("Inner");
            let inner_ty = flatten_ty(program, &inner_path, ty);

            Type::Cond(Rc::new(inner_ty), pred_expr.clone())
        }
        binary::Type::Interp(_, ref ty, ref conv_expr, ref conv_ty) => {
            let inner_path = path.append_child("Inner");
            let inner_ty = flatten_ty(program, &inner_path, ty);

            Type::Interp(Rc::new(inner_ty), conv_expr.clone(), conv_ty.clone())
        }
        binary::Type::Abs(_, _, _) => unimplemented!(),
        binary::Type::App(_, _, _) => unimplemented!(),
    }
}
