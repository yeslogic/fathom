use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

use name::Name;
use structural::ast::{host, Field};

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

    pub fn define_union<P: Into<Path<N>>>(&mut self, path: P, variants: Vec<Field<N, Type<N>>>) {
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
    Union(Vec<Field<N, Type<N>>>),
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
    Assert(RcType<N>, host::RcExpr<N>),
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
