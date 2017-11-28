//! Abstract syntax of the intermediate representation

use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

use name::{Name, Named};
pub use syntax::ast::Field;
pub use syntax::ast::host::{Binop, Const, TypeConst, Unop};
pub use syntax::ast::host::{FloatType, SignedType, UnsignedType};
pub use syntax::ast::binary::TypeConst as BinaryTypeConst;
use var::{ScopeIndex, Var};

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

    pub fn define_alias<P, D>(&mut self, path: P, doc: D, ty: RcType<N>)
    where
        P: Into<Path<N>>,
        D: Into<Rc<str>>,
    {
        self.define(path, Definition::Alias(doc.into(), ty));
    }

    pub fn define_struct<P, D>(
        &mut self,
        path: P,
        doc: D,
        fields: Vec<Field<N, RcType<N>>>,
        parser: Option<RcParseExpr<N>>,
    ) where
        P: Into<Path<N>>,
        D: Into<Rc<str>>,
    {
        self.define(path, Definition::Struct(doc.into(), fields, parser));
    }

    pub fn define_union<P, D>(
        &mut self,
        path: P,
        doc: D,
        variants: Vec<Field<N, RcType<N>>>,
        parser: Option<RcParseExpr<N>>,
    ) where
        P: Into<Path<N>>,
        D: Into<Rc<str>>,
    {
        self.define(path, Definition::Union(doc.into(), variants, parser));
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

impl Path<String> {
    /// Join path elements to make a camel-case path
    ///
    /// ```rust
    /// use ddl::ir::ast::Path;
    ///
    /// assert_eq!(
    ///     Path::from("Data3D::some_field::BLARGH::Vec3d").to_camel_case(),
    ///     "Data3DSomeFieldBlarghVec3d"
    /// );
    /// ```
    pub fn to_camel_case(&self) -> String {
        use heck::CamelCase;

        self.children
            .iter()
            .map(|child| child.to_camel_case())
            .fold(self.base.to_camel_case(), |acc, child| acc + &child)
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
    Alias(Rc<str>, RcType<N>),
    /// Struct definition
    Struct(Rc<str>, Vec<Field<N, RcType<N>>>, Option<RcParseExpr<N>>),
    /// Union type definition
    Union(Rc<str>, Vec<Field<N, RcType<N>>>, Option<RcParseExpr<N>>),
}

/// Structural types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<N> {
    /// Type constants
    Const(TypeConst),
    /// A fully qualified path to a type definition
    Path(Path<N>),
    /// Array types. These are usually available in languages as primitives,
    /// so there is no need to generate new types for these
    Array(RcType<N>),
    /// Arrow types.
    Arrow(Vec<RcType<N>>, RcType<N>),
}

pub type RcType<N> = Rc<Type<N>>;

/// A bounded repitition
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RepeatBound<N> {
    /// A constant expression that bounds the repition
    Exact(RcExpr<N>),
}

/// A small parser combinator language
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseExpr<N> {
    /// A reference to another parser
    Var(Var<N>),
    /// Parse a binary constant
    Const(BinaryTypeConst),
    /// Parse that is repeated for the given bound
    ///
    /// ```plain
    /// p1 ** expr
    /// ```
    Repeat(RcParseExpr<N>, RepeatBound<N>),
    /// Parse that only succeeds if the predicate holds
    Assert(RcParseExpr<N>, RcExpr<N>),
    /// Parse the subparsers in sequence, binding the results to the specified names
    /// then executing them in the environment of the given expression
    ///
    /// An empty list of parsers represents a parser that always succeeds
    ///
    /// For example:
    ///
    /// ```plain
    /// (x : p1) (y : p2) (z : p3) => expr
    /// ```
    Sequence(Vec<Named<N, RcParseExpr<N>>>, RcExpr<N>),
    /// Try to match the parsers in order, returning the result of the first on that succeeds
    ///
    /// An empty list of parsers represents a parser that never succeeds
    ///
    /// For example:
    ///
    /// ```plain
    /// p1 | p2 | p3
    /// ```
    Choice(Vec<RcParseExpr<N>>),
    /// Applies the result of one parser to an unary function
    Apply(RcExpr<N>, RcParseExpr<N>),
}

impl<N: Name> ParseExpr<N> {
    pub fn abstract_names_at(&mut self, names: &[N], scope: ScopeIndex) {
        match *self {
            ParseExpr::Var(ref mut var) => var.abstract_names_at(names, scope),
            ParseExpr::Const(_) => {}
            ParseExpr::Repeat(ref mut parse_expr, ref mut size_bound) => {
                Rc::make_mut(parse_expr).abstract_names_at(names, scope);

                match *size_bound {
                    RepeatBound::Exact(ref mut size_expr) => {
                        Rc::make_mut(size_expr).abstract_names_at(names, scope);
                    }
                }
            }
            ParseExpr::Assert(ref mut parse_expr, ref mut pred_expr) => {
                Rc::make_mut(parse_expr).abstract_names_at(names, scope);
                Rc::make_mut(pred_expr).abstract_names_at(names, scope);
            }
            ParseExpr::Sequence(ref mut parse_exprs, ref mut expr) => {
                for (i, &mut Named(_, ref mut parse_expr)) in parse_exprs.iter_mut().enumerate() {
                    Rc::make_mut(parse_expr).abstract_names_at(names, scope.shift(i as u32));
                }
                Rc::make_mut(expr).abstract_names_at(names, scope.shift(parse_exprs.len() as u32));
            }
            ParseExpr::Choice(ref mut parse_exprs) => for parse_expr in parse_exprs {
                Rc::make_mut(parse_expr).abstract_names_at(names, scope);
            },
            ParseExpr::Apply(ref mut fn_expr, ref mut parse_expr) => {
                Rc::make_mut(fn_expr).abstract_names_at(names, scope);
                Rc::make_mut(parse_expr).abstract_names_at(names, scope);
            }
        }
    }

    pub fn abstract_names(&mut self, names: &[N]) {
        self.abstract_names_at(names, ScopeIndex(0));
    }
}

pub type RcParseExpr<N> = Rc<ParseExpr<N>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<N> {
    Const(Const),
    Prim(&'static str, RcType<N>),
    Var(Var<N>),
    Unop(Unop, RcExpr<N>),
    Binop(Binop, RcExpr<N>, RcExpr<N>),
    Struct(Path<N>, Vec<Field<N, RcExpr<N>>>),
    Proj(RcExpr<N>, N),
    Intro(Path<N>, N, RcExpr<N>),
    Subscript(RcExpr<N>, RcExpr<N>),
    Abs(Vec<Named<N, RcType<N>>>, RcExpr<N>),
    App(RcExpr<N>, Vec<RcExpr<N>>),
}

pub type RcExpr<N> = Rc<Expr<N>>;

impl<N: Name> Expr<N> {
    pub fn abstract_names_at(&mut self, names: &[N], scope: ScopeIndex) {
        match *self {
            Expr::Var(ref mut var) => var.abstract_names_at(names, scope),
            Expr::Const(_) => {}
            // Expr::Prim(_, ref mut repr_ty) => Rc::make_mut(repr_ty).abstract_names_at(names, scope),
            Expr::Prim(_, _) => {}
            Expr::Unop(_, ref mut expr) | Expr::Proj(ref mut expr, _) => {
                Rc::make_mut(expr).abstract_names_at(names, scope);
            }
            Expr::Intro(_, _, ref mut expr) => {
                Rc::make_mut(expr).abstract_names_at(names, scope);
            }
            Expr::Binop(_, ref mut lhs_expr, ref mut rhs_expr) => {
                Rc::make_mut(lhs_expr).abstract_names_at(names, scope);
                Rc::make_mut(rhs_expr).abstract_names_at(names, scope);
            }
            Expr::Struct(_, ref mut fields) => for field in fields {
                Rc::make_mut(&mut field.value).abstract_names_at(names, scope);
            },
            Expr::Subscript(ref mut array_expr, ref mut index_expr) => {
                Rc::make_mut(array_expr).abstract_names_at(names, scope);
                Rc::make_mut(index_expr).abstract_names_at(names, scope);
            }
            Expr::Abs(_, ref mut body_expr) => {
                Rc::make_mut(body_expr).abstract_names_at(names, scope.succ());
            }
            Expr::App(ref mut fn_expr, ref mut arg_exprs) => {
                Rc::make_mut(fn_expr).abstract_names_at(names, scope);

                for arg_expr in arg_exprs {
                    Rc::make_mut(arg_expr).abstract_names_at(names, scope);
                }
            }
        }
    }

    pub fn abstract_names(&mut self, names: &[N]) {
        self.abstract_names_at(names, ScopeIndex(0));
    }
}
