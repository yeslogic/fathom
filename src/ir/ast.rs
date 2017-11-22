//! Abstract syntax of the intermediate representation

use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

use name::{Name, Named};
pub use syntax::ast::Field;
pub use syntax::ast::host::{Binop, Const, Unop};
use var::{BoundVar, ScopeIndex, Var};

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

    pub fn define_alias<P: Into<Path<N>>>(&mut self, path: P, ty: RcType<N>) {
        self.define(path, Definition::Alias(ty));
    }

    pub fn define_struct<P: Into<Path<N>>>(
        &mut self,
        path: P,
        fields: Vec<Field<N, RcType<N>>>,
        parser: Option<RcParseExpr<N>>,
    ) {
        self.define(path, Definition::Struct(fields, parser));
    }

    pub fn define_union<P: Into<Path<N>>>(
        &mut self,
        path: P,
        variants: Vec<Field<N, RcType<N>>>,
        parser: Option<RcParseExpr<N>>,
    ) {
        self.define(path, Definition::Union(variants, parser));
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
    Alias(RcType<N>),
    /// Struct definition
    Struct(Vec<Field<N, RcType<N>>>, Option<RcParseExpr<N>>),
    /// Union type definition
    Union(Vec<Field<N, RcType<N>>>, Option<RcParseExpr<N>>),
}

/// Structural types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<N> {
    /// A fully qualified path to a type definition
    Path(Path<N>),
    /// Array types. These are usually available in languages as primitives,
    /// so there is no need to generate new types for these
    Array(RcType<N>),
    /// Arrow types.
    Arrow(Vec<RcType<N>>, RcType<N>),
    U8,
    Bool,
    Int,
}

pub type RcType<N> = Rc<Type<N>>;

impl<N: Name> Type<N> {
    pub fn path<P: Into<Path<N>>>(path: P) -> Type<N> {
        Type::Path(path.into())
    }

    pub fn array<T1: Into<RcType<N>>>(elem_ty: T1) -> Type<N> {
        Type::Array(elem_ty.into())
    }
}

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
    /// Parse a byte
    U8,
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
}

pub type RcParseExpr<N> = Rc<ParseExpr<N>>;

impl<N: Name> ParseExpr<N> {
    pub fn repeat<E1>(elem_expr: E1, bound: RepeatBound<N>) -> ParseExpr<N>
    where
        E1: Into<RcParseExpr<N>>,
    {
        ParseExpr::Repeat(elem_expr.into(), bound)
    }

    pub fn assert<E1, E2>(parse_expr: E1, pred_expr: E2) -> ParseExpr<N>
    where
        E1: Into<RcParseExpr<N>>,
        E2: Into<RcExpr<N>>,
    {
        ParseExpr::Assert(parse_expr.into(), pred_expr.into())
    }

    pub fn sequence<E1>(parse_exprs: Vec<(N, RcParseExpr<N>)>, expr: E1) -> ParseExpr<N>
    where
        E1: Into<RcExpr<N>>,
    {
        let mut expr = expr.into();
        let mut named_exprs = Vec::with_capacity(parse_exprs.len());

        for (name, parse_exprs) in parse_exprs.into_iter().rev() {
            // FIXME: abstract parse exprs???
            Rc::make_mut(&mut expr).abstract_names(&[name.clone()]);
            named_exprs.push(Named(name, parse_exprs));
        }

        ParseExpr::Sequence(named_exprs, expr.into())
    }

    pub fn choice(parse_exprs: Vec<RcParseExpr<N>>) -> ParseExpr<N> {
        ParseExpr::Choice(parse_exprs)
    }
}

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
    pub fn u8(value: u8) -> Expr<N> {
        Expr::Const(Const::U8(value))
    }

    pub fn bool(value: bool) -> Expr<N> {
        Expr::Const(Const::Bool(value))
    }

    pub fn int(value: i64) -> Expr<N> {
        Expr::Const(Const::Int(value))
    }

    pub fn prim<T1: Into<RcType<N>>>(name: &'static str, repr_ty: T1) -> Expr<N> {
        Expr::Prim(name, repr_ty.into())
    }

    pub fn fvar<N1: Into<N>>(x: N1) -> Expr<N> {
        Expr::Var(Var::Free(x.into()))
    }

    pub fn bvar<N1: Into<N>>(x: N1, var: BoundVar) -> Expr<N> {
        Expr::Var(Var::Bound(Named(x.into(), var)))
    }

    pub fn unop<E1: Into<RcExpr<N>>>(op: Unop, x: E1) -> Expr<N> {
        Expr::Unop(op, x.into())
    }

    pub fn binop<E1, E2>(op: Binop, x: E1, y: E2) -> Expr<N>
    where
        E1: Into<RcExpr<N>>,
        E2: Into<RcExpr<N>>,
    {
        Expr::Binop(op, x.into(), y.into())
    }

    pub fn struct_<P>(path: P, fields: Vec<Field<N, RcExpr<N>>>) -> Expr<N>
    where
        P: Into<Path<N>>,
    {
        Expr::Struct(path.into(), fields)
    }

    pub fn proj<E1, N1>(expr: E1, field_name: N1) -> Expr<N>
    where
        E1: Into<RcExpr<N>>,
        N1: Into<N>,
    {
        Expr::Proj(expr.into(), field_name.into())
    }

    pub fn intro<P, N1, E1>(path: P, variant_name: N1, expr: E1) -> Expr<N>
    where
        P: Into<Path<N>>,
        N1: Into<N>,
        E1: Into<RcExpr<N>>,
    {
        Expr::Intro(path.into(), variant_name.into(), expr.into())
    }

    pub fn subscript<E1, E2>(expr: E1, index_expr: E2) -> Expr<N>
    where
        E1: Into<RcExpr<N>>,
        E2: Into<RcExpr<N>>,
    {
        Expr::Subscript(expr.into(), index_expr.into())
    }

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
