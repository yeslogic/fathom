//! Abstract syntax of the intermediate representation

use std::fmt;
use std::rc::Rc;

use name::Named;
pub use syntax::ast::Field;
pub use syntax::ast::host::{Binop, Const, IntSuffix, TypeConst, Unop};
pub use syntax::ast::host::{FloatType, SignedType, UnsignedType};
pub use syntax::ast::binary::{Endianness, TypeConst as BinaryTypeConst};
use var::{ScopeIndex, Var};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub definitions: Vec<Definition>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            definitions: Vec::new(),
        }
    }

    pub fn define(&mut self, definition: Definition) {
        self.definitions.push(definition);
    }
}

/// A fully qualified path to a type definition
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Path {
    /// The base definition name from the source AST
    pub base: String,
    /// The path through a structural type in the source AST
    pub children: Vec<String>,
}

impl Path {
    pub fn new(base: String) -> Path {
        Path {
            base,
            children: vec![],
        }
    }

    pub fn append_child<N: Into<String>>(&self, name: N) -> Path {
        let mut path = self.clone();
        path.children.push(name.into());
        path
    }
}

impl Path {
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

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.base)?;
        for child in &self.children {
            write!(f, "::{}", child)?;
        }
        Ok(())
    }
}

impl<'a> From<&'a str> for Path {
    fn from(src: &'a str) -> Path {
        let mut parts = src.split("::").map(String::from);

        let base = parts.next().unwrap();
        let children = parts.collect();

        Path { base, children }
    }
}

/// A top level definition
#[derive(Debug, Clone, PartialEq)]
pub struct Definition {
    /// Doc comment
    pub doc: Rc<str>,
    /// The path of this definition (should be unique)
    pub path: Path,
    /// Type parameters for the definition
    pub params: Vec<String>,
    /// The defined item
    pub item: Item,
}

/// Top level type items
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    /// Type alias
    Alias(RcType),
    /// Struct definition
    Struct(Vec<Field<RcType>>, Option<RcParseExpr>),
    /// Union type definition
    Union(Vec<Field<RcType>>, Option<RcParseExpr>),
}

/// Structural types
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Type constants
    Const(TypeConst),
    /// A fully qualified path to a type definition
    Path(Path, Vec<RcType>),
    /// Array types. These are usually available in languages as primitives,
    /// so there is no need to generate new types for these
    Array(RcType),
    /// Arrow types.
    Arrow(Vec<RcType>, RcType),
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

/// A bounded repitition
#[derive(Debug, Clone, PartialEq)]
pub enum RepeatBound {
    /// A constant expression that bounds the repition
    Exact(RcExpr),
}

/// A small parser combinator language
#[derive(Debug, Clone, PartialEq)]
pub enum ParseExpr {
    /// A reference to another parser
    Var(Var),
    /// Parse a binary constant
    Const(BinaryTypeConst),
    /// Parse that is repeated for the given bound
    ///
    /// ```plain
    /// p1 ** expr
    /// ```
    Repeat(RcParseExpr, RepeatBound),
    /// Parse that only succeeds if the predicate holds
    Assert(RcParseExpr, RcExpr),
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
    Sequence(Vec<Named<RcParseExpr>>, RcExpr),
    /// Try to match the parsers in order, returning the result of the first on that succeeds
    ///
    /// An empty list of parsers represents a parser that never succeeds
    ///
    /// For example:
    ///
    /// ```plain
    /// (cond1 => p1) | (cond2 => p2) | (cond3 => p3)
    /// ```
    Cond(Vec<(RcExpr, RcParseExpr)>),
    /// Applies the result of one parser to an unary function
    Apply(RcExpr, RcParseExpr),
}

#[derive(Clone, PartialEq)]
pub struct RcParseExpr {
    pub inner: Rc<ParseExpr>,
}

impl From<ParseExpr> for RcParseExpr {
    fn from(src: ParseExpr) -> RcParseExpr {
        RcParseExpr {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcParseExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl RcParseExpr {
    pub fn abstract_names_at(&mut self, names: &[&str], scope: ScopeIndex) {
        match *Rc::make_mut(&mut self.inner) {
            ParseExpr::Var(ref mut var) => var.abstract_names_at(names, scope),
            ParseExpr::Const(_) => {}
            ParseExpr::Repeat(ref mut parse_expr, ref mut size_bound) => {
                parse_expr.abstract_names_at(names, scope);

                match *size_bound {
                    RepeatBound::Exact(ref mut size_expr) => {
                        size_expr.abstract_names_at(names, scope);
                    }
                }
            }
            ParseExpr::Assert(ref mut parse_expr, ref mut pred_expr) => {
                parse_expr.abstract_names_at(names, scope);
                pred_expr.abstract_names_at(names, scope);
            }
            ParseExpr::Sequence(ref mut parse_exprs, ref mut expr) => {
                for (i, &mut Named(_, ref mut parse_expr)) in parse_exprs.iter_mut().enumerate() {
                    parse_expr.abstract_names_at(names, scope.shift(i as u32));
                }
                expr.abstract_names_at(names, scope.shift(parse_exprs.len() as u32));
            }
            ParseExpr::Cond(ref mut options) => for option in options {
                option.0.abstract_names_at(names, scope);
                option.1.abstract_names_at(names, scope);
            },
            ParseExpr::Apply(ref mut fn_expr, ref mut parse_expr) => {
                fn_expr.abstract_names_at(names, scope);
                parse_expr.abstract_names_at(names, scope);
            }
        }
    }

    pub fn abstract_names(&mut self, names: &[&str]) {
        self.abstract_names_at(names, ScopeIndex(0));
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Const(Const),
    Var(Var),
    Unop(Unop, RcExpr),
    Binop(Binop, RcExpr, RcExpr),
    Struct(Path, Vec<Field<RcExpr>>),
    Proj(RcExpr, String),
    Intro(Path, String, RcExpr),
    Subscript(RcExpr, RcExpr),
    Cast(RcExpr, RcType),
    Lam(Vec<Named<RcType>>, RcExpr),
    App(RcExpr, Vec<RcExpr>),
}

#[derive(Clone, PartialEq)]
pub struct RcExpr {
    pub inner: Rc<Expr>,
}

impl From<Expr> for RcExpr {
    fn from(src: Expr) -> RcExpr {
        RcExpr {
            inner: Rc::new(src),
        }
    }
}

impl fmt::Debug for RcExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl RcExpr {
    pub fn abstract_names_at(&mut self, names: &[&str], scope: ScopeIndex) {
        match *Rc::make_mut(&mut self.inner) {
            Expr::Var(ref mut var) => var.abstract_names_at(names, scope),
            Expr::Const(_) => {}
            Expr::Unop(_, ref mut expr) | Expr::Proj(ref mut expr, _) => {
                expr.abstract_names_at(names, scope);
            }
            Expr::Intro(_, _, ref mut expr) => {
                expr.abstract_names_at(names, scope);
            }
            Expr::Binop(_, ref mut lhs_expr, ref mut rhs_expr) => {
                lhs_expr.abstract_names_at(names, scope);
                rhs_expr.abstract_names_at(names, scope);
            }
            Expr::Struct(_, ref mut fields) => for field in fields {
                field.value.abstract_names_at(names, scope);
            },
            Expr::Subscript(ref mut array_expr, ref mut index_expr) => {
                array_expr.abstract_names_at(names, scope);
                index_expr.abstract_names_at(names, scope);
            }
            Expr::Cast(ref mut src_expr, _) => {
                src_expr.abstract_names_at(names, scope);
                // TODO: abstract dst_ty???
            }
            Expr::Lam(_, ref mut body_expr) => {
                body_expr.abstract_names_at(names, scope.succ());
            }
            Expr::App(ref mut fn_expr, ref mut arg_exprs) => {
                fn_expr.abstract_names_at(names, scope);

                for arg_expr in arg_exprs {
                    arg_expr.abstract_names_at(names, scope);
                }
            }
        }
    }

    pub fn abstract_names(&mut self, names: &[&str]) {
        self.abstract_names_at(names, ScopeIndex(0));
    }
}
