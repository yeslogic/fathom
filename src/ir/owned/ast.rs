use std::rc::Rc;

use name::{Name, Named};
use source::Span;
use syntax;
use syntax::ast::{binary, host, Field, Var};

/// The definitions in this program
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<N> {
    pub defs: Vec<Definition<N>>,
}

/// The definition of a parseable type
///
/// For example:
///
/// ```plain
/// define Bmp {
///     type = struct {
///         width : u32be,
///         height : u32be,
///         data : [u8],
///     };
///
///     parser =
///         (width : u32be)
///         (height : u32be)
///         (data : u8 ** (width * height))
///             => struct { width, data, height };
/// };
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition<N> {
    name: N,
    ty: host::RcType<N>,
    parser: RcParseExpr<N>,
}

/// A bounded repitition
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RepeatBound<N> {
    /// A constant expression that bounds the repition
    Exact(host::RcExpr<N>),
}

/// A small parser combinator language
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseExpr<N> {
    /// A reference to another parser
    Var(Var<N, u32>),
    /// Parse a byte
    U8,
    /// The name of another parsable type
    Ident(N),
    /// Parse that is repeated for the given bound
    ///
    /// ```plain
    /// p1 ** expr
    /// ```
    Repeat(RcParseExpr<N>, RepeatBound<N>),
    /// Parse that only succeeds if the predicate holds
    Assert(RcParseExpr<N>, host::RcExpr<N>),
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
    Sequence(Vec<Named<N, RcParseExpr<N>>>, host::RcExpr<N>),
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
        E2: Into<host::RcExpr<N>>,
    {
        ParseExpr::Assert(parse_expr.into(), pred_expr.into())
    }

    pub fn sequence<E1>(parse_exprs: Vec<(N, RcParseExpr<N>)>, expr: E1) -> ParseExpr<N>
    where
        E1: Into<host::RcExpr<N>>,
    {
        let mut expr = expr.into();
        let mut named_exprs = Vec::with_capacity(parse_exprs.len());

        for (name, parse_exprs) in parse_exprs.into_iter().rev() {
            // FIXME: abstract parse exprs???
            Rc::make_mut(&mut expr).abstract_name(&name);
            named_exprs.push(Named(name, parse_exprs));
        }

        ParseExpr::Sequence(named_exprs, expr.into())
    }

    pub fn choice(parse_exprs: Vec<RcParseExpr<N>>) -> ParseExpr<N> {
        ParseExpr::Choice(parse_exprs)
    }
}

// Lowering

impl<'a, N: Name + for<'b> From<&'b str>> From<&'a syntax::ast::Program<N>> for Program<N> {
    fn from(src: &'a syntax::ast::Program<N>) -> Program<N> {
        Program {
            defs: src.defs.iter().map(Definition::from).collect(),
        }
    }
}

impl<'a, N: Name + for<'b> From<&'b str>> From<&'a syntax::ast::Definition<N>> for Definition<N> {
    fn from(src: &'a syntax::ast::Definition<N>) -> Definition<N> {
        Definition {
            name: src.name.clone(),
            ty: src.ty.repr(),
            parser: Rc::new(ParseExpr::from(&*src.ty)),
        }
    }
}

impl<'a, N: Name + for<'b> From<&'b str>> From<&'a binary::Type<N>> for ParseExpr<N> {
    fn from(src: &'a binary::Type<N>) -> ParseExpr<N> {
        use syntax::ast::binary::{Type, TypeConst};
        use syntax::ast::host::Expr;

        match *src {
            Type::Var(_, ref var) => ParseExpr::Var(var.clone()),
            Type::Const(TypeConst::U8) => ParseExpr::U8,
            Type::Array(_, ref elem_ty, ref size_expr) => {
                let elem_parser = ParseExpr::from(&**elem_ty);
                ParseExpr::repeat(elem_parser, RepeatBound::Exact(size_expr.clone()))
            }
            Type::Union(_, ref variants) => {
                let union_ty = src.repr();
                let lower_variant = |variant: &Field<N, binary::RcType<N>>| {
                    let variant_parser = Rc::new(ParseExpr::from(&*variant.value));
                    let variant_expr = Rc::new(Expr::intro(
                        Span::start(),
                        variant.name.clone(),
                        // FIXME: fresh variable?
                        Expr::bvar(Span::start(), "x", 0),
                        union_ty.clone(),
                    ));

                    Rc::new(ParseExpr::Sequence(
                        vec![Named(N::from("x"), variant_parser)],
                        variant_expr,
                    ))
                };

                ParseExpr::choice(variants.iter().map(lower_variant).collect())
            }
            Type::Struct(_, ref fields) => {
                let lower_to_field_parser = |field: &Field<N, binary::RcType<N>>| {
                    (field.name.clone(), Rc::new(ParseExpr::from(&*field.value)))
                };
                let lower_to_expr_field = |field: &Field<N, binary::RcType<N>>| {
                    Field::new(
                        field.name.clone(),
                        Expr::fvar(Span::start(), field.name.clone()),
                    )
                };

                ParseExpr::sequence(
                    fields.iter().map(lower_to_field_parser).collect(),
                    Expr::struct_(fields.iter().map(lower_to_expr_field).collect()),
                )
            }
            Type::Assert(_, ref ty, ref pred_expr) => {
                let ty_parser = ParseExpr::from(&**ty);
                ParseExpr::assert(ty_parser, pred_expr.clone())
            }
            Type::Interp(_, ref ty, ref conv, _) => {
                let ty_parser = Rc::new(ParseExpr::from(&**ty));

                ParseExpr::Sequence(
                    // FIXME: generate fresh name?
                    vec![Named(N::from("x"), ty_parser)],
                    Rc::new(Expr::app(
                        Span::start(),
                        conv.clone(),
                        Expr::bvar(Span::start(), N::from("x"), 0),
                    )),
                )
            }
            Type::Abs(_, _, _) => unimplemented!("Abs: {:?}", src),
            Type::App(_, _, _) => unimplemented!("App: {:?}", src),
        }
    }
}
