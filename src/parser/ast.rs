//! The parse tree of the DDL
//!
//! This has a 1:1 mapping to the exposed concrete syntax. In order to do
//! anything interesting with this, one must first lower it to the core
//! representation.

use parser::{from_lalrpop_err, grammar, GrammarError, ParseError};
use parser::lexer::Lexer;
use source::{BytePos, Span};

pub use syntax::ast::{Binop, Const, FloatType, IntSuffix, TypeConst, Unop};

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'src> {
    pub imports: Vec<(&'src str, Exposing<'src>)>,
    pub definitions: Vec<Definition<'src>>,
}

impl<'src> Module<'src> {
    /// Attempt to parse a module from a source string
    pub fn from_str(src: &'src str) -> Result<Module<'src>, ParseError> {
        grammar::parse_Module(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map_err(from_lalrpop_err)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Exposing<'src> {
    All,
    Names(Vec<&'src str>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Definition<'src> {
    pub doc: Vec<&'src str>,
    pub span: Span,
    pub name: &'src str,
    pub param_names: Vec<&'src str>,
    pub body_ty: Type<'src>,
}

impl<'src> Definition<'src> {
    /// Attempt to parse a definition from a source string
    pub fn from_str(src: &'src str) -> Result<Definition<'src>, ParseError> {
        grammar::parse_Definition(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map_err(from_lalrpop_err)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field<'src, T> {
    pub doc: Vec<&'src str>,
    pub name: &'src str,
    pub value: T,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'src> {
    Var(Span, &'src str),
    Array(Span, Box<Type<'src>>, Box<Expr<'src>>),
    Cond(Span, Vec<Field<'src, (Expr<'src>, Type<'src>)>>),
    Struct(Span, Vec<Field<'src, Type<'src>>>),
    Where(Span, Box<Type<'src>>, BytePos, &'src str, Box<Expr<'src>>),
    Compute(Span, TypeConst, Box<Expr<'src>>),
    App(Span, Box<Type<'src>>, Vec<Type<'src>>),
}

impl<'src> Type<'src> {
    /// Attempt to parse a type from a source string
    pub fn from_str(src: &'src str) -> Result<Type<'src>, ParseError> {
        grammar::parse_BinaryType(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map_err(from_lalrpop_err)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'src> {
    Ann(Span, Box<Expr<'src>>, TypeConst),
    Const(Span, Const),
    Var(Span, &'src str),
    Unop(Span, Unop, Box<Expr<'src>>),
    Binop(Span, Binop, Box<Expr<'src>>, Box<Expr<'src>>),
    Array(Span, Vec<Expr<'src>>),
    Proj(Span, Box<Expr<'src>>, &'src str),
    Subscript(Span, Box<Expr<'src>>, Box<Expr<'src>>),
    Cast(Span, Box<Expr<'src>>, TypeConst),
}

impl<'src> Expr<'src> {
    /// Attempt to parse an expression from a source string
    pub fn from_str(src: &'src str) -> Result<Expr<'src>, ParseError> {
        grammar::parse_HostExpr(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map_err(from_lalrpop_err)
    }
}
