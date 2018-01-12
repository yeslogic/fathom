//! The parse tree of the DDL
//!
//! This has a 1:1 mapping to the exposed concrete syntax. In order to do
//! anything interesting with this, one must first lower it to the core
//! representation.

use parser::{from_lalrpop_err, grammar, GrammarError, ParseError};
use parser::lexer::Lexer;
use source::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'src> {
    pub definitions: Vec<Definition<'src>>,
}

impl<'src> Program<'src> {
    /// Attempt to parse a program from a source string
    pub fn from_str(src: &'src str) -> Result<Program<'src>, ParseError> {
        grammar::parse_Program(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
            .map_err(from_lalrpop_err)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Definition<'src> {
    pub doc: Vec<&'src str>,
    pub span: Span,
    pub name: &'src str,
    pub param_names: Vec<&'src str>,
    pub body_ty: binary::Type<'src>,
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

pub mod binary {
    use source::BytePos;

    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    pub enum Type<'src> {
        Var(Span, &'src str),
        Array(Span, Box<Type<'src>>, Box<host::Expr<'src>>),
        Cond(Span, Vec<Field<'src, (host::Expr<'src>, Type<'src>)>>),
        Struct(Span, Vec<Field<'src, Type<'src>>>),
        Where(
            Span,
            Box<Type<'src>>,
            BytePos,
            &'src str,
            Box<host::Expr<'src>>,
        ),
        Compute(Span, host::TypeConst, Box<host::Expr<'src>>),
        App(Span, Box<Type<'src>>, Vec<Type<'src>>),
    }

    impl<'src> Type<'src> {
        /// Attempt to parse a type from a source string
        pub fn from_str(src: &'src str) -> Result<Type<'src>, ParseError> {
            grammar::parse_BinaryType(Lexer::new(src).map(|x| x.map_err(GrammarError::from)))
                .map_err(from_lalrpop_err)
        }
    }
}

pub mod host {
    pub use syntax::ast::host::{Binop, Const, FloatType, IntSuffix, TypeConst, Unop};

    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    pub enum Expr<'src> {
        Const(Span, Const),
        Var(Span, &'src str),
        Unop(Span, Unop, Box<Expr<'src>>),
        Binop(Span, Binop, Box<Expr<'src>>, Box<Expr<'src>>),
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
}
