//! The parse tree of the DDL
//!
//! This has a 1:1 mapping to the exposed concrete syntax. In order to do
//! anything interesting with this, one must first lower it to the core
//! representation.

use codespan::{BytePos, Span};

pub use syntax::ast::{Binop, Const, FloatType, IntSuffix, TypeConst, Unop};

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'input> {
    pub definitions: Vec<Definition<'input>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Definition<'input> {
    pub doc: Vec<&'input str>,
    pub span: Span,
    pub name: &'input str,
    pub param_names: Vec<&'input str>,
    pub body_ty: Type<'input>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field<'input, T> {
    pub doc: Vec<&'input str>,
    pub name: &'input str,
    pub value: T,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'input> {
    Var(Span, &'input str),
    Array(Span, Box<Type<'input>>, Box<Expr<'input>>),
    Cond(Span, Vec<Field<'input, (Expr<'input>, Type<'input>)>>),
    Struct(Span, Vec<Field<'input, Type<'input>>>),
    Where(
        Span,
        Box<Type<'input>>,
        BytePos,
        &'input str,
        Box<Expr<'input>>,
    ),
    Compute(Span, TypeConst, Box<Expr<'input>>),
    App(Span, Box<Type<'input>>, Vec<Type<'input>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'input> {
    Ann(Span, Box<Expr<'input>>, TypeConst),
    Const(Span, Const),
    Var(Span, &'input str),
    Unop(Span, Unop, Box<Expr<'input>>),
    Binop(Span, Binop, Box<Expr<'input>>, Box<Expr<'input>>),
    Array(Span, Vec<Expr<'input>>),
    Proj(Span, Box<Expr<'input>>, &'input str),
    Subscript(Span, Box<Expr<'input>>, Box<Expr<'input>>),
    Cast(Span, Box<Expr<'input>>, TypeConst),
}
