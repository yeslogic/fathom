//! The parse tree of the DDL
//!
//! This has a 1:1 mapping to the exposed concrete syntax. In order to do
//! anything interesting with this, one must first lower it to the core
//! representation.

use codespan::{ByteIndex, ByteSpan};

pub use syntax::core::{Binop, Const, FloatType, IntSuffix, TypeConst, Unop};

#[derive(Debug, Clone, PartialEq)]
pub enum Module<'input> {
    Valid(Vec<Definition<'input>>),
    Error(ByteSpan),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition<'input> {
    Valid {
        doc: Vec<&'input str>,
        span: ByteSpan,
        name: &'input str,
        param_names: Vec<&'input str>,
        body_ty: Type<'input>,
    },
    Error(ByteSpan),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field<'input, T> {
    pub doc: Vec<&'input str>,
    pub name: &'input str,
    pub value: T,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'input> {
    Var(ByteSpan, &'input str),
    Array(ByteSpan, Box<Type<'input>>, Box<Expr<'input>>),
    Cond(ByteSpan, Vec<Field<'input, (Expr<'input>, Type<'input>)>>),
    Struct(ByteSpan, Vec<Field<'input, Type<'input>>>),
    Where(
        ByteSpan,
        Box<Type<'input>>,
        ByteIndex,
        &'input str,
        Box<Expr<'input>>,
    ),
    Compute(ByteSpan, TypeConst, Box<Expr<'input>>),
    App(ByteSpan, Box<Type<'input>>, Vec<Type<'input>>),
    Error(ByteSpan),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'input> {
    Ann(ByteSpan, Box<Expr<'input>>, TypeConst),
    Const(ByteSpan, Const),
    Var(ByteSpan, &'input str),
    Unop(ByteSpan, Unop, Box<Expr<'input>>),
    Binop(ByteSpan, Binop, Box<Expr<'input>>, Box<Expr<'input>>),
    Array(ByteSpan, Vec<Expr<'input>>),
    Proj(ByteSpan, Box<Expr<'input>>, &'input str),
    Subscript(ByteSpan, Box<Expr<'input>>, Box<Expr<'input>>),
    Cast(ByteSpan, Box<Expr<'input>>, TypeConst),
    Error(ByteSpan),
}
