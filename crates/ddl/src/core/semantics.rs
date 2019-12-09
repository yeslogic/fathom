//! Operational semantics of the data description language.

use codespan::Span;
use std::sync::Arc;

use crate::core::{Constant, Elim, Head, Term, Value};

/// Evaluate a term into a semantic value.
pub fn eval(term: &Term) -> Value {
    match term {
        Term::Item(_, label) => Value::Neutral(Head::Item(label.clone()), Vec::new()), // TODO: Evaluate to value in environment
        Term::Ann(term, _) => eval(term),
        Term::Universe(_, universe) => Value::Universe(*universe),
        Term::Constant(_, constant) => Value::Constant(constant.clone()),
        Term::U8Type(_) => Value::U8Type,
        Term::U16LeType(_) => Value::U16LeType,
        Term::U16BeType(_) => Value::U16BeType,
        Term::U32LeType(_) => Value::U32LeType,
        Term::U32BeType(_) => Value::U32BeType,
        Term::U64LeType(_) => Value::U64LeType,
        Term::U64BeType(_) => Value::U64BeType,
        Term::S8Type(_) => Value::S8Type,
        Term::S16LeType(_) => Value::S16LeType,
        Term::S16BeType(_) => Value::S16BeType,
        Term::S32LeType(_) => Value::S32LeType,
        Term::S32BeType(_) => Value::S32BeType,
        Term::S64LeType(_) => Value::S64LeType,
        Term::S64BeType(_) => Value::S64BeType,
        Term::F32LeType(_) => Value::F32LeType,
        Term::F32BeType(_) => Value::F32BeType,
        Term::F64LeType(_) => Value::F64LeType,
        Term::F64BeType(_) => Value::F64BeType,
        Term::BoolType(_) => Value::BoolType,
        Term::IntType(_) => Value::IntType,
        Term::F32Type(_) => Value::F32Type,
        Term::F64Type(_) => Value::F64Type,
        Term::BoolElim(_, head, if_true, if_false) => match eval(head) {
            Value::Constant(Constant::Bool(true)) => eval(if_true),
            Value::Constant(Constant::Bool(false)) => eval(if_false),
            Value::Neutral(head, mut elims) => {
                elims.push(Elim::Bool(if_true.clone(), if_false.clone()));
                Value::Neutral(head, elims)
            }
            _ => Value::Neutral(
                Head::Error,
                vec![Elim::Bool(if_true.clone(), if_false.clone())],
            ),
        },
        Term::IntElim(_, head, branches, default) => match eval(head) {
            Value::Constant(Constant::Int(value)) => match branches.get(&value) {
                Some(term) => eval(term),
                None => eval(default),
            },
            Value::Neutral(head, mut elims) => {
                elims.push(Elim::Int(branches.clone(), default.clone()));
                Value::Neutral(head, elims)
            }
            _ => Value::Neutral(
                Head::Error,
                vec![Elim::Int(branches.clone(), default.clone())],
            ),
        },
        Term::Error(_) => Value::Error,
    }
}

/// Read a neutral term back into the term syntax.
fn readback_neutral(head: &Head, elims: &[Elim]) -> Term {
    elims.iter().fold(
        match head {
            Head::Item(label) => Term::Item(Span::initial(), label.clone()),
            Head::Error => Term::Error(Span::initial()),
        },
        |acc, elim| match elim {
            Elim::Bool(if_true, if_false) => Term::BoolElim(
                Span::initial(),
                Arc::new(acc),
                if_true.clone(),
                if_false.clone(),
            ),
            Elim::Int(branches, default) => Term::IntElim(
                Span::initial(),
                Arc::new(acc),
                branches.clone(),
                default.clone(),
            ),
        },
    )
}

/// Read a value back into the term syntax.
pub fn readback(value: &Value) -> Term {
    match value {
        Value::Neutral(head, elims) => readback_neutral(head, elims),
        Value::Universe(universe) => Term::Universe(Span::initial(), *universe),
        Value::Constant(constant) => Term::Constant(Span::initial(), constant.clone()),
        Value::U8Type => Term::U8Type(Span::initial()),
        Value::U16LeType => Term::U16LeType(Span::initial()),
        Value::U16BeType => Term::U16BeType(Span::initial()),
        Value::U32LeType => Term::U32LeType(Span::initial()),
        Value::U32BeType => Term::U32BeType(Span::initial()),
        Value::U64LeType => Term::U64LeType(Span::initial()),
        Value::U64BeType => Term::U64BeType(Span::initial()),
        Value::S8Type => Term::S8Type(Span::initial()),
        Value::S16LeType => Term::S16LeType(Span::initial()),
        Value::S16BeType => Term::S16BeType(Span::initial()),
        Value::S32LeType => Term::S32LeType(Span::initial()),
        Value::S32BeType => Term::S32BeType(Span::initial()),
        Value::S64LeType => Term::S64LeType(Span::initial()),
        Value::S64BeType => Term::S64BeType(Span::initial()),
        Value::F32LeType => Term::F32LeType(Span::initial()),
        Value::F32BeType => Term::F32BeType(Span::initial()),
        Value::F64LeType => Term::F64LeType(Span::initial()),
        Value::F64BeType => Term::F64BeType(Span::initial()),
        Value::BoolType => Term::BoolType(Span::initial()),
        Value::IntType => Term::IntType(Span::initial()),
        Value::F32Type => Term::F32Type(Span::initial()),
        Value::F64Type => Term::F64Type(Span::initial()),
        Value::Error => Term::Error(Span::initial()),
    }
}

/// Check that two values are equal.
pub fn equal(val1: &Value, val2: &Value) -> bool {
    match (val1, val2) {
        (Value::Neutral(head0, elims0), Value::Neutral(head1, elims1)) => {
            readback_neutral(head0, elims0) == readback_neutral(head1, elims1)
        }
        (Value::Universe(universe0), Value::Universe(universe1)) => universe0 == universe1,
        (Value::Constant(constant0), Value::Constant(constant1)) => constant0 == constant1,
        (Value::U8Type, Value::U8Type)
        | (Value::U16LeType, Value::U16LeType)
        | (Value::U16BeType, Value::U16BeType)
        | (Value::U32LeType, Value::U32LeType)
        | (Value::U32BeType, Value::U32BeType)
        | (Value::U64LeType, Value::U64LeType)
        | (Value::U64BeType, Value::U64BeType)
        | (Value::S8Type, Value::S8Type)
        | (Value::S16LeType, Value::S16LeType)
        | (Value::S16BeType, Value::S16BeType)
        | (Value::S32LeType, Value::S32LeType)
        | (Value::S32BeType, Value::S32BeType)
        | (Value::S64LeType, Value::S64LeType)
        | (Value::S64BeType, Value::S64BeType)
        | (Value::F32LeType, Value::F32LeType)
        | (Value::F32BeType, Value::F32BeType)
        | (Value::F64LeType, Value::F64LeType)
        | (Value::F64BeType, Value::F64BeType)
        | (Value::BoolType, Value::BoolType)
        | (Value::IntType, Value::IntType)
        | (Value::F32Type, Value::F32Type)
        | (Value::F64Type, Value::F64Type) => true,
        // Errors are always treated as equal
        (Value::Error, _) | (_, Value::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}
