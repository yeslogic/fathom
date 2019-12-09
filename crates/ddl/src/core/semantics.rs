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
        // Errors are always treated as equal
        (Value::Error, _) | (_, Value::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}
