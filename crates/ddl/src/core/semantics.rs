//! Operational semantics of the data description language.

use codespan::Span;
use std::sync::Arc;

use crate::core::{Constant, Elim, Globals, Head, Term, Value};

/// Evaluate a term into a semantic value.
pub fn eval(globals: &Globals, /* TODO: items, locals, */ term: &Term) -> Arc<Value> {
    match term {
        Term::Global(_, name) => match globals.get(name) {
            None => Arc::new(Value::Error),
            Some((_, term)) => match term {
                Some(term) => eval(globals, term),
                None => Arc::new(Value::Neutral(Head::Global(name.clone()), Vec::new())),
            },
        },
        Term::Item(_, name) => Arc::new(Value::Neutral(Head::Item(name.clone()), Vec::new())), // TODO: Evaluate to value in environment
        Term::Ann(term, _) => eval(globals, term),
        Term::Universe(_, universe) => Arc::new(Value::Universe(*universe)),
        Term::Constant(_, constant) => Arc::new(Value::Constant(constant.clone())),
        Term::BoolElim(_, head, if_true, if_false) => match eval(globals, head).as_ref() {
            Value::Neutral(Head::Global(name), elims) if elims.is_empty() => match name.as_str() {
                "true" => eval(globals, if_true),
                "false" => eval(globals, if_false),
                _ => {
                    let mut elims = elims.clone(); // FIXME: clone?
                    elims.push(Elim::Bool(if_true.clone(), if_false.clone()));
                    Arc::new(Value::Neutral(Head::Global(name.clone()), elims))
                }
            },
            Value::Neutral(head, elims) => {
                let mut elims = elims.clone(); // FIXME: clone?
                elims.push(Elim::Bool(if_true.clone(), if_false.clone()));
                Arc::new(Value::Neutral(head.clone(), elims))
            }
            _ => Arc::new(Value::Neutral(
                Head::Error,
                vec![Elim::Bool(if_true.clone(), if_false.clone())],
            )),
        },
        Term::IntElim(_, head, branches, default) => match eval(globals, head).as_ref() {
            Value::Constant(Constant::Int(value)) => match branches.get(&value) {
                Some(term) => eval(globals, term),
                None => eval(globals, default),
            },
            Value::Neutral(head, elims) => {
                let mut elims = elims.clone(); // FIXME: clone?
                elims.push(Elim::Int(branches.clone(), default.clone()));
                Arc::new(Value::Neutral(head.clone(), elims))
            }
            _ => Arc::new(Value::Neutral(
                Head::Error,
                vec![Elim::Int(branches.clone(), default.clone())],
            )),
        },
        Term::Error(_) => Arc::new(Value::Error),
    }
}

/// Read a neutral term back into the term syntax.
fn readback_neutral(head: &Head, elims: &[Elim]) -> Term {
    elims.iter().fold(
        match head {
            Head::Global(name) => Term::Global(Span::initial(), name.clone()),
            Head::Item(name) => Term::Item(Span::initial(), name.clone()),
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
