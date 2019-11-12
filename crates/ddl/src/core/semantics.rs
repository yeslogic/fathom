//! Operational semantics of the data description language.

use codespan::Span;
use std::sync::Arc;

use crate::core::{Elim, Head, Term, Value};
use crate::ieee754;

/// Evaluate a term into a semantic value.
pub fn eval(term: &Term) -> Value {
    match term {
        Term::Item(_, label) => Value::Neutral(Head::Item(label.clone()), Vec::new()), // TODO: Evaluate to value in environment
        Term::Ann(term, _) => eval(term),
        Term::Universe(_, universe) => Value::Universe(*universe),
        Term::IntConst(_, value) => Value::IntConst(value.clone()),
        Term::F32Const(_, value) => Value::F32Const(*value),
        Term::F64Const(_, value) => Value::F64Const(*value),
        Term::BoolElim(_, term, if_true, if_false) => match eval(term) {
            Value::Neutral(Head::Item(name), spine) if spine.is_empty() => match name.0.as_str() {
                // TODO: lookup in externs
                "true" => eval(if_true),
                "false" => eval(if_false),
                _ => Value::Neutral(
                    Head::Error,
                    vec![Elim::Bool(if_true.clone(), if_false.clone())],
                ),
            },
            Value::Neutral(head, mut elims) => {
                elims.push(Elim::Bool(if_true.clone(), if_false.clone()));
                Value::Neutral(head, elims)
            }
            _ => Value::Neutral(
                Head::Error,
                vec![Elim::Bool(if_true.clone(), if_false.clone())],
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
        },
    )
}

/// Read a value back into the term syntax.
pub fn readback(value: &Value) -> Term {
    match value {
        Value::Neutral(head, elims) => readback_neutral(head, elims),
        Value::Universe(universe) => Term::Universe(Span::initial(), *universe),
        Value::IntConst(value) => Term::IntConst(Span::initial(), value.clone()),
        Value::F32Const(value) => Term::F32Const(Span::initial(), *value),
        Value::F64Const(value) => Term::F64Const(Span::initial(), *value),
        Value::Error => Term::Error(Span::initial()),
    }
}

/// Check that two values are equal.
pub fn equal(val1: &Value, val2: &Value) -> bool {
    match (val1, val2) {
        (Value::Neutral(head0, elims0), Value::Neutral(head1, elims1)) => {
            readback_neutral(head0, elims0) == readback_neutral(head1, elims1)
        }
        (Value::IntConst(value0), Value::IntConst(value1)) => value0 == value1,
        (Value::F32Const(value0), Value::F32Const(value1)) => ieee754::logical_eq(*value0, *value1),
        (Value::F64Const(value0), Value::F64Const(value1)) => ieee754::logical_eq(*value0, *value1),
        (Value::Universe(universe0), Value::Universe(universe1)) => universe0 == universe1,
        // Errors are always treated as equal
        (Value::Error, _) | (_, Value::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}
