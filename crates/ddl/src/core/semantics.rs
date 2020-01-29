//! Operational semantics of the data description language.

use std::collections::HashMap;
use std::sync::Arc;

use crate::core::{Constant, Elim, Globals, Head, Item, Term, Value};

/// Evaluate a term into a semantic value.
pub fn eval(
    globals: &Globals,
    items: &HashMap<&str, Item>,
    // TODO: locals: &Locals<Arc<Value>>,
    term: &Term,
) -> Arc<Value> {
    match term {
        Term::Global(span, name) => match globals.get(name) {
            None => Arc::new(Value::Error(*span)),
            Some((_, term)) => match term {
                Some(term) => eval(globals, items, term),
                None => Arc::new(Value::Neutral(
                    Head::Global(*span, name.clone()),
                    Vec::new(),
                )),
            },
        },
        Term::Item(span, name) => match items.get(name.as_str()) {
            None => Arc::new(Value::Error(*span)),
            Some(Item::Alias(alias)) => eval(globals, items, &alias.term),
            Some(Item::Struct(_)) => {
                Arc::new(Value::Neutral(Head::Item(*span, name.clone()), Vec::new()))
            }
        },
        Term::Ann(term, _) => eval(globals, items, term),
        Term::Universe(span, universe) => Arc::new(Value::Universe(*span, *universe)),
        Term::Constant(span, constant) => Arc::new(Value::Constant(*span, constant.clone())),
        Term::BoolElim(span, head, if_true, if_false) => {
            match eval(globals, items, head).as_ref() {
                Value::Neutral(Head::Global(span, name), elims) if elims.is_empty() => {
                    match name.as_str() {
                        "true" => eval(globals, items, if_true),
                        "false" => eval(globals, items, if_false),
                        _ => {
                            let mut elims = elims.clone(); // FIXME: clone?
                            elims.push(Elim::Bool(*span, if_true.clone(), if_false.clone()));
                            Arc::new(Value::Neutral(Head::Global(*span, name.clone()), elims))
                        }
                        _ => Arc::new(Value::Neutral(
                            Head::Error,
                            vec![Elim::Bool(*span, if_true.clone(), if_false.clone())],
                        )),
                    }
                }
                _ => Arc::new(Value::Neutral(
                    Head::Error(head.span()),
                    vec![Elim::Bool(*span, if_true.clone(), if_false.clone())],
                )),
            }
        }
        Term::IntElim(span, head, branches, default) => match eval(globals, items, head).as_ref() {
            Value::Constant(_, Constant::Int(value)) => match branches.get(&value) {
                Some(term) => eval(globals, items, term),
                None => eval(globals, items, default),
            },
            Value::Neutral(head, elims) => {
                let mut elims = elims.clone(); // FIXME: clone?
                elims.push(Elim::Int(*span, branches.clone(), default.clone()));
                Arc::new(Value::Neutral(head.clone(), elims))
            }
            _ => Arc::new(Value::Neutral(
                Head::Error(head.span()),
                vec![Elim::Int(*span, branches.clone(), default.clone())],
            )),
        },
        Term::Error(span) => Arc::new(Value::Error(*span)),
    }
}

/// Read a neutral term back into the term syntax.
fn readback_neutral(head: &Head, elims: &[Elim]) -> Term {
    elims.iter().fold(
        match head {
            Head::Global(span, name) => Term::Global(*span, name.clone()),
            Head::Item(span, name) => Term::Item(*span, name.clone()),
            Head::Error(span) => Term::Error(*span),
        },
        |acc, elim| match elim {
            Elim::Bool(span, if_true, if_false) => {
                Term::BoolElim(*span, Arc::new(acc), if_true.clone(), if_false.clone())
            }
            Elim::Int(span, branches, default) => {
                Term::IntElim(*span, Arc::new(acc), branches.clone(), default.clone())
            }
        },
    )
}

/// Read a value back into the term syntax.
pub fn readback(value: &Value) -> Term {
    match value {
        Value::Neutral(head, elims) => readback_neutral(head, elims),
        Value::Universe(span, universe) => Term::Universe(*span, *universe),
        Value::Constant(span, constant) => Term::Constant(*span, constant.clone()),
        Value::Error(span) => Term::Error(*span),
    }
}

/// Check that two values are equal.
pub fn equal(val1: &Value, val2: &Value) -> bool {
    match (val1, val2) {
        (Value::Neutral(head0, elims0), Value::Neutral(head1, elims1)) => {
            readback_neutral(head0, elims0) == readback_neutral(head1, elims1)
        }
        (Value::Universe(_, universe0), Value::Universe(_, universe1)) => universe0 == universe1,
        (Value::Constant(_, constant0), Value::Constant(_, constant1)) => constant0 == constant1,
        // Errors are always treated as equal
        (Value::Error(_), _) | (_, Value::Error(_)) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}
