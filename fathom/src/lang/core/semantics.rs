//! Operational semantics of Fathom's core language.

use num_bigint::BigInt;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::sync::Arc;

use crate::lang::core::{Constant, Globals, Item, ItemData, Term, TermData};

/// Values.
#[derive(Debug, Clone)]
pub enum Value {
    /// A suspended elimination.
    ///
    /// This is more commonly known as a 'neutral value' or sometimes as an
    /// 'accumulator'.
    ///
    /// These eliminations cannot be reduced further as a result of being stuck
    /// on some head that also cannot be reduced further (eg. a parameter, an
    /// abstract global, or an unsolved metavariable).
    Stuck(Head, Vec<Elim>),

    /// Type of types.
    TypeType,
    /// Function types.
    FunctionType(Arc<Value>, Arc<Value>),
    /// Constants.
    Constant(Constant),
    /// Type of format types.
    FormatType,

    /// Error sentinel.
    Error,
}

impl Value {
    /// Create a global variable.
    pub fn global(name: impl Into<String>) -> Value {
        Value::Stuck(Head::Global(name.into()), Vec::new())
    }
}

/// The head of a stuck elimination.
#[derive(Debug, Clone)]
pub enum Head {
    /// Global variables.
    Global(String),
    /// Item variables.
    Item(String),
    /// Errors.
    Error,
}

/// An eliminator that cannot be reduced further is 'stuck' on some [`Head`].
#[derive(Debug, Clone)]
pub enum Elim {
    /// Function eliminatiors (function application).
    Function(Arc<Value>),
    /// Boolean eliminators.
    // FIXME: environment?
    Bool(Arc<Term>, Arc<Term>),
    /// Integer eliminators.
    Int(BTreeMap<BigInt, Arc<Term>>, Arc<Term>),
}

/// Evaluate a [`core::Term`] into a [`Value`].
///
/// [`Value`]: crate::lang::core::semantics::Value
/// [`core::Term`]: crate::lang::core::Term
pub fn eval(globals: &Globals, items: &HashMap<&str, Item>, term: &Term) -> Arc<Value> {
    match &term.data {
        TermData::Global(name) => match globals.get(name) {
            None => Arc::new(Value::Error),
            Some((_, global_term)) => match global_term {
                Some(global_term) => eval(globals, items, global_term),
                None => Arc::new(Value::Stuck(Head::Global(name.clone()), Vec::new())),
            },
        },
        TermData::Item(name) => match items.get(name.as_str()) {
            None => Arc::new(Value::Error),
            Some(item) => match &item.data {
                ItemData::Alias(alias) => eval(globals, items, &alias.term),
                ItemData::Struct(_) => Arc::new(Value::Stuck(Head::Item(name.clone()), Vec::new())),
            },
        },
        TermData::Ann(term, _) => eval(globals, items, term),
        TermData::TypeType => Arc::new(Value::TypeType),
        TermData::FunctionType(param_type, body_type) => {
            let param_type = eval(globals, items, param_type);
            let body_type = eval(globals, items, body_type);

            Arc::new(Value::FunctionType(param_type, body_type))
        }
        TermData::FunctionElim(head, argument) => match eval(globals, items, head).as_ref() {
            Value::Stuck(head, elims) => {
                let mut elims = elims.clone(); // FIXME: clone?
                elims.push(Elim::Function(eval(globals, items, argument)));
                Arc::new(Value::Stuck(head.clone(), elims))
            }
            _ => Arc::new(Value::Error),
        },
        TermData::Constant(constant) => Arc::new(Value::Constant(constant.clone())),
        TermData::BoolElim(head, if_true, if_false) => {
            match eval(globals, items, head).as_ref() {
                Value::Stuck(Head::Global(name), elims) if elims.is_empty() => {
                    match name.as_str() {
                        "true" => eval(globals, items, if_true),
                        "false" => eval(globals, items, if_false),
                        _ => {
                            let mut elims = elims.clone(); // FIXME: clone?
                            elims.push(Elim::Bool(if_true.clone(), if_false.clone()));
                            Arc::new(Value::Stuck(Head::Global(name.clone()), elims))
                        }
                    }
                }
                Value::Stuck(head, elims) => {
                    let mut elims = elims.clone(); // FIXME: clone?
                    elims.push(Elim::Bool(if_true.clone(), if_false.clone()));
                    Arc::new(Value::Stuck(head.clone(), elims))
                }
                _ => Arc::new(Value::Stuck(
                    Head::Error,
                    vec![Elim::Bool(if_true.clone(), if_false.clone())],
                )),
            }
        }
        TermData::IntElim(head, branches, default) => {
            match eval(globals, items, head).as_ref() {
                Value::Constant(Constant::Int(value)) => match branches.get(&value) {
                    Some(term) => eval(globals, items, term),
                    None => eval(globals, items, default),
                },
                Value::Stuck(head, elims) => {
                    let mut elims = elims.clone(); // FIXME: clone?
                    elims.push(Elim::Int(branches.clone(), default.clone()));
                    Arc::new(Value::Stuck(head.clone(), elims))
                }
                _ => Arc::new(Value::Stuck(
                    Head::Error,
                    vec![Elim::Int(branches.clone(), default.clone())],
                )),
            }
        }

        TermData::FormatType => Arc::new(Value::FormatType),

        TermData::Error => Arc::new(Value::Error),
    }
}

/// Read a neutral term back into the term syntax.
fn read_back_neutral(head: &Head, elims: &[Elim]) -> Term {
    elims.iter().fold(
        match head {
            Head::Global(name) => Term::from(TermData::Global(name.clone())),
            Head::Item(name) => Term::from(TermData::Item(name.clone())),
            Head::Error => Term::from(TermData::Error),
        },
        |head, elim| {
            Term::from(match elim {
                Elim::Function(argument) => {
                    TermData::FunctionElim(Arc::new(head), Arc::new(read_back(argument)))
                }
                Elim::Bool(if_true, if_false) => {
                    TermData::BoolElim(Arc::new(head), if_true.clone(), if_false.clone())
                }
                Elim::Int(branches, default) => {
                    TermData::IntElim(Arc::new(head), branches.clone(), default.clone())
                }
            })
        },
    )
}

/// Read a value back into the term syntax.
pub fn read_back(value: &Value) -> Term {
    match value {
        Value::Stuck(head, elims) => read_back_neutral(head, elims),
        Value::TypeType => Term::from(TermData::TypeType),
        Value::FunctionType(param_type, body_type) => Term::from(TermData::FunctionType(
            Arc::new(read_back(param_type)),
            Arc::new(read_back(body_type)),
        )),
        Value::Constant(constant) => Term::from(TermData::Constant(constant.clone())),
        Value::FormatType => Term::from(TermData::FormatType),
        Value::Error => Term::from(TermData::Error),
    }
}

/// Check that one [`Head`] is equal to another [`Head`].
fn is_equal_head(head0: &Head, head1: &Head) -> bool {
    match (head0, head1) {
        (Head::Global(name0), Head::Global(name1)) if name0 == name1 => true,
        (Head::Item(name0), Head::Item(name1)) if name0 == name1 => true,

        // Errors are always treated as equal
        (Head::Error, _) | (_, Head::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}

/// Check that one elimination spine is equal to another elimination spine.
fn is_equal_spine(
    globals: &Globals,
    items: &HashMap<&str, Item>,
    spine0: &[Elim],
    spine1: &[Elim],
) -> bool {
    if spine0.len() != spine1.len() {
        return false;
    }

    for (elim0, elim1) in Iterator::zip(spine0.iter(), spine1.iter()) {
        match (elim0, elim1) {
            (Elim::Function(input0), Elim::Function(input1))
                if is_equal(globals, items, input0, input1) => {}
            (Elim::Bool(if_true0, if_false0), Elim::Bool(if_true1, if_false1)) => {
                let if_true0 = eval(globals, items, if_true0);
                let if_true1 = eval(globals, items, if_true1);
                if !is_equal(globals, items, &if_true0, &if_true1) {
                    return false;
                }

                let if_false0 = eval(globals, items, if_false0);
                let if_false1 = eval(globals, items, if_false1);
                if !is_equal(globals, items, &if_false0, &if_false1) {
                    return false;
                }
            }
            (Elim::Int(branches0, default0), Elim::Int(branches1, default1)) => {
                if branches0.len() != branches1.len() {
                    return false;
                }

                if !Iterator::zip(branches0.iter(), branches1.iter()).all(
                    |((int0, body0), (int1, body1))| {
                        int0 == int1 && {
                            let body0 = eval(globals, items, body0);
                            let body1 = eval(globals, items, body1);
                            is_equal(globals, items, &body0, &body1)
                        }
                    },
                ) {
                    return false;
                }

                let default0 = eval(globals, items, default0);
                let default1 = eval(globals, items, default1);
                if !is_equal(globals, items, &default0, &default1) {
                    return false;
                }
            }

            (_, _) => return false,
        }
    }

    true
}

/// Check that one  [`Value`] is [computationally equal] to another  [`Value`].
///
/// [`Value`]: crate::lang::core::semantics::Value
/// [computationally equal]: https://ncatlab.org/nlab/show/equality#computational_equality
pub fn is_equal(
    globals: &Globals,
    items: &HashMap<&str, Item>,
    value0: &Value,
    value1: &Value,
) -> bool {
    match (value0, value1) {
        (Value::Stuck(head0, spine0), Value::Stuck(head1, spine1)) => {
            is_equal_head(head0, head1) && is_equal_spine(globals, items, spine0, spine1)
        }
        (Value::TypeType, Value::TypeType) => true,
        (
            Value::FunctionType(param_type0, body_type0),
            Value::FunctionType(param_type1, body_type1),
        ) => {
            is_equal(globals, items, param_type1, param_type0)
                && is_equal(globals, items, body_type0, body_type1)
        }
        (Value::Constant(constant0), Value::Constant(constant1)) => constant0 == constant1,
        (Value::FormatType, Value::FormatType) => true,

        // Errors are always treated as equal
        (Value::Error, _) | (_, Value::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}
