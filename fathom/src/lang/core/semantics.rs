//! Operational semantics of Fathom's core language.

use num_bigint::BigInt;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::sync::Arc;

use crate::lang::core::{
    FieldDefinition, Globals, Item, ItemData, Primitive, Sort, Term, TermData,
};
use crate::lang::Ranged;

/// Values.
#[derive(Debug, Clone)]
pub enum Value {
    /// A computation that is stuck on some [head value][`Head`] that cannot be
    /// reduced further in the current scope. We maintain a 'spine' of
    /// [eliminators][`Elim`], that can be applied if the head becomes unstuck
    /// later on.
    ///
    /// This is more commonly known as a 'neutral value' in the type theory
    /// literature.
    Stuck(Head, Vec<Elim>),

    /// Sorts.
    Sort(Sort),

    /// Function types.
    FunctionType(Arc<Value>, Arc<Value>),

    /// Struct terms.
    StructTerm(BTreeMap<String, Arc<Value>>),

    /// Primitives.
    Primitive(Primitive),

    /// Type of format types.
    FormatType,

    /// Convert a format to its host representation.
    Repr,

    /// Error sentinel.
    Error,
}

impl Value {
    /// Create a global variable.
    pub fn global(name: impl Into<String>) -> Value {
        Value::Stuck(Head::Global(name.into()), Vec::new())
    }
}

/// The head of a [stuck value][`Value::Stuck`].
///
/// This cannot currently be reduced in the current scope.
#[derive(Debug, Clone)]
pub enum Head {
    /// Global variables.
    Global(String),
    /// Item variables.
    Item(String),
    /// Errors.
    Error,
}

/// An eliminator that is part of the spine of a [stuck value][`Value::Stuck`].
#[derive(Debug, Clone)]
pub enum Elim {
    /// Function eliminators (function application).
    ///
    /// This can be applied with the [`apply_function_elim`] function.
    Function(Arc<Value>),
    /// Struct eliminators.
    ///
    /// This can be applied with the [`apply_struct_elim`] function.
    Struct(String),
    /// Boolean eliminators.
    ///
    /// This can be applied with the [`apply_bool_elim`] function.
    Bool(Arc<Term>, Arc<Term>), // FIXME: turn this into a closure once we add local environments
    /// Integer eliminators.
    ///
    /// This can be applied with the [`apply_int_elim`] function.
    Int(BTreeMap<BigInt, Arc<Term>>, Arc<Term>), // FIXME: turn this into a closure once we add local environments
    /// Convert a format to its host representation.
    ///
    /// This can be applied with the [`apply_repr_elim`] function.
    Repr,
}

/// Evaluate a [`core::Term`] into a [`Value`].
///
/// [`Value`]: crate::lang::core::semantics::Value
/// [`core::Term`]: crate::lang::core::Term
pub fn eval(globals: &Globals, items: &HashMap<String, Item>, term: &Term) -> Arc<Value> {
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
                ItemData::Constant(constant) => eval(globals, items, &constant.term),
                ItemData::StructType(_) | ItemData::StructFormat(_) => {
                    Arc::new(Value::Stuck(Head::Item(name.clone()), Vec::new()))
                }
            },
        },
        TermData::Ann(term, _) => eval(globals, items, term),

        TermData::Sort(sort) => Arc::new(Value::Sort(*sort)),

        TermData::FunctionType(param_type, body_type) => {
            let param_type = eval(globals, items, param_type);
            let body_type = eval(globals, items, body_type);

            Arc::new(Value::FunctionType(param_type, body_type))
        }
        TermData::FunctionElim(head, argument) => {
            let head = eval(globals, items, head);
            let argument = eval(globals, items, argument);
            apply_function_elim(head, argument)
        }

        TermData::StructTerm(field_definitions) => {
            let field_definitions = field_definitions
                .iter()
                .map(|field_definition| {
                    (
                        field_definition.label.data.clone(),
                        eval(globals, items, &field_definition.term),
                    )
                })
                .collect();

            Arc::new(Value::StructTerm(field_definitions))
        }
        TermData::StructElim(head, field) => {
            let head = eval(globals, items, head);
            apply_struct_elim(head, field)
        }

        TermData::Primitive(primitive) => Arc::new(Value::Primitive(primitive.clone())),
        TermData::BoolElim(head, if_true, if_false) => {
            let head = eval(globals, items, head);
            apply_bool_elim(globals, items, head, if_true, if_false)
        }
        TermData::IntElim(head, branches, default) => {
            let head = eval(globals, items, head);
            apply_int_elim(globals, items, head, branches, default)
        }

        TermData::FormatType => Arc::new(Value::FormatType),

        TermData::Repr => Arc::new(Value::Repr),

        TermData::Error => Arc::new(Value::Error),
    }
}

fn apply_function_elim(mut head: Arc<Value>, argument: Arc<Value>) -> Arc<Value> {
    match Arc::make_mut(&mut head) {
        Value::Repr => apply_repr(argument),
        Value::Stuck(_, elims) => {
            elims.push(Elim::Function(argument));
            head
        }
        _ => Arc::new(Value::Error),
    }
}

fn apply_struct_elim(mut head: Arc<Value>, field_name: &str) -> Arc<Value> {
    match Arc::make_mut(&mut head) {
        Value::StructTerm(fields) => match fields.get(field_name) {
            Some(field) => field.clone(),
            None => Arc::new(Value::Error),
        },
        Value::Stuck(_, elims) => {
            elims.push(Elim::Struct(field_name.to_owned()));
            head
        }
        _ => Arc::new(Value::Error),
    }
}

fn apply_bool_elim(
    globals: &Globals,
    items: &HashMap<String, Item>,
    mut head: Arc<Value>,
    if_true: &Arc<Term>,
    if_false: &Arc<Term>,
) -> Arc<Value> {
    match Arc::make_mut(&mut head) {
        Value::Stuck(Head::Global(name), elims) => match (name.as_str(), elims.as_slice()) {
            ("true", []) => eval(globals, items, if_true),
            ("false", []) => eval(globals, items, if_false),
            _ => Arc::new(Value::Error),
        },
        Value::Stuck(_, elims) => {
            elims.push(Elim::Bool(if_true.clone(), if_false.clone()));
            head
        }
        _ => Arc::new(Value::Error),
    }
}

fn apply_int_elim(
    globals: &Globals,
    items: &HashMap<String, Item>,
    mut head: Arc<Value>,
    branches: &BTreeMap<BigInt, Arc<Term>>,
    default: &Arc<Term>,
) -> Arc<Value> {
    match Arc::make_mut(&mut head) {
        Value::Primitive(Primitive::Int(value)) => match branches.get(&value) {
            Some(term) => eval(globals, items, term),
            None => eval(globals, items, default),
        },
        Value::Stuck(_, elims) => {
            elims.push(Elim::Int(branches.clone(), default.clone()));
            head
        }
        _ => Arc::new(Value::Error),
    }
}

fn apply_repr(mut argument: Arc<Value>) -> Arc<Value> {
    match Arc::make_mut(&mut argument) {
        Value::Stuck(Head::Global(name), elims) => match (name.as_str(), elims.as_slice()) {
            ("U8", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("U16Be", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("U16Le", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("U32Le", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("U32Be", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("U64Le", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("U64Be", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("S8", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("S16Le", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("S16Be", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("S32Le", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("S32Be", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("S64Le", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("S64Be", []) => Arc::new(Value::Stuck(Head::Global("Int".to_owned()), Vec::new())),
            ("F32Le", []) => Arc::new(Value::Stuck(Head::Global("F32".to_owned()), Vec::new())),
            ("F32Be", []) => Arc::new(Value::Stuck(Head::Global("F32".to_owned()), Vec::new())),
            ("F64Le", []) => Arc::new(Value::Stuck(Head::Global("F64".to_owned()), Vec::new())),
            ("F64Be", []) => Arc::new(Value::Stuck(Head::Global("F64".to_owned()), Vec::new())),
            ("FormatArray", [Elim::Function(len), Elim::Function(elem_type)]) => {
                Arc::new(Value::Stuck(
                    Head::Global("Array".to_owned()),
                    vec![
                        Elim::Function(len.clone()),
                        Elim::Function(apply_repr(elem_type.clone())),
                    ],
                ))
            }
            _ => Arc::new(Value::Error),
        },
        Value::Stuck(_, elims) => {
            elims.push(Elim::Repr);
            argument
        }
        _ => Arc::new(Value::Error),
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
                Elim::Struct(field_name) => {
                    TermData::StructElim(Arc::new(head), field_name.clone())
                }
                Elim::Bool(if_true, if_false) => {
                    TermData::BoolElim(Arc::new(head), if_true.clone(), if_false.clone())
                }
                Elim::Int(branches, default) => {
                    TermData::IntElim(Arc::new(head), branches.clone(), default.clone())
                }
                Elim::Repr => {
                    TermData::FunctionElim(Arc::new(Term::from(TermData::Repr)), Arc::new(head))
                }
            })
        },
    )
}

/// Read a value back into the term syntax.
pub fn read_back(value: &Value) -> Term {
    match value {
        Value::Stuck(head, elims) => read_back_neutral(head, elims),

        Value::Sort(sort) => Term::from(TermData::Sort(*sort)),

        Value::FunctionType(param_type, body_type) => Term::from(TermData::FunctionType(
            Arc::new(read_back(param_type)),
            Arc::new(read_back(body_type)),
        )),

        Value::StructTerm(field_definitions) => Term::from(TermData::StructTerm(
            field_definitions
                .iter()
                .map(|(label, value)| FieldDefinition {
                    label: Ranged::from(label.clone()),
                    term: Arc::new(read_back(value)),
                })
                .collect(),
        )),

        Value::Primitive(primitive) => Term::from(TermData::Primitive(primitive.clone())),

        Value::FormatType => Term::from(TermData::FormatType),

        Value::Repr => Term::from(TermData::Repr),

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
    items: &HashMap<String, Item>,
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
            (Elim::Repr, Elim::Repr) => {}

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
    items: &HashMap<String, Item>,
    value0: &Value,
    value1: &Value,
) -> bool {
    match (value0, value1) {
        (Value::Stuck(head0, spine0), Value::Stuck(head1, spine1)) => {
            is_equal_head(head0, head1) && is_equal_spine(globals, items, spine0, spine1)
        }

        (Value::Sort(sort0), Value::Sort(sort1)) => sort0 == sort1,

        (
            Value::FunctionType(param_type0, body_type0),
            Value::FunctionType(param_type1, body_type1),
        ) => {
            is_equal(globals, items, param_type1, param_type0)
                && is_equal(globals, items, body_type0, body_type1)
        }

        (Value::Primitive(primitive0), Value::Primitive(primitive1)) => primitive0 == primitive1,

        (Value::FormatType, Value::FormatType) => true,

        (Value::Repr, Value::Repr) => true,

        // Errors are always treated as equal
        (Value::Error, _) | (_, Value::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}
