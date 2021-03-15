//! Operational semantics of Fathom's core language.

use contracts::debug_ensures;
use num_bigint::BigInt;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::sync::Arc;

use crate::lang::core::{
    FieldDeclaration, FieldDefinition, Globals, LocalLevel, LocalSize, Locals, Primitive, Sort,
    Term, TermData,
};
use crate::lang::Located;

/// Evaluated items.
pub type Item = Located<ItemData>;

/// Evaluated item data.
#[derive(Debug, Clone)]
pub enum ItemData {
    Constant(Arc<Value>),
    StructType(Arc<[FieldDeclaration]>),
    StructFormat(Arc<[FieldDeclaration]>),
}

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

    /// Array terms.
    ArrayTerm(Vec<Arc<Value>>),

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
    pub fn global(name: impl Into<String>, elims: impl Into<Vec<Elim>>) -> Value {
        Value::Stuck(Head::Global(name.into()), elims.into())
    }

    /// Create an item variable.
    pub fn item(name: impl Into<String>, elims: impl Into<Vec<Elim>>) -> Value {
        Value::Stuck(Head::Item(name.into()), elims.into())
    }

    /// Create a local variable.
    pub fn local(level: impl Into<LocalLevel>, elims: impl Into<Vec<Elim>>) -> Value {
        Value::Stuck(Head::Local(level.into()), elims.into())
    }

    /// Create an integer primitive.
    pub fn int(data: impl Into<BigInt>) -> Value {
        Value::Primitive(Primitive::Int(data.into()))
    }

    /// Create a 32-bit float primitive.
    pub fn f32(data: impl Into<f32>) -> Value {
        Value::Primitive(Primitive::F32(data.into()))
    }

    /// Create a 64-bit float primitive.
    pub fn f64(data: impl Into<f64>) -> Value {
        Value::Primitive(Primitive::F64(data.into()))
    }

    /// Create a stream position.
    pub fn pos(data: usize) -> Value {
        Value::Primitive(Primitive::Pos(data))
    }

    /// Attempt to match against a stuck global.
    ///
    /// This can help to clean up pattern matches in lieu of
    /// [`match_default_bindings`](https://github.com/rust-lang/rust/issues/42640).
    pub fn try_global(&self) -> Option<(&str, &[Elim])> {
        match self {
            Value::Stuck(Head::Global(name), elims) => Some((name, elims)),
            _ => None,
        }
    }

    /// Attempt to match against a stuck item.
    ///
    /// This can help to clean up pattern matches in lieu of
    /// [`match_default_bindings`](https://github.com/rust-lang/rust/issues/42640).
    pub fn try_item(&self) -> Option<(&str, &[Elim])> {
        match self {
            Value::Stuck(Head::Item(name), elims) => Some((name, elims)),
            _ => None,
        }
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
    /// Local variables.
    Local(LocalLevel),
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
    Bool(Locals<Arc<Value>>, Arc<Term>, Arc<Term>),
    /// Integer eliminators.
    ///
    /// This can be applied with the [`apply_int_elim`] function.
    Int(Locals<Arc<Value>>, BTreeMap<BigInt, Arc<Term>>, Arc<Term>),
    /// Convert a format to its host representation.
    ///
    /// This can be applied with the [`apply_repr_elim`] function.
    Repr,
}

/// Normalize a [`Term`] using [normalization by evaluation].
///
/// [`Term`]: crate::lang::core::Term
/// [normalization by evaluation]: https://en.wikipedia.org/wiki/Normalisation_by_evaluation
#[debug_ensures(locals.size() == old(locals.size()))]
pub fn normalize(
    globals: &Globals,
    items: &HashMap<String, Item>,
    locals: &mut Locals<Arc<Value>>,
    term: &Term,
) -> Term {
    let value = eval(globals, items, locals, term);
    read_back(globals, items, locals.size(), &value)
}

/// Evaluate a [`core::Term`] into a [`Value`].
///
/// [`Value`]: crate::lang::core::semantics::Value
/// [`core::Term`]: crate::lang::core::Term
#[debug_ensures(locals.size() == old(locals.size()))]
pub fn eval(
    globals: &Globals,
    items: &HashMap<String, Item>,
    locals: &mut Locals<Arc<Value>>,
    term: &Term,
) -> Arc<Value> {
    match &term.data {
        TermData::Global(global_name) => match globals.get(global_name) {
            None => Arc::new(Value::Error),
            Some((_, global_term)) => match global_term {
                Some(global_term) => eval(globals, items, locals, global_term),
                None => Arc::new(Value::global(global_name.clone(), Vec::new())),
            },
        },
        TermData::Item(item_name) => match items.get(item_name.as_str()) {
            None => Arc::new(Value::Error),
            Some(item) => match &item.data {
                ItemData::Constant(value) => value.clone(),
                ItemData::StructType(_) | ItemData::StructFormat(_) => {
                    Arc::new(Value::item(item_name.clone(), Vec::new()))
                }
            },
        },
        TermData::Local(local_index) => match locals.get(*local_index) {
            Some(value) => value.clone(),
            None => {
                let local_level = local_index.to_level(locals.size()).unwrap();
                Arc::new(Value::local(local_level, Vec::new()))
            }
        },

        TermData::Ann(term, _) => eval(globals, items, locals, term),
        TermData::Sort(sort) => Arc::new(Value::Sort(*sort)),

        TermData::FunctionType(param_type, body_type) => {
            let param_type = eval(globals, items, locals, param_type);
            let body_type = eval(globals, items, locals, body_type);

            Arc::new(Value::FunctionType(param_type, body_type))
        }
        TermData::FunctionElim(head, argument) => {
            let head = eval(globals, items, locals, head);
            let argument = eval(globals, items, locals, argument);
            apply_function_elim(head, argument)
        }

        TermData::StructTerm(field_definitions) => {
            let field_definitions = field_definitions
                .iter()
                .map(|field_definition| {
                    (
                        field_definition.label.data.clone(),
                        eval(globals, items, locals, &field_definition.term),
                    )
                })
                .collect();

            Arc::new(Value::StructTerm(field_definitions))
        }
        TermData::StructElim(head, field) => {
            let head = eval(globals, items, locals, head);
            apply_struct_elim(head, field)
        }

        TermData::ArrayTerm(elem_terms) => {
            let elem_values = elem_terms
                .iter()
                .map(|elem_term| eval(globals, items, locals, elem_term))
                .collect();

            Arc::new(Value::ArrayTerm(elem_values))
        }

        TermData::Primitive(primitive) => Arc::new(Value::Primitive(primitive.clone())),
        TermData::BoolElim(head, if_true, if_false) => {
            let head = eval(globals, items, locals, head);
            apply_bool_elim(globals, items, locals, head, if_true, if_false)
        }
        TermData::IntElim(head, branches, default) => {
            let head = eval(globals, items, locals, head);
            apply_int_elim(globals, items, locals, head, branches, default)
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

pub fn apply_struct_elim(mut head: Arc<Value>, field_name: &str) -> Arc<Value> {
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

#[debug_ensures(locals.size() == old(locals.size()))]
fn apply_bool_elim(
    globals: &Globals,
    items: &HashMap<String, Item>,
    locals: &mut Locals<Arc<Value>>,
    mut head: Arc<Value>,
    if_true: &Arc<Term>,
    if_false: &Arc<Term>,
) -> Arc<Value> {
    match Arc::make_mut(&mut head) {
        Value::Stuck(Head::Global(name), elims) => match (name.as_str(), elims.as_slice()) {
            ("true", []) => eval(globals, items, locals, if_true),
            ("false", []) => eval(globals, items, locals, if_false),
            _ => Arc::new(Value::Error),
        },
        Value::Stuck(_, elims) => {
            elims.push(Elim::Bool(
                locals.clone(),
                if_true.clone(),
                if_false.clone(),
            ));
            head
        }
        _ => Arc::new(Value::Error),
    }
}

#[debug_ensures(locals.size() == old(locals.size()))]
fn apply_int_elim(
    globals: &Globals,
    items: &HashMap<String, Item>,
    locals: &mut Locals<Arc<Value>>,
    mut head: Arc<Value>,
    branches: &BTreeMap<BigInt, Arc<Term>>,
    default: &Arc<Term>,
) -> Arc<Value> {
    match Arc::make_mut(&mut head) {
        Value::Primitive(Primitive::Int(value)) => match branches.get(&value) {
            Some(term) => eval(globals, items, locals, term),
            None => eval(globals, items, locals, default),
        },
        Value::Stuck(_, elims) => {
            elims.push(Elim::Int(locals.clone(), branches.clone(), default.clone()));
            head
        }
        _ => Arc::new(Value::Error),
    }
}

pub fn apply_repr(mut argument: Arc<Value>) -> Arc<Value> {
    match Arc::make_mut(&mut argument) {
        Value::Stuck(Head::Global(name), elims) => match (name.as_str(), elims.as_slice()) {
            ("U8", []) => Arc::new(Value::global("Int", Vec::new())),
            ("U16Be", []) => Arc::new(Value::global("Int", Vec::new())),
            ("U16Le", []) => Arc::new(Value::global("Int", Vec::new())),
            ("U32Le", []) => Arc::new(Value::global("Int", Vec::new())),
            ("U32Be", []) => Arc::new(Value::global("Int", Vec::new())),
            ("U64Le", []) => Arc::new(Value::global("Int", Vec::new())),
            ("U64Be", []) => Arc::new(Value::global("Int", Vec::new())),
            ("S8", []) => Arc::new(Value::global("Int", Vec::new())),
            ("S16Le", []) => Arc::new(Value::global("Int", Vec::new())),
            ("S16Be", []) => Arc::new(Value::global("Int", Vec::new())),
            ("S32Le", []) => Arc::new(Value::global("Int", Vec::new())),
            ("S32Be", []) => Arc::new(Value::global("Int", Vec::new())),
            ("S64Le", []) => Arc::new(Value::global("Int", Vec::new())),
            ("S64Be", []) => Arc::new(Value::global("Int", Vec::new())),
            ("F32Le", []) => Arc::new(Value::global("F32", Vec::new())),
            ("F32Be", []) => Arc::new(Value::global("F32", Vec::new())),
            ("F64Le", []) => Arc::new(Value::global("F64", Vec::new())),
            ("F64Be", []) => Arc::new(Value::global("F64", Vec::new())),
            ("FormatArray", [Elim::Function(len), Elim::Function(elem_type)]) => {
                Arc::new(Value::global(
                    "Array",
                    vec![
                        Elim::Function(len.clone()),
                        Elim::Function(apply_repr(elem_type.clone())),
                    ],
                ))
            }
            ("CurrentPos", []) => {
                Arc::new(Value::Stuck(Head::Global("Pos".to_owned()), Vec::new()))
            }
            ("Link", [Elim::Function(_), Elim::Function(_), Elim::Function(_)]) => {
                Arc::new(Value::Stuck(Head::Global("Pos".to_owned()), Vec::new()))
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
fn read_back_neutral(
    globals: &Globals,
    items: &HashMap<String, Item>,
    local_size: LocalSize,
    head: &Head,
    elims: &[Elim],
) -> Term {
    let head = match head {
        Head::Global(global_name) => Term::generated(TermData::Global(global_name.clone())),
        Head::Item(item_name) => Term::generated(TermData::Item(item_name.clone())),
        Head::Local(local_level) => {
            Term::generated(TermData::Local(local_level.to_index(local_size).unwrap()))
        }
        Head::Error => Term::generated(TermData::Error),
    };

    elims.iter().fold(head, |head, elim| {
        Term::generated(match elim {
            Elim::Function(argument) => TermData::FunctionElim(
                Arc::new(head),
                Arc::new(read_back(globals, items, local_size, argument)),
            ),
            Elim::Struct(label) => TermData::StructElim(Arc::new(head), label.clone()),
            Elim::Bool(locals, if_true, if_false) => {
                let mut locals = locals.clone();
                let if_true = normalize(globals, items, &mut locals, if_true);
                let if_false = normalize(globals, items, &mut locals, if_false);

                TermData::BoolElim(Arc::new(head), Arc::new(if_true), Arc::new(if_false))
            }
            Elim::Int(locals, branches, default) => {
                let mut locals = locals.clone();
                let branches = branches
                    .iter()
                    .map(|(pattern, body)| {
                        let body = Arc::new(normalize(globals, items, &mut locals, body));
                        (pattern.clone(), body)
                    })
                    .collect();
                let default = normalize(globals, items, &mut locals, default);

                TermData::IntElim(Arc::new(head), branches, Arc::new(default))
            }
            Elim::Repr => {
                TermData::FunctionElim(Arc::new(Term::generated(TermData::Repr)), Arc::new(head))
            }
        })
    })
}

/// Read a [`Value`] back into a [`core::Term`].
///
/// [`Value`]: crate::lang::core::semantics::Value
/// [`core::Term`]: crate::lang::core::Term
pub fn read_back(
    globals: &Globals,
    items: &HashMap<String, Item>,
    local_size: LocalSize,
    value: &Value,
) -> Term {
    match value {
        Value::Stuck(head, elims) => read_back_neutral(globals, items, local_size, head, elims),

        Value::Sort(sort) => Term::generated(TermData::Sort(*sort)),

        Value::FunctionType(param_type, body_type) => Term::generated(TermData::FunctionType(
            Arc::new(read_back(globals, items, local_size, param_type)),
            Arc::new(read_back(globals, items, local_size, body_type)),
        )),

        Value::StructTerm(field_definitions) => Term::generated(TermData::StructTerm(
            field_definitions
                .iter()
                .map(|(label, value)| FieldDefinition {
                    label: Located::generated(label.clone()),
                    term: Arc::new(read_back(globals, items, local_size, value)),
                })
                .collect(),
        )),

        Value::ArrayTerm(elem_values) => Term::generated(TermData::ArrayTerm(
            elem_values
                .iter()
                .map(|elem_value| Arc::new(read_back(globals, items, local_size, elem_value)))
                .collect(),
        )),

        Value::Primitive(primitive) => Term::generated(TermData::Primitive(primitive.clone())),

        Value::FormatType => Term::generated(TermData::FormatType),

        Value::Repr => Term::generated(TermData::Repr),

        Value::Error => Term::generated(TermData::Error),
    }
}

/// Check that one [`Head`] is equal to another [`Head`].
fn is_equal_head(head0: &Head, head1: &Head) -> bool {
    match (head0, head1) {
        (Head::Global(global_name0), Head::Global(global_name1)) => global_name0 == global_name1,
        (Head::Item(item_name0), Head::Item(item_name1)) => item_name0 == item_name1,
        (Head::Local(local_index0), Head::Local(local_index1)) => local_index0 == local_index1,

        // Errors are always treated as equal
        (Head::Error, _) | (_, Head::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}

/// Check that one spine of eliminators is equal to another spine of eliminators.
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
            (
                Elim::Bool(locals0, if_true0, if_false0),
                Elim::Bool(locals1, if_true1, if_false1),
            ) => {
                let mut locals0 = locals0.clone();
                let mut locals1 = locals1.clone();

                let if_true0 = eval(globals, items, &mut locals0, if_true0);
                let if_true1 = eval(globals, items, &mut locals1, if_true1);
                if !is_equal(globals, items, &if_true0, &if_true1) {
                    return false;
                }

                let if_false0 = eval(globals, items, &mut locals0, if_false0);
                let if_false1 = eval(globals, items, &mut locals1, if_false1);
                if !is_equal(globals, items, &if_false0, &if_false1) {
                    return false;
                }
            }
            (Elim::Int(locals0, branches0, default0), Elim::Int(locals1, branches1, default1)) => {
                if branches0.len() != branches1.len() {
                    return false;
                }

                let mut locals0 = locals0.clone();
                let mut locals1 = locals1.clone();

                if !Iterator::zip(branches0.iter(), branches1.iter()).all(
                    |((int0, body0), (int1, body1))| {
                        int0 == int1 && {
                            let body0 = eval(globals, items, &mut locals0, body0);
                            let body1 = eval(globals, items, &mut locals1, body1);
                            is_equal(globals, items, &body0, &body1)
                        }
                    },
                ) {
                    return false;
                }

                let default0 = eval(globals, items, &mut locals0, default0);
                let default1 = eval(globals, items, &mut locals1, default1);
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

        (Value::StructTerm(field_definitions0), Value::StructTerm(field_definitions1)) => {
            field_definitions0.len() == field_definitions1.len()
                && field_definitions0.keys().all(|label| {
                    match (field_definitions1.get(label), field_definitions0.get(label)) {
                        (Some(value0), Some(value1)) => is_equal(globals, items, value0, value1),
                        (_, _) => false,
                    }
                })
        }

        (Value::ArrayTerm(elem_values0), Value::ArrayTerm(elem_values1)) => {
            elem_values0.len() == elem_values1.len()
                && Iterator::zip(elem_values0.iter(), elem_values1.iter()).all(
                    |(elem_value0, elem_value1)| is_equal(globals, items, elem_value0, elem_value1),
                )
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
