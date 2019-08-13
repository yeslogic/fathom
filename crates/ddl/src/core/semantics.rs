//! Operational semantics of the data description language.

use codespan::Span;

use crate::core::{Term, Value};

/// Evaluate a term into a semantic value.
pub fn eval(term: &Term) -> Value {
    match term {
        Term::Item(_, label) => Value::Item(label.clone()), // TODO: Evaluate to value in environment
        Term::Ann(term, _) => eval(term),
        Term::Kind(_) => Value::Kind,
        Term::Type(_) => Value::Type,
        Term::U8(_) => Value::U8,
        Term::U16Le(_) => Value::U16Le,
        Term::U16Be(_) => Value::U16Be,
        Term::U32Le(_) => Value::U32Le,
        Term::U32Be(_) => Value::U32Be,
        Term::U64Le(_) => Value::U64Le,
        Term::U64Be(_) => Value::U64Be,
        Term::S8(_) => Value::S8,
        Term::S16Le(_) => Value::S16Le,
        Term::S16Be(_) => Value::S16Be,
        Term::S32Le(_) => Value::S32Le,
        Term::S32Be(_) => Value::S32Be,
        Term::S64Le(_) => Value::S64Le,
        Term::S64Be(_) => Value::S64Be,
        Term::F32Le(_) => Value::F32Le,
        Term::F32Be(_) => Value::F32Be,
        Term::F64Le(_) => Value::F64Le,
        Term::F64Be(_) => Value::F64Be,
        Term::Error(_) => Value::Error,
    }
}

/// Read a value back into the term syntax.
pub fn readback(value: &Value) -> Term {
    match value {
        Value::Item(label) => Term::Item(Span::initial(), label.clone()),
        Value::Kind => Term::Kind(Span::initial()),
        Value::Type => Term::Type(Span::initial()),
        Value::U8 => Term::U8(Span::initial()),
        Value::U16Le => Term::U16Le(Span::initial()),
        Value::U16Be => Term::U16Be(Span::initial()),
        Value::U32Le => Term::U32Le(Span::initial()),
        Value::U32Be => Term::U32Be(Span::initial()),
        Value::U64Le => Term::U64Le(Span::initial()),
        Value::U64Be => Term::U64Be(Span::initial()),
        Value::S8 => Term::S8(Span::initial()),
        Value::S16Le => Term::S16Le(Span::initial()),
        Value::S16Be => Term::S16Be(Span::initial()),
        Value::S32Le => Term::S32Le(Span::initial()),
        Value::S32Be => Term::S32Be(Span::initial()),
        Value::S64Le => Term::S64Le(Span::initial()),
        Value::S64Be => Term::S64Be(Span::initial()),
        Value::F32Le => Term::F32Le(Span::initial()),
        Value::F32Be => Term::F32Be(Span::initial()),
        Value::F64Le => Term::F64Le(Span::initial()),
        Value::F64Be => Term::F64Be(Span::initial()),
        Value::Error => Term::Error(Span::initial()),
    }
}

pub fn equal(val1: &Value, val2: &Value) -> bool {
    match (val1, val2) {
        (Value::Item(label0), Value::Item(label1)) => label0 == label1,
        (Value::Kind, Value::Kind)
        | (Value::Type, Value::Type)
        | (Value::U8, Value::U8)
        | (Value::U16Le, Value::U16Le)
        | (Value::U16Be, Value::U16Be)
        | (Value::U32Le, Value::U32Le)
        | (Value::U32Be, Value::U32Be)
        | (Value::U64Le, Value::U64Le)
        | (Value::U64Be, Value::U64Be)
        | (Value::S8, Value::S8)
        | (Value::S16Le, Value::S16Le)
        | (Value::S16Be, Value::S16Be)
        | (Value::S32Le, Value::S32Le)
        | (Value::S32Be, Value::S32Be)
        | (Value::S64Le, Value::S64Le)
        | (Value::S64Be, Value::S64Be)
        | (Value::F32Le, Value::F32Le)
        | (Value::F32Be, Value::F32Be)
        | (Value::F64Le, Value::F64Le)
        | (Value::F64Be, Value::F64Be) => true,
        // Errors are always treated as equal
        (Value::Error, _) | (_, Value::Error) => true,
        // Anything else is not equal!
        (_, _) => false,
    }
}
