//! Operational semantics of the data description language.

use codespan::Span;

use crate::core::{Term, Value};
use crate::ieee754;

/// Evaluate a term into a semantic value.
pub fn eval(term: &Term) -> Value {
    match term {
        Term::Item(_, label) => Value::Item(label.clone()), // TODO: Evaluate to value in environment
        Term::Ann(term, _) => eval(term),
        Term::Type(_) => Value::Type,
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
        Term::BoolConst(_, value) => Value::BoolConst(*value),
        Term::IntConst(_, value) => Value::IntConst(value.clone()),
        Term::F32Const(_, value) => Value::F32Const(*value),
        Term::F64Const(_, value) => Value::F64Const(*value),
        Term::Error(_) => Value::Error,
    }
}

/// Read a value back into the term syntax.
pub fn readback(value: &Value) -> Term {
    match value {
        Value::Item(label) => Term::Item(Span::initial(), label.clone()),
        Value::Type => Term::Type(Span::initial()),
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
        Value::BoolConst(value) => Term::BoolConst(Span::initial(), *value),
        Value::IntConst(value) => Term::IntConst(Span::initial(), value.clone()),
        Value::F32Const(value) => Term::F32Const(Span::initial(), *value),
        Value::F64Const(value) => Term::F64Const(Span::initial(), *value),
        Value::Error => Term::Error(Span::initial()),
    }
}

pub fn equal(val1: &Value, val2: &Value) -> bool {
    match (val1, val2) {
        (Value::Item(label0), Value::Item(label1)) => label0 == label1,
        (Value::BoolConst(value0), Value::BoolConst(value1)) => value0 == value1,
        (Value::IntConst(value0), Value::IntConst(value1)) => value0 == value1,
        (Value::F32Const(value0), Value::F32Const(value1)) => ieee754::logical_eq(*value0, *value1),
        (Value::F64Const(value0), Value::F64Const(value1)) => ieee754::logical_eq(*value0, *value1),
        (Value::Type, Value::Type)
        | (Value::U8Type, Value::U8Type)
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
