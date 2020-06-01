use fathom_rt::ReadFormat;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::collections::HashMap;

use crate::ast::core::binary::Term;
use crate::ast::core::semantics::{self, Elim, Head, Value};
use crate::ast::core::{Constant, Globals, Item, Module, StructType};

/// Contextual information to be used when parsing items.
pub struct Context<'me> {
    globals: &'me Globals,
    items: HashMap<&'me str, Item>,
    reader: fathom_rt::FormatReader<'me>,
}

impl<'me> Context<'me> {
    /// Create a new item context.
    pub fn new(globals: &'me Globals, reader: fathom_rt::FormatReader<'me>) -> Context<'me> {
        Context {
            globals,
            items: HashMap::new(),
            reader,
        }
    }

    /// Read some binary data in the context.
    fn read<T: ReadFormat<'me>>(&mut self) -> Result<T::Host, fathom_rt::ReadError> {
        self.reader.read::<T>()
    }
}

pub fn from_module_item<'module>(
    context: &mut Context<'module>,
    module: &'module Module,
    name: &str,
) -> Result<Term, fathom_rt::ReadError> {
    for item in &module.items {
        match item {
            Item::Alias(alias) if alias.name == name => {
                let value = semantics::eval(context.globals, &context.items, &alias.term);
                return from_ty(context, &value);
            }
            Item::Struct(struct_ty) if struct_ty.name == name => {
                return from_struct_ty(context, struct_ty);
            }
            Item::Alias(alias) => {
                context.items.insert(&alias.name, item.clone());
            }
            Item::Struct(struct_ty) => {
                context.items.insert(&struct_ty.name, item.clone());
            }
        }
    }

    Err(fathom_rt::ReadError::InvalidDataDescription)
}

pub fn from_struct_ty(
    context: &mut Context<'_>,
    struct_ty: &StructType,
) -> Result<Term, fathom_rt::ReadError> {
    let fields = struct_ty
        .fields
        .iter()
        .map(|field| {
            let value = semantics::eval(context.globals, &context.items, &field.term);
            Ok((field.name.clone(), from_ty(context, &value)?))
        })
        .collect::<Result<_, fathom_rt::ReadError>>()?;

    Ok(Term::Struct(fields))
}

pub fn from_ty(context: &mut Context<'_>, ty: &Value) -> Result<Term, fathom_rt::ReadError> {
    match ty {
        Value::Neutral(Head::Global(_, name), elims) => match (name.as_str(), elims.as_slice()) {
            ("U8", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::U8>()?))),
            ("U16Le", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::U16Le>()?))),
            ("U16Be", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::U16Be>()?))),
            ("U32Le", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::U32Le>()?))),
            ("U32Be", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::U32Be>()?))),
            ("U64Le", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::U64Le>()?))),
            ("U64Be", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::U64Be>()?))),
            ("S8", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::I8>()?))),
            ("S16Le", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::I16Le>()?))),
            ("S16Be", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::I16Be>()?))),
            ("S32Le", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::I32Le>()?))),
            ("S32Be", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::I32Be>()?))),
            ("S64Le", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::I64Le>()?))),
            ("S64Be", []) => Ok(Term::Int(BigInt::from(context.read::<fathom_rt::I64Be>()?))),
            ("F32Le", []) => Ok(Term::F32(context.read::<fathom_rt::F32Le>()?)),
            ("F32Be", []) => Ok(Term::F32(context.read::<fathom_rt::F32Be>()?)),
            ("F64Le", []) => Ok(Term::F64(context.read::<fathom_rt::F64Le>()?)),
            ("F64Be", []) => Ok(Term::F64(context.read::<fathom_rt::F64Be>()?)),
            ("FormatArray", [Elim::Function(_, len), Elim::Function(_, elem_ty)]) => {
                match len.as_ref() {
                    Value::Constant(_, Constant::Int(len)) => match len.to_usize() {
                        Some(len) => Ok(Term::Seq(
                            (0..len)
                                .map(|_| from_ty(context, elem_ty))
                                .collect::<Result<_, _>>()?,
                        )),
                        None => Err(fathom_rt::ReadError::InvalidDataDescription),
                    },
                    _ => Err(fathom_rt::ReadError::InvalidDataDescription),
                }
            }
            ("List", [Elim::Function(_, _)]) | (_, _) => {
                Err(fathom_rt::ReadError::InvalidDataDescription)
            }
        },
        Value::Neutral(Head::Item(_, name), elims) => {
            match (context.items.get(name.as_str()).cloned(), elims.as_slice()) {
                (Some(Item::Struct(struct_ty)), []) => from_struct_ty(context, &struct_ty),
                (Some(_), _) | (None, _) => Err(fathom_rt::ReadError::InvalidDataDescription),
            }
        }
        Value::Neutral(Head::Error(_), _)
        | Value::TypeType(_)
        | Value::FunctionType(_, _)
        | Value::Constant(_, _)
        | Value::FormatType(_)
        | Value::Error(_) => Err(fathom_rt::ReadError::InvalidDataDescription),
    }
}
