use fathom_runtime::ReadFormat;
use num_traits::ToPrimitive;
use std::collections::HashMap;
use std::sync::Arc;

use crate::lang::core;
use crate::lang::core::binary::Term;
use crate::lang::core::semantics::{self, Elim, Head, Value};
use crate::lang::core::{Constant, Globals, Item, ItemData, Module, StructFormat};

/// Contextual information to be used when parsing items.
pub struct Context<'me> {
    globals: &'me Globals,
    items: HashMap<String, Item>,
    reader: fathom_runtime::FormatReader<'me>,
}

impl<'me> Context<'me> {
    /// Create a new context.
    pub fn new(globals: &'me Globals, reader: fathom_runtime::FormatReader<'me>) -> Context<'me> {
        Context {
            globals,
            items: HashMap::new(),
            reader,
        }
    }

    /// Evaluate a term in the parser context.
    fn eval(&self, term: &core::Term) -> Arc<Value> {
        semantics::eval(self.globals, &self.items, term)
    }

    /// Read a module item in the context.
    pub fn read_item(
        &mut self,
        module: &'me Module,
        name: &str,
    ) -> Result<Term, fathom_runtime::ReadError> {
        for item in &module.items {
            let name = match &item.data {
                ItemData::Alias(alias) if alias.name == name => {
                    let value = self.eval(&alias.term);
                    return self.read_format(&value);
                }
                ItemData::StructFormat(struct_format) if struct_format.name == name => {
                    return self.read_struct_format(struct_format);
                }
                ItemData::Alias(alias) => alias.name.clone(),
                ItemData::StructType(struct_type) => struct_type.name.clone(),
                ItemData::StructFormat(struct_format) => struct_format.name.clone(),
            };
            self.items.insert(name, item.clone());
        }

        Err(fathom_runtime::ReadError::InvalidDataDescription)
    }

    fn read<T: ReadFormat<'me>>(&mut self) -> Result<T::Host, fathom_runtime::ReadError> {
        self.reader.read::<T>()
    }

    fn read_struct_format(
        &mut self,
        struct_type: &StructFormat,
    ) -> Result<Term, fathom_runtime::ReadError> {
        let fields = struct_type
            .fields
            .iter()
            .map(|field| {
                let value = self.eval(&field.term);
                Ok((field.name.clone(), self.read_format(&value)?))
            })
            .collect::<Result<_, fathom_runtime::ReadError>>()?;

        Ok(Term::Struct(fields))
    }

    fn read_format(&mut self, format: &Value) -> Result<Term, fathom_runtime::ReadError> {
        match format {
            Value::Stuck(Head::Global(name), elims) => match (name.as_str(), elims.as_slice()) {
                ("U8", []) => Ok(Term::int(self.read::<fathom_runtime::U8>()?)),
                ("U16Le", []) => Ok(Term::int(self.read::<fathom_runtime::U16Le>()?)),
                ("U16Be", []) => Ok(Term::int(self.read::<fathom_runtime::U16Be>()?)),
                ("U32Le", []) => Ok(Term::int(self.read::<fathom_runtime::U32Le>()?)),
                ("U32Be", []) => Ok(Term::int(self.read::<fathom_runtime::U32Be>()?)),
                ("U64Le", []) => Ok(Term::int(self.read::<fathom_runtime::U64Le>()?)),
                ("U64Be", []) => Ok(Term::int(self.read::<fathom_runtime::U64Be>()?)),
                ("S8", []) => Ok(Term::int(self.read::<fathom_runtime::I8>()?)),
                ("S16Le", []) => Ok(Term::int(self.read::<fathom_runtime::I16Le>()?)),
                ("S16Be", []) => Ok(Term::int(self.read::<fathom_runtime::I16Be>()?)),
                ("S32Le", []) => Ok(Term::int(self.read::<fathom_runtime::I32Le>()?)),
                ("S32Be", []) => Ok(Term::int(self.read::<fathom_runtime::I32Be>()?)),
                ("S64Le", []) => Ok(Term::int(self.read::<fathom_runtime::I64Le>()?)),
                ("S64Be", []) => Ok(Term::int(self.read::<fathom_runtime::I64Be>()?)),
                ("F32Le", []) => Ok(Term::F32(self.read::<fathom_runtime::F32Le>()?)),
                ("F32Be", []) => Ok(Term::F32(self.read::<fathom_runtime::F32Be>()?)),
                ("F64Le", []) => Ok(Term::F64(self.read::<fathom_runtime::F64Le>()?)),
                ("F64Be", []) => Ok(Term::F64(self.read::<fathom_runtime::F64Be>()?)),
                ("FormatArray", [Elim::Function(len), Elim::Function(elem_type)]) => {
                    match len.as_ref() {
                        Value::Constant(Constant::Int(len)) => match len.to_usize() {
                            Some(len) => Ok(Term::Seq(
                                (0..len)
                                    .map(|_| self.read_format(elem_type))
                                    .collect::<Result<_, _>>()?,
                            )),
                            None => Err(fathom_runtime::ReadError::InvalidDataDescription),
                        },
                        _ => Err(fathom_runtime::ReadError::InvalidDataDescription),
                    }
                }
                (_, _) => Err(fathom_runtime::ReadError::InvalidDataDescription),
            },
            Value::Stuck(Head::Item(name), elims) => {
                match (self.items.get(name.as_str()).cloned(), elims.as_slice()) {
                    (Some(item), []) => match item.data {
                        ItemData::StructFormat(struct_format) => {
                            self.read_struct_format(&struct_format)
                        }
                        _ => Err(fathom_runtime::ReadError::InvalidDataDescription),
                    },
                    (Some(_), _) | (None, _) => {
                        Err(fathom_runtime::ReadError::InvalidDataDescription)
                    }
                }
            }
            Value::Stuck(Head::Error, _)
            | Value::Sort(_)
            | Value::FunctionType(_, _)
            | Value::Constant(_)
            | Value::FormatType
            | Value::Repr
            | Value::Error => Err(fathom_runtime::ReadError::InvalidDataDescription),
        }
    }
}
