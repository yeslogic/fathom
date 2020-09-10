use fathom_runtime::ReadFormat;
use num_traits::ToPrimitive;
use std::collections::HashMap;

use crate::lang::core::binary::Term;
use crate::lang::core::semantics::{self, Elim, Head, Value};
use crate::lang::core::{Constant, Globals, Item, Module, StructType};

/// Contextual information to be used when parsing items.
pub struct Context<'me> {
    globals: &'me Globals,
    items: HashMap<&'me str, Item>,
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

    /// Read a module item in the context.
    pub fn read_item(
        &mut self,
        module: &'me Module,
        name: &str,
    ) -> Result<Term, fathom_runtime::ReadError> {
        for item in &module.items {
            match item {
                Item::Alias(alias) if alias.name == name => {
                    let value = semantics::eval(self.globals, &self.items, &alias.term);
                    return self.read_format(&value);
                }
                Item::Struct(struct_ty) if struct_ty.name == name => {
                    return self.read_struct_format(struct_ty);
                }
                Item::Alias(alias) => {
                    self.items.insert(&alias.name, item.clone());
                }
                Item::Struct(struct_ty) => {
                    self.items.insert(&struct_ty.name, item.clone());
                }
            }
        }

        Err(fathom_runtime::ReadError::InvalidDataDescription)
    }

    fn read<T: ReadFormat<'me>>(&mut self) -> Result<T::Host, fathom_runtime::ReadError> {
        self.reader.read::<T>()
    }

    fn read_struct_format(
        &mut self,
        struct_ty: &StructType,
    ) -> Result<Term, fathom_runtime::ReadError> {
        let fields = struct_ty
            .fields
            .iter()
            .map(|field| {
                let value = semantics::eval(self.globals, &self.items, &field.term);
                Ok((field.name.clone(), self.read_format(&value)?))
            })
            .collect::<Result<_, fathom_runtime::ReadError>>()?;

        Ok(Term::Struct(fields))
    }

    fn read_format(&mut self, format: &Value) -> Result<Term, fathom_runtime::ReadError> {
        match format {
            Value::Neutral(Head::Global(_, name), elims) => match (name.as_str(), elims.as_slice())
            {
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
                ("FormatArray", [Elim::Function(_, len), Elim::Function(_, elem_ty)]) => {
                    match len.as_ref() {
                        Value::Constant(_, Constant::Int(len)) => match len.to_usize() {
                            Some(len) => Ok(Term::Seq(
                                (0..len)
                                    .map(|_| self.read_format(elem_ty))
                                    .collect::<Result<_, _>>()?,
                            )),
                            None => Err(fathom_runtime::ReadError::InvalidDataDescription),
                        },
                        _ => Err(fathom_runtime::ReadError::InvalidDataDescription),
                    }
                }
                ("List", [Elim::Function(_, _)]) | (_, _) => {
                    Err(fathom_runtime::ReadError::InvalidDataDescription)
                }
            },
            Value::Neutral(Head::Item(_, name), elims) => {
                match (self.items.get(name.as_str()).cloned(), elims.as_slice()) {
                    (Some(Item::Struct(struct_ty)), []) => self.read_struct_format(&struct_ty),
                    (Some(_), _) | (None, _) => {
                        Err(fathom_runtime::ReadError::InvalidDataDescription)
                    }
                }
            }
            Value::Neutral(Head::Error(_), _)
            | Value::TypeType(_)
            | Value::FunctionType(_, _)
            | Value::Constant(_, _)
            | Value::FormatType(_)
            | Value::Error(_) => Err(fathom_runtime::ReadError::InvalidDataDescription),
        }
    }
}
