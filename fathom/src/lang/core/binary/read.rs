use contracts::debug_ensures;
use fathom_runtime::{FormatReader, ReadError};
use num_traits::ToPrimitive;
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use crate::lang::core;
use crate::lang::core::semantics::{self, Elim, Head, Value};
use crate::lang::core::{FieldDeclaration, Globals, ItemData, Module, Primitive};

/// Contextual information to be used when parsing items.
pub struct Context<'me> {
    globals: &'me Globals,
    items: HashMap<String, semantics::Item>,
    locals: core::Locals<Arc<Value>>,
}

impl<'me> Context<'me> {
    /// Create a new context.
    pub fn new(globals: &'me Globals) -> Context<'me> {
        Context {
            globals,
            items: HashMap::new(),
            locals: core::Locals::new(),
        }
    }

    /// Evaluate a term in the parser context.
    fn eval(&mut self, term: &core::Term) -> Arc<Value> {
        semantics::eval(self.globals, &self.items, &mut self.locals, term)
    }

    /// Evaluate a term using the supplied local environment.
    fn eval_with_locals(
        &mut self,
        locals: &mut core::Locals<Arc<Value>>,
        term: &core::Term,
    ) -> Arc<Value> {
        semantics::eval(self.globals, &self.items, locals, term)
    }

    /// Read a module item in the context.
    #[debug_ensures(self.items.is_empty())]
    #[debug_ensures(self.locals.is_empty())]
    pub fn read_item(
        &mut self,
        reader: &mut FormatReader<'_>,
        module: &'me Module,
        name: &str,
    ) -> Result<Value, ReadError> {
        for item in &module.items {
            let (name, item_data) = match &item.data {
                ItemData::Constant(constant) if constant.name == name => {
                    let format = self.eval(&constant.term);
                    let value = self.read_format(reader, &format);
                    self.items.clear();
                    return value;
                }
                ItemData::StructType(struct_type) if struct_type.name == name => {
                    return Err(ReadError::InvalidDataDescription);
                }
                ItemData::StructFormat(struct_format) if struct_format.name == name => {
                    let value = self.read_struct_format(reader, &struct_format.fields);
                    self.items.clear();
                    return value;
                }
                ItemData::Constant(constant) => (
                    constant.name.clone(),
                    semantics::ItemData::Constant(self.eval(&constant.term)),
                ),
                ItemData::StructType(struct_type) => (
                    struct_type.name.clone(),
                    semantics::ItemData::StructType(struct_type.fields.clone()),
                ),
                ItemData::StructFormat(struct_format) => (
                    struct_format.name.clone(),
                    semantics::ItemData::StructFormat(struct_format.fields.clone()),
                ),
            };

            let item = semantics::Item::new(item.range, item_data);
            self.items.insert(name, item);
        }

        self.items.clear();

        Err(ReadError::InvalidDataDescription)
    }

    #[debug_ensures(self.items.len() == old(self.items.len()))]
    #[debug_ensures(self.locals.size() == old(self.locals.size()))]
    fn read_struct_format(
        &mut self,
        reader: &mut FormatReader<'_>,
        field_declarations: &[FieldDeclaration],
    ) -> Result<Value, ReadError> {
        let mut fields = BTreeMap::new();
        // Local environment for evaluating the field formats with the
        // values that have been parsed from the binary data.
        let mut format_locals = core::Locals::new();

        for field_declaration in field_declarations.iter() {
            let label = field_declaration.label.data.clone();
            let format = self.eval_with_locals(&mut format_locals, &field_declaration.type_);
            let value = Arc::new(self.read_format(reader, &format)?);
            format_locals.push(value.clone());
            fields.insert(label, value);
        }

        Ok(Value::StructTerm(fields))
    }

    #[debug_ensures(self.items.len() == old(self.items.len()))]
    #[debug_ensures(self.locals.size() == old(self.locals.size()))]
    fn read_format(
        &mut self,
        reader: &mut FormatReader<'_>,
        format: &Value,
    ) -> Result<Value, ReadError> {
        match format {
            Value::Stuck(Head::Global(name), elims) => match (name.as_str(), elims.as_slice()) {
                ("U8", []) => Ok(Value::int(reader.read::<fathom_runtime::U8>()?)),
                ("U16Le", []) => Ok(Value::int(reader.read::<fathom_runtime::U16Le>()?)),
                ("U16Be", []) => Ok(Value::int(reader.read::<fathom_runtime::U16Be>()?)),
                ("U32Le", []) => Ok(Value::int(reader.read::<fathom_runtime::U32Le>()?)),
                ("U32Be", []) => Ok(Value::int(reader.read::<fathom_runtime::U32Be>()?)),
                ("U64Le", []) => Ok(Value::int(reader.read::<fathom_runtime::U64Le>()?)),
                ("U64Be", []) => Ok(Value::int(reader.read::<fathom_runtime::U64Be>()?)),
                ("S8", []) => Ok(Value::int(reader.read::<fathom_runtime::I8>()?)),
                ("S16Le", []) => Ok(Value::int(reader.read::<fathom_runtime::I16Le>()?)),
                ("S16Be", []) => Ok(Value::int(reader.read::<fathom_runtime::I16Be>()?)),
                ("S32Le", []) => Ok(Value::int(reader.read::<fathom_runtime::I32Le>()?)),
                ("S32Be", []) => Ok(Value::int(reader.read::<fathom_runtime::I32Be>()?)),
                ("S64Le", []) => Ok(Value::int(reader.read::<fathom_runtime::I64Le>()?)),
                ("S64Be", []) => Ok(Value::int(reader.read::<fathom_runtime::I64Be>()?)),
                ("F32Le", []) => Ok(Value::f32(reader.read::<fathom_runtime::F32Le>()?)),
                ("F32Be", []) => Ok(Value::f32(reader.read::<fathom_runtime::F32Be>()?)),
                ("F64Le", []) => Ok(Value::f64(reader.read::<fathom_runtime::F64Le>()?)),
                ("F64Be", []) => Ok(Value::f64(reader.read::<fathom_runtime::F64Be>()?)),
                ("FormatArray", [Elim::Function(len), Elim::Function(elem_type)]) => {
                    match len.as_ref() {
                        Value::Primitive(Primitive::Int(len)) => match len.to_usize() {
                            Some(len) => Ok(Value::ArrayTerm(
                                (0..len)
                                    .map(|_| Ok(Arc::new(self.read_format(reader, elem_type)?)))
                                    .collect::<Result<_, ReadError>>()?,
                            )),
                            None => Err(ReadError::InvalidDataDescription),
                        },
                        _ => Err(ReadError::InvalidDataDescription),
                    }
                }
                (_, _) => Err(ReadError::InvalidDataDescription),
            },
            Value::Stuck(Head::Item(item_name), elims) => {
                match (self.items.get(item_name).cloned(), elims.as_slice()) {
                    (Some(item), []) => match item.data {
                        semantics::ItemData::StructFormat(field_declarations) => {
                            self.read_struct_format(reader, &field_declarations)
                        }
                        // NOTE: We expect that all constants should be reduced
                        // during evaluation, but this assumption could be
                        // invalidated if we ever introduce 'opaque' constants.
                        semantics::ItemData::Constant(_) | semantics::ItemData::StructType(_) => {
                            Err(ReadError::InvalidDataDescription)
                        }
                    },
                    (Some(_), _) | (None, _) => Err(ReadError::InvalidDataDescription),
                }
            }
            Value::Stuck(Head::Local(local_level), elims) => {
                let local_index = local_level.to_index(self.locals.size()).unwrap();
                match (self.locals.get(local_index).cloned(), elims.as_slice()) {
                    (Some(value), []) => self.read_format(reader, &value),
                    (Some(_), _) | (None, _) => Err(ReadError::InvalidDataDescription),
                }
            }
            Value::Stuck(Head::Error, _)
            | Value::Sort(_)
            | Value::FunctionType(_, _)
            | Value::StructTerm(_)
            | Value::ArrayTerm(_)
            | Value::Primitive(_)
            | Value::FormatType
            | Value::Repr
            | Value::Error => Err(ReadError::InvalidDataDescription),
        }
    }
}
