use contracts::debug_ensures;
use fathom_runtime::{FormatReader, ReadError};
use num_traits::ToPrimitive;
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::sync::Arc;

use crate::lang::core;
use crate::lang::core::semantics::{self, Elim, Head, Value};
use crate::lang::core::{FieldDeclaration, Globals, ItemData, Module, Primitive};

/// Contextual information to be used when parsing items.
pub struct Context<'me> {
    globals: &'me Globals,
    items: HashMap<String, semantics::Item>,
    locals: core::Locals<Arc<Value>>,
    pending_links: VecDeque<(usize, Arc<Value>)>,
}

impl<'me> Context<'me> {
    /// Create a new context.
    pub fn new(globals: &'me Globals, module: &Module) -> Context<'me> {
        let mut context = Context {
            globals,
            items: HashMap::new(),
            locals: core::Locals::new(),
            pending_links: VecDeque::new(),
        };

        for item in &module.items {
            let (name, item_data) = match &item.data {
                ItemData::Constant(constant) => (
                    constant.name.clone(),
                    semantics::ItemData::Constant(context.eval(&constant.term)),
                ),
                ItemData::StructType(struct_type) => (
                    struct_type.name.clone(),
                    semantics::ItemData::StructType(
                        struct_type.params.len(),
                        struct_type.fields.clone(),
                    ),
                ),
                ItemData::StructFormat(struct_format) => (
                    struct_format.name.clone(),
                    semantics::ItemData::StructFormat(
                        struct_format.params.len(),
                        struct_format.fields.clone(),
                    ),
                ),
            };

            let item = semantics::Item::new(item.location, item_data);
            context.items.insert(name, item);
        }

        context
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
    #[debug_ensures(self.locals.is_empty())]
    #[debug_ensures(self.pending_links.is_empty())]
    pub fn read_item(
        &mut self,
        reader: &mut FormatReader<'_>,
        name: &str,
    ) -> Result<(Value, HashMap<usize, Arc<Value>>), ReadError> {
        let root_scope = reader.scope();
        let parsed_value = match self.items.get(name).cloned().map(|item| item.data) {
            Some(semantics::ItemData::Constant(value)) => self.read_format(reader, &value),
            Some(semantics::ItemData::StructFormat(0, field_declarations)) => {
                self.read_struct_format(reader, &field_declarations, &[])
            }
            Some(semantics::ItemData::StructFormat(_, _))
            | Some(semantics::ItemData::StructType(_, _))
            | None => Err(ReadError::InvalidDataDescription), // TODO: Improve error!
        };

        let result = match parsed_value {
            Err(error) => Err(error),
            Ok(parsed_value) => {
                let mut parsed_links = HashMap::new();

                // Follow pending offsets until exhausted (ᴗ˳ᴗ) ..zzZ
                while let Some((offset, format)) = self.pending_links.pop_front() {
                    use std::collections::hash_map::Entry;

                    match parsed_links.entry(offset) {
                        // The offset has not yet been parsed...
                        Entry::Vacant(parsed_entry) => {
                            let mut inner_reader = root_scope.offset(offset).reader();
                            let value = match self.read_format(&mut inner_reader, &format) {
                                Ok(value) => value,
                                Err(error) => {
                                    self.pending_links.clear();
                                    return Err(error);
                                }
                            };
                            parsed_entry.insert(Arc::new(value));
                        }
                        // The offset has already been parsed!
                        Entry::Occupied(_) => {
                            self.pending_links.clear();
                            return Err(ReadError::DuplicatePosition { offset });
                        }
                    }
                }

                Ok((parsed_value, parsed_links))
            }
        };

        self.pending_links.clear();

        result
    }

    #[debug_ensures(self.items.len() == old(self.items.len()))]
    #[debug_ensures(self.locals.size() == old(self.locals.size()))]
    fn read_struct_format(
        &mut self,
        reader: &mut FormatReader<'_>,
        field_declarations: &[FieldDeclaration],
        elims: &[Elim],
    ) -> Result<Value, ReadError> {
        let mut fields = BTreeMap::new();
        // Local environment for evaluating the field formats with the
        // values that have been parsed from the binary data.
        let mut format_locals = core::Locals::new();

        for elim in elims {
            match elim {
                Elim::Function(value) => format_locals.push(value.clone()),
                _ => panic!("invalid elimination"),
            }
        }

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
                ("CurrentPos", []) => match reader.current_pos() {
                    Some(offset) => Ok(Value::Primitive(Primitive::Pos(offset))),
                    None => Err(ReadError::OverflowingPosition),
                },
                (
                    "Link",
                    [Elim::Function(base), Elim::Function(offset), Elim::Function(format)],
                ) => {
                    let (base, offset) = match (base.as_ref(), offset.as_ref()) {
                        (
                            Value::Primitive(Primitive::Pos(base)),
                            Value::Primitive(Primitive::Int(offset)),
                        ) => (base, offset),
                        (_, _) => return Err(ReadError::InvalidDataDescription),
                    };

                    let position = (offset + base)
                        .to_usize()
                        .ok_or(ReadError::OverflowingPosition)?;

                    self.pending_links.push_back((position, format.clone()));

                    Ok(Value::Primitive(Primitive::Pos(position)))
                }
                (_, _) => Err(ReadError::InvalidDataDescription),
            },
            Value::Stuck(Head::Item(item_name), elims) => {
                match (self.items.get(item_name).cloned(), elims.as_slice()) {
                    (Some(item), elims) => match item.data {
                        semantics::ItemData::StructFormat(arity, field_declarations) => {
                            self.read_struct_format(reader, &field_declarations, &elims[..arity])
                        }
                        // NOTE: We expect that all constants should be reduced
                        // during evaluation, but this assumption could be
                        // invalidated if we ever introduce 'opaque' constants.
                        semantics::ItemData::Constant(_)
                        | semantics::ItemData::StructType(_, _) => {
                            Err(ReadError::InvalidDataDescription)
                        }
                    },
                    (None, _) => Err(ReadError::InvalidDataDescription),
                }
            }
            Value::Stuck(Head::Local(local_level), elims) => {
                let local_index = self.locals.size().level_to_index(*local_level).unwrap();
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
