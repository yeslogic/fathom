use ddl_rt::ReadFormat;
use num_bigint::BigInt;
use std::collections::HashMap;

use crate::binary::Term;
use crate::core;

/// Contextual information to be used when parsing items.
pub struct Context<'me> {
    globals: &'me core::Globals,
    items: HashMap<&'me str, core::Item>,
    reader: ddl_rt::FormatReader<'me>,
}

impl<'me> Context<'me> {
    /// Create a new item context.
    pub fn new(globals: &'me core::Globals, reader: ddl_rt::FormatReader<'me>) -> Context<'me> {
        Context {
            globals,
            items: HashMap::new(),
            reader,
        }
    }

    /// Read some binary data in the context.
    fn read<T: ReadFormat<'me>>(&mut self) -> Result<T::Host, ddl_rt::ReadError> {
        self.reader.read::<T>()
    }
}

pub fn read_module_item<'module>(
    context: &mut Context<'module>,
    module: &'module core::Module,
    name: &str,
) -> Result<Term, ddl_rt::ReadError> {
    for item in &module.items {
        match item {
            core::Item::Alias(alias) if alias.name == name => {
                return read_ty(context, &alias.term);
            }
            core::Item::Struct(struct_ty) if struct_ty.name == name => {
                return read_struct_ty(context, struct_ty);
            }
            core::Item::Alias(alias) => {
                context.items.insert(&alias.name, item.clone());
            }
            core::Item::Struct(struct_ty) => {
                context.items.insert(&struct_ty.name, item.clone());
            }
        }
    }

    Err(ddl_rt::ReadError::InvalidDataDescription)
}

pub fn read_struct_ty(
    context: &mut Context<'_>,
    struct_ty: &core::StructType,
) -> Result<Term, ddl_rt::ReadError> {
    let fields = struct_ty
        .fields
        .iter()
        .map(|field| Ok((field.name.clone(), read_ty(context, &field.term)?)))
        .collect::<Result<_, ddl_rt::ReadError>>()?;

    Ok(Term::Struct(fields))
}

pub fn read_ty(context: &mut Context<'_>, term: &core::Term) -> Result<Term, ddl_rt::ReadError> {
    match core::semantics::eval(context.globals, &context.items, term).as_ref() {
        core::Value::Neutral(core::Head::Global(name), elims) => {
            match (name.as_str(), elims.as_slice()) {
                ("U8", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::U8>()?))),
                ("U16Le", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::U16Le>()?))),
                ("U16Be", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::U16Be>()?))),
                ("U32Le", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::U32Le>()?))),
                ("U32Be", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::U32Be>()?))),
                ("U64Le", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::U64Le>()?))),
                ("U64Be", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::U64Be>()?))),
                ("S8", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::I8>()?))),
                ("S16Le", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::I16Le>()?))),
                ("S16Be", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::I16Be>()?))),
                ("S32Le", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::I32Le>()?))),
                ("S32Be", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::I32Be>()?))),
                ("S64Le", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::I64Le>()?))),
                ("S64Be", []) => Ok(Term::Int(BigInt::from(context.read::<ddl_rt::I64Be>()?))),
                ("F32Le", []) => Ok(Term::F32(context.read::<ddl_rt::F32Le>()?)),
                ("F32Be", []) => Ok(Term::F32(context.read::<ddl_rt::F32Be>()?)),
                ("F64Le", []) => Ok(Term::F64(context.read::<ddl_rt::F64Le>()?)),
                ("F64Be", []) => Ok(Term::F64(context.read::<ddl_rt::F64Be>()?)),
                (_, _) => Err(ddl_rt::ReadError::InvalidDataDescription),
            }
        }
        core::Value::Neutral(core::Head::Item(name), elims) => {
            match (context.items.get(name.as_str()).cloned(), elims.as_slice()) {
                (Some(core::Item::Struct(struct_ty)), []) => read_struct_ty(context, &struct_ty),
                (Some(_), _) | (None, _) => Err(ddl_rt::ReadError::InvalidDataDescription),
            }
        }
        core::Value::Neutral(core::Head::Error, _)
        | core::Value::Universe(_)
        | core::Value::Constant(_)
        | core::Value::Error => Err(ddl_rt::ReadError::InvalidDataDescription),
    }
}
