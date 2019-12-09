use num_bigint::BigInt;
use std::collections::HashMap;

use crate::binary::Term;
use crate::core;

/// Contextual information to be used when parsing items.
pub struct ItemContext<'module> {
    items: HashMap<String, &'module core::Item>,
}

impl<'module> ItemContext<'module> {
    /// Create a new item context.
    pub fn new() -> ItemContext<'module> {
        ItemContext {
            items: HashMap::new(),
        }
    }
}

pub fn read_module_item(
    module: &core::Module,
    name: &str,
    reader: &mut ddl_rt::FormatReader<'_>,
) -> Result<Term, ddl_rt::ReadError> {
    let mut context = ItemContext::new();

    for item in &module.items {
        match item {
            core::Item::Alias(alias) if alias.name == name => {
                return read_ty(&context, &alias.term, reader);
            }
            core::Item::Struct(struct_ty) if struct_ty.name == name => {
                return read_struct_ty(&context, struct_ty, reader);
            }
            core::Item::Alias(alias) => {
                context.items.insert(alias.name.clone(), item);
            }
            core::Item::Struct(struct_ty) => {
                context.items.insert(struct_ty.name.clone(), item);
            }
        }
    }

    Err(ddl_rt::ReadError::InvalidDataDescription)
}

pub fn read_struct_ty(
    context: &ItemContext<'_>,
    struct_ty: &core::StructType,
    reader: &mut ddl_rt::FormatReader<'_>,
) -> Result<Term, ddl_rt::ReadError> {
    let fields = struct_ty
        .fields
        .iter()
        .map(|field| Ok((field.name.clone(), read_ty(context, &field.term, reader)?)))
        .collect::<Result<_, ddl_rt::ReadError>>()?;

    Ok(Term::Struct(fields))
}

pub fn read_ty(
    context: &ItemContext<'_>,
    term: &core::Term,
    reader: &mut ddl_rt::FormatReader<'_>,
) -> Result<Term, ddl_rt::ReadError> {
    match term {
        core::Term::Item(_, name) => match name.as_str() {
            "U8" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U8>()?))),
            "U16Le" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U16Le>()?))),
            "U16Be" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U16Be>()?))),
            "U32Le" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U32Le>()?))),
            "U32Be" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U32Be>()?))),
            "U64Le" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U64Le>()?))),
            "U64Be" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U64Be>()?))),
            "S8" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I8>()?))),
            "S16Le" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I16Le>()?))),
            "S16Be" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I16Be>()?))),
            "S32Le" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I32Le>()?))),
            "S32Be" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I32Be>()?))),
            "S64Le" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I64Le>()?))),
            "S64Be" => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I64Be>()?))),
            "F32Le" => Ok(Term::F32(reader.read::<ddl_rt::F32Le>()?)),
            "F32Be" => Ok(Term::F32(reader.read::<ddl_rt::F32Be>()?)),
            "F64Le" => Ok(Term::F64(reader.read::<ddl_rt::F64Le>()?)),
            "F64Be" => Ok(Term::F64(reader.read::<ddl_rt::F64Be>()?)),
            _ => match context.items.get(name) {
                Some(core::Item::Alias(alias)) => read_ty(&context, &alias.term, reader),
                Some(core::Item::Struct(struct_ty)) => read_struct_ty(&context, struct_ty, reader),
                None => Err(ddl_rt::ReadError::InvalidDataDescription),
            },
        },
        core::Term::Ann(term, _) => read_ty(context, term, reader),
        core::Term::BoolElim(_, head, if_true, if_false) => match &core::semantics::eval(head) {
            core::Value::Neutral(core::Head::Item(name), elims)
                if name == "true" && elims.is_empty() =>
            {
                read_ty(context, if_true, reader)
            }
            core::Value::Neutral(core::Head::Item(name), elims)
                if name == "false" && elims.is_empty() =>
            {
                read_ty(context, if_false, reader)
            }
            _ => Err(ddl_rt::ReadError::InvalidDataDescription),
        },
        core::Term::IntElim(_, head, branches, default) => match core::semantics::eval(head) {
            core::Value::Constant(core::Constant::Int(value)) => match branches.get(&value) {
                Some(term) => read_ty(context, term, reader),
                None => read_ty(context, default, reader),
            },
            _ => Err(ddl_rt::ReadError::InvalidDataDescription),
        },
        core::Term::Universe(_, _) | core::Term::Constant(_, _) | core::Term::Error(_) => {
            Err(ddl_rt::ReadError::InvalidDataDescription)
        }
    }
}
