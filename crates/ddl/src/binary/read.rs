use num_bigint::BigInt;
use std::collections::HashMap;

use crate::binary::Term;
use crate::core;

/// Contextual information to be used when parsing items.
pub struct ItemContext<'module> {
    items: HashMap<core::Label, &'module core::Item>,
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
            core::Item::Alias(alias) if alias.name.0 == name => {
                return read_ty(&context, &alias.term, reader);
            }
            core::Item::Struct(struct_ty) if struct_ty.name.0 == name => {
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
        .map(|field| Ok((field.name.0.clone(), read_ty(context, &field.term, reader)?)))
        .collect::<Result<_, ddl_rt::ReadError>>()?;

    Ok(Term::Struct(fields))
}

pub fn read_ty(
    context: &ItemContext<'_>,
    term: &core::Term,
    reader: &mut ddl_rt::FormatReader<'_>,
) -> Result<Term, ddl_rt::ReadError> {
    match term {
        core::Term::Item(_, label) => match context.items.get(label) {
            Some(core::Item::Alias(alias)) => read_ty(&context, &alias.term, reader),
            Some(core::Item::Struct(struct_ty)) => read_struct_ty(&context, struct_ty, reader),
            None => Err(ddl_rt::ReadError::InvalidDataDescription),
        },
        core::Term::Ann(term, _) => read_ty(context, term, reader),
        core::Term::U8Type(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U8>()?))),
        core::Term::U16LeType(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U16Le>()?))),
        core::Term::U16BeType(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U16Be>()?))),
        core::Term::U32LeType(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U32Le>()?))),
        core::Term::U32BeType(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U32Be>()?))),
        core::Term::U64LeType(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U64Le>()?))),
        core::Term::U64BeType(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::U64Be>()?))),
        core::Term::S8Type(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I8>()?))),
        core::Term::S16LeType(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I16Le>()?))),
        core::Term::S16BeType(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I16Be>()?))),
        core::Term::S32LeType(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I32Le>()?))),
        core::Term::S32BeType(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I32Be>()?))),
        core::Term::S64LeType(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I64Le>()?))),
        core::Term::S64BeType(_) => Ok(Term::Int(BigInt::from(reader.read::<ddl_rt::I64Be>()?))),
        core::Term::F32LeType(_) => Ok(Term::F32(reader.read::<ddl_rt::F32Le>()?)),
        core::Term::F32BeType(_) => Ok(Term::F32(reader.read::<ddl_rt::F32Be>()?)),
        core::Term::F64LeType(_) => Ok(Term::F64(reader.read::<ddl_rt::F64Le>()?)),
        core::Term::F64BeType(_) => Ok(Term::F64(reader.read::<ddl_rt::F64Be>()?)),
        core::Term::BoolElim(_, term, if_true, if_false) => match core::semantics::eval(term) {
            core::Value::BoolConst(true) => read_ty(context, if_true, reader),
            core::Value::BoolConst(false) => read_ty(context, if_false, reader),
            _ => Err(ddl_rt::ReadError::InvalidDataDescription),
        },
        core::Term::Universe(_, _)
        | core::Term::BoolType(_)
        | core::Term::IntType(_)
        | core::Term::F32Type(_)
        | core::Term::F64Type(_)
        | core::Term::BoolConst(_, _)
        | core::Term::IntConst(_, _)
        | core::Term::F32Const(_, _)
        | core::Term::F64Const(_, _)
        | core::Term::Error(_) => Err(ddl_rt::ReadError::InvalidDataDescription),
    }
}
