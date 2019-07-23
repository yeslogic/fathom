use num_bigint::BigInt;

use crate::binary::Term;
use crate::core;

pub fn read_module_item(
    module: &core::Module,
    name: &str,
    ctxt: &mut ddl_rt::ReadCtxt<'_>,
) -> Result<Term, ddl_rt::ReadError> {
    for item in &module.items {
        match item {
            core::Item::Alias(alias) if alias.name.0 == name => {
                return read_ty(&alias.term, ctxt);
            }
            core::Item::Struct(struct_ty) if struct_ty.name.0 == name => {
                return read_struct_ty(struct_ty, ctxt);
            }
            core::Item::Alias(_) | core::Item::Struct(_) => {}
        }
    }

    Err(ddl_rt::ReadError::InvalidDataDescription)
}

pub fn read_struct_ty(
    struct_ty: &core::StructType,
    ctxt: &mut ddl_rt::ReadCtxt<'_>,
) -> Result<Term, ddl_rt::ReadError> {
    let fields = struct_ty
        .fields
        .iter()
        .map(|field| Ok((field.name.0.clone(), read_ty(&field.term, ctxt)?)))
        .collect::<Result<_, ddl_rt::ReadError>>()?;

    Ok(Term::Struct(fields))
}

pub fn read_ty(
    term: &core::Term,
    ctxt: &mut ddl_rt::ReadCtxt<'_>,
) -> Result<Term, ddl_rt::ReadError> {
    match term {
        core::Term::U8(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::U8>()?))),
        core::Term::U16Le(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::U16Le>()?))),
        core::Term::U16Be(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::U16Be>()?))),
        core::Term::U32Le(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::U32Le>()?))),
        core::Term::U32Be(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::U32Be>()?))),
        core::Term::U64Le(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::U64Le>()?))),
        core::Term::U64Be(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::U64Be>()?))),
        core::Term::S8(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::I8>()?))),
        core::Term::S16Le(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::I16Le>()?))),
        core::Term::S16Be(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::I16Be>()?))),
        core::Term::S32Le(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::I32Le>()?))),
        core::Term::S32Be(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::I32Be>()?))),
        core::Term::S64Le(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::I64Le>()?))),
        core::Term::S64Be(_) => Ok(Term::Int(BigInt::from(ctxt.read::<ddl_rt::I64Be>()?))),
        core::Term::F32Le(_) => Ok(Term::F32(ctxt.read::<ddl_rt::F32Le>()?)),
        core::Term::F32Be(_) => Ok(Term::F32(ctxt.read::<ddl_rt::F32Be>()?)),
        core::Term::F64Le(_) => Ok(Term::F64(ctxt.read::<ddl_rt::F64Le>()?)),
        core::Term::F64Be(_) => Ok(Term::F64(ctxt.read::<ddl_rt::F64Be>()?)),
        core::Term::Error(_) => Err(ddl_rt::ReadError::InvalidDataDescription),
    }
}
