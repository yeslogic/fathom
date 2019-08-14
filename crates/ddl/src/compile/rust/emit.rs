use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use num_bigint::BigInt;
use std::io;
use std::io::prelude::*;

use crate::compile::rust::Type;
use crate::core;

pub fn emit_module(
    writer: &mut impl Write,
    module: &core::Module,
    report: &mut dyn FnMut(Diagnostic),
) -> io::Result<()> {
    let context = ModuleContext {
        _file_id: module.file_id,
    };

    let pkg_name = env!("CARGO_PKG_NAME");
    let pkg_version = env!("CARGO_PKG_VERSION");

    writeln!(
        writer,
        "// This file is automatically @generated by {} {}",
        pkg_name, pkg_version,
    )?;
    writeln!(writer, "// It is not intended for manual editing.")?;

    for item in &module.items {
        writeln!(writer)?;
        match item {
            core::Item::Alias(alias) => emit_alias(&context, writer, alias, report)?,
            core::Item::Struct(struct_ty) => emit_struct_ty(&context, writer, struct_ty, report)?,
        }
    }

    Ok(())
}

struct ModuleContext {
    _file_id: FileId,
}

fn emit_alias(
    context: &ModuleContext,
    writer: &mut impl Write,
    alias: &core::Alias,
    report: &mut dyn FnMut(Diagnostic),
) -> io::Result<()> {
    for doc_line in alias.doc.iter() {
        writeln!(writer, "///{}", doc_line)?;
    }

    let ty = compile_ty(context, &alias.term, report);
    writeln!(writer, "pub type {} = {};", alias.name, ty.0)?;

    Ok(())
}

fn emit_struct_ty(
    context: &ModuleContext,
    writer: &mut impl Write,
    struct_ty: &core::StructType,
    report: &mut dyn FnMut(Diagnostic),
) -> io::Result<()> {
    // Struct definition

    for doc_line in struct_ty.doc.iter() {
        writeln!(writer, "///{}", doc_line)?;
    }

    if struct_ty.fields.is_empty() {
        writeln!(writer, "pub struct {} {{}}", struct_ty.name)?;
    } else {
        writeln!(writer, "pub struct {} {{", struct_ty.name)?;
        for field in &struct_ty.fields {
            for doc_line in field.doc.iter() {
                writeln!(writer, "    ///{}", doc_line)?;
            }

            let ty = compile_host_ty(context, &field.term, report);
            write!(writer, "    pub {}: {},", field.name, ty.0)?;
            writeln!(writer)?;
        }
        writeln!(writer, "}}")?;
    }
    writeln!(writer)?;

    // Binary impl

    writeln!(writer, "impl ddl_rt::Binary for {} {{", struct_ty.name,)?;
    writeln!(writer, "    type Host = {};", struct_ty.name)?;
    writeln!(writer, "}}")?;
    writeln!(writer)?;

    // ReadBinary impl

    writeln!(
        writer,
        "impl<'data> ddl_rt::ReadBinary<'data> for {} {{",
        struct_ty.name,
    )?;
    if struct_ty.fields.is_empty() {
        writeln!(
            writer,
            "    fn read(_: &mut ddl_rt::ReadCtxt<'data>) -> Result<{}, ddl_rt::ReadError> {{",
            struct_ty.name,
        )?;
        writeln!(writer, "        Ok({} {{}})", struct_ty.name)?;
        writeln!(writer, "    }}")?;
    } else {
        writeln!(
            writer,
            "    fn read(ctxt: &mut ddl_rt::ReadCtxt<'data>) -> Result<{}, ddl_rt::ReadError> {{",
            struct_ty.name,
        )?;
        for field in &struct_ty.fields {
            write!(
                writer,
                "        let {} = ctxt.read::<{}>()?;",
                field.name,
                compile_ty(context, &field.term, report).0,
            )?;
            writeln!(writer)?;
        }
        writeln!(writer)?;
        writeln!(writer, "        Ok({} {{", struct_ty.name)?;
        for field in &struct_ty.fields {
            writeln!(writer, "            {},", field.name)?;
        }
        writeln!(writer, "        }})")?;
        writeln!(writer, "    }}")?;
    }
    writeln!(writer, "}}")?;

    Ok(())
}

fn compile_ty<'term>(
    context: &ModuleContext,
    term: &'term core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> Type {
    match term {
        core::Term::Item(_, label) => Type(label.0.clone().into()), // TODO: check if in scope, warn if not
        core::Term::Ann(term, _) => compile_ty(context, term, report),
        core::Term::U8(_) => Type("ddl_rt::U8".into()),
        core::Term::U16Le(_) => Type("ddl_rt::U16Le".into()),
        core::Term::U16Be(_) => Type("ddl_rt::U16Be".into()),
        core::Term::U32Le(_) => Type("ddl_rt::U32Le".into()),
        core::Term::U32Be(_) => Type("ddl_rt::U32Be".into()),
        core::Term::U64Le(_) => Type("ddl_rt::U64Le".into()),
        core::Term::U64Be(_) => Type("ddl_rt::U64Be".into()),
        core::Term::S8(_) => Type("ddl_rt::I8".into()),
        core::Term::S16Le(_) => Type("ddl_rt::I16Le".into()),
        core::Term::S16Be(_) => Type("ddl_rt::I16Be".into()),
        core::Term::S32Le(_) => Type("ddl_rt::I32Le".into()),
        core::Term::S32Be(_) => Type("ddl_rt::I32Be".into()),
        core::Term::S64Le(_) => Type("ddl_rt::I64Le".into()),
        core::Term::S64Be(_) => Type("ddl_rt::I64Be".into()),
        core::Term::F32Le(_) => Type("ddl_rt::F32Le".into()),
        core::Term::F32Be(_) => Type("ddl_rt::F32Be".into()),
        core::Term::F64Le(_) => Type("ddl_rt::F64Le".into()),
        core::Term::F64Be(_) => Type("ddl_rt::F64Be".into()),
        core::Term::Kind(_) | core::Term::Type(_) => Type("ddl_rt::InvalidDataDescription".into()), // TODO: skip
        core::Term::Error(_) => Type("ddl_rt::InvalidDataDescription".into()),
    }
}

fn compile_host_ty<'term>(
    context: &ModuleContext,
    term: &'term core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> Type {
    match term {
        core::Term::Item(_, label) => Type(label.0.clone().into()), // TODO: check if in scope, warn if not
        core::Term::Ann(term, _) => compile_host_ty(context, term, report),
        core::Term::U8(_) => Type("u8".into()),
        core::Term::U16Le(_) => Type("u16".into()),
        core::Term::U16Be(_) => Type("u16".into()),
        core::Term::U32Le(_) => Type("u32".into()),
        core::Term::U32Be(_) => Type("u32".into()),
        core::Term::U64Le(_) => Type("u64".into()),
        core::Term::U64Be(_) => Type("u64".into()),
        core::Term::S8(_) => Type("i8".into()),
        core::Term::S16Le(_) => Type("i16".into()),
        core::Term::S16Be(_) => Type("i16".into()),
        core::Term::S32Le(_) => Type("i32".into()),
        core::Term::S32Be(_) => Type("i32".into()),
        core::Term::S64Le(_) => Type("i64".into()),
        core::Term::S64Be(_) => Type("i64".into()),
        core::Term::F32Le(_) => Type("f32".into()),
        core::Term::F32Be(_) => Type("f32".into()),
        core::Term::F64Le(_) => Type("f64".into()),
        core::Term::F64Be(_) => Type("f64".into()),
        core::Term::Kind(_) | core::Term::Type(_) => Type("ddl_rt::InvalidDataDescription".into()), // TODO: skip
        core::Term::Error(_) => Type("ddl_rt::InvalidDataDescription".into()),
    }
}

#[allow(dead_code)]
fn host_int(min: &BigInt, max: &BigInt) -> Option<Type> {
    use std::{i16, i32, i64, i8, u16, u32, u64, u8};

    match () {
        () if *min >= u8::MIN.into() && *max <= u8::MAX.into() => Some(Type("u8".into())),
        () if *min >= u16::MIN.into() && *max <= u16::MAX.into() => Some(Type("u16".into())),
        () if *min >= u32::MIN.into() && *max <= u32::MAX.into() => Some(Type("u32".into())),
        () if *min >= u64::MIN.into() && *max <= u64::MAX.into() => Some(Type("u64".into())),
        () if *min >= i8::MIN.into() && *max <= i8::MAX.into() => Some(Type("i8".into())),
        () if *min >= i16::MIN.into() && *max <= i16::MAX.into() => Some(Type("i16".into())),
        () if *min >= i32::MIN.into() && *max <= i32::MAX.into() => Some(Type("i32".into())),
        () if *min >= i64::MIN.into() && *max <= i64::MAX.into() => Some(Type("i64".into())),
        () if min > max => None, // Impossible range
        _ => None,               // TODO: use bigint if outside bounds
    }
}
