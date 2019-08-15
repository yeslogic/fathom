use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use num_bigint::BigInt;
use std::collections::HashMap;

use crate::compile::rust::{Item, Module, StructType, Type, TypeAlias, TypeField};
use crate::core;

pub fn compile_module(module: &core::Module, report: &mut dyn FnMut(Diagnostic)) -> Module {
    let mut context = ModuleContext {
        _file_id: module.file_id,
        items: HashMap::default(),
    };

    Module {
        items: (module.items.iter())
            .filter_map(|item| {
                let (name, universe, item) = compile_item(&context, item, report);
                context.items.insert(name, universe); // Check overwriting?
                item
            })
            .collect(),
    }
}

#[derive(Debug, Copy, Clone)]
enum Universe {
    Kind,
    Format,
}

struct ModuleContext {
    _file_id: FileId,
    items: HashMap<String, Universe>,
}

fn compile_item<'item>(
    context: &ModuleContext,
    item: &'item core::Item,
    report: &mut dyn FnMut(Diagnostic),
) -> (String, Universe, Option<Item>) {
    match item {
        core::Item::Alias(alias) => {
            let (universe, item) = match compile_term_as_format_ty(context, &alias.term, report) {
                None => (Universe::Kind, None),
                Some(ty) => (
                    Universe::Format,
                    Some(Item::TypeAlias(TypeAlias {
                        doc: alias.doc.clone(),
                        name: alias.name.0.clone(),
                        ty,
                    })),
                ),
            };

            (alias.name.0.clone(), universe, item)
        }
        core::Item::Struct(struct_ty) => (
            struct_ty.name.0.clone(),
            Universe::Format,
            Some(Item::Struct(StructType {
                doc: struct_ty.doc.clone(),
                name: struct_ty.name.0.clone(),
                fields: (struct_ty.fields.iter())
                    .map(|field| compile_field_ty(context, field, report))
                    .collect::<Vec<_>>(),
            })),
        ),
    }
}

fn compile_field_ty<'field>(
    context: &ModuleContext,
    field: &'field core::TypeField,
    report: &mut dyn FnMut(Diagnostic),
) -> TypeField {
    TypeField {
        doc: field.doc.clone(),
        name: field.name.0.clone(),
        format_ty: compile_term_as_format_ty(context, &field.term, report)
            .unwrap_or_else(|| Type::Ident("ddl_rt::InvalidDataDescription".into())),
        host_ty: compile_term_as_host_ty(context, &field.term, report)
            .unwrap_or_else(|| Type::Ident("ddl_rt::InvalidDataDescription".into())),
    }
}

fn compile_term_as_format_ty<'term>(
    context: &ModuleContext,
    term: &'term core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> Option<Type> {
    match term {
        core::Term::Item(_, label) => {
            match context.items.get(&label.0) {
                Some(Universe::Format) => Some(Type::Ident(label.0.clone().into())),
                Some(Universe::Kind) => None,
                // TODO: report a bug!
                None => Some(Type::Ident("ddl_rt::InvalidDataDescription".into())),
            }
        }
        core::Term::Ann(term, _) => compile_term_as_format_ty(context, term, report),
        core::Term::U8(_) => Some(Type::Ident("ddl_rt::U8".into())),
        core::Term::U16Le(_) => Some(Type::Ident("ddl_rt::U16Le".into())),
        core::Term::U16Be(_) => Some(Type::Ident("ddl_rt::U16Be".into())),
        core::Term::U32Le(_) => Some(Type::Ident("ddl_rt::U32Le".into())),
        core::Term::U32Be(_) => Some(Type::Ident("ddl_rt::U32Be".into())),
        core::Term::U64Le(_) => Some(Type::Ident("ddl_rt::U64Le".into())),
        core::Term::U64Be(_) => Some(Type::Ident("ddl_rt::U64Be".into())),
        core::Term::S8(_) => Some(Type::Ident("ddl_rt::I8".into())),
        core::Term::S16Le(_) => Some(Type::Ident("ddl_rt::I16Le".into())),
        core::Term::S16Be(_) => Some(Type::Ident("ddl_rt::I16Be".into())),
        core::Term::S32Le(_) => Some(Type::Ident("ddl_rt::I32Le".into())),
        core::Term::S32Be(_) => Some(Type::Ident("ddl_rt::I32Be".into())),
        core::Term::S64Le(_) => Some(Type::Ident("ddl_rt::I64Le".into())),
        core::Term::S64Be(_) => Some(Type::Ident("ddl_rt::I64Be".into())),
        core::Term::F32Le(_) => Some(Type::Ident("ddl_rt::F32Le".into())),
        core::Term::F32Be(_) => Some(Type::Ident("ddl_rt::F32Be".into())),
        core::Term::F64Le(_) => Some(Type::Ident("ddl_rt::F64Le".into())),
        core::Term::F64Be(_) => Some(Type::Ident("ddl_rt::F64Be".into())),
        core::Term::Kind(_) | core::Term::Type(_) => None,
        core::Term::Error(_) => Some(Type::Ident("ddl_rt::InvalidDataDescription".into())),
    }
}

fn compile_term_as_host_ty<'term>(
    context: &ModuleContext,
    term: &'term core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> Option<Type> {
    match term {
        core::Term::Item(_, label) => {
            match context.items.get(&label.0) {
                // Format types are reused as host types, so this is ok!
                Some(Universe::Format) => Some(Type::Ident(label.0.clone().into())),
                Some(Universe::Kind) => None,
                // TODO: report a bug!
                None => Some(Type::Ident("ddl_rt::InvalidDataDescription".into())),
            }
        }
        core::Term::Ann(term, _) => compile_term_as_host_ty(context, term, report),
        core::Term::U8(_) => Some(Type::Ident("u8".into())),
        core::Term::U16Le(_) => Some(Type::Ident("u16".into())),
        core::Term::U16Be(_) => Some(Type::Ident("u16".into())),
        core::Term::U32Le(_) => Some(Type::Ident("u32".into())),
        core::Term::U32Be(_) => Some(Type::Ident("u32".into())),
        core::Term::U64Le(_) => Some(Type::Ident("u64".into())),
        core::Term::U64Be(_) => Some(Type::Ident("u64".into())),
        core::Term::S8(_) => Some(Type::Ident("i8".into())),
        core::Term::S16Le(_) => Some(Type::Ident("i16".into())),
        core::Term::S16Be(_) => Some(Type::Ident("i16".into())),
        core::Term::S32Le(_) => Some(Type::Ident("i32".into())),
        core::Term::S32Be(_) => Some(Type::Ident("i32".into())),
        core::Term::S64Le(_) => Some(Type::Ident("i64".into())),
        core::Term::S64Be(_) => Some(Type::Ident("i64".into())),
        core::Term::F32Le(_) => Some(Type::Ident("f32".into())),
        core::Term::F32Be(_) => Some(Type::Ident("f32".into())),
        core::Term::F64Le(_) => Some(Type::Ident("f64".into())),
        core::Term::F64Be(_) => Some(Type::Ident("f64".into())),
        core::Term::Kind(_) | core::Term::Type(_) => None,
        core::Term::Error(_) => Some(Type::Ident("ddl_rt::InvalidDataDescription".into())),
    }
}

#[allow(dead_code)]
fn host_int(min: &BigInt, max: &BigInt) -> Option<Type> {
    use std::{i16, i32, i64, i8, u16, u32, u64, u8};

    match () {
        () if *min >= u8::MIN.into() && *max <= u8::MAX.into() => Some(Type::Ident("u8".into())),
        () if *min >= u16::MIN.into() && *max <= u16::MAX.into() => Some(Type::Ident("u16".into())),
        () if *min >= u32::MIN.into() && *max <= u32::MAX.into() => Some(Type::Ident("u32".into())),
        () if *min >= u64::MIN.into() && *max <= u64::MAX.into() => Some(Type::Ident("u64".into())),
        () if *min >= i8::MIN.into() && *max <= i8::MAX.into() => Some(Type::Ident("i8".into())),
        () if *min >= i16::MIN.into() && *max <= i16::MAX.into() => Some(Type::Ident("i16".into())),
        () if *min >= i32::MIN.into() && *max <= i32::MAX.into() => Some(Type::Ident("i32".into())),
        () if *min >= i64::MIN.into() && *max <= i64::MAX.into() => Some(Type::Ident("i64".into())),
        () if min > max => None, // Impossible range
        _ => None,               // TODO: use bigint if outside bounds
    }
}
