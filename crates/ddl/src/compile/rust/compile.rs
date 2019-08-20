use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use num_bigint::BigInt;
use std::collections::HashMap;

use crate::compile::diagnostics;
use crate::compile::rust::{Item, Module, RtType, StructType, Type, TypeAlias, TypeField};
use crate::core;

pub fn compile_module(module: &core::Module, report: &mut dyn FnMut(Diagnostic)) -> Module {
    let mut context = ModuleContext {
        file_id: module.file_id,
        items: HashMap::new(),
    };

    let items = module.items.iter().filter_map(|core_item| {
        use std::collections::hash_map::Entry;

        let (label, universe, item) = compile_item(&context, core_item, report);
        match context.items.entry(label) {
            Entry::Occupied(entry) => {
                report(diagnostics::bug::item_name_reused(
                    context.file_id,
                    entry.key(),
                    core_item.span(),
                    entry.get().0,
                ));
                None
            }
            Entry::Vacant(entry) => {
                entry.insert((core_item.span(), universe));
                item
            }
        }
    });

    Module {
        items: items.collect(),
    }
}

#[derive(Debug, Copy, Clone)]
enum Universe {
    FormatType,
    HostType,
    Erased,
    Error,
}

struct ModuleContext {
    file_id: FileId,
    items: HashMap<core::Label, (Span, Universe)>,
}

fn compile_item(
    context: &ModuleContext,
    core_item: &core::Item,
    report: &mut dyn FnMut(Diagnostic),
) -> (core::Label, Universe, Option<Item>) {
    match core_item {
        core::Item::Alias(core_alias) => compile_alias(context, core_alias, report),
        core::Item::Struct(core_struct_ty) => compile_struct_ty(context, core_struct_ty, report),
    }
}

fn compile_alias(
    context: &ModuleContext,
    core_alias: &core::Alias,
    report: &mut dyn FnMut(Diagnostic),
) -> (core::Label, Universe, Option<Item>) {
    let name = core_alias.name.clone();
    let ty_alias = |ty| {
        Item::TypeAlias(TypeAlias {
            doc: core_alias.doc.clone(),
            name: core_alias.name.0.clone(),
            ty,
        })
    };

    match compile_term(context, &core_alias.term, report) {
        CompiledTerm::Erased => (name, Universe::Erased, None),
        CompiledTerm::Error => (name, Universe::Error, None),
        CompiledTerm::FormatType(ty, _) => (name, Universe::FormatType, Some(ty_alias(ty))),
        CompiledTerm::HostType(ty) => (name, Universe::HostType, Some(ty_alias(ty))),
    }
}

fn compile_struct_ty(
    context: &ModuleContext,
    core_struct_ty: &core::StructType,
    report: &mut dyn FnMut(Diagnostic),
) -> (core::Label, Universe, Option<Item>) {
    const INVALID_TYPE: Type = Type::Rt(RtType::InvalidDataDescription);

    let mut fields = Vec::with_capacity(core_struct_ty.fields.len());

    for field in &core_struct_ty.fields {
        let (format_ty, host_ty) = match compile_term(context, &field.term, report) {
            CompiledTerm::FormatType(format_ty, host_ty) => (format_ty, host_ty),
            CompiledTerm::HostType(_) => {
                report(diagnostics::warning::host_type_found_in_field(
                    context.file_id,
                    core_struct_ty.span,
                    field.term.span(),
                ));
                return (core_struct_ty.name.clone(), Universe::Error, None);
            }
            CompiledTerm::Erased => {
                report(diagnostics::bug::non_format_type_as_host_type(
                    context.file_id,
                    field.term.span(),
                ));
                (INVALID_TYPE, INVALID_TYPE)
            }
            CompiledTerm::Error => (INVALID_TYPE, INVALID_TYPE),
        };

        fields.push(TypeField {
            doc: field.doc.clone(),
            name: field.name.0.clone(),
            format_ty,
            host_ty,
        })
    }

    (
        core_struct_ty.name.clone(),
        Universe::FormatType,
        Some(Item::Struct(StructType {
            doc: core_struct_ty.doc.clone(),
            name: core_struct_ty.name.0.clone(),
            fields,
        })),
    )
}

enum CompiledTerm {
    FormatType(Type, Type),
    HostType(Type),
    Erased,
    Error,
}

fn compile_term(
    context: &ModuleContext,
    core_term: &core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> CompiledTerm {
    let file_id = context.file_id;

    match core_term {
        core::Term::Item(span, label) => match context.items.get(label) {
            Some((_, Universe::FormatType)) => {
                CompiledTerm::FormatType(Type::Var(label.0.clone()), Type::Var(label.0.clone()))
            }
            Some((_, Universe::HostType)) => CompiledTerm::HostType(Type::Var(label.0.clone())),
            Some((_, Universe::Erased)) => CompiledTerm::Erased,
            Some((_, Universe::Error)) => CompiledTerm::Error,
            None => {
                report(diagnostics::bug::unbound_item(file_id, label, *span));
                CompiledTerm::Error
            }
        },
        core::Term::Ann(term, _) => compile_term(context, term, report),
        core::Term::U8Type(_) => CompiledTerm::FormatType(Type::Rt(RtType::U8), Type::U8),
        core::Term::U16LeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::U16Le), Type::U16),
        core::Term::U16BeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::U16Be), Type::U16),
        core::Term::U32LeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::U32Le), Type::U32),
        core::Term::U32BeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::U32Be), Type::U32),
        core::Term::U64LeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::U64Le), Type::U64),
        core::Term::U64BeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::U64Be), Type::U64),
        core::Term::S8Type(_) => CompiledTerm::FormatType(Type::Rt(RtType::I8), Type::I8),
        core::Term::S16LeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::I16Le), Type::I16),
        core::Term::S16BeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::I16Be), Type::I16),
        core::Term::S32LeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::I32Le), Type::I32),
        core::Term::S32BeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::I32Be), Type::I32),
        core::Term::S64LeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::I64Le), Type::I64),
        core::Term::S64BeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::I64Be), Type::I64),
        core::Term::F32LeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::F32Le), Type::F32),
        core::Term::F32BeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::F32Be), Type::F32),
        core::Term::F64LeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::F64Le), Type::F64),
        core::Term::F64BeType(_) => CompiledTerm::FormatType(Type::Rt(RtType::F64Be), Type::F64),
        core::Term::BoolType(_) => CompiledTerm::HostType(Type::Bool),
        core::Term::IntType(span) => {
            report(diagnostics::error::unconstrained_int(file_id, *span));
            CompiledTerm::HostType(Type::Rt(RtType::InvalidDataDescription))
        }
        core::Term::F32Type(_) => CompiledTerm::HostType(Type::F32),
        core::Term::F64Type(_) => CompiledTerm::HostType(Type::F64),
        core::Term::Kind(_) | core::Term::Type(_) => CompiledTerm::Erased,
        core::Term::Error(_) => CompiledTerm::Error,
    }
}

#[allow(dead_code)]
fn host_int(min: &BigInt, max: &BigInt) -> Option<Type> {
    use std::{i16, i32, i64, i8, u16, u32, u64, u8};

    match () {
        () if *min >= u8::MIN.into() && *max <= u8::MAX.into() => Some(Type::U8),
        () if *min >= u16::MIN.into() && *max <= u16::MAX.into() => Some(Type::U16),
        () if *min >= u32::MIN.into() && *max <= u32::MAX.into() => Some(Type::U32),
        () if *min >= u64::MIN.into() && *max <= u64::MAX.into() => Some(Type::U64),
        () if *min >= i8::MIN.into() && *max <= i8::MAX.into() => Some(Type::I8),
        () if *min >= i16::MIN.into() && *max <= i16::MAX.into() => Some(Type::I16),
        () if *min >= i32::MIN.into() && *max <= i32::MAX.into() => Some(Type::I32),
        () if *min >= i64::MIN.into() && *max <= i64::MAX.into() => Some(Type::I64),
        () if min > max => None, // Impossible range
        _ => None,               // TODO: use bigint if outside bounds
    }
}
