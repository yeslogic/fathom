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

        let (label, compiled_item, item) = compile_item(&context, core_item, report);
        match context.items.entry(label) {
            Entry::Occupied(entry) => {
                report(diagnostics::bug::item_name_reused(
                    context.file_id,
                    entry.key(),
                    core_item.span(),
                    entry.get().span(),
                ));
                None
            }
            Entry::Vacant(entry) => {
                entry.insert(compiled_item);
                item
            }
        }
    });

    Module {
        items: items.collect(),
    }
}

#[derive(Debug, Copy, Clone)]
struct CopyTrait;

#[derive(Debug, Clone)]
struct BinaryTrait {
    host_ty: Type,
}

#[derive(Debug, Clone)]
struct Traits {
    copy: Option<CopyTrait>,
    binary: Option<BinaryTrait>,
}

#[derive(Debug, Clone)]
enum CompiledItem {
    // TODO: Term(Span, String, Type),
    Type(Span, String, Traits),
    Erased(Span),
    Error(Span),
}

impl CompiledItem {
    fn span(&self) -> Span {
        match self {
            CompiledItem::Type(span, _, _)
            | CompiledItem::Erased(span)
            | CompiledItem::Error(span) => *span,
        }
    }
}

struct ModuleContext {
    file_id: FileId,
    items: HashMap<core::Label, CompiledItem>,
}

fn compile_item(
    context: &ModuleContext,
    core_item: &core::Item,
    report: &mut dyn FnMut(Diagnostic),
) -> (core::Label, CompiledItem, Option<Item>) {
    match core_item {
        core::Item::Alias(core_alias) => compile_alias(context, core_alias, report),
        core::Item::Struct(core_struct_ty) => compile_struct_ty(context, core_struct_ty, report),
    }
}

fn compile_alias(
    context: &ModuleContext,
    core_alias: &core::Alias,
    report: &mut dyn FnMut(Diagnostic),
) -> (core::Label, CompiledItem, Option<Item>) {
    let span = core_alias.span;
    match compile_term(context, &core_alias.term, report) {
        CompiledTerm::Erased => (core_alias.name.clone(), CompiledItem::Erased(span), None),
        CompiledTerm::Error => (core_alias.name.clone(), CompiledItem::Error(span), None),
        CompiledTerm::Type(ty, traits) => {
            let doc = core_alias.doc.clone();
            let name = core_alias.name.0.clone(); // TODO: PascalCase and name avoidance

            (
                core_alias.name.clone(),
                CompiledItem::Type(span, name.clone(), traits),
                Some(Item::TypeAlias(TypeAlias { doc, name, ty })),
            )
        }
    }
}

fn compile_struct_ty(
    context: &ModuleContext,
    core_struct_ty: &core::StructType,
    report: &mut dyn FnMut(Diagnostic),
) -> (core::Label, CompiledItem, Option<Item>) {
    const INVALID_TYPE: Type = Type::Rt(RtType::InvalidDataDescription);

    let mut copy = Some(CopyTrait);
    let mut fields = Vec::with_capacity(core_struct_ty.fields.len());

    for field in &core_struct_ty.fields {
        let (format_ty, host_ty, field_copy) = match compile_term(context, &field.term, report) {
            CompiledTerm::Type(ty, traits) => match &traits.binary {
                Some(binary) => (ty, binary.host_ty.clone(), traits.copy),
                None => {
                    report(diagnostics::warning::host_type_found_in_field(
                        context.file_id,
                        core_struct_ty.span,
                        field.term.span(),
                    ));
                    return (
                        core_struct_ty.name.clone(),
                        CompiledItem::Error(field.span()),
                        None,
                    );
                }
            },
            CompiledTerm::Erased => {
                report(diagnostics::bug::non_format_type_as_host_type(
                    context.file_id,
                    field.term.span(),
                ));
                (INVALID_TYPE, INVALID_TYPE, None)
            }
            CompiledTerm::Error => (INVALID_TYPE, INVALID_TYPE, None),
        };

        copy = Option::and(copy, field_copy);
        fields.push(TypeField {
            doc: field.doc.clone(),
            name: field.name.0.clone(),
            format_ty,
            host_ty,
        })
    }

    let doc = core_struct_ty.doc.clone();
    let name = core_struct_ty.name.0.clone(); // TODO: PascalCase and name avoidance
    let mut derives = Vec::new();
    if copy.is_some() {
        derives.push("Copy".to_owned());
        derives.push("Clone".to_owned());
    }
    let binary = Some(BinaryTrait {
        host_ty: Type::Var(name.clone()),
    });

    (
        core_struct_ty.name.clone(),
        CompiledItem::Type(core_struct_ty.span, name.clone(), Traits { copy, binary }),
        Some(Item::Struct(StructType {
            derives,
            doc,
            name,
            fields,
        })),
    )
}

enum CompiledTerm {
    Type(Type, Traits),
    Erased,
    Error,
}

fn compile_term(
    context: &ModuleContext,
    core_term: &core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> CompiledTerm {
    let file_id = context.file_id;

    let host_ty = |ty, copy| CompiledTerm::Type(ty, Traits { copy, binary: None });
    let format_ty = |ty, host_ty| {
        let copy = Some(CopyTrait);
        let binary = Some(BinaryTrait { host_ty });
        CompiledTerm::Type(ty, Traits { copy, binary })
    };

    match core_term {
        core::Term::Item(span, label) => match context.items.get(label) {
            Some(CompiledItem::Type(_, ty_name, traits)) => {
                CompiledTerm::Type(Type::Var(ty_name.clone()), traits.clone())
            }
            Some(CompiledItem::Erased(_)) => CompiledTerm::Erased,
            Some(CompiledItem::Error(_)) => CompiledTerm::Error,
            None => {
                report(diagnostics::bug::unbound_item(file_id, label, *span));
                CompiledTerm::Error
            }
        },
        core::Term::Ann(term, _) => compile_term(context, term, report),
        core::Term::U8Type(_) => format_ty(Type::Rt(RtType::U8), Type::U8),
        core::Term::U16LeType(_) => format_ty(Type::Rt(RtType::U16Le), Type::U16),
        core::Term::U16BeType(_) => format_ty(Type::Rt(RtType::U16Be), Type::U16),
        core::Term::U32LeType(_) => format_ty(Type::Rt(RtType::U32Le), Type::U32),
        core::Term::U32BeType(_) => format_ty(Type::Rt(RtType::U32Be), Type::U32),
        core::Term::U64LeType(_) => format_ty(Type::Rt(RtType::U64Le), Type::U64),
        core::Term::U64BeType(_) => format_ty(Type::Rt(RtType::U64Be), Type::U64),
        core::Term::S8Type(_) => format_ty(Type::Rt(RtType::I8), Type::I8),
        core::Term::S16LeType(_) => format_ty(Type::Rt(RtType::I16Le), Type::I16),
        core::Term::S16BeType(_) => format_ty(Type::Rt(RtType::I16Be), Type::I16),
        core::Term::S32LeType(_) => format_ty(Type::Rt(RtType::I32Le), Type::I32),
        core::Term::S32BeType(_) => format_ty(Type::Rt(RtType::I32Be), Type::I32),
        core::Term::S64LeType(_) => format_ty(Type::Rt(RtType::I64Le), Type::I64),
        core::Term::S64BeType(_) => format_ty(Type::Rt(RtType::I64Be), Type::I64),
        core::Term::F32LeType(_) => format_ty(Type::Rt(RtType::F32Le), Type::F32),
        core::Term::F32BeType(_) => format_ty(Type::Rt(RtType::F32Be), Type::F32),
        core::Term::F64LeType(_) => format_ty(Type::Rt(RtType::F64Le), Type::F64),
        core::Term::F64BeType(_) => format_ty(Type::Rt(RtType::F64Be), Type::F64),
        core::Term::BoolType(_) => host_ty(Type::Bool, Some(CopyTrait)),
        core::Term::IntType(span) => {
            report(diagnostics::error::unconstrained_int(file_id, *span));
            host_ty(Type::Rt(RtType::InvalidDataDescription), None)
        }
        core::Term::F32Type(_) => host_ty(Type::F32, Some(CopyTrait)),
        core::Term::F64Type(_) => host_ty(Type::F64, Some(CopyTrait)),
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
