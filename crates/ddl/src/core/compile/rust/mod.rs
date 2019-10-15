use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use inflector::Inflector;
use num_bigint::BigInt;
use std::collections::HashMap;

use crate::{core, rust};

mod diagnostics;

pub fn compile_module(module: &core::Module, report: &mut dyn FnMut(Diagnostic)) -> rust::Module {
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

    rust::Module {
        doc: module.doc.clone(),
        items: items.collect(),
    }
}

#[derive(Debug, Copy, Clone)]
struct CopyTrait;

#[derive(Debug, Clone)]
struct BinaryTrait {
    host_ty: rust::Type,
}

#[derive(Debug, Clone)]
struct Traits {
    copy: Option<CopyTrait>,
    binary: Option<BinaryTrait>,
}

#[derive(Debug, Clone)]
enum CompiledItem {
    Term(Span, String, rust::Type),
    Type(Span, String, Traits),
    Erased(Span),
    Error(Span),
}

impl CompiledItem {
    fn span(&self) -> Span {
        match self {
            CompiledItem::Term(span, _, _)
            | CompiledItem::Type(span, _, _)
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
) -> (core::Label, CompiledItem, Option<rust::Item>) {
    match core_item {
        core::Item::Alias(core_alias) => compile_alias(context, core_alias, report),
        core::Item::Struct(core_struct_ty) => compile_struct_ty(context, core_struct_ty, report),
    }
}

fn compile_alias(
    context: &ModuleContext,
    core_alias: &core::Alias,
    report: &mut dyn FnMut(Diagnostic),
) -> (core::Label, CompiledItem, Option<rust::Item>) {
    let span = core_alias.span;
    match compile_term(context, &core_alias.term, report) {
        CompiledTerm::Term(term, ty) => {
            let doc = core_alias.doc.clone();
            let name = core_alias.name.0.to_screaming_snake_case(); // TODO: name avoidance

            (
                core_alias.name.clone(),
                CompiledItem::Term(span, name.clone(), ty.clone()),
                Some(rust::Item::Const(rust::Const {
                    doc,
                    name,
                    ty,
                    term,
                })),
            )
        }
        CompiledTerm::Erased => (core_alias.name.clone(), CompiledItem::Erased(span), None),
        CompiledTerm::Error => (core_alias.name.clone(), CompiledItem::Error(span), None),
        CompiledTerm::Type(ty, traits) => {
            let doc = core_alias.doc.clone();
            let name = core_alias.name.0.to_pascal_case(); // TODO: name avoidance

            (
                core_alias.name.clone(),
                CompiledItem::Type(span, name.clone(), traits),
                Some(rust::Item::TypeAlias(rust::TypeAlias { doc, name, ty })),
            )
        }
    }
}

fn compile_struct_ty(
    context: &ModuleContext,
    core_struct_ty: &core::StructType,
    report: &mut dyn FnMut(Diagnostic),
) -> (core::Label, CompiledItem, Option<rust::Item>) {
    const INVALID_TYPE: rust::Type = rust::Type::Rt(rust::RtType::InvalidDataDescription);
    let error = |field: &core::TypeField| {
        (
            core_struct_ty.name.clone(),
            CompiledItem::Error(field.span()),
            None,
        )
    };

    let mut copy = Some(CopyTrait);
    let mut fields = Vec::with_capacity(core_struct_ty.fields.len());

    for field in &core_struct_ty.fields {
        let (format_ty, host_ty, field_copy) = match compile_term(context, &field.term, report) {
            CompiledTerm::Term(_, _) => {
                // TODO: Bug!
                return error(field);
            }
            CompiledTerm::Type(ty, traits) => match &traits.binary {
                Some(binary) => (ty, binary.host_ty.clone(), traits.copy),
                None => {
                    report(diagnostics::warning::host_type_found_in_field(
                        context.file_id,
                        core_struct_ty.span,
                        field.term.span(),
                    ));
                    return error(field);
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
        fields.push(rust::TypeField {
            doc: field.doc.clone(),
            name: field.name.0.clone(),
            format_ty,
            host_ty,
        })
    }

    let doc = core_struct_ty.doc.clone();
    let name = core_struct_ty.name.0.to_pascal_case(); // TODO: name avoidance
    let mut derives = Vec::new();
    if copy.is_some() {
        derives.push("Copy".to_owned());
        derives.push("Clone".to_owned());
    }
    let binary = Some(BinaryTrait {
        host_ty: rust::Type::Var(name.clone()),
    });

    (
        core_struct_ty.name.clone(),
        CompiledItem::Type(core_struct_ty.span, name.clone(), Traits { copy, binary }),
        Some(rust::Item::Struct(rust::StructType {
            derives,
            doc,
            name,
            fields,
        })),
    )
}

enum CompiledTerm {
    Term(rust::Term, rust::Type),
    Type(rust::Type, Traits),
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
            Some(CompiledItem::Term(_, name, ty)) => {
                CompiledTerm::Term(rust::Term::Var(name.clone()), ty.clone())
            }
            Some(CompiledItem::Type(_, ty_name, traits)) => {
                CompiledTerm::Type(rust::Type::Var(ty_name.clone()), traits.clone())
            }
            Some(CompiledItem::Erased(_)) => CompiledTerm::Erased,
            Some(CompiledItem::Error(_)) => CompiledTerm::Error,
            None => {
                report(diagnostics::bug::unbound_item(file_id, label, *span));
                CompiledTerm::Error
            }
        },
        core::Term::Ann(term, _) => compile_term(context, term, report),
        core::Term::U8Type(_) => format_ty(rust::Type::Rt(rust::RtType::U8), rust::Type::U8),
        core::Term::U16LeType(_) => format_ty(rust::Type::Rt(rust::RtType::U16Le), rust::Type::U16),
        core::Term::U16BeType(_) => format_ty(rust::Type::Rt(rust::RtType::U16Be), rust::Type::U16),
        core::Term::U32LeType(_) => format_ty(rust::Type::Rt(rust::RtType::U32Le), rust::Type::U32),
        core::Term::U32BeType(_) => format_ty(rust::Type::Rt(rust::RtType::U32Be), rust::Type::U32),
        core::Term::U64LeType(_) => format_ty(rust::Type::Rt(rust::RtType::U64Le), rust::Type::U64),
        core::Term::U64BeType(_) => format_ty(rust::Type::Rt(rust::RtType::U64Be), rust::Type::U64),
        core::Term::S8Type(_) => format_ty(rust::Type::Rt(rust::RtType::I8), rust::Type::I8),
        core::Term::S16LeType(_) => format_ty(rust::Type::Rt(rust::RtType::I16Le), rust::Type::I16),
        core::Term::S16BeType(_) => format_ty(rust::Type::Rt(rust::RtType::I16Be), rust::Type::I16),
        core::Term::S32LeType(_) => format_ty(rust::Type::Rt(rust::RtType::I32Le), rust::Type::I32),
        core::Term::S32BeType(_) => format_ty(rust::Type::Rt(rust::RtType::I32Be), rust::Type::I32),
        core::Term::S64LeType(_) => format_ty(rust::Type::Rt(rust::RtType::I64Le), rust::Type::I64),
        core::Term::S64BeType(_) => format_ty(rust::Type::Rt(rust::RtType::I64Be), rust::Type::I64),
        core::Term::F32LeType(_) => format_ty(rust::Type::Rt(rust::RtType::F32Le), rust::Type::F32),
        core::Term::F32BeType(_) => format_ty(rust::Type::Rt(rust::RtType::F32Be), rust::Type::F32),
        core::Term::F64LeType(_) => format_ty(rust::Type::Rt(rust::RtType::F64Le), rust::Type::F64),
        core::Term::F64BeType(_) => format_ty(rust::Type::Rt(rust::RtType::F64Be), rust::Type::F64),
        core::Term::BoolType(_) => host_ty(rust::Type::Bool, Some(CopyTrait)),
        core::Term::IntType(span) => {
            report(diagnostics::error::unconstrained_int(file_id, *span));
            host_ty(rust::Type::Rt(rust::RtType::InvalidDataDescription), None)
        }
        core::Term::F32Type(_) => host_ty(rust::Type::F32, Some(CopyTrait)),
        core::Term::F64Type(_) => host_ty(rust::Type::F64, Some(CopyTrait)),
        core::Term::BoolConst(_, value) => {
            CompiledTerm::Term(rust::Term::Bool(*value), rust::Type::Bool)
        }
        core::Term::IntConst(span, value) => {
            use num_traits::cast::ToPrimitive;

            match value.to_i64() {
                // TODO: don't default to I64.
                Some(value) => CompiledTerm::Term(rust::Term::I64(value), rust::Type::I64),
                None => {
                    report(crate::diagnostics::bug::not_yet_implemented(
                        context.file_id,
                        *span,
                        "non-i64 types",
                    ));
                    CompiledTerm::Error
                }
            }
        }
        core::Term::F32Const(_, value) => {
            CompiledTerm::Term(rust::Term::F32(*value), rust::Type::F32)
        }
        core::Term::F64Const(_, value) => {
            CompiledTerm::Term(rust::Term::F64(*value), rust::Type::F64)
        }
        core::Term::Sort(_, _) => CompiledTerm::Erased,
        core::Term::Error(_) => CompiledTerm::Error,
    }
}

#[allow(dead_code)]
fn host_int(min: &BigInt, max: &BigInt) -> Option<rust::Type> {
    use std::{i16, i32, i64, i8, u16, u32, u64, u8};

    match () {
        () if *min >= u8::MIN.into() && *max <= u8::MAX.into() => Some(rust::Type::U8),
        () if *min >= u16::MIN.into() && *max <= u16::MAX.into() => Some(rust::Type::U16),
        () if *min >= u32::MIN.into() && *max <= u32::MAX.into() => Some(rust::Type::U32),
        () if *min >= u64::MIN.into() && *max <= u64::MAX.into() => Some(rust::Type::U64),
        () if *min >= i8::MIN.into() && *max <= i8::MAX.into() => Some(rust::Type::I8),
        () if *min >= i16::MIN.into() && *max <= i16::MAX.into() => Some(rust::Type::I16),
        () if *min >= i32::MIN.into() && *max <= i32::MAX.into() => Some(rust::Type::I32),
        () if *min >= i64::MIN.into() && *max <= i64::MAX.into() => Some(rust::Type::I64),
        () if min > max => None, // Impossible range
        _ => None,               // TODO: use bigint if outside bounds
    }
}
