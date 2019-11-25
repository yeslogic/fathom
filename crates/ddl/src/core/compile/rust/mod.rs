use codespan::{FileId, Span};
use codespan_reporting::diagnostic::Diagnostic;
use inflector::Inflector;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use std::collections::HashMap;
use std::sync::Arc;

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

#[derive(Debug, Clone)]
enum CompiledItem {
    Term {
        span: Span,
        name: String,
        ty: rust::Type,
        is_function: bool,
        is_const: bool,
    },
    Type {
        span: Span,
        name: String,
        is_copy: bool,
        host_ty: Option<rust::Type>,
    },
    Erased(Span),
    Error(Span),
}

impl CompiledItem {
    fn span(&self) -> Span {
        match self {
            CompiledItem::Term { span, .. }
            | CompiledItem::Type { span, .. }
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
        CompiledTerm::Term { term, ty, is_const } => {
            let doc = core_alias.doc.clone();
            if is_const {
                let name = core_alias.name.0.to_screaming_snake_case(); // TODO: name avoidance
                (
                    core_alias.name.clone(),
                    CompiledItem::Term {
                        span,
                        name: name.clone(),
                        ty: ty.clone(),
                        is_function: false,
                        is_const,
                    },
                    Some(rust::Item::Const(rust::Const {
                        doc,
                        name,
                        ty,
                        term,
                    })),
                )
            } else {
                let name = core_alias.name.0.to_snake_case(); // TODO: name avoidance
                (
                    core_alias.name.clone(),
                    CompiledItem::Term {
                        span,
                        name: name.clone(),
                        ty: ty.clone(),
                        is_function: true,
                        is_const,
                    },
                    Some(rust::Item::Function(rust::Function {
                        doc,
                        name,
                        is_const,
                        ty,
                        term,
                    })),
                )
            }
        }
        CompiledTerm::Erased => (core_alias.name.clone(), CompiledItem::Erased(span), None),
        CompiledTerm::Error => (core_alias.name.clone(), CompiledItem::Error(span), None),
        CompiledTerm::Type {
            ty,
            is_copy,
            host_ty,
        } => {
            let doc = core_alias.doc.clone();
            let name = core_alias.name.0.to_pascal_case(); // TODO: name avoidance
            let mut derives = Vec::new();
            if is_copy {
                derives.push("Copy".to_owned());
                derives.push("Clone".to_owned());
            }

            match ty {
                ty @ rust::Type::If(_, _, _) => match host_ty {
                    None => unreachable!("type level if for non-format type"),
                    Some(host_ty) => (
                        core_alias.name.clone(),
                        CompiledItem::Type {
                            span,
                            name: name.clone(),
                            is_copy,
                            host_ty: Some(rust::Type::Var(name.clone())),
                        },
                        Some(rust::Item::Struct(rust::StructType {
                            derives,
                            doc,
                            name,
                            fields: vec![rust::TypeField {
                                doc: Arc::new([]),
                                name: "inner".to_owned(),
                                format_ty: ty,
                                host_ty,
                                by_ref: !is_copy,
                            }],
                        })),
                    ),
                },
                ty => (
                    core_alias.name.clone(),
                    CompiledItem::Type {
                        span,
                        name: name.clone(),
                        is_copy,
                        host_ty,
                    },
                    Some(rust::Item::Alias(rust::Alias { doc, name, ty })),
                ),
            }
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

    let mut is_copy = true;
    let mut fields = Vec::with_capacity(core_struct_ty.fields.len());

    for field in &core_struct_ty.fields {
        let (format_ty, host_ty, is_field_copy) = match compile_term(context, &field.term, report) {
            CompiledTerm::Term { .. } => {
                // TODO: Bug!
                return error(field);
            }
            CompiledTerm::Type {
                ty,
                is_copy,
                host_ty,
            } => match &host_ty {
                Some(host_ty) => (ty, host_ty.clone(), is_copy),
                None => {
                    report(diagnostics::bug::host_type_found_in_field(
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
                (INVALID_TYPE, INVALID_TYPE, true)
            }
            CompiledTerm::Error => (INVALID_TYPE, INVALID_TYPE, true),
        };

        is_copy &= is_field_copy;
        fields.push(rust::TypeField {
            doc: field.doc.clone(),
            name: field.name.0.clone(),
            format_ty,
            host_ty,
            by_ref: !is_field_copy,
        })
    }

    let doc = core_struct_ty.doc.clone();
    let name = core_struct_ty.name.0.to_pascal_case(); // TODO: name avoidance
    let mut derives = Vec::new();
    if is_copy {
        derives.push("Copy".to_owned());
        derives.push("Clone".to_owned());
    }

    (
        core_struct_ty.name.clone(),
        CompiledItem::Type {
            span: core_struct_ty.span,
            name: name.clone(),
            is_copy,
            host_ty: Some(rust::Type::Var(name.clone())),
        },
        Some(rust::Item::Struct(rust::StructType {
            derives,
            doc,
            name,
            fields,
        })),
    )
}

enum CompiledTerm {
    Term {
        term: rust::Term,
        ty: rust::Type,
        is_const: bool,
    },
    Type {
        ty: rust::Type,
        is_copy: bool,
        host_ty: Option<rust::Type>,
    },
    Erased,
    Error,
}

fn compile_term(
    context: &ModuleContext,
    core_term: &core::Term,
    report: &mut dyn FnMut(Diagnostic),
) -> CompiledTerm {
    let file_id = context.file_id;

    let host_ty = |ty| CompiledTerm::Type {
        ty,
        is_copy: true,
        host_ty: None,
    };
    let format_ty = |ty, host_ty| CompiledTerm::Type {
        ty,
        is_copy: true,
        host_ty: Some(host_ty),
    };

    match core_term {
        core::Term::Item(span, label) => match context.items.get(label) {
            Some(CompiledItem::Term {
                name,
                ty,
                is_function,
                is_const,
                ..
            }) => CompiledTerm::Term {
                term: if *is_function {
                    rust::Term::Call(Box::new(rust::Term::Var(name.clone())))
                } else {
                    rust::Term::Var(name.clone())
                },
                ty: ty.clone(),
                is_const: *is_const,
            },
            Some(CompiledItem::Type {
                name,
                is_copy,
                host_ty,
                ..
            }) => CompiledTerm::Type {
                ty: rust::Type::Var(name.clone()),
                is_copy: *is_copy,
                host_ty: host_ty.clone(),
            },
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
        core::Term::BoolType(_) => host_ty(rust::Type::Bool),
        core::Term::IntType(span) => {
            report(diagnostics::error::unconstrained_int(file_id, *span));
            host_ty(rust::Type::Rt(rust::RtType::InvalidDataDescription))
        }
        core::Term::F32Type(_) => host_ty(rust::Type::F32),
        core::Term::F64Type(_) => host_ty(rust::Type::F64),
        core::Term::BoolConst(_, value) => CompiledTerm::Term {
            term: rust::Term::Bool(*value),
            ty: rust::Type::Bool,
            is_const: true,
        },
        core::Term::IntConst(span, value) => {
            match value.to_i64() {
                // TODO: don't default to I64.
                Some(value) => CompiledTerm::Term {
                    term: rust::Term::I64(value),
                    ty: rust::Type::I64,
                    is_const: true,
                },
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
        core::Term::F32Const(_, value) => CompiledTerm::Term {
            term: rust::Term::F32(*value),
            ty: rust::Type::F32,
            is_const: true,
        },
        core::Term::F64Const(_, value) => CompiledTerm::Term {
            term: rust::Term::F64(*value),
            ty: rust::Type::F64,
            is_const: true,
        },
        core::Term::BoolElim(span, head, if_true, if_false) => {
            let head = match compile_term(context, head, report) {
                CompiledTerm::Term { term: head, .. } => head,
                // TODO: Error
                _ => return CompiledTerm::Error,
            };

            match (
                compile_term(context, if_true, report),
                compile_term(context, if_false, report),
            ) {
                (
                    CompiledTerm::Term {
                        term: if_true,
                        ty: if_true_ty,
                        ..
                    },
                    CompiledTerm::Term { term: if_false, .. },
                ) => CompiledTerm::Term {
                    term: rust::Term::If(Box::new(head), Box::new(if_true), Box::new(if_false)),
                    ty: if_true_ty, // TODO: check if arms match
                    is_const: false,
                },
                (
                    CompiledTerm::Type {
                        ty: true_ty,
                        is_copy: true_is_copy,
                        host_ty: true_host_ty,
                    },
                    CompiledTerm::Type {
                        ty: false_ty,
                        is_copy: false_is_copy,
                        host_ty: false_host_ty,
                    },
                ) => match (true_host_ty, false_host_ty) {
                    (Some(true_host_ty), Some(false_host_ty)) => CompiledTerm::Type {
                        ty: rust::Type::If(Box::new(head), Box::new(true_ty), Box::new(false_ty)),
                        is_copy: true_is_copy && false_is_copy,
                        host_ty: Some(rust::Type::Rt(rust::RtType::Either(
                            Box::new(true_host_ty),
                            Box::new(false_host_ty),
                        ))),
                    },
                    (_, _) => {
                        report(diagnostics::error::type_level_if_expression(file_id, *span));
                        CompiledTerm::Error
                    }
                },

                (CompiledTerm::Erased, CompiledTerm::Erased) => CompiledTerm::Erased,
                (CompiledTerm::Error, _) | (_, CompiledTerm::Error) => CompiledTerm::Error,

                // TODO: report bug: mismatched arms of if expression
                (_, _) => unimplemented!(),
            }
        }
        core::Term::IntElim(span, head, branches, default) => {
            let head = match compile_term(context, head, report) {
                CompiledTerm::Term { term: head, .. } => head,
                // TODO: Error
                _ => return CompiledTerm::Error,
            };

            match compile_term(context, default, report) {
                CompiledTerm::Term {
                    term: default,
                    ty: default_ty,
                    ..
                } => {
                    let branches = branches
                        .iter()
                        .filter_map(|(value, term)| match value.to_i64() {
                            Some(value) => Some((
                                rust::Pattern::I64(value),
                                match compile_term(context, term, report) {
                                    CompiledTerm::Term { term, .. } => term,
                                    // TODO: report bug: mismatched arms of match expression
                                    _ => rust::Term::Panic("error term".to_owned()),
                                },
                            )),
                            None => {
                                report(crate::diagnostics::bug::not_yet_implemented(
                                    context.file_id,
                                    *span,
                                    "non-i64 patterns",
                                ));
                                None
                            }
                        })
                        .chain(std::iter::once((
                            // TODO: Use pattern name
                            rust::Pattern::Name("_".to_owned()),
                            default,
                        )))
                        .collect();

                    CompiledTerm::Term {
                        term: rust::Term::Match(Box::new(head), branches),
                        ty: default_ty,
                        is_const: false,
                    }
                }
                CompiledTerm::Type {
                    ty: _default_ty,
                    is_copy: _default_is_copy,
                    host_ty: _default_host_ty,
                } => {
                    report(crate::diagnostics::bug::not_yet_implemented(
                        context.file_id,
                        *span,
                        "type-level match expressions",
                    ));
                    CompiledTerm::Error
                }
                CompiledTerm::Erased => CompiledTerm::Erased,
                CompiledTerm::Error => CompiledTerm::Error,
            }
        }
        core::Term::Universe(_, _) => CompiledTerm::Erased,
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
